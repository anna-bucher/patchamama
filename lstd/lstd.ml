
module Fmt = struct

  (* Formatting *)

  let pf = Format.fprintf
  let pr = Format.printf
  let epr = Format.eprintf
  let str = Format.asprintf
  let kpf = Format.kfprintf
  let kstr = Format.kasprintf
  let failwith fmt = kstr failwith fmt
  let failwith_notrace fmt = kstr (fun s -> raise_notrace (Failure s)) fmt
  let invalid_arg fmt = kstr invalid_arg fmt
  let error fmt = kstr (fun s -> Error s) fmt

  (* Formatters *)

  type 'a t = Format.formatter -> 'a -> unit

  let flush ppf _ = Format.pp_print_flush ppf ()
  let flush_nl ppf _ = Format.pp_print_newline ppf ()
  let nop _ppf _ = ()
  let any fmt ppf _ = pf ppf fmt
  let using f pp_v ppf v = pp_v ppf (f v)

  (* Separators *)

  let cut ppf _ = Format.pp_print_cut ppf ()
  let sp ppf _ = Format.pp_print_space ppf ()
  let sps n ppf _ = Format.pp_print_break ppf n 0
  let comma ppf _ = Format.pp_print_string ppf ","; sp ppf ()
  let semi ppf _ = Format.pp_print_string ppf ";"; sp ppf ()

  (* Sequencing *)

  let iter ?sep:(pp_sep = cut) iter pp_elt ppf v =
    let is_first = ref true in
    let pp_elt v =
      if !is_first then (is_first := false) else pp_sep ppf ();
      pp_elt ppf v
    in
    iter pp_elt v

  let iter_bindings ?sep:(pp_sep = cut) iter pp_binding ppf v =
    let is_first = ref true in
    let pp_binding k v =
      if !is_first then (is_first := false) else pp_sep ppf ();
      pp_binding ppf (k, v)
    in
    iter pp_binding v

  let append pp_v0 pp_v1 ppf v = pp_v0 ppf v; pp_v1 ppf v
  let ( ++ ) = append
  let concat ?sep pps ppf v =
    iter ?sep List.iter (fun ppf pp -> pp ppf v) ppf pps

  (* Boxes *)

  let box ?(indent = 0) pp_v ppf v =
    Format.(pp_open_box ppf indent; pp_v ppf v; pp_close_box ppf ())

  let hbox pp_v ppf v =
    Format.(pp_open_hbox ppf (); pp_v ppf v; pp_close_box ppf ())

  let vbox ?(indent = 0) pp_v ppf v =
    Format.(pp_open_vbox ppf indent; pp_v ppf v; pp_close_box ppf ())

  let hvbox ?(indent = 0) pp_v ppf v =
    Format.(pp_open_hvbox ppf indent; pp_v ppf v; pp_close_box ppf ())

  let hovbox ?(indent = 0) pp_v ppf v =
    Format.(pp_open_hovbox ppf indent; pp_v ppf v; pp_close_box ppf ())

  (* Brackets *)

  let surround s1 s2 pp_v ppf v =
    Format.(pp_print_string ppf s1; pp_v ppf v; pp_print_string ppf s2)

  let parens pp_v = box ~indent:1 (surround "(" ")" pp_v)
  let brackets pp_v = box ~indent:1 (surround "[" "]" pp_v)
  let oxford_brackets pp_v = box ~indent:2 (surround "[|" "|]" pp_v)
  let braces pp_v = box ~indent:1 (surround "{" "}" pp_v)
  let quote ?(mark = "\"") pp_v =
    let pp_mark ppf _ = Format.pp_print_as ppf 1 mark in
    box ~indent:1 (pp_mark ++ pp_v ++ pp_mark)

  (* Stdlib formatters *)

  let bool = Format.pp_print_bool
  let int = Format.pp_print_int
  let int32 ppf i = pf ppf "%ld" i
  let int64 ppf i = pf ppf "%Ld" i
  let float ppf f = pf ppf "%g" f
  let char = Format.pp_print_char
  let string = Format.pp_print_string
  let sys_signal ppf snum =
    let sigs = [
      Sys.sigabrt, "SIGABRT"; Sys.sigalrm, "SIGALRM"; Sys.sigfpe, "SIGFPE";
      Sys.sighup, "SIGHUP"; Sys.sigill, "SIGILL"; Sys.sigint, "SIGINT";
      Sys.sigkill, "SIGKILL"; Sys.sigpipe, "SIGPIPE"; Sys.sigquit, "SIGQUIT";
      Sys.sigsegv, "SIGSEGV"; Sys.sigterm, "SIGTERM"; Sys.sigusr1, "SIGUSR1";
      Sys.sigusr2, "SIGUSR2"; Sys.sigchld, "SIGCHLD"; Sys.sigcont, "SIGCONT";
      Sys.sigstop, "SIGSTOP"; Sys.sigtstp, "SIGTSTP"; Sys.sigttin, "SIGTTIN";
      Sys.sigttou, "SIGTTOU"; Sys.sigvtalrm, "SIGVTALRM";
      Sys.sigprof, "SIGPROF"; Sys.sigbus, "SIGBUS"; Sys.sigpoll, "SIGPOLL";
      Sys.sigsys, "SIGSYS"; Sys.sigtrap, "SIGTRAP"; Sys.sigurg, "SIGURG";
      Sys.sigxcpu, "SIGXCPU"; Sys.sigxfsz, "SIGXFSZ"; ]
    in
    try string ppf (List.assoc snum sigs) with
    | Not_found -> pf ppf "SIG(%d)" snum

  let pp_backtrace_str ppf s =
    let stop = String.length s - 1 (* there's a newline at the end *) in
    let rec loop left right =
      if right = stop then string ppf (String.sub s left (right - left)) else
      if s.[right] <> '\n' then loop left (right + 1) else
      begin
        string ppf (String.sub s left (right - left));
        cut ppf ();
        loop (right + 1) (right + 1)
      end
    in
    if s = "" then (string ppf "No backtrace available.") else
    loop 0 0

  let backtrace =
    vbox @@ (using Printexc.raw_backtrace_to_string) pp_backtrace_str

  let exn ppf e = string ppf (Printexc.to_string e)
  let exn_backtrace ppf (e, bt) =
    pf ppf "@[<v>Exception: %a@,%a@]"
      exn e pp_backtrace_str (Printexc.raw_backtrace_to_string bt)

  let pair ?sep:(pp_sep = cut) pp_fst pp_snd ppf (fst, snd) =
    pp_fst ppf fst; pp_sep ppf (); pp_snd ppf snd

  let option ?none:(pp_none = nop) pp_v ppf = function
  | None -> pp_none ppf ()
  | Some v -> pp_v ppf v

  let none ppf () = string ppf "<none>"

  let list ?(empty = nop) ?sep:pp_sep pp_elt ppf = function
  | [] -> empty ppf ()
  | l -> Format.pp_print_list ?pp_sep pp_elt ppf l

  let array ?(empty = nop) ?sep pp_elt ppf a = match Array.length a with
  | 0 -> empty ppf ()
  | _n -> iter ?sep Array.iter pp_elt ppf a

  (* Magnitudes *)

  let ilog10 x =
    let rec loop p x = if x = 0 then p else loop (p + 1) (x / 10) in
    loop (-1) x

  let ipow10 n =
    let rec loop acc n = if n = 0 then acc else loop (acc * 10) (n - 1) in
    loop 1 n

  let si_symb_max = 16
  let si_symb =
    [| "y"; "z"; "a"; "f"; "p"; "n"; "u"; "m"; ""; "k"; "M"; "G"; "T"; "P";
       "E"; "Z"; "Y"|]

  let rec pp_at_factor ~scale u symb factor ppf s =
    let m = s / factor in
    let n = s mod factor in
    match m with
    | m when m >= 100 -> (* No fractional digit *)
        let m_up = if n > 0 then m + 1 else m in
        if m_up >= 1000 then si_size ~scale u ppf (m_up * factor) else
        pf ppf "%d%s%s" m_up symb u
    | m when m >= 10 -> (* One fractional digit w.o. trailing 0 *)
        let f_factor = factor / 10 in
        let f_m = n / f_factor in
        let f_n = n mod f_factor in
        let f_m_up = if f_n > 0 then f_m + 1 else f_m in
        begin match f_m_up with
        | 0 -> pf ppf "%d%s%s" m symb u
        | f when f >= 10 -> si_size ~scale u ppf (m * factor + f * f_factor)
        | f -> pf ppf "%d.%d%s%s" m f symb u
        end
    | m -> (* Two or zero fractional digits w.o. trailing 0 *)
        let f_factor = factor / 100 in
        let f_m = n / f_factor in
        let f_n = n mod f_factor in
        let f_m_up = if f_n > 0 then f_m + 1 else f_m in
        match f_m_up with
        | 0 -> pf ppf "%d%s%s" m symb u
        | f when f >= 100 -> si_size ~scale u ppf (m * factor + f * f_factor)
        | f when f mod 10 = 0 -> pf ppf "%d.%d%s%s" m (f / 10) symb u
        | f -> pf ppf "%d.%02d%s%s" m f symb u

  and si_size ~scale u ppf s = match scale < -8 || scale > 8 with
  | true -> invalid_arg "~scale is %d, must be in [-8;8]" scale
  | false ->
      let pow_div_3 = if s = 0 then 0 else (ilog10 s / 3) in
      let symb = (scale + 8) + pow_div_3 in
      let symb, factor = match symb > si_symb_max with
      | true -> si_symb_max, ipow10 ((8 - scale) * 3)
      | false -> symb, ipow10 (pow_div_3 * 3)
      in
      if factor = 1
      then pf ppf "%d%s%s" s si_symb.(symb) u
      else pp_at_factor ~scale u si_symb.(symb) factor ppf s

  let byte_size ppf s = si_size ~scale:0 "B" ppf s

  let us_span   =                  1_000L
  let ms_span   =              1_000_000L
  let sec_span  =          1_000_000_000L
  let min_span  =         60_000_000_000L
  let hour_span =       3600_000_000_000L
  let day_span  =     86_400_000_000_000L
  let year_span = 31_557_600_000_000_000L

  let rec pp_si_span unit_str si_unit si_higher_unit ppf span =
    let geq x y = Int64.unsigned_compare x y >= 0 in
    let m = Int64.unsigned_div span si_unit in
    let n = Int64.unsigned_rem span si_unit in
    match m with
    | m when geq m 100L -> (* No fractional digit *)
        let m_up = if Int64.equal n 0L then m else Int64.succ m in
        let span' = Int64.mul m_up si_unit in
        if geq span' si_higher_unit then uint64_ns_span ppf span' else
        pf ppf "%Ld%s" m_up unit_str
    | m when geq m 10L -> (* One fractional digit w.o. trailing zero *)
        let f_factor = Int64.unsigned_div si_unit 10L in
        let f_m = Int64.unsigned_div n f_factor in
        let f_n = Int64.unsigned_rem n f_factor in
        let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
        begin match f_m_up with
        | 0L -> pf ppf "%Ld%s" m unit_str
        | f when geq f 10L ->
            uint64_ns_span ppf Int64.(add (mul m si_unit) (mul f f_factor))
        | f -> pf ppf "%Ld.%Ld%s" m f unit_str
        end
    | m -> (* Two or zero fractional digits w.o. trailing zero *)
        let f_factor = Int64.unsigned_div si_unit 100L in
        let f_m = Int64.unsigned_div n f_factor in
        let f_n = Int64.unsigned_rem n f_factor in
        let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
        match f_m_up with
        | 0L -> pf ppf "%Ld%s" m unit_str
        | f when geq f 100L ->
            uint64_ns_span ppf Int64.(add (mul m si_unit) (mul f f_factor))
        | f when Int64.equal (Int64.rem f 10L) 0L ->
            pf ppf "%Ld.%Ld%s" m (Int64.div f 10L) unit_str
        | f ->
            pf ppf "%Ld.%02Ld%s" m f unit_str

  and pp_non_si unit_str unit unit_lo_str unit_lo unit_lo_size ppf span =
    let geq x y = Int64.unsigned_compare x y >= 0 in
    let m = Int64.unsigned_div span unit in
    let n = Int64.unsigned_rem span unit in
    if Int64.equal n 0L then pf ppf "%Ld%s" m unit_str else
    let f_m = Int64.unsigned_div n unit_lo in
    let f_n = Int64.unsigned_rem n unit_lo in
    let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
    match f_m_up with
    | f when geq f unit_lo_size ->
        uint64_ns_span ppf Int64.(add (mul m unit) (mul f unit_lo))
    | f ->
        pf ppf "%Ld%s%Ld%s" m unit_str f unit_lo_str

  and uint64_ns_span ppf span =
    let geq x y = Int64.unsigned_compare x y >= 0 in
    let lt x y = Int64.unsigned_compare x y = -1 in
    match span with
    | s when lt s us_span -> pf ppf "%Ldns" s
    | s when lt s ms_span -> pp_si_span "us" us_span ms_span ppf s
    | s when lt s sec_span -> pp_si_span "ms" ms_span sec_span ppf s
    | s when lt s min_span -> pp_si_span "s" sec_span min_span ppf s
    | s when lt s hour_span -> pp_non_si "min" min_span "s" sec_span 60L ppf s
    | s when lt s day_span -> pp_non_si "h" hour_span "min" min_span 60L ppf s
    | s when lt s year_span -> pp_non_si "d" day_span "h" hour_span 24L ppf s
    | s ->
        let m = Int64.unsigned_div s year_span in
        let n = Int64.unsigned_rem s year_span in
        if Int64.equal n 0L then pf ppf "%Lda" m else
        let f_m = Int64.unsigned_div n day_span in
        let f_n = Int64.unsigned_rem n day_span in
        let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
        match f_m_up with
        | f when geq f 366L -> pf ppf "%Lda" (Int64.succ m)
        | f -> pf ppf "%Lda%Ldd" m f
end

module Result = struct

  include Stdlib.Result

  let product r0 r1 = match r0, r1 with
  | (Error _ as r), _ | _, (Error _ as r) -> r
  | Ok v0, Ok v1 -> Ok (v0, v1)

  (* Interacting with Stdlib exceptions *)

  let to_failure = function Ok v -> v | Error e -> failwith e
  let to_invalid_arg = function Ok v -> v | Error e -> invalid_arg e

  (* Syntax *)

  module Syntax = struct
    let ( let* ) x f = bind x f
    let ( and* ) a b = product a b
    let ( let+ ) x f = map f x
    let ( and+ ) a b = product a b
  end
end

module String = struct
  include Stdlib.String

  let subrange ?(first = 0) ?last s =
    let max = String.length s - 1 in
    let last = match last with
    | None -> max
    | Some l when l > max -> max
    | Some l -> l
    in
    let first = if first < 0 then 0 else first in
    if first > last then "" else
    String.sub s first (last - first + 1)

  (* Formatting *)

  let pp = Fmt.string
  let pp_dump ppf s = Fmt.pf ppf "%S" s

   (* String map and sets *)

  module Set = struct
    include Set.Make (String)
    let pp ?sep pp_elt = Fmt.iter ?sep iter pp_elt
    let pp_dump ppf ss = Fmt.pf ppf "@[<1>{%a}@]" (pp ~sep:Fmt.sp pp_dump) ss
  end

  module Map = struct
    include Map.Make (String)
    let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
    let of_list bs = List.fold_left (fun m (k,v) -> add k v m) empty bs

    let add_to_list k v m = match find_opt k m with
    | None -> add k [v] m
    | Some l -> add k (v :: l) m

    let add_to_set
        (type set) (type elt)
        (module S : Stdlib.Set.S with type elt = elt and type t = set)
        k v m
      =
      match find_opt k m with
      | None -> add k (S.singleton v) m
      | Some set -> add k (S.add v set) m

    let pp ?sep pp_binding = Fmt.iter_bindings ?sep iter pp_binding
    let pp_dump_str = pp_dump
    let pp_dump pp_v ppf m =
      let pp_binding ppf (k, v) =
        Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]" pp_dump_str k pp_v v
      in
      Fmt.pf ppf "@[<1>{%a}@]" (pp ~sep:Fmt.sp pp_binding) m

    let pp_dump_string_map ppf m = pp_dump pp_dump_str ppf m
  end
end

module Mtime = struct
  type uint64 = int64

  module Span = struct

    (* Time spans

       Represented by a nanosecond magnitude stored in an unsigned 64-bit
       integer. Allows to represent spans for ~584.5 Julian years. *)

    type t = uint64
    let zero = 0L
    let one = 1L
    let max_span = -1L
    let add = Int64.add
    let abs_diff s0 s1 = match Int64.unsigned_compare s0 s1 < 0 with
    | true ->  Int64.sub s1 s0
    | false -> Int64.sub s0 s1

    (* Predicates and comparisons *)

    let equal = Int64.equal
    let compare = Int64.unsigned_compare

    (* Durations *)

    let ( * ) n s = Int64.mul (Int64.of_int n) s
    let ns   =                      1L
    let us   =                  1_000L
    let ms   =              1_000_000L
    let s    =          1_000_000_000L
    let min  =         60_000_000_000L
    let hour =       3600_000_000_000L
    let day  =      86400_000_000_000L
    let year = 31_557_600_000_000_000L

    (* Conversions *)

    let to_uint64_ns s = s
    let of_uint64_ns ns = ns
    let pp = Fmt.uint64_ns_span
    let pp_ns ppf s = Fmt.pf ppf "%Luns" s
  end

  type span = Span.t

  (* Monotonic timestamps *)

  type t = uint64

  let to_uint64_ns s = s
  let of_uint64_ns ns = ns
  let min_stamp = 0L
  let max_stamp = -1L
  let pp ppf s = Fmt.pf ppf "%Lu" s

  (* Predicates *)

  let equal = Int64.equal
  let compare = Int64.unsigned_compare
  let is_earlier t ~than = compare t than < 0
  let is_later t ~than = compare t than > 0

  (* Arithmetic *)

  let span t0 t1 = match compare t0 t1 < 0 with
  | true -> Int64.sub t1 t0
  | false -> Int64.sub t0 t1

  let add_span t s =
    let sum = Int64.add t s in
    if compare t sum <= 0 then Some sum else None

  let sub_span t s =
    if compare t s < 0 then None else Some (Int64.sub t s)
end

module Os = struct
  external mtime_now_ns : unit -> Mtime.t = "ocaml_l_monotonic_now_ns"

  module Signal = struct
    type t = int

    let set s b = match Sys.signal s b with
    | b -> Ok b | exception Sys_error e -> Error e

    let set_noerr s b = try Sys.set_signal s b with Sys_error _ -> ()
  end

  module Fd = struct
    let uerror e = Unix.error_message e

    (* Closing *)

    let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

    (* Read and write *)

    let rec read fd b ~start ~len = match Unix.read fd b start len with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> read fd b ~start ~len
    | 0 when len <> 0 -> false
    | c when c < len -> read fd b ~start:(start + c) ~len:(len - c)
    | _ -> true

    let rec write fd b ~start ~len = match Unix.single_write fd b start len with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write fd b ~start ~len
    | c when c < len -> write fd b ~start:(start + c) ~len:(len - c)
    | _ -> ()

    (* Socket endpoints *)

    type endpoint =
    [ `Host of string * int
    | `Sockaddr of Unix.sockaddr
    | `Fd of Unix.file_descr ]

    let endpoint_of_string ~default_port s =
      match String.contains s Filename.dir_sep.[0] with
      | true -> Ok (`Sockaddr (Unix.ADDR_UNIX s))
      | false ->
          match String.rindex_opt s ':' with
          | None -> Ok (`Host (s, default_port))
          | Some i ->
              match String.index_from_opt s i ']' with (* beware IPv6 *)
              | Some _ -> Ok (`Host (s, default_port))
              | None ->
                  let h = String.subrange ~last:(i - 1) s in
                  let p = String.subrange ~first:(i + 1) s in
                  match int_of_string_opt p with
                  | None -> Fmt.error "port %S not an integer" p
                  | Some p -> Ok (`Host (h, p))

    let pp_endpoint ppf ep =
      let pp_name_port ppf (n, p) = Fmt.pf ppf "%s:%d" n p in
      match ep with
      | `Host (n, p) -> pp_name_port ppf (n, p)
      | `Fd _fd -> Fmt.pf ppf "<fd>"
      | `Sockaddr (Unix.ADDR_UNIX s) -> Fmt.string ppf s
      | `Sockaddr (Unix.ADDR_INET (a, p)) ->
          pp_name_port ppf (Unix.string_of_inet_addr a, p)

    let rec socket_of_endpoint ep stype = match ep with
    | `Fd fd -> Ok (None, fd, false)
    | `Host (name, port) ->
        begin match Unix.gethostbyname name with
        | exception Not_found -> Fmt.error "%s: host not found" name
        | h ->
            let c = `Sockaddr (Unix.ADDR_INET (h.h_addr_list.(0), port)) in
            socket_of_endpoint c stype
        end
    | `Sockaddr addr ->
        let domain = Unix.domain_of_sockaddr addr in
        match Unix.socket ~cloexec:true domain stype 0 with
        | exception Unix.Unix_error (e, _, _) -> Error (uerror e)
        | fd -> Ok (Some addr, fd, true)

    (* Sets *)

    module T = struct
      type t = Unix.file_descr
      let compare : Unix.file_descr -> Unix.file_descr -> int = compare
    end

    module Set = Set.Make (T)
  end

  module Ev = struct

    (* Callbacks *)

    type cb =
    | Nop
    | Cb of (unit -> unit)
    | Cb_expirable of (expired:bool -> unit -> unit)

    (* Events *)

    type t = { time : Mtime.t; mutable cb : cb }

    let create_untimed cb = { time = Mtime.max_stamp; cb }
    let create ~dur cb = { time = Int64.add (mtime_now_ns ()) dur; cb }
    let farthest = create_untimed Nop

    (* Heap priority queue, classical imperative implementation. *)

    let heap_compare h i i' = Mtime.compare h.(i).time h.(i').time
    let heap_swap h i i' = let v = h.(i) in h.(i) <- h.(i'); h.(i') <- v

    let rec heap_up h i =
      if i = 0 then () else
      let p = (i - 1) / 2 in (* parent index. *)
      if heap_compare h i p < 0 then (heap_swap h i p; heap_up h p)

    let rec heap_down h max i =
      let start = 2 * i in
      let l = start + 1 in (* left child index. *)
      let r = start + 2 in (* right child index. *)
      if l > max then () (* no child, stop *) else (* find smallest child k. *)
      let k = if r > max then l else (if heap_compare h l r < 0 then l else r)
      in
      if heap_compare h i k > 0 then (heap_swap h i k; heap_down h max k)

    let heap () = Array.make 256 farthest

    (* Event sets *)

    type set =
      { mutable r : (Unix.file_descr * t) list;
        mutable w : (Unix.file_descr * t) list;
        mutable ready_fds : (Unix.file_descr * (unit -> unit)) list;
        mutable timeouts : t array; (* Heap priority queue for timeouts *)
        mutable timeout_max : int; (* Index of tast element of [timeouts]. *) }

    let set () =
      { r = []; w = []; ready_fds = []; timeouts = heap (); timeout_max = -1}

    let timeout_exists s = s.timeout_max > -1
    let fd_exists s = s.r <> [] || s.w <> [] || s.ready_fds <> []

    (* Timeout handling *)

    let shrink_timeouts_threshold = 262144
    let shrink_timeouts s = (* assert (s.timeout_max < 0). *)
      if Array.length s.timeouts < shrink_timeouts_threshold then () else
      s.timeouts <- heap ()

    let grow_timeouts s =
      let len = s.timeout_max + 1 in
      let timeouts' = Array.make (2 * len) farthest in
      Array.blit s.timeouts 0 timeouts' 0 len; s.timeouts <- timeouts'

    let add_timeout s ev =
      let max = s.timeout_max + 1 in
      if max = Array.length s.timeouts then grow_timeouts s;
      s.timeout_max <- max;
      s.timeouts.(s.timeout_max) <- ev; heap_up s.timeouts s.timeout_max

    let pop_timeout s =
      let last = s.timeouts.(s.timeout_max) in
      s.timeouts.(s.timeout_max) <- farthest;
      s.timeout_max <- s.timeout_max - 1;
      if s.timeout_max < 0 then shrink_timeouts s else
      (s.timeouts.(0) <- last; heap_down s.timeouts s.timeout_max 0)

    let dur_to_next_timeout s =
      let rec loop s now =
        if s.timeout_max < 0 then None else
        if s.timeouts.(0).cb = Nop then (pop_timeout s; loop s now) else
        let time = s.timeouts.(0).time in
        let late = Mtime.is_earlier time ~than:now in
        Some (if late then Mtime.Span.zero else (Mtime.span now time))
      in
      loop s (mtime_now_ns ())

    let expired_timeout s =
      let rec loop s now =
        if s.timeout_max < 0 then None else
        if s.timeouts.(0).cb = Nop then (pop_timeout s; loop s now) else
        let time = s.timeouts.(0).time in
        if not (Mtime.is_earlier time ~than:now) then None else
        let ev = s.timeouts.(0) in
        pop_timeout s;
        match ev.cb with
        | Cb cb -> ev.cb <- Nop; Some (`Event cb)
        | Cb_expirable cb -> ev.cb <- Nop; Some (`Event (cb ~expired:true))
        | Nop -> assert false
      in
      loop s (mtime_now_ns ())

    (* Fd handling *)

    let ready_fd s = match s.ready_fds with
    | [] -> None | (_, cb) :: rs -> s.ready_fds <- rs; Some (`Event cb)

    let rec x_set fds xs = function
    | [] -> fds, xs
    | (fd, ev as x) :: rest ->
        if ev.cb = Nop then x_set fds xs rest else
        x_set (Fd.Set.add fd fds) (x :: xs) rest

    let r_set s =
      let fds, r = x_set Fd.Set.empty [] s.r in
      s.r <- r; Fd.Set.elements fds

    let w_set s =
      let fds, w = x_set Fd.Set.empty [] s.w in
      s.w <- w; Fd.Set.elements fds

    let rec x_ready fds ready xs = function
    | [] -> ready, xs
    | (fd, ev as x) :: rest ->
        if not (Fd.Set.mem fd fds) then x_ready fds ready (x :: xs) rest else
        match ev.cb with
        | Nop -> assert false
        | Cb cb -> ev.cb <- Nop; x_ready fds ((fd, cb) :: ready) xs rest
        | Cb_expirable cb ->
            ev.cb <- Nop; x_ready fds ((fd, cb ~expired:false) :: ready) xs rest

    let ready_r_set s fds =
      let ready, r = x_ready (Fd.Set.of_list fds) s.ready_fds [] s.r in
      s.ready_fds <- ready; s.r <- r

    let ready_w_set s fds =
      let ready, w = x_ready (Fd.Set.of_list fds) s.ready_fds [] s.w in
      s.ready_fds <- ready; s.w <- w

    (* Waiting for next event *)

    let wait s = (* assert s.ready_fds = [] *)
      let timeout_s = match dur_to_next_timeout s with
      | None -> -1.
      | Some dur -> Int64.to_float (Mtime.Span.to_uint64_ns dur) *. 1e-9
      in
      let r_set = r_set s and w_set = w_set s in
      if timeout_s = -1. && r_set = [] && w_set = [] then `Empty else
      match Unix.select r_set w_set [] timeout_s with
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> `Signal
      | [], [], [] -> `Next
      | r_set, w_set, _ -> ready_r_set s r_set; ready_w_set s w_set; `Next

    let rec next s = match ready_fd s with
    | Some _ as cb -> cb
    | None ->
        match expired_timeout s with
        | Some _ as cb -> cb
        | None ->
            match wait s with
            | `Empty -> None
            | `Signal -> (Some `Signal)
            | `Next -> next s

    (* Timeout events *)

    let timeout s ~dur cb = let ev = create ~dur (Cb cb) in add_timeout s ev; ev

    (* File descriptor events *)

    type fd = [`R|`W]

    let fd_ev s fd what ev = match what with
    | `R -> s.r <- (fd, ev) :: s.r; ev
    | `W -> s.w <- (fd, ev) :: s.w; ev

    let fd s fd what cb = fd_ev s fd what (create_untimed (Cb cb))
    let fd_or_timeout s ~dur fd what cb =
      let ev = create ~dur (Cb_expirable cb) in
      add_timeout s ev; fd_ev s fd what ev

    (* Removing events *)

    let remove _s ev = ev.cb <- Nop
    let remove_fd s fd =
      let not_fd (fd', ev) = fd' <> fd || (remove s ev; false) in
      s.r <- List.filter not_fd s.r;
      s.w <- List.filter not_fd s.w;
      s.ready_fds <- List.filter (fun (fd', _) -> fd <> fd') s.ready_fds

    let remove_all s =
      s.r <- []; s.w <- []; s.ready_fds <- [];
      s.timeouts <- heap (); s.timeout_max <- -1

  end

  module Mtime = struct

    (* Monotonic clock *)

    let origin = mtime_now_ns ()
    let elapsed () = Int64.sub (mtime_now_ns ()) origin
    let now = mtime_now_ns

    (* Monotonic time counter *)

    type counter = Mtime.t
    let counter = mtime_now_ns
    let count c = Int64.sub (mtime_now_ns ()) c
  end
end

module Cli = struct

  open Cmdliner

  let endpoint_conv ~default_port =
    let parse s =
      Result.map_error (fun e -> `Msg e) @@
      Os.Fd.endpoint_of_string ~default_port s
    in
    Arg.conv (parse, Os.Fd.pp_endpoint)

  let endpoint
      ?(opts = ["s"; "socket"]) ?docs ~default_port ~default_endpoint ()
    =
    let doc =
      Fmt.str "Connect socket on address $(i,ADDR) and port $(i,PORT) \
        (defaults to %d) or Unix domain socket $(i,PATH)" default_port
    in
    let docv = "ADDR[:PORT]|PATH" in
    let epconv = endpoint_conv ~default_port in
    Arg.(value & opt epconv default_endpoint & info opts ?docs ~doc ~docv)
end
