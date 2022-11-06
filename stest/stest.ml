type t = string * (unit -> unit)
exception Fail_test

let color c s = 
  let color = match c with
    | `Group -> 206
    | `Sub_group -> 168
    | `Test -> 81
    | `Fail -> 197
    | `Info -> 123
    | `Skip -> 214
    | `Grey -> 243
    | `Ok -> 112
  in
  Printf.sprintf "\u{001b}[38;5;%im%s\u{001b}[0m" color s

let fail_loc () = match Printexc.backtrace_slots (Printexc.get_callstack 10) with
  | None -> "No backtrace. Did you compile with -g ?"
  | Some slots ->
    let rec aux = function
      | [] -> "Backtrace not found."
      | t :: ts -> match Printexc.Slot.location t with
        | None -> aux ts
        | Some l when l.Printexc.filename = "core/nandi/nandi_test.ml" -> aux ts
        | Some l -> Printf.sprintf "%s:%d" l.filename l.line_number
    in
    aux (Array.to_list slots)

let pass () = 
  Printf.printf "%s\n" (color `Ok "OK");
  flush_all ()

let skip_test () = 
  Printf.printf "%s\n" (color `Skip "SKIP");
  flush_all ()
let fail msg = 
  Printf.printf "%s\n" (color `Fail "FAIL");
  Printf.printf "      %s\n" msg;
  Printf.printf "      %s\n" (color `Fail (fail_loc ()));
  flush_all ();
  raise Fail_test

let run title ?(skip=[]) ?(group='-') tests = 
  Printexc.record_backtrace true;
  Printf.printf "%s\n" (color `Group title);
  let rec aux grp = function
    | [] -> ()
    | (test, cb) :: t ->
      let new_grp, test_name = match String.index_opt test group with
        | None -> "", test
        | Some i -> String.sub test 0 i, String.sub test i ((String.length test) - i)
      in
      if new_grp <> grp then Printf.printf "    %s\n" (color `Sub_group new_grp);
      Printf.printf "        %-35s  " (color `Test test_name);
      flush_all ();
      let () = match List.find_opt (fun k -> test = k) skip with
        | Some _ -> skip_test ()
        | None -> 
          begin try
              cb (); pass () 
            with Fail_test -> ()
          end
      in
      aux new_grp t
  in
  aux "" tests;
  Printf.printf "\n"


module Utils : sig
  val read_file : string -> bytes option
  val write_file : string -> bytes -> unit
  val with_tmp_dir : (string -> 'a) -> 'a
end = struct
  let read_file path = 
    match Sys.file_exists path with
    | true ->
      let f = open_in path in
      let r = match in_channel_length f with
        | 0 -> None
        | len -> 
          let buff = Bytes.create len in
          match Stdlib.input f buff 0 len with
          | l when l = len -> Some buff
          | _ -> None
      in close_in f; r
    | false -> None

  let write_file path b = 
    let f = open_out path in
    Stdlib.output_bytes f b;
    ignore @@ close_out f
  let with_tmp_dir fn =
    let r = Bos.OS.Dir.with_tmp "%s" begin fun path f ->
        f (Fpath.to_string path)
      end fn
    in match r with
    | Ok r -> r
    | Error _ -> fail "Bos.OS.Dir.with_tmp error"
end


module Assert : sig
  val true_or : string -> bool -> unit
  val equal_or : ('a -> 'a -> string, unit, string) format -> 'a -> 'a -> unit
  val throws : string -> string -> (unit -> 'b) -> unit
  val fail : string -> 'b
  val failf : ('a -> string, unit, string) format -> 'a -> 'b
  val snapshot : string -> string -> string -> string -> unit
end = struct
  let true_or msg act = if act then () else fail (color `Fail msg)
  let equal_or msg exp act = if exp = act then () else fail (color `Fail (Printf.sprintf msg exp act))
  let fail msg = fail (color `Fail msg)
  let failf msg act = fail (color `Fail (Printf.sprintf msg act))
  let throws exp title fn =
    let actual = try 
        ignore @@ fn (); "Pass"
      with Invalid_argument s -> s in
    if actual = exp then () else fail (color `Fail (Printf.sprintf "%s should throw '%s' but returned '%s'" title exp actual))
  let snapshot base_path ext title result = 
    let b = String.to_bytes result in
    let spath = base_path ^ "/" ^ title ^ "." ^ ext in
    let r_opt = Utils.read_file spath in
    match r_opt with
    | None -> Utils.write_file spath b
    | Some r -> 
      if Bytes.compare b r != 0 then
        let npath = base_path ^ "/" ^ title ^ "-new." ^ ext in
        Utils.write_file npath b;
        fail (Printf.sprintf "%s %s\n%s %s\n" 
                (color `Grey "Snapshot")
                (color `Info npath)
                (color `Grey "does not match")
                (color `Info spath)
             )
      else
        ()
end