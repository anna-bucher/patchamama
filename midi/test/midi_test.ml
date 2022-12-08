open Stest
open Midi

let snapshots_path = "midi/test/snapshots"

let snapshot = Assert.snapshot snapshots_path "yml"

module Event_test = struct
  module M = Event

  let n_3_64_101 = { Event.c = 3; n = 64; v = 101 }
  let n_1_4_0 = { Event.c = 1; n = 4; v = 0 }

  let bytes_as_string b =
    let rec aux acc = function
      | 0 -> acc
      | n -> 
        let j = if n = 1 then "" else "-" in
        let hex = Printf.sprintf "%02X" (int_of_char (Bytes.get b (n-1))) in
        let acc = j ^ hex ^ acc in
        aux acc (n - 1) in
    aux "" (Bytes.length b)


  let string_as_bytes s =
    let l = String.split_on_char '-' s in
    let b = Bytes.create (List.length l) in
    let rec aux n = function
      | [] -> ()
      | h :: t ->
        Bytes.set b n (char_of_int (int_of_string ("0x" ^ h)));
        aux (n+1) t in
    aux 0 l;
    b

  let test_parse_bytes () = 
    let rec aux = function
      | [] -> ()
      | h :: t ->
        let p = (bytes_as_string (string_as_bytes h)) in
        Assert.equal_or "parsed should be %s but found %s" h p;
        aux t
    in
    aux [ "40"; "00-00"; "FF-FF"; "40-10" ]

  let bytes_events = 
    let open M in 
    [
      ("82-40-65", Note_off n_3_64_101);
      ("92-40-65", Note_on n_3_64_101);
      ("A2-40-65", Poly_pressure n_3_64_101);
      ("B2-40-65", Ctrl n_3_64_101);
      ("C0-04", Prog_change n_1_4_0);
      ("D0-04", Chan_pressure n_1_4_0);
      ("E4-7F-7F", Pitch_bend { c = 5; n = 0x3FFF; v = 0 });
      ("E4-00-40", Pitch_bend { c = 5; n = 0x2000; v = 0 });
      ("E4-00-00", Pitch_bend { c = 5; n = 0x0000; v = 0 });
    ] 
  let to_bytes () =
    let rec aux = function
      | [] -> ()
      | (b, n) :: t ->
        let s = bytes_as_string (M.to_bytes n) in
        let err = Printf.sprintf "%s: bytes should be %s but found %s" (M.to_string n) b s in
        if b <> s then Assert.fail err;
        aux t
    in
    aux bytes_events

  let of_bytes () =
    let rec aux = function
      | [] -> ()
      | (b, n) :: t ->
        let nr = M.to_string (M.of_bytes (string_as_bytes b)) in
        let ns = M.to_string n in
        let err = Printf.sprintf "%s: event should be %s but found %s" b ns nr in
        if ns <> nr then Assert.fail err;
        aux t
    in
    aux bytes_events

  let test () = run "Event" [
      ("to_bytes", to_bytes);
      ("of_bytes", of_bytes);
    ];
    Gc.full_major () (* crash test *)
end

module Input_test = struct
  module M = Input
  let name = "midi_test_in"
  let in_port = "X-Touch INT"

  let list () =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> 
        let acc = acc ^ (M.name h) ^ "; " in
        aux acc t in
    Printf.printf "[%s] " (aux "" (M.port_list ()))

  let test_listen = false

  let listen () = if test_listen then
      match M.find_port in_port with
      | None -> ()
      | Some p -> M.listen p @@ fun ev ->
        Printf.printf "Received %s\n" (Event.to_string ev); flush_all ()

  let test () = run "Input" [
      ("port list", list);
      ("listen", listen);
    ];
    Gc.full_major () (* crash test *)
end

module Output_test = struct
  module M = Output
  (*   let name = "midi_test_out" *)

  let in_port = "X-Touch INT"

  let list () =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> 
        let acc = acc ^ (M.name h) ^ "; " in
        aux acc t in
    Printf.printf "[%s] " (aux "" (M.port_list ()))

  let send () =
    match M.find_port in_port with
    | None -> ()
    | Some p -> M.open' p @@ fun m ->
      M.send m (Event.Pitch_bend { c = 1; n = 0x2000; v = 0 } )

  let test () = run "Output" [
      ("port list", list);
      ("send", send);
    ];
    Gc.full_major () (* crash test *)
end

let main () = 
  Event_test.test ();
  Input_test.test ();
  Output_test.test ();
  ()

let () = main ()
