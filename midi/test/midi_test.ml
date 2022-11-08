open Stest
open Midi

let snapshots_path = "midi/test/snapshots"
let scripts_path = "midi/test/scripts"

let snapshot = Assert.snapshot snapshots_path "yml"

module Event_test = struct
  module M = Event

  let note_64 = { Event.c = 3; n = 64; v = 101 }

  let as_string ev =
    let b = M.to_bytes ev in
    let rec aux acc = function
      | 0 -> acc
      | n -> 
        let j = if n = 1 then "" else "-" in
        let hex = Printf.sprintf "%X" (int_of_char (Bytes.get b (n-1))) in
        let acc = j ^ hex ^ acc in
        aux acc (n - 1) in
    aux "" (Bytes.length b)

  let note_on_to_bytes () =
    let n = M.NoteOn note_64 in
    Assert.equal_or "Bytes expected to be %s but found %s" "92-40-65" (as_string n)

  let note_on_of_bytes () =
    let n = note_64 in
    let b = Bytes.create 3 in
    Bytes.set b 0 (char_of_int (0x90 -1 + n.c));
    Bytes.set b 1 (char_of_int n.n);
    Bytes.set b 2 (char_of_int n.v);
    let ev = M.of_bytes b in
    match ev with
    | NoteOn e ->
      Assert.equal_or "channel expected to be %i but found %i" n.c e.c;
      Assert.equal_or "note expected to be %i but found %i" n.n e.n;
      Assert.equal_or "velocity expected to be %i but found %i" n.v e.v;
    | _ -> Assert.fail "invalid event type"

  let test () = run "Midi Event" [
      ("NoteOn to_bytes", note_on_to_bytes);
      ("NoteOn of_bytes", note_on_of_bytes);
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

  let test () = run "Midi Input" [
      ("port list", list);
    ];
    Gc.full_major () (* crash test *)
end

module Output_test = struct
  module M = Output
  let name = "midi_test_out"

  let create_virtual () = ()
  (*
  let get_name () =  with_device @@ fun dev -> 
    Assert.equal_or "'%s' expected but found '%s'" name (M.name (M.info dev))
  let get_interface () = with_device @@ fun dev -> 
    Assert.equal_or "'%s' expected but found '%s'" 
      (interface_to_string interf)
      (interface_to_string (M.interface (M.info dev)))
  let list () = open' @@ fun m ->
    let l = M.list m in
    ignore @@ List.map (fun info -> Printf.printf "output: %s\n" (M.name info)) l
    *)

  let test () = run "Midi Output" [
      ("create virtual", create_virtual);
    ];
    Gc.full_major () (* crash test *)
end

let main () = 
  Event_test.test ();
  Input_test.test ();
  Output_test.test ();
  ()

let () = main ()
