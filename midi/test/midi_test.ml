open Stest
open Midi

let snapshots_path = "midi/test/snapshots"
let scripts_path = "midi/test/scripts"

let snapshot = Assert.snapshot snapshots_path "yml"

module Input = struct
  module M = Input_device
  let name = "midi_test_in"
  let interf = CoreMIDI
  let create_virtual () = M.create interf name @@ fun _dev -> ()
  let get_name () = M.create interf name @@ fun dev -> 
    Assert.equal_or "'%s' expected but found '%s'" name (M.name dev)
  let get_interface () = M.create interf name @@ fun dev -> 
    Assert.equal_or "'%s' expected but found '%s'" 
      (interface_to_string interf)
      (interface_to_string (M.interface dev))

  let test () = run "Midi Input_device" [
      ("create virtual", create_virtual);
      ("get name", get_name);
      ("get interface", get_interface);
    ];
    Gc.full_major () (* crash test *)
end

module Output = struct
  module M = Output_device
  let name = "midi_test_out"
  let interf = CoreMIDI
  let create_virtual () = M.create interf name @@ fun _dev -> ()
  let get_name () = M.create interf "midi_test_out" @@ fun dev -> 
    Assert.equal_or "'%s' expected but found '%s'" name (M.name dev)

  let get_interface () = M.create interf name @@ fun dev -> 
    Assert.equal_or "'%s' expected but found '%s'" 
      (interface_to_string interf)
      (interface_to_string (M.interface dev))

  let test () = run "Midi Output_device" [
      ("create virtual", create_virtual);
      ("get name", get_name);
      ("get interface", get_interface);
    ];
    Gc.full_major () (* crash test *)
end

let main () = 
  ignore @@ List.map (fun f -> f ()) @@
  [ Input.test; Output.test ]

let () = main ()
