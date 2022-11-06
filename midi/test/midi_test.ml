open Stest
open Midi

let snapshots_path = "midi/test/snapshots"
let scripts_path = "midi/test/scripts"

let snapshot = Assert.snapshot snapshots_path "yml"

let device_create_virtual_input () = Device.create_virtual_input CoreMidi "midi_test" @@ fun _dev -> ()

let main () = run "Midi" [
    ("device create virtual input", device_create_virtual_input);
  ];
  Gc.full_major () (* crash test *)

let () = main ()
