open Stest
open Reaper

let snapshots_path = "reaper/test/snapshots"

let snapshot = Assert.snapshot snapshots_path "yml"

let main () = run "Reaper" [
    ("test", test);
  ];
  Gc.full_major () (* crash test *)

let () = main ()
