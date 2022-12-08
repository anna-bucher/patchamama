open Stest

module O : Dyl_loader.Options = struct
  let path = "_build/default/dyltest/dyl_ext.so"

  let fn_name = "do_something"
end


let test () =
  let module M = Dyl_loader.Make(O) in
  Assert.equal_or "Expected %i but found %i" 109 (M.call 9)

let main () = run "Reaper" [
    ("test", test);
  ];
  Gc.full_major () (* crash test *)

let () = main ()
