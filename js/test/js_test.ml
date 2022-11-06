open Stest
open Js

let snapshots_path = "js/test/snapshots"
let scripts_path = "js/test/scripts"

let snapshot = Assert.snapshot snapshots_path "yml"
let open' p = open' (scripts_path ^ "/" ^ p)

let open_void () = open' "function_calls.js" @@ fun _js -> ()
let call_add_numbers () = open' "function_calls.js" @@ fun js -> 
  match call js "add" [Number 10.; Number 14.] with
  | Number f -> Assert.equal_or "expected %f but received %f" 24. f
  | r -> Assert.failf "expected return type to be number but found %s" (typeof r)

let call_add_strings () = open' "function_calls.js" @@ fun js -> 
  match call js "add" [String "Hello "; String "World!"] with
  | String s -> Assert.equal_or "expected %s but received %s" "Hello World!" s
  | r -> Assert.failf "expected return type to be string but found %s" (typeof r)

let main () = run "Js" [
    ("open void", open_void);
    ("call add (numbers)", call_add_numbers);
    ("call add (strings)", call_add_strings);
  ];
  Gc.full_major () (* crash test *)

let () = main ()
