open Stest
open Midi

let interf = 
  if Sys.win32 then MMSystem else
  if Sys.unix then CoreMIDI else ALSA

let snapshots_path = "midi/test/snapshots"
let scripts_path = "midi/test/scripts"

let snapshot = Assert.snapshot snapshots_path "yml"

module Input = struct
  module M = Input_device
  let name = "midi_test_in"
  let with_device fn = open' @@ fun m -> M.create m interf name fn
  let create_virtual () = with_device @@ fun _ -> ()
  let list () = open' @@ fun m ->
    let l = M.list m in
    ignore @@ List.map (fun info -> Printf.printf "input: %s\n" (M.name info)) l

  let get_name () =  with_device @@ fun dev -> 
    Assert.equal_or "'%s' expected but found '%s'" name (M.name (M.info dev))
  let get_interface () = with_device @@ fun dev -> 
    Assert.equal_or "'%s' expected but found '%s'" 
      (interface_to_string interf)
      (interface_to_string (M.interface (M.info dev)))
  let receive_midi () = open' @@ fun m ->
    match M.find_device m "X-Touch INT" with
    | Some info -> 
      M.open' m info @@ fun dev -> 
      let rec aux = function
        | 0 -> ()
        | n -> Printf.printf "READ %i\n" (M.read dev); flush_all ();
          aux (n-1) in
      aux 100
    | None -> ()

  let test () = run "Midi Input_device" [
      ("create virtual", create_virtual);
      ("get name", get_name);
      ("get interface", get_interface);
      ("list", list);
      ("receive_midi", receive_midi);
    ];
    Gc.full_major () (* crash test *)
end

module Output = struct
  module M = Output_device
  let name = "midi_test_out"

  let with_device fn = open' @@ fun m -> M.create m interf name fn
  let create_virtual () = with_device @@ fun _ -> ()
  let get_name () =  with_device @@ fun dev -> 
    Assert.equal_or "'%s' expected but found '%s'" name (M.name (M.info dev))
  let get_interface () = with_device @@ fun dev -> 
    Assert.equal_or "'%s' expected but found '%s'" 
      (interface_to_string interf)
      (interface_to_string (M.interface (M.info dev)))
  let list () = open' @@ fun m ->
    let l = M.list m in
    ignore @@ List.map (fun info -> Printf.printf "output: %s\n" (M.name info)) l

  let test () = run "Midi Output_device" [
      ("create virtual", create_virtual);
      ("get name", get_name);
      ("get interface", get_interface);
      ("list", list);
    ];
    Gc.full_major () (* crash test *)
end

let main () = 
  Input.test ();
  Output.test ();
  ()

let () = main ()
