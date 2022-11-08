let default_buf_size=1024
let default_latency=0

type interface = MMSystem | CoreMIDI | ALSA (* DO NOT CHANGE ORDER *)
let interface_to_string = function
  | MMSystem -> "MMSystem"
  | CoreMIDI -> "CoreMIDI"
  | ALSA -> "ALSA"

module T = struct
  type stream
  type device_info = { 
    structVersion : int;
    interface : interface;
    name : string;
    input : bool;
    output : bool;
    opened : bool;
    is_virtual : bool;
    id : int;
  } (* DO NOT CHANGE ORDER *)

  external initialize : unit -> unit ="caml_midi_initialize"
  external terminate : unit -> unit ="caml_midi_terminate"

  external count_devices : unit -> int = "caml_midi_count_devices"
  external create_virtual_input : interface -> string -> int = "caml_midi_create_virtual_input"
  external create_virtual_output : interface -> string -> int = "caml_midi_create_virtual_output"
  external delete_virtual_device : int -> unit = "caml_midi_delete_virtual_device"
  external get_device_info : int -> device_info = "caml_midi_get_device_info"
  external open_input : int -> int -> stream = "caml_midi_open_input"
  external open_output : int -> int -> int -> stream = "caml_midi_open_output"
  external close : stream -> unit = "caml_midi_close"
  external read : stream -> int = "caml_midi_read"

  let id d = d.id
  let name d = d.name
  let interface d = d.interface
  let is_input d = d.input
  let is_output d = d.output
end

type m = { devices: T.device_info list }

let open' fn = 
  T.initialize ();
  let count = T.count_devices () in
  let rec aux acc = function
    | n when n = count -> acc
    | n -> let info = T.get_device_info n in
      aux (info::acc) (n+1) in
  let devices = aux [] 0 in
  let m = { devices } in
  Fun.protect ~finally:T.terminate @@ fun () -> fn m

module Input_device = struct
  type info = T.device_info
  type t = { info : info; stream : T.stream }

  let list m = List.filter T.is_input m.devices
  let find_device m name = List.find_opt (fun info -> (T.name info) = name) (list m)

  let info d = d.info
  let interface = T.interface 
  let name = T.name 

  let open' (_:m) ?(buf_size=default_buf_size) info fn = 
    let stream = T.open_input (T.id info) buf_size in
    let finally () = T.close stream in
    Fun.protect ~finally @@ fun () -> fn { info; stream } 

  let create m ?(buf_size=default_buf_size) interface name fn = 
    let id = T.create_virtual_input interface name in
    let info = T.get_device_info id in
    let finally () = T.delete_virtual_device id in
    Fun.protect ~finally @@ fun () -> open' m ~buf_size info fn

  let read m = T.read m.stream
end

module Output_device = struct
  type info = T.device_info
  type t = { info : info; stream : T.stream }

  let list m = List.filter T.is_output m.devices
  let find_device m name = List.find_opt (fun info -> (T.name info) = name) (list m)

  let info d = d.info
  let interface = T.interface
  let name = T.name

  let open' (_:m) ?(buf_size=default_buf_size) ?(latency=default_latency) info fn = 
    let stream = T.open_output (T.id info) buf_size latency in
    let finally () = T.close stream in
    Fun.protect ~finally @@ fun () -> fn { info; stream } 

  let create m ?(buf_size=default_buf_size) ?(latency=default_latency) interface name fn = 
    let id = T.create_virtual_output interface name in
    let info = T.get_device_info id in
    let finally () = T.delete_virtual_device id in
    Fun.protect ~finally @@ fun () -> open' m ~buf_size ~latency info fn

end