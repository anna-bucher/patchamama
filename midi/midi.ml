type interface = MMSystem | CoreMIDI | ALSA (* DO NOT CHANGE ORDER *)
let interface_to_string = function
  | MMSystem -> "MMSystem"
  | CoreMIDI -> "CoreMIDI"
  | ALSA -> "ALSA"

module T = struct
  type device_info = { 
    structVersion : int;
    interface : interface;
    name : string;
    input : int;
    output : int;
    opened : bool;
    is_virtual : bool;
  } (* DO NOT CHANGE ORDER *)
  external create_virtual_input : interface -> string -> int = "caml_midi_create_virtual_input"
  external create_virtual_output : interface -> string -> int = "caml_midi_create_virtual_output"
  external delete_virtual_device : int -> unit = "caml_midi_delete_virtual_device"
  external get_device_info : int -> device_info = "caml_midi_get_device_info"

  let name m = m.name
  let interface m = m.interface
end

module Input_device = struct
  type t = T.device_info
  let create interface name fn = 
    let id = T.create_virtual_input interface name in
    let m = T.get_device_info id in
    let () = fn m in
    T.delete_virtual_device id
  let interface = T.interface
  let name = T.name 
end

module Output_device = struct
  type t = T.device_info
  let create interface name fn = 
    let id = T.create_virtual_output interface name in
    let m = T.get_device_info id in
    let () = fn m in
    T.delete_virtual_device id
  let interface = T.interface
  let name = T.name
end