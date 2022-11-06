

module Device = struct
  type interface = MMSystem | CoreMidi | ALSA
  module T = struct
    external create_virtual_input : interface -> string -> int = "caml_midi_create_virtual_input"
    external delete_virtual_device : int -> unit = "caml_midi_delete_virtual_device"
  end

  type t = { name : string; id : int }
  let create_virtual_input interface name fn = 
    let id = T.create_virtual_input interface name in
    let () = fn { name ; id } in
    T.delete_virtual_device id
end