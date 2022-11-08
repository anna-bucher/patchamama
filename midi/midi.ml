module Event = struct
  type ev = { c : int; n : int; v : int; }
  type t = NoteOn of ev | NoteOff of ev | Ctrl of ev | Raw of bytes
  let status_mask = 0xF0
  let note_off_mask = 0x80
  let note_on_mask = 0x90
  let ctrl_mask = 0xB0

  let set_status b c m = Bytes.set b 0 (char_of_int (c + m - 1))
  let set_data b e = Bytes.set b 1 (char_of_int e.n); Bytes.set b 2 (char_of_int e.v)
  let get_channel b m = (int_of_char (Bytes.get b 0)) - m + 1
  let get_x b i = (int_of_char (Bytes.get b i))

  let to_bytes = function
    | NoteOff e -> 
      let b = Bytes.create 3 in
      set_status b e.c note_off_mask;
      set_data b e; b
    | NoteOn e ->
      let b = Bytes.create 3 in
      set_status b e.c note_on_mask;
      set_data b e; b
    | Ctrl e ->
      let b = Bytes.create 3 in
      set_status b e.c ctrl_mask;
      set_data b e; b
    | Raw b -> b

  let of_bytes b =
    let s = (int_of_char (Bytes.get b 0)) land status_mask in
    if s = note_off_mask then NoteOff { c = get_channel b note_off_mask; n = get_x b 1; v = get_x b 2 } else
    if s = note_on_mask then NoteOn   { c = get_channel b note_on_mask;  n = get_x b 1; v = get_x b 2 } else
    if s = note_on_mask then Ctrl     { c = get_channel b ctrl_mask;     n = get_x b 1; v = get_x b 2 } else
      Raw b
end


module T = struct
  type midiin (* boxed RtMidiIn *)
  (*   type event = bytes *)
  (* type midiout *)

  external midiin_new : unit -> midiin = "caml_midiin_new"
  external midiin_destroy : midiin -> unit = "caml_midiin_destroy"

  external midiin_getPortCount : unit -> int = "caml_midiin_getPortCount"
  external midiin_getPortName : int -> string = "caml_midiin_getPortName"
  (*   external midiin_listen : (event -> unit) -> int -> unit = "caml_midiin_listen" *)
end

module Input = struct
  type t = T.midiin
  type port = { id : int; name : string }

  let _get_port id =
    let name = T.midiin_getPortName id in
    { id; name }

  let port_list () =
    let rec aux acc = function
      | -1 -> acc
      | n ->
        let acc = (_get_port n) :: acc in
        aux acc (n-1) in
    aux [] ((T.midiin_getPortCount ()) -1 )

  let find_port name = List.find_opt (fun p -> p.name = name) (port_list ())

  let name port = port.name

  let listen _fn _port = 
    let m = T.midiin_new () in
    let finally () = T.midiin_destroy m in
    Fun.protect ~finally (fun () -> 
        (*         let clbk b = fn (Event.of_bytes b) in
                   T.midiin_listen clbk port.id *)
        ()
      )
end

module Output = struct
  type t = T.midiin (* FIXME *)
  type port = { id : int; name : string }
  let port_list () = []
  let find_port name = List.find_opt (fun p -> p.name = name) (port_list ())
end