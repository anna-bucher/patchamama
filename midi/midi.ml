module Event = struct
  type ev = { c : int; n : int; v : int; }
  type t = 
    | Note_off of ev
    | Note_on of ev
    | Poly_pressure of ev
    | Ctrl of ev
    | Prog_change of ev (* v is ignored *)
    | Chan_pressure of ev (* v is ignored *)
    | Pitch_bend of ev (* v is ignored *)
    | Raw of bytes

  let status_mask = 0xF0
  let note_off_mask = 0x80
  let note_on_mask = 0x90
  let poly_pressure_mask = 0xA0
  let ctrl_mask = 0xB0
  let prog_change_mask = 0xC0
  let chan_pressure_mask = 0xD0
  let pitch_bend_mask = 0xE0

  let _message2 m ev = 
    let b = Bytes.create 2 in
    Bytes.set b 0 (char_of_int (ev.c + m - 1));
    Bytes.set b 1 (char_of_int ev.n);
    b

  let _message3 m ev = 
    let b = Bytes.create 3 in
    Bytes.set b 0 (char_of_int (ev.c + m - 1));
    Bytes.set b 1 (char_of_int ev.n);
    Bytes.set b 2 (char_of_int ev.v);
    b

  let _channel b m = (int_of_char (Bytes.get b 0)) - m + 1
  let _data b i = (int_of_char (Bytes.get b i))
  let _bend_bytes ev =
    let b = Bytes.create 3 in
    Bytes.set b 0 (char_of_int (ev.c + pitch_bend_mask - 1));
    Bytes.set b 1 (char_of_int (ev.n land 0x7F)); (* lsb *)
    Bytes.set b 2 (char_of_int ((ev.n lsr 7) land 0x7F)); (* msb *)
    b
  let _bend b = 
    let lsb = (int_of_char (Bytes.get b 1)) in
    let msb = (int_of_char (Bytes.get b 2)) in
    (lsb land 0x7F) + ((msb land 0x7F) lsl 7)

  let _ev_to_string ev = Printf.sprintf "%2i %3i %3i" ev.c ev.n ev.v
  let _bytes_hex b = 
    let rec aux acc = function
      | 0 -> acc
      | n -> aux ((Printf.sprintf "%02X" (int_of_char (Bytes.get b (n-1)))) ^ acc) (n - 1) in
    aux "" (Bytes.length b)

  let to_string = function
    | Note_off ev 
      -> "Note Off      " ^ _ev_to_string ev
    | Note_on ev
      -> "Note On       " ^ _ev_to_string ev
    | Poly_pressure ev
      -> "Poly Pressure " ^ _ev_to_string ev
    | Ctrl ev
      -> "Ctrl     " ^ _ev_to_string ev
    | Prog_change ev (* v is ignored *)
      -> "Prog Change   " ^ _ev_to_string ev
    | Chan_pressure ev (* v is ignored *)
      -> "Chan Pressure " ^ _ev_to_string ev
    | Pitch_bend ev (* v is ignored *)
      -> "Pitch Bend    " ^ _ev_to_string ev
    | Raw b 
      -> "Raw           " ^ _bytes_hex b

  let to_bytes = function
    | Note_off      ev -> _message3 note_off_mask ev
    | Note_on       ev -> _message3 note_on_mask ev
    | Poly_pressure ev -> _message3 poly_pressure_mask ev
    | Ctrl          ev -> _message3 ctrl_mask ev
    | Prog_change   ev -> _message2 prog_change_mask ev
    | Chan_pressure ev -> _message2 chan_pressure_mask ev
    | Pitch_bend    ev -> _bend_bytes ev
    | Raw b -> b

  let of_bytes b =
    let s = (int_of_char (Bytes.get b 0)) land status_mask in
    if s = note_off_mask then 
      Note_off      { c = _channel b note_off_mask;      n = _data b 1; v = _data b 2 } else
    if s = note_on_mask  then 
      Note_on       { c = _channel b note_on_mask;       n = _data b 1; v = _data b 2 } else
    if s = poly_pressure_mask then 
      Poly_pressure { c = _channel b poly_pressure_mask; n = _data b 1; v = _data b 2 } else
    if s = ctrl_mask then 
      Ctrl          { c = _channel b ctrl_mask;          n = _data b 1; v = _data b 2 } else
    if s = prog_change_mask then   
      Prog_change   { c = _channel b prog_change_mask;   n = _data b 1; v = 0         } else
    if s = chan_pressure_mask then 
      Chan_pressure { c = _channel b chan_pressure_mask; n = _data b 1; v = 0          } else
    if s = pitch_bend_mask then    
      Pitch_bend    { c = _channel b pitch_bend_mask;    n = _bend b;   v = 0          } else
      Raw b
end

module Tin = struct
  type t (* boxed MyInput class *)

  external open' : int -> t = "caml_midiin_open"
  (* Open port and setup callback. *)

  external close : t -> unit = "caml_midiin_close"

  external read : t -> bytes = "caml_midiin_read"
  (* Blocking read*)

  external getPortCount : unit -> int = "caml_midiin_getPortCount"
  external getPortName : int -> string = "caml_midiin_getPortName"
end

module Tout = struct
  type t (* boxed RtMidiOut *)

  external open' : int -> t = "caml_midiout_open"

  external close : t -> unit = "caml_midiout_close"

  external sendMessage : t -> bytes -> unit = "caml_midiout_sendMessage"

  external getPortCount : unit -> int = "caml_midiout_getPortCount"
  external getPortName : int -> string = "caml_midiout_getPortName"
end


module Input = struct
  type t = Tin.t
  type port = { id : int; name : string }

  let _get_port id =
    let name = Tin.getPortName id in
    { id; name }

  let port_list () =
    let rec aux acc = function
      | -1 -> acc
      | n ->
        let acc = (_get_port n) :: acc in
        aux acc (n-1) in
    aux [] ((Tin.getPortCount ()) -1 )

  let find_port name = List.find_opt (fun p -> p.name = name) (port_list ())

  let name port = port.name

  let listen port fn = 
    let m = Tin.open' port.id in
    let finally () = Tin.close m in
    Fun.protect ~finally (fun () -> 
        let rec loop () =
          let b = Tin.read m in
          fn (Event.of_bytes b);
          loop () in
        loop ()
      )
end

module Output = struct
  type t = Tout.t
  type port = { id : int; name : string }

  let _get_port id =
    let name = Tout.getPortName id in
    { id; name }

  let port_list () =
    let rec aux acc = function
      | -1 -> acc
      | n ->
        let acc = (_get_port n) :: acc in
        aux acc (n-1) in
    aux [] ((Tout.getPortCount ()) -1 )

  let find_port name = List.find_opt (fun p -> p.name = name) (port_list ())

  let name port = port.name

  let open' port fn =
    let m = Tout.open' port.id in
    let finally () = Tout.close m in
    Fun.protect ~finally @@ fun () -> fn m

  let send m e =
    Tout.sendMessage m (Event.to_bytes e)
end