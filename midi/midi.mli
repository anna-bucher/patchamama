module Event : sig
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

  val to_string : t -> string (* for debugging *)
  val to_bytes : t -> bytes
  val of_bytes : bytes -> t
end

module Input : sig
  type t
  type port
  val port_list : unit -> port list
  val find_port : string -> port option
  val name : port -> string

(*   val create : string -> (Event.t -> unit) -> unit *)
  val listen : port -> (Event.t -> unit) -> unit
end

module Output : sig
  type t
  type port
  val port_list : unit -> port list
  val find_port : string -> port option
  val name : port -> string

  val open' : port -> (t -> unit) -> unit

  val send : t -> Event.t -> unit
end