module Event : sig
  type ev = { c : int; n : int; v : int; }
  type t = NoteOn of ev | NoteOff of ev | Ctrl of ev | Raw of bytes

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
  val listen : (Event.t -> unit) -> port -> unit
end

module Output : sig
  type t
  type port
  val port_list : unit -> port list
  val find_port : string -> port option
end