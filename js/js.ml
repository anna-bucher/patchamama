module T = struct
  type t (* boxed rocksdb_t *)
  external open' : string -> t = "caml_js_open"
  external close : t -> unit = "caml_js_close"

  external getglobal : t -> string -> unit = "caml_js_getglobal"
  external pushundefined : t -> unit = "caml_js_pushundefined"
  external pushnull : t -> unit = "caml_js_pushnull"
  external pushboolean : t -> bool -> unit = "caml_js_pushboolean"
  external pushstring : t -> string -> unit = "caml_js_pushstring"
  external pushnumber: t -> float -> unit = "caml_js_pushnumber"
  external pcall : t -> int -> unit = "caml_js_pcall"
  external pop : t -> int -> unit = "caml_js_pop"
  type [@warning "-37"] js_type = 
    | JS_ISUNDEFINED
    | JS_ISNULL
    | JS_ISBOOLEAN
    | JS_ISNUMBER
    | JS_ISSTRING
    | JS_ISFUNCTION
    | JS_ISOBJECT
  external type' : t -> int -> js_type = "caml_js_type"
  external toboolean : t -> int -> bool = "caml_js_toboolean"
  external tonumber : t -> int -> float = "caml_js_tonumber"
  external tostring : t -> int -> string = "caml_js_tostring"
  (*   external tofunction : t -> int -> fn = "caml_js_tofunction"
       external toobject : t -> int -> obj = "caml_js_toobject" *)
end

type t = T.t
type value =
  | Undefined
  | Null
  | Bool of bool
  | String of string
  | Number of float
let typeof = function
  | Undefined -> "undefined"
  | Null -> "null"
  | Bool _ -> "bool"
  | String _ -> "string"
  | Number _ -> "number"

let open' path fn = 
  let m = T.open' path in
  fn m;
  T.close m 

let push m = function
  | Undefined -> T.pushundefined m
  | Null -> T.pushnull m
  | Bool b -> T.pushboolean m b
  | String s -> T.pushstring m s
  | Number f -> T.pushnumber m f

let pushargs m args = 
  let rec aux count = function
    | [] -> count
    | h :: t -> push m h; aux (count + 1) t in
  aux 0 args

let value m idx =
  match T.type' m idx with
  | JS_ISUNDEFINED -> Undefined
  | JS_ISNULL -> Null
  | JS_ISBOOLEAN -> Bool (T.toboolean m idx)
  | JS_ISNUMBER -> Number (T.tonumber m idx)
  | JS_ISSTRING -> String (T.tostring m idx)
  | JS_ISFUNCTION -> raise (Invalid_argument "function value not implemented")
  | JS_ISOBJECT -> raise (Invalid_argument "object value not implemented")

let call m fn_name args =
  T.getglobal m fn_name;
  (* 'this' arg *)
  T.pushnull m;
  let count = pushargs m args in
  T.pcall m count;
  let v = value m (-1) in
  T.pop m 1;
  v