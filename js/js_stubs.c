#include "js_stubs.h"

#define Js_State_val(v) (*((js_State **)Data_abstract_val (v)))

// open' : string -> t = "caml_js_open"
CAMLprim value caml_js_open (value file) {
  CAMLparam1 (file);
  CAMLlocal1 (js);

  if (!caml_string_is_c_safe (file))
    caml_invalid_argument ("js_open: file path string is not C safe.");

  char *filec = caml_stat_strdup (String_val (file));

  caml_release_runtime_system ();

  js_State *J = js_newstate (NULL, NULL, JS_STRICT);
  js_dofile (J, filec);
  caml_stat_free (filec);

  caml_acquire_runtime_system ();

  js = caml_alloc (1, Abstract_tag);
  Js_State_val (js) = J;
  CAMLreturn (js);
}

// close : t -> unit = "caml_js_close"
CAMLprim value caml_js_close (value js) {
  CAMLparam1 (js);
  js_freestate (Js_State_val (js));
  CAMLreturn (Val_unit);
}

//  getglobal : t -> string -> unit = "caml_js_getglobal"
CAMLprim value caml_js_getglobal (value js, value name) {
  CAMLparam2 (js, name);
  js_getglobal (Js_State_val (js), String_val (name));
  CAMLreturn (Val_unit);
}

//  pushundefined : t -> unit = "caml_js_pushundefined"
CAMLprim value caml_js_pushundefined (value js) {
  CAMLparam1 (js);
  js_pushundefined (Js_State_val (js));
  CAMLreturn (Val_unit);
}

//  pushnull : t -> unit = "caml_js_pushnull"
CAMLprim value caml_js_pushnull (value js) {
  CAMLparam1 (js);
  js_pushnull (Js_State_val (js));
  CAMLreturn (Val_unit);
}

// pushboolean : t -> bool -> unit = "caml_js_pushbool"
CAMLprim value caml_js_pushboolean (value js, value s) {
  CAMLparam2 (js, s);
  js_pushboolean (Js_State_val (js), Bool_val (s));
  CAMLreturn (Val_unit);
}

// pushstring : t -> string -> unit = "caml_js_pushstring"
CAMLprim value caml_js_pushstring (value js, value s) {
  CAMLparam2 (js, s);
  js_pushstring (Js_State_val (js), String_val (s));
  CAMLreturn (Val_unit);
}

// pushnumber : t -> float -> unit = "caml_js_pushnumber"
CAMLprim value caml_js_pushnumber (value js, value n) {
  CAMLparam2 (js, n);
  js_pushnumber (Js_State_val (js), Double_val (n));
  CAMLreturn (Val_unit);
}

// pcall : t -> int -> unit = "caml_js_pcall"
CAMLprim value caml_js_pcall (value js, value argc) {
  CAMLparam2 (js, argc);
  if (js_pcall (Js_State_val (js), Int_val (argc))) {
    caml_invalid_argument ("An exception occured in js callback.");
  }
  CAMLreturn (Val_unit);
}

// pop : t -> int -> unit = "caml_js_pop"
CAMLprim value caml_js_pop (value js, value count) {
  CAMLparam2 (js, count);
  int n = Int_val (count);
  js_pop (Js_State_val (js), n);
  CAMLreturn (Val_unit);
}

// type : t -> int -> js_type = "caml_js_type"
CAMLprim value caml_js_type (value js, value idx) {
  CAMLparam2 (js, idx);
  int n = js_type (Js_State_val (js), idx);
  CAMLreturn (Val_int (n));
}

// toboolean : t -> int -> bool = "caml_js_toboolean"
CAMLprim value caml_js_toboolean (value js, value idx) {
  CAMLparam2 (js, idx);
  int b = js_toboolean (Js_State_val (js), idx);
  CAMLreturn (Val_bool (b));
}

// tonumber : t -> int -> int = "caml_js_tonumber"
CAMLprim value caml_js_tonumber (value js, value idx) {
  CAMLparam2 (js, idx);
  CAMLlocal1 (ret);
  ret = caml_copy_double (js_tonumber (Js_State_val (js), idx));
  CAMLreturn (ret);
}

// tostring : t -> int -> string = "caml_js_tostring"
CAMLprim value caml_js_tostring (value js, value idx) {
  CAMLparam2 (js, idx);
  CAMLlocal1 (ret);
  const char *s = js_tostring (Js_State_val (js), idx);
  ret = caml_copy_string (s);
  CAMLreturn (ret);
}
