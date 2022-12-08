#include "dyl_loader_stubs.h"

#include <dlfcn.h>

typedef int (do_something_t) (int);

#define Fn_val(v) (*((do_something_t **)Data_abstract_val (v)))

STUB value caml_load (value file, value fn_name) {
  CAMLparam2 (file, fn_name);
  CAMLlocal2 (err, fn);

  if (!caml_string_is_c_safe (file))
    caml_invalid_argument ("dyl_loader: file path string is not C safe.");

  if (!caml_string_is_c_safe (fn_name))
    caml_invalid_argument ("dyl_loader: function name is not C safe.");

  char *filec = caml_stat_strdup (String_val (file));
  char *fn_namec = caml_stat_strdup (String_val (fn_name));

  caml_release_runtime_system ();
  void *lib_handle = dlopen (filec, RTLD_LOCAL | RTLD_NOW);
  caml_acquire_runtime_system ();

  caml_stat_free (filec);

  if (!lib_handle) {
    caml_stat_free (fn_namec);
    printf ("%s: Unable to load library: %s\n", __FILE__, dlerror ());
    err = caml_copy_string (dlerror ());
    caml_invalid_argument_value (err);
  }

  do_something_t *func = dlsym (lib_handle, fn_namec);

  caml_stat_free (fn_namec);

  if (!func) {
    printf ("[%s] Unable to get symbol: %s\n", __FILE__, dlerror ());
    err = caml_copy_string (dlerror ());
    caml_invalid_argument_value (err);
  }

  fn = caml_alloc (1, Abstract_tag);
  Fn_val (fn) = func;

  CAMLreturn (fn);
}

STUB value caml_call (value fn, value foo) {
  CAMLparam2 (fn, foo);
  CAMLlocal2 (err, ret);

  do_something_t *func = Fn_val (fn);
  int r = (*func) (Int_val (foo));
  CAMLreturn (Val_int (r));
}