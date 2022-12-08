#include "reaper_stubs.h"

// #define Foo_val(v) (*((Foo **)Data_abstract_val (v)))

STUB value caml_reaper_test (value unit) {
  CAMLparam1 (unit);
  CAMLreturn (Val_unit);
}
