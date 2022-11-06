#include "midi_stubs.h"

enum { MMSystem, CoreMidi, ALSA };

const char *interface_name (int interface) {
  switch (interface) {
  case CoreMidi:
    return "CoreMIDI";
  case MMSystem:
    return "MMSystem";
  case ALSA:
    return "ALSA";
  default:
    return "";
  }
}

const char *pm_error_string (PmError pm_error) {
  switch (pm_error) {
  case pmInvalidDeviceId:
    return "Out of range or output device when input is requested or "
           "input device when output is requested or device is already "
           "opened.";

  case pmBadPtr:
    return "PortMidiStream parameter is NULL or stream is not opened or"
           "stream is output when input is required or"
           "stream is input when output is required.";

  case pmBadData:
    return "Illegal midi data, e.g., missing EOX.";

  case pmBufferMaxSize:
    return "Buffer is already as large as it can be.";

  case pmNotImplemented:
    return "The function is not implemented, nothing was done.";

  case pmInterfaceNotSupported:
    return "The requested interface is not supported.";

  case pmNameConflict:
    return "Cannot create virtual device because name is taken.";

  default:
    return "Unknown error.";
  }
}

#define PmDeviceID_val(v) ((PmDeviceID)Int_val (v))
#define check_no_error(res, err)                                               \
  if ((res) < 0) {                                                             \
    err = caml_copy_string (pm_error_string (res));                            \
    caml_invalid_argument_value (err);                                         \
  }

// open' : interface -> string -> t = "caml_midi_create_virtual_input"
CAMLprim value caml_midi_create_virtual_input (value interface, value name) {
  CAMLparam1 (name);
  CAMLlocal1 (err);

  if (!caml_string_is_c_safe (name))
    caml_invalid_argument (
        "midi_create_virtual_input: name string is not C safe.");

  char *namec = caml_stat_strdup (String_val (name));
  const char *interf = "";
  void *device_info = NULL;

  caml_release_runtime_system ();
  PMEXPORT PmError res = Pm_CreateVirtualInput (
      namec, interface_name (Int_val (interface)), device_info);
  caml_acquire_runtime_system ();

  caml_stat_free (namec);
  check_no_error (res, err);

  CAMLreturn (Val_int (res));
}

// delete_virtual_device : t -> unit = "caml_midi_delete_virtual_device"
CAMLprim value caml_midi_delete_virtual_device (value dev) {
  CAMLparam1 (dev);
  CAMLlocal1 (err);
  PMEXPORT PmError res = Pm_DeleteVirtualDevice (PmDeviceID_val (dev));
  check_no_error (res, err);
  CAMLreturn (Val_unit);
}
