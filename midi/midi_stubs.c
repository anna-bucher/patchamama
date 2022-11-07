#include "midi_stubs.h"

enum { MMSystem, CoreMidi, ALSA };

const char *interface_name (int interface) {
  switch (interface) {
  case MMSystem:
    return "MMSystem";
  case CoreMidi:
    return "CoreMIDI";
  case ALSA:
    return "ALSA";
  default:
    return "";
  }
}

int interface_type (value err, const char *interface) {
  if (strcmp (interface, "MMSystem") == 0)
    return 0;
  else if (strcmp (interface, "CoreMIDI") == 0)
    return 1;
  else if (strcmp (interface, "ALSA") == 0)
    return 2;
  else {
    err = caml_copy_string ("invalid interface");
    caml_invalid_argument_value (err);
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

// create_virtual_input : interface -> string -> t =
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
  PmDeviceID res = Pm_CreateVirtualInput (
      namec, interface_name (Int_val (interface)), device_info);
  caml_acquire_runtime_system ();

  caml_stat_free (namec);
  check_no_error (res, err);

  CAMLreturn (Val_int (res));
}

// create_virtual_intput : interface -> string -> t =
CAMLprim value caml_midi_create_virtual_output (value interface, value name) {
  CAMLparam1 (name);
  CAMLlocal1 (err);

  if (!caml_string_is_c_safe (name))
    caml_invalid_argument (
        "midi_create_virtual_output: name string is not C safe.");

  char *namec = caml_stat_strdup (String_val (name));
  const char *interf = "";
  void *device_info = NULL;

  caml_release_runtime_system ();
  PmDeviceID res = Pm_CreateVirtualOutput (
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
  PmError res = Pm_DeleteVirtualDevice (PmDeviceID_val (dev));
  check_no_error (res, err);
  CAMLreturn (Val_unit);
}

// get_device_info : int -> device_info = "caml_midi_get_device_info"
CAMLprim value caml_midi_get_device_info (value dev) {
  CAMLparam1 (dev);
  CAMLlocal2 (info, err);
  const PmDeviceInfo *infoc = Pm_GetDeviceInfo (PmDeviceID_val (dev));
  if (infoc == NULL) {
    err = caml_copy_string ("midi_get_device_info: invalid device id");
    caml_invalid_argument_value (err);
  }
  info = caml_alloc (7, 0);
  Store_field (info, 0, Val_int (infoc->structVersion));
  Store_field (info, 1, Val_int (interface_type (err, infoc->interf)));
  Store_field (info, 2, caml_copy_string (infoc->name));
  Store_field (info, 3, Val_int (infoc->input));
  Store_field (info, 4, Val_int (infoc->output));
  Store_field (info, 5, Val_bool (infoc->opened));
  Store_field (info, 6, Val_bool (infoc->is_virtual));
  CAMLreturn (info);
}