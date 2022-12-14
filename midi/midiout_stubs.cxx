#include "midi_stubs.h"

#define RtMidiOut_val(v) (*((RtMidiOut **)Data_abstract_val (v)))

STUB value caml_midiout_open (value port) {
  CAMLparam1 (port);
  CAMLlocal2 (err, midiout);
  try {
    RtMidiOut *m = new RtMidiOut ();
    m->openPort (Int_val (port));
    midiout = caml_alloc (1, Abstract_tag);
    RtMidiOut_val (midiout) = m;
    CAMLreturn (midiout);
  } catch (RtMidiError &error) {
    err = caml_copy_string ((error.getMessage ().c_str ()));
    caml_invalid_argument_value (err);
  }
}

STUB value caml_midiout_sendMessage (value midiout, value bytes) {
  CAMLparam2 (midiout, bytes);
  CAMLlocal1 (err);
  try {
    RtMidiOut *midioutc = RtMidiOut_val (midiout);

    unsigned char *b = Bytes_val (bytes);
    size_t len = caml_string_length (bytes);
    Message m = Message (b, b + len);
    midioutc->sendMessage (&m);
    CAMLreturn (Val_unit);
  } catch (RtMidiError &error) {
    err = caml_copy_string ((error.getMessage ().c_str ()));
    caml_invalid_argument_value (err);
  }
}

STUB value caml_midiout_close (value midiout) {
  CAMLparam1 (midiout);
  CAMLlocal1 (err);
  try {
    RtMidiOut *midioutc = RtMidiOut_val (midiout);

    delete midioutc;
    CAMLreturn (Val_unit);
  } catch (RtMidiError &error) {
    err = caml_copy_string ((error.getMessage ().c_str ()));
    caml_invalid_argument_value (err);
  }
}

STUB value caml_midiout_getPortCount (value unit) {
  CAMLparam1 (unit);
  CAMLlocal1 (err);
  try {
    RtMidiOut m;
    CAMLreturn (Val_int (m.getPortCount ()));
  } catch (RtMidiError &error) {
    err = caml_copy_string ((error.getMessage ().c_str ()));
    caml_invalid_argument_value (err);
  }
}

STUB value caml_midiout_getPortName (value id) {
  CAMLparam1 (id);
  CAMLlocal2 (err, name);
  try {
    RtMidiOut m;
    std::string namec = m.getPortName (Int_val (id));
    name = caml_copy_string (namec.c_str ());
    CAMLreturn (name);
  } catch (RtMidiError &error) {
    err = caml_copy_string ((error.getMessage ().c_str ()));
    caml_invalid_argument_value (err);
  }
}

/*
#define PortMidiStream_val(v) (*((PortMidiStream **)Data_abstract_val (v)))
#define PmDeviceID_val(v) ((PmDeviceID)Int_val (v))
#define check_no_error(errc, err)                                              \
  if ((errc) < 0) {                                                            \
    err = caml_copy_string (Pm_GetErrorText (errc));                           \
    caml_invalid_argument_value (err);                                         \
  }

// initialize : unit -> unit =
STUBexport value caml_midi_initialize (value unit) {
  CAMLparam1 (unit);
  CAMLlocal1 (err);

  PmError errc = Pm_Initialize ();
  check_no_error (errc, err);

  CAMLreturn (Val_unit);
}

// terminate : unit -> unit =
STUBexport value caml_midi_terminate (value unit) {
  CAMLparam1 (unit);
  CAMLlocal1 (err);

  PmError errc = Pm_Terminate ();
  check_no_error (errc, err);

  CAMLreturn (Val_unit);
}

// create_virtual_input : interface -> string -> t =
STUBexport value caml_midi_create_virtual_input (value interface, value name) {
  CAMLparam1 (name);
  CAMLlocal1 (err);

  if (!caml_string_is_c_safe (name))
    caml_invalid_argument (
        "midi_create_virtual_input: name string is not C safe.");

  char *namec = caml_stat_strdup (String_val (name));
  const char *interf = "";
  void *device_info = NULL;

  caml_release_runtime_system ();
  PmError errc = Pm_CreateVirtualInput (
      namec, interface_name (Int_val (interface)), device_info);
  caml_acquire_runtime_system ();

  caml_stat_free (namec);
  check_no_error (errc, err);

  CAMLreturn (Val_int ((PmDeviceID)errc));
}

// create_virtual_intput : interface -> string -> t =
STUBexport value caml_midi_create_virtual_output (value interface, value name) {
  CAMLparam1 (name);
  CAMLlocal1 (err);

  if (!caml_string_is_c_safe (name))
    caml_invalid_argument (
        "midi_create_virtual_output: name string is not C safe.");

  char *namec = caml_stat_strdup (String_val (name));
  const char *interf = "";
  void *device_info = NULL;

  caml_release_runtime_system ();
  PmError errc = Pm_CreateVirtualOutput (
      namec, interface_name (Int_val (interface)), device_info);
  caml_acquire_runtime_system ();

  caml_stat_free (namec);
  check_no_error (errc, err);

  CAMLreturn (Val_int ((PmDeviceID)errc));
}

// delete_virtual_device : t -> unit = "caml_midi_delete_virtual_device"
STUBexport value caml_midi_delete_virtual_device (value dev) {
  CAMLparam1 (dev);
  CAMLlocal1 (err);
  PmError errc = Pm_DeleteVirtualDevice (PmDeviceID_val (dev));
  check_no_error (errc, err);
  CAMLreturn (Val_unit);
}

// get_device_info : int -> device_info = "caml_midi_get_device_info"
STUBexport value caml_midi_get_device_info (value dev) {
  CAMLparam1 (dev);
  CAMLlocal2 (info, err);
  const PmDeviceInfo *infoc = Pm_GetDeviceInfo (PmDeviceID_val (dev));
  if (infoc == NULL) {
    err = caml_copy_string ("midi_get_device_info: invalid device id");
    caml_invalid_argument_value (err);
  }
  info = caml_alloc (8, 0);
  Store_field (info, 0, Val_int (infoc->structVersion));
  Store_field (info, 1, Val_int (interface_type (err, infoc->interf)));
  Store_field (info, 2, caml_copy_string (infoc->name));
  Store_field (info, 3, Val_bool (infoc->input));
  Store_field (info, 4, Val_bool (infoc->output));
  Store_field (info, 5, Val_bool (infoc->opened));
  Store_field (info, 6, Val_bool (infoc->is_virtual));
  Store_field (info, 7, dev);
  CAMLreturn (info);
}

// count_devices : unit -> int = "caml_midi_count_devices"
STUBexport value caml_midi_count_devices (value unit) {
  CAMLparam1 (unit);
  CAMLreturn (Val_int (Pm_CountDevices ()));
}

// open_input : int -> int -> stream = "caml_midi_open_input"
STUBexport value caml_midi_open_input (value dev, value buf_size) {
  CAMLparam2 (dev, buf_size);
  CAMLlocal2 (err, stream);
  PortMidiStream *streamc = NULL;
  PmError errc = Pm_OpenInput (&streamc, PmDeviceID_val (dev), NULL,
                               Int_val (buf_size), NULL, NULL);
  check_no_error (errc, err);
  stream = caml_alloc (1, Abstract_tag);
  PortMidiStream_val (stream) = streamc;
  CAMLreturn (stream);
}

// open_output : int -> int -> int -> stream = "caml_midi_open_output"
STUBexport value caml_midi_open_output (value dev, value buf_size,
                                        value latency) {
  CAMLparam3 (dev, buf_size, latency);
  CAMLlocal2 (err, stream);
  PortMidiStream *streamc = NULL;
  PmError errc =
      Pm_OpenOutput (&streamc, PmDeviceID_val (dev), NULL, Int_val (buf_size),
                     NULL, NULL, Int_val (latency));
  check_no_error (errc, err);
  stream = caml_alloc (1, Abstract_tag);
  PortMidiStream_val (stream) = streamc;
  CAMLreturn (stream);
}

// close_input : int -> int -> int = "caml_midi_close_input"
STUBexport value caml_midi_close (value stream) {
  CAMLparam1 (stream);
  CAMLlocal1 (err);
  PmError errc = Pm_Close (PortMidiStream_val (stream));
  check_no_error (errc, err);
  CAMLreturn (Val_unit);
}

STUBexport value caml_midi_read (value stream) {
  CAMLparam1 (stream);
  CAMLlocal1 (err);
  PmEvent buffer[1];

  int countc = Pm_Read (PortMidiStream_val (stream), buffer, 1);
  CAMLreturn (Val_int (countc));
}
*/