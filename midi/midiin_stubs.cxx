#include "midi_stubs.h"

typedef std::mutex Mutex;
typedef std::unique_lock<Mutex> Lock;
typedef std::condition_variable Condition;

class MyInput {
  Mutex mut;
  Message message;
  Condition has_data;
  RtMidiIn midi;
  static void read_callback (double deltatime, Message *message,
                             void *userData) {
    MyInput *m = (MyInput *)userData;
    m->write (*message);
  }

public:
  MyInput (int n) {
    midi.openPort (n);
    midi.setCallback (&read_callback, (void *)this);
  }

  // Blocking read until a message is posted.
  Message read () {
    Lock l (mut);
    has_data.wait (l);
    return message;
  }

  // Write message and unlock read thread.
  void write (const Message &m) {
    Lock l (mut);
    message = m;
    has_data.notify_one ();
  }
};

#define MyInput_val(v) (*((MyInput **)Data_abstract_val (v)))

STUB value caml_midiin_open (value port) {
  CAMLparam1 (port);
  CAMLlocal2 (err, midiin);
  try {
    MyInput *m = new MyInput (Int_val (port));
    midiin = caml_alloc (1, Abstract_tag);
    MyInput_val (midiin) = m;
    CAMLreturn (midiin);
  } catch (RtMidiError &error) {
    err = caml_copy_string ((error.getMessage ().c_str ()));
    caml_invalid_argument_value (err);
  }
}

STUB value caml_midiin_read (value midiin) {
  CAMLparam1 (midiin);
  CAMLlocal2 (err, bytes);
  try {
    MyInput *midiinc = MyInput_val (midiin);

    Message m = midiinc->read ();
    bytes = caml_alloc_string (m.size ());
    unsigned char *b = Bytes_val (bytes);
    for (int i = 0; i < m.size (); ++i)
      b[i] = m[i];

    CAMLreturn (bytes);
  } catch (RtMidiError &error) {
    err = caml_copy_string ((error.getMessage ().c_str ()));
    caml_invalid_argument_value (err);
  }
}

STUB value caml_midiin_close (value midiin) {
  CAMLparam1 (midiin);
  CAMLlocal1 (err);
  try {
    MyInput *midiinc = MyInput_val (midiin);

    delete midiinc;
    CAMLreturn (Val_unit);
  } catch (RtMidiError &error) {
    err = caml_copy_string ((error.getMessage ().c_str ()));
    caml_invalid_argument_value (err);
  }
}

STUB value caml_midiin_getPortCount (value unit) {
  CAMLparam1 (unit);
  CAMLlocal1 (err);
  try {
    RtMidiIn m;
    CAMLreturn (Val_int (m.getPortCount ()));
  } catch (RtMidiError &error) {
    err = caml_copy_string ((error.getMessage ().c_str ()));
    caml_invalid_argument_value (err);
  }
}

STUB value caml_midiin_getPortName (value id) {
  CAMLparam1 (id);
  CAMLlocal2 (err, name);
  try {
    RtMidiIn m;
    std::string namec = m.getPortName (Int_val (id));
    name = caml_copy_string (namec.c_str ());
    CAMLreturn (name);
  } catch (RtMidiError &error) {
    err = caml_copy_string ((error.getMessage ().c_str ()));
    caml_invalid_argument_value (err);
  }
}
