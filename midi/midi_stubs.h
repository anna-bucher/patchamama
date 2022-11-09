#ifndef _MIDI_STUBS
#define _MIDI_STUBS

#ifdef __cplusplus
#define STUB extern "C"
#else
#define STUB
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <RtMidi.h>

typedef std::vector<unsigned char> Message;

#endif // _MIDI_STUBS