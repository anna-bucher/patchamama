#ifndef _REAPER_STUBS
#define _REAPER_STUBS

#ifdef __cplusplus
#define STUB extern "C"
#include <cstdio>
#include <cstdlib>
#include <cstring>
using namespace std;
#else
#define STUB
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include "vendor/reaper-plugins/reaper_csurf/csurf.h"

#endif // _REAPER_STUBS