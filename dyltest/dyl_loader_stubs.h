#ifndef _DYL_LOADER_STUBS
#define _DYL_LOADER_STUBS

#ifdef __cplusplus
#define STUB extern "C"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <dlfcn.h>
using namespace std;
#else
#define STUB
#include <dlfcn.h>
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

#endif // _DYL_LOADER_STUBS