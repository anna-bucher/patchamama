#include "dlfcn.h"
#include "stdio.h"

typedef int (*dynamic_fn) ();
const char *lib = "./dynlib.so";
const char *fn_name = "do_something";

/*
> gcc -C -o main main.c
> gcc -shared -o dynlib.so dynlib.c
> ./main
main: start
load: start
load: opened './dynlib.so'
main: result = 9893
main: done
> nm -C dynlib.so
0000000000003fb0 T _do_something
                 U dyld_stub_binder
> file dynlib.so
dynlib.so: Mach-O 64-bit dynamically linked shared library arm64
*/

dynamic_fn load () {
  printf ("load: start\n");
  void *handle = dlopen (lib, RTLD_NOW);
  if (!handle) {
    printf ("could not dlopen: %s\n", dlerror ());
    return NULL;
  }
  printf ("load: opened '%s'\n", lib);

  dynamic_fn func = (dynamic_fn)dlsym (handle, fn_name);
  const char *err = dlerror ();
  if (err) {
    printf ("load: could not dlsym '%s' in '%s' (%s)\n", fn_name, lib, err);
    return NULL;
  }
  return func;
}

int main () {
  printf ("main: start\n");

  dynamic_fn fn = load ();
  if (!fn)
    return 0;

  printf ("main: result = %d\n", fn ());
  printf ("main: done\n");
  return 0;
}