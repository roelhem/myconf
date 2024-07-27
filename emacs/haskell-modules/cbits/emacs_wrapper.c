#include <emacs-module.h>

#include "HsFFI.h"
#include "Rts.h"
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif
extern HsBool initialise(struct emacs_runtime *ert);
#ifdef __cplusplus
}
#endif

#define ARR_SIZE(x) (sizeof(x) / sizeof(x[0]))

int plugin_is_GPL_compatible = 1;

HsBool init(void) {
  char *argv[] = {"libmy-emacs-native", "+RTS", "-N", "-RTS", NULL};
  int argc = ARR_SIZE(argv) - 1;
  char **pargv = argv;

  {
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    hs_init_ghc(&argc, &pargv, conf);
  }

  return HS_BOOL_TRUE;
}

void deinit(void) { hs_exit(); }

static emacs_value Fdeinit(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                           void *data) {
  deinit();
  return env->intern(env, "nil");
}

static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  emacs_value args[] = {Qsym, Sfun};

  env->funcall(env, Qfset, ARR_SIZE(args), args);
}

int emacs_module_init(struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);

  const char *deinit_doc = "Finalise haskell-native module, no functions from "
                           "it may be called after this function.";

  emacs_value deinitFun =
      env->make_function(env, 0, 0, Fdeinit, deinit_doc, NULL);

  bind_function(env, "hs+deinit", deinitFun);

  return !(init() && initialise(ert));
}
