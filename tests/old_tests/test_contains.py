import sys

_set = set()
_globals = set()
_have_functions = set()

def _add(str, fn):
    if (fn in _globals) and (str in _have_functions):
        _set.add(_globals[fn])

_add("HAVE_FACCESSAT", "access")
sys.argv.append("this is something")
