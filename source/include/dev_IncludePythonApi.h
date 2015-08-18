#ifndef INCLUDED_PYTHON

// If we're not processed by the moc, undefine the slots symbol. Qt defines
// slots as being empty, which messes up Python headers which use slots for
// other things.
// See also qobjectdefs.h.
#ifndef QT_MOC_CPP
  #undef slots
#endif

/*
  From the Python docs (http://docs.python.org/ext/simpleExample.html):
  Since Python may define some pre-processor definitions which affect
  the standard headers on some systems, you must include Python.h before
  any standard headers are included.

  From the boost docs:
  If you should ever have occasion to #include "python.h" directly in
  a translation unit of a program using Boost.Python, use #include
  "boost/python/detail/wrap_python.hpp" instead. It handles several
  issues necessary for use with Boost.Python, one of which is mentioned
  in the next section.

  It is assumed here that if you're interested in the Python API, you are
  using Boost.Python.
*/
#include <boost/python/detail/wrap_python.hpp>
// #include <Python.h>

// Reset the slots symbol.
#ifndef QT_MOC_CPP
  #define slots
#endif

#define INCLUDED_PYTHON
#endif
