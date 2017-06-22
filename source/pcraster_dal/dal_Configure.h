#ifndef INCLUDED_DAL_CONFIGURE
#define INCLUDED_DAL_CONFIGURE



// External headers.
#ifndef INCLUDED_BOOST_CONFIG
#include <boost/config.hpp>
#define INCLUDED_BOOST_CONFIG
#endif

// Project headers.

// Module headers.



#ifdef BOOST_HAS_DECLSPEC
#  if defined(PCR_DAL_SHARED_LINK)
#    ifdef PCR_DAL_SOURCE
#      define PCR_DAL_DECL __declspec(dllexport)
#    else
#      define PCR_DAL_DECL __declspec(dllimport)
#    endif
#  endif
#else
#  if defined(PCR_DAL_SHARED_LINK)
#    ifdef PCR_DAL_SOURCE
#      define PCR_DAL_DECL __attribute((visibility("default")))
#    else
#      define PCR_DAL_DECL __attribute((visibility("default")))
#    endif
#  endif
#endif

#ifndef PCR_DAL_DECL
#  define PCR_DAL_DECL
#endif



namespace dal {


} // namespace dal

#endif
