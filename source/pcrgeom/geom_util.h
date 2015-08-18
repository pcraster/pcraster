#ifndef INCLUDED_GEOM_UTIL
#define INCLUDED_GEOM_UTIL



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEOM_RECTANGLE
#include "geom_rectangle.h"
#define INCLUDED_GEOM_RECTANGLE
#endif

#ifndef INCLUDED_GEOM_SIZE
#include "geom_size.h"
#define INCLUDED_GEOM_SIZE
#endif



/*!
  \struct geom_Util
  \brief The geom_Util class is for static utility functions.
*/
//       1         2         3         4         5         6         7         8
struct geom_Util
{

  static geom_Size<int> adjustWidthOrHeight(const geom_Size<int> &size,
                                            double                width,
                                            double                height);

  static geom::Rectangle<int> reAspect (const geom::Rectangle<int> &size,
                                        double                     width,
                                        double                     height);

};

#endif



