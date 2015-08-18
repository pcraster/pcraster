#ifndef INCLUDED_GEO_CONST
#define INCLUDED_GEO_CONST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



/*!
  \class geo_Const
  \brief The geo_Const class contains library wide constants.

  The geo_Const class is designed for library wide constants. No object of
  this type can be made. The class only contains static constant values.
*/
struct geo_Const
{
  // illegal const value.
  static const int ILLEGAL;

  // projections
  static const int YINCRT2B;
  static const int YINCRB2T;

  static const int UPPERLEFT;
  static const int LOWERRIGHT;

  static const int LEGENDINWINDOW;
  static const int LEGENDUPPERLEFT;
  static const int LEGENDUPPERRIGHT;
  static const int LEGENDLOWERLEFT;
  static const int LEGENDLOWERRIGHT;

  // topologies
  static const int VECTOR;
  static const int RASTER;

  // valuescales
  enum ValueScale { BOOLEAN, NOMINAL, ORDINAL, SCALAR, DIRECTIONAL, LDD,
                    ILLEGALVS };

};

#endif

