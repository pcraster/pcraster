#ifndef INCLUDED_COM_CONST
#define INCLUDED_COM_CONST





/*!
  \class com_Const
  \brief The com_Const class contains library wide constants.

  The com_Const class is designed for library wide constants. No object of
  this type can be made. The class only contains static constant values.
*/
struct com_Const
{

  static const double      LOG_MIN;

  static const double      LOG_MAX;

  static const double      StepEps;

};

namespace com {
  extern const double minEps;

  extern const double DEG2RAD;
  extern const double SMALL;

  extern const char*  DEVELOPERMESSAGE_KOR;
  extern const char*  DEVELOPERMESSAGE_KOR_EDZER;
  // extern const char*  DEVELOPERMESSAGE_KOR_CEES;
} // namespace com

#endif

