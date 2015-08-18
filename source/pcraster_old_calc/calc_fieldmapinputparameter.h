#ifndef INCLUDED_CALC_FIELDMAPINPUTPARAMETER
#define INCLUDED_CALC_FIELDMAPINPUTPARAMETER

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_CALC_FIELDNEWPARAMETER
#include "calc_fieldnewparameter.h"
#define INCLUDED_CALC_FIELDNEWPARAMETER
#endif

namespace calc {
  class IoFieldStrategy;
}

namespace calc {
//! used when external maps are read
/*! Just like FieldNewParameters, but they do have an initial
 *  value. This means this parameter is always read FIRST from
 *  an external source. A new value for the parameter CAN be
 *  written later.
 */
class  FieldMapInputParameter : public FieldNewParameter {
 private:
    //! filenames to read maps from
    const std::vector<std::string> d_initVals;
    //! implements the format of the maps
    const IoFieldStrategy& d_ioFieldStrategy;
 public:
    FieldMapInputParameter(const ParsPar& par, bool constant,
                      VS vs, const std::vector<std::string>& vals,
                      const IoFieldStrategy& s);

   // MANIPULATORS
   void goInScope();
};

}

#endif
