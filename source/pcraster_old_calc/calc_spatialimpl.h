#ifndef INCLUDED_CALC_SPATIALIMPL
#define INCLUDED_CALC_SPATIALIMPL

#ifndef INCLUDED_CALC_FIELDVALUE
#include "calc_fieldvalue.h"
#define INCLUDED_CALC_FIELDVALUE
#endif

namespace calc {

class Spatial;
class FieldParameter;

//! spatial value owned by a parameter
class SpatialImpl : public FieldValue {
private:
  double d_min;
  double d_max;

  void write();
public:
  //! used for initialization of computed parameter
  SpatialImpl(const FieldParameter& p,size_t index);

  //! used for initialization of input parameter
  SpatialImpl(const FieldParameter& p,size_t index, Spatial *initValue);

  virtual ~SpatialImpl();
};

}

#endif
