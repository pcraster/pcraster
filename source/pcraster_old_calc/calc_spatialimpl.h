#ifndef INCLUDED_OLDCALC_SPATIALIMPL
#define INCLUDED_OLDCALC_SPATIALIMPL

#include "calc_fieldvalue.h"



namespace calc {

class Spatial;
class FieldParameter;

//! spatial value owned by a parameter
class SpatialImpl : public FieldValue {
private:
  double d_min{};
  double d_max{};

  void write() override;
public:
  //! used for initialization of computed parameter
  SpatialImpl(const FieldParameter& p,size_t index);

  //! used for initialization of input parameter
  SpatialImpl(const FieldParameter& p,size_t index, Spatial *initValue);

  ~SpatialImpl() override;
};

}

#endif
