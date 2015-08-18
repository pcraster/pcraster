#ifndef INCLUDED_CALC_NONSPATIALIMPL
#define INCLUDED_CALC_NONSPATIALIMPL

#ifndef INCLUDED_CALC_FIELDVALUE
#include "calc_fieldvalue.h"
#define INCLUDED_CALC_FIELDVALUE
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

namespace calc {

class NonSpatial;

//! spatial value owned by a parameter
class NonSpatialImpl : public FieldValue {
protected:
	virtual void write();
public:
	//! used for initialization of computed parameter
	NonSpatialImpl(const FieldParameter& p,size_t index);

	//! used for initialization of input parameter
	NonSpatialImpl(const FieldParameter& p,size_t index, NonSpatial *initValue);

	virtual ~NonSpatialImpl();

	void assign(FieldHandle f, const class Pos *assignPoint);
};

}

#endif
