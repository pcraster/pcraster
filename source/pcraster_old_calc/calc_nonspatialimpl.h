#ifndef INCLUDED_OLDCALC_NONSPATIALIMPL
#define INCLUDED_OLDCALC_NONSPATIALIMPL

#include "calc_fieldvalue.h"
#include "calc_field.h"



namespace calc {

class NonSpatial;

//! spatial value owned by a parameter
class NonSpatialImpl : public FieldValue {
protected:
	void write() override;
public:
	//! used for initialization of computed parameter
	NonSpatialImpl(const FieldParameter& p,size_t index);

	//! used for initialization of input parameter
	NonSpatialImpl(const FieldParameter& p,size_t index, NonSpatial *initValue);

	~NonSpatialImpl() override;

	void assign(FieldHandle f, const class Pos *assignPoint);
};

}

#endif
