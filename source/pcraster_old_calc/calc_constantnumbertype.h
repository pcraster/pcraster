#ifndef INCLUDED_CALC_CONSTANTNUMBERTYPE
#define INCLUDED_CALC_CONSTANTNUMBERTYPE

#ifndef INCLUDED_CALC_FIELDTYPE
#include "calc_fieldtype.h"
#define INCLUDED_CALC_FIELDTYPE
#endif

namespace calc {

class ConstantNumberType : public FieldType {
 private:
	//! forbidden
 	ConstantNumberType(); 
	// like a non-spatial map, that is constant
 public:
	// CREATORS
 	ConstantNumberType(double value, VS restrictBy);
};

}

#endif
