#ifndef INCLUDED_CALC_CONSTANTNUMBERTYPE
#define INCLUDED_CALC_CONSTANTNUMBERTYPE

#include "calc_fieldtype.h"


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
