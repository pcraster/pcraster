#ifndef INCLUDED_OLDCALC_DYNAMICSECTION
#define INCLUDED_OLDCALC_DYNAMICSECTION

#include "calc_innerstatementblock.h"



namespace calc {

class DynamicSection : public InnerStatementBlock {
 public:
	DynamicSection(const Element& p,
		StatementBlock *b):
	 InnerStatementBlock(p,b) {}

	// MANIPULATORS
	void executeBlock() override;
	// ACCESSORS
 	bool inDynamic() const override;
	void print(InfoScript& i)const override;
};

}

#endif
