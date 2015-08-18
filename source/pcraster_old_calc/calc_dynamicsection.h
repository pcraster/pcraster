#ifndef INCLUDED_CALC_DYNAMICSECTION
#define INCLUDED_CALC_DYNAMICSECTION

#ifndef INCLUDED_CALC_INNERSTATEMENTBLOCK
# include "calc_innerstatementblock.h"
#define INCLUDED_CALC_INNERSTATEMENTBLOCK
#endif



namespace calc {

class DynamicSection : public InnerStatementBlock {
 public:
	DynamicSection(const Element& p,
		StatementBlock *b):
	 InnerStatementBlock(p,b) {}; 

	// MANIPULATORS
	void executeBlock();
	// ACCESSORS
 	bool inDynamic() const;
	void print(InfoScript& i)const;
};

}

#endif
