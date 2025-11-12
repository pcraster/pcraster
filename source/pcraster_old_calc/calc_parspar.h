#ifndef  INCLUDED_CALC_PARSPAR
#define  INCLUDED_CALC_PARSPAR

#include  "calc_bindedsymbol.h"

#include  <vector>



namespace calc {

class StatementBlock;
struct ConstructPar;
class ArrayDefVector;

//! abstract class for DefPar and UsePar
class ParsPar : public BindedSymbol {
protected:
  //! block where this parsed parameter instance  is constructed
  StatementBlock      *d_block;
  //! 0-size if none
  std::vector<Symbol>  d_index;

  // CONSTRUCTORS
  ParsPar(const ConstructPar& p);
  ParsPar(StatementBlock *block, const Symbol& p);

public:
  virtual const ArrayDefVector& descriptor() const=0;


  bool isArray() const;
  StatementBlock* block() const { return d_block; }
};

}

#endif
