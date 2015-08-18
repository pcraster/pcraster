#ifndef  INCLUDED_CALC_CONSTRUCTPAR
#define  INCLUDED_CALC_CONSTRUCTPAR

#ifndef  INCLUDED_CALC_SYMBOL
#include  "calc_symbol.h"
#define  INCLUDED_CALC_SYMBOL
#endif

#ifndef  INCLUDED_VECTOR
#include  <vector>
#define  INCLUDED_VECTOR
#endif

namespace calc {

class StatementBlock;

//! collector to create parameters
/*!
 * parsing forces to do this: build this struct up
 * and when finished pass it to another constructor
 */
typedef struct ConstructPar {

  ConstructPar() {};

  ConstructPar(
    StatementBlock *block, const Symbol& name):
    d_block(block), d_name(name) {};

  //! block where parameter is constructed
  StatementBlock      *d_block;

  //! name
  Symbol               d_name;

  //! it's indices
  std::vector<Symbol>  d_index;

} ConstructPar;

}

#endif
