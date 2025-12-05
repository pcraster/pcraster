#ifndef INCLUDED_OLDCALC_DEFPAR
#define INCLUDED_OLDCALC_DEFPAR

#include "calc_parspar.h"
#include "calc_arraydefvector.h"


namespace com {
  class Exception;
}

namespace calc {

struct ConstructPar;
class  IndexTable;
class  FieldType;
class  SubParameter;

//! define par by it's definition (in the binding)
/*! is only used by the parser class
 */
class DefPar : public ParsPar {
  //! descriptor
  ArrayDefVector       d_descriptor;

  void initError(const com::Exception& msg)const;
public:
  DefPar(const ConstructPar& p);
  const ArrayDefVector& descriptor() const override;

  SubParameter *indexTable(
    const IndexTable *it,
    bool constant,
    const FieldType& useType) const;
};

}

#endif
