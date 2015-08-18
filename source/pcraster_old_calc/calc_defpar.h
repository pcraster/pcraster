#ifndef  INCLUDED_CALC_DEFPAR
#define  INCLUDED_CALC_DEFPAR

#ifndef  INCLUDED_CALC_PARSPAR
#include  "calc_parspar.h"
#define  INCLUDED_CALC_PARSPAR
#endif

#ifndef  INCLUDED_CALC_ARRAYDEFVECTOR
#include  "calc_arraydefvector.h"
#define  INCLUDED_CALC_ARRAYDEFVECTOR
#endif

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
  const ArrayDefVector& descriptor() const;

  SubParameter *indexTable(
    const IndexTable *it,
    bool constant,
    const FieldType& useType) const;
};

}

#endif
