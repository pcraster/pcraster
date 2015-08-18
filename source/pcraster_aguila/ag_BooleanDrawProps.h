#ifndef INCLUDED_AG_BOOLEANDRAWPROPS
#define INCLUDED_AG_BOOLEANDRAWPROPS



#include "pcrtypes.h"
#include "com_classclassifier.h"
#include "ag_ClassDrawProps.h"



namespace ag {



//! The BooleanDrawProps class holds info for drawing boolean data.
/*!
*/
class BooleanDrawProps: public ClassDrawProps
{

private:

  //! Class info object.
  const com_ClassClassifier<UINT1>* _classifier;

  //! Assignment operator. NOT IMPLEMENTED.
  BooleanDrawProps& operator=          (const BooleanDrawProps&);

protected:

  void             reMapColours        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BooleanDrawProps    (const std::string& title,
                                        const com::RawPalette* p,
                                   const com_ClassClassifier<UINT1>* c);

                   BooleanDrawProps    (const BooleanDrawProps& properties);

  /* virtual */    ~BooleanDrawProps   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const com_ClassClassifier<UINT1>& classifier() const;

  std::string      label               (UINT1 const& value);

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
