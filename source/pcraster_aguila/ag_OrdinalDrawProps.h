#ifndef INCLUDED_AG_ORDINALDRAWPROPS
#define INCLUDED_AG_ORDINALDRAWPROPS



#include "pcrtypes.h"
#include "com_classclassifier.h"
#include "ag_ClassDrawProps.h"



namespace ag {



//! The OrdinalDrawProps class holds info for drawing ordinal data.
/*!
*/
class OrdinalDrawProps: public ClassDrawProps
{

private:

  //! Class info object.
  const com_ClassClassifier<INT4>* _classifier;

  //! Assignment operator. NOT IMPLEMENTED.
  OrdinalDrawProps& operator=          (const OrdinalDrawProps&);

protected:

  void             reMapColours        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   OrdinalDrawProps    (const std::string& title,
                                        const com::RawPalette* p,
                                  const com_ClassClassifier<INT4>* c);

  //! Copy constructor.
                   OrdinalDrawProps    (const OrdinalDrawProps& properties);

  /* virtual */    ~OrdinalDrawProps   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const com_ClassClassifier<INT4>& classifier() const;

  std::string      label               (INT4 const& value) const;

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
