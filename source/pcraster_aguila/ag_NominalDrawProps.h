#ifndef INCLUDED_AG_NOMINALDRAWPROPS
#define INCLUDED_AG_NOMINALDRAWPROPS



#include "pcrtypes.h"
#include "com_classclassifier.h"
#include "ag_ClassDrawProps.h"



namespace ag {



//! The NominalDrawProps class holds info for drawing nominal data.
/*!
*/
class NominalDrawProps: public ClassDrawProps
{

private:

  //! Class info object.
  com_ClassClassifier<INT4>* _classifier;

  //! Assignment operator. NOT IMPLEMENTED.
  NominalDrawProps& operator=          (const NominalDrawProps&);

protected:

  void             reMapColours        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   NominalDrawProps    (const std::string& title,
                                        const com::RawPalette* p,
                                        com_ClassClassifier<INT4>* c);

                   NominalDrawProps    (const NominalDrawProps& properties);

  /* virtual */    ~NominalDrawProps   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const com_ClassClassifier<INT4>& classifier() const;

  com_ClassClassifier<INT4>& classifier();

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
