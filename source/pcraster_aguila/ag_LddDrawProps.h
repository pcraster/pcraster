#ifndef INCLUDED_AG_LDDDRAWPROPS
#define INCLUDED_AG_LDDDRAWPROPS



#include "pcrtypes.h"
#include <QtOpenGL>

#ifndef INCLUDED_COM_CLASSCLASSIFIER
#include "com_classclassifier.h"
#endif
#include "ag_ClassDrawProps.h"



namespace ag {
  class LddDrawPropsPrivate;
}



namespace ag {



//! The LddDrawProps class holds info for drawing ldd data.
/*!
*/
class LddDrawProps: public ClassDrawProps
{

private:

  LddDrawPropsPrivate* _data;

  //! Assignment operator. NOT IMPLEMENTED.
  LddDrawProps&    operator=           (const LddDrawProps&);

protected:

  void             reMapColours        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LddDrawProps        (const std::string& title,
                                        const com::RawPalette* p,
                                    const com_ClassClassifier<UINT1>* c);

  //! Copy constructor.
                   LddDrawProps        (const LddDrawProps& properties);

  /* virtual */    ~LddDrawProps       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  static const std::string& label      ();

  const com_ClassClassifier<UINT1>& classifier() const;

  GLuint           texture             (unsigned char gdd,
                                        UINT1 ldd) const;

  std::string      label               (UINT1 const& value) const;

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
