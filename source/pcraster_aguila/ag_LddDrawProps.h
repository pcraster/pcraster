#ifndef INCLUDED_AG_LDDDRAWPROPS
#define INCLUDED_AG_LDDDRAWPROPS

#include "pcrtypes.h"
#include "com_classclassifier.h"
#include "ag_ClassDrawProps.h"

#ifdef AGUILA_WITH_OPENGL
  #include <QtOpenGL>
#endif

#ifndef AGUILA_WITH_OPENGL
  typedef unsigned int  GLuint;
#endif


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

  void             reMapColours        () override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LddDrawProps        (const std::string& title,
                                        const com::RawPalette* p,
                                    const com_ClassClassifier<UINT1>* c);

  //! Copy constructor.
                   LddDrawProps        (const LddDrawProps& properties);

  /* virtual */    ~LddDrawProps       () override;

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
