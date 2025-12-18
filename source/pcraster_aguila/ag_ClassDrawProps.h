#ifndef INCLUDED_AG_CLASSDRAWPROPS
#define INCLUDED_AG_CLASSDRAWPROPS

#include "ag_DrawProps.h"

#include <string>



namespace com {
  class RawPalette;
}



namespace ag {



//! The ClassDrawProps class contains properties for drawing classified data.
/*!
*/
class ClassDrawProps: public DrawProps
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ClassDrawProps&  operator=           (const ClassDrawProps&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ClassDrawProps      (const std::string& title,
                                        const com::RawPalette* p);

                   ClassDrawProps      (const ClassDrawProps& properties);

           ~ClassDrawProps     () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
