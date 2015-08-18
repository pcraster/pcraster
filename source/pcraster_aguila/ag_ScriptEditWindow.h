#ifndef INCLUDED_AG_SCRIPTEDITWINDOW
#define INCLUDED_AG_SCRIPTEDITWINDOW



// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_VisualisationWindow.h"



namespace ag {
  // ScriptEditWindow declarations.
  class DataGuide;
  class ScriptEdit;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class ScriptEditWindow: public VisualisationWindow
{

private:

  ScriptEdit*      d_edit;

  //! Assignment operator. NOT IMPLEMENTED.
  ScriptEditWindow& operator=          (const ScriptEditWindow& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ScriptEditWindow    (const ScriptEditWindow& rhs);

  void             createInterface     ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ScriptEditWindow    (const qt::AppWindowProperties& props,
                                        DataObject* object);

  /* virtual */    ~ScriptEditWindow   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (const DataGuide& dataGuide);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
