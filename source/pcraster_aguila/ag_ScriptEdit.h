#ifndef INCLUDED_AG_SCRIPTEDIT
#define INCLUDED_AG_SCRIPTEDIT



// Library headers.
#include <QTextEdit>

// PCRaster library headers.

// Module headers.



namespace ag {
  // ScriptEdit declarations.
  class DataGuide;
  class DataObject;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class ScriptEdit: public QTextEdit
{

private:

  DataObject*      d_dataObject;

  //! Assignment operator. NOT IMPLEMENTED.
  ScriptEdit&      operator=           (const ScriptEdit& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ScriptEdit          (const ScriptEdit& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ScriptEdit          (DataObject* object,
                                        QWidget* parent = 0);

  /* virtual */    ~ScriptEdit         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setModelScript      (const DataGuide& guide);

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
