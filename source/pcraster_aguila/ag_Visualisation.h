#ifndef INCLUDED_AG_VISUALISATION
#define INCLUDED_AG_VISUALISATION



// Library headers.
#include <QWidget>

// PCRaster library headers.
#include "ag_IVisualisation.h"

// Module headers.



namespace ag {
  // Visualisation declarations.
  class DataObject;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
template<class Widget=QWidget>
class Visualisation: public Widget,
                     public IVisualisation
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Visualisation&   operator=           (const Visualisation& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Visualisation       (const Visualisation& rhs);

protected:

                   Visualisation       (DataObject* object,
                                        const std::string& visualisationName,
                                        QWidget* parent = 0,
                                        Qt::WindowFlags flags = Qt::Widget);

  // virtual void     process             ();

  // virtual void     visualise           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~Visualisation      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  bool             close               ();

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
