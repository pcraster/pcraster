#ifndef INCLUDED_AG_BUFFEREDVISUALISATION
#define INCLUDED_AG_BUFFEREDVISUALISATION



// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_BufferedWidget.h"
#include "ag_IVisualisation.h"



namespace ag {
  // BufferedVisualisation declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class BufferedVisualisation: public BufferedWidget,
                             public IVisualisation
{

private:

  Q_OBJECT

  //! Assignment operator. NOT IMPLEMENTED.
  BufferedVisualisation& operator=     (const BufferedVisualisation& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   BufferedVisualisation(const BufferedVisualisation& rhs);

protected:

                   BufferedVisualisation(
                                        DataObject* object,
                                        std::string const& visualisationName,
                                        Alignment alignment,
                                        QWidget* parent=0,
                                        Qt::WindowFlags flags=Qt::Widget);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~BufferedVisualisation();

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
