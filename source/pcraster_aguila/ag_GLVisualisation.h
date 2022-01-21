#ifndef INCLUDED_AG_GLVISUALISATION
#define INCLUDED_AG_GLVISUALISATION



// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_IVisualisation.h"
#include "ag_SceneView.h"



namespace ag {
  // GLVisualisation declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class GLVisualisation: public SceneView,
                       public ag::IVisualisation
{

private:

  Q_OBJECT

  //! Assignment operator. NOT IMPLEMENTED.
  GLVisualisation& operator=           (const GLVisualisation& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   GLVisualisation     (const GLVisualisation& rhs);

protected:

                   GLVisualisation     (DataObject* object,
                                        const std::string& visualisationName,
                                        QWidget* parent = nullptr);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~GLVisualisation              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  bool             close               () override;

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
