#ifndef INCLUDED_AG_MAP
#define INCLUDED_AG_MAP

#include "ag_Visualisation.h"



namespace ag {
  // Map declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class Map: public Visualisation<>
{

  friend class MapTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Map&             operator=           (Map const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Map                 (Map const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Map                 (DataObject* object,
                                        std::string const& visualisationName,
                                        QWidget* parent);

  /* virtual */    ~Map                () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  QSize            sizeHint            () const override;

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
