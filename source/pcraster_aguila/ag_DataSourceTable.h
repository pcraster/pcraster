#ifndef INCLUDED_AG_DATASOURCETABLE
#define INCLUDED_AG_DATASOURCETABLE



// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_TableVisualisation.h"



namespace ag {
  // DataSourceTable declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class DataSourceTable: public TableVisualisation
{

  friend class DataSourceTableTest;

private:

  Q_OBJECT

  //! Assignment operator. NOT IMPLEMENTED.
  DataSourceTable& operator=           (DataSourceTable const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DataSourceTable     (DataSourceTable const& rhs);

  void             createInterface     ();

  void             updateValues        ();

  // void             contentsMousePressEvent(
  //                                       QMouseEvent *event);

  void             rescan              ();

  void             process             ();

  void             visualise           ();

  void             fillTable           ();

private Q_SLOTS:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataSourceTable     (DataObject *object,
                                        QWidget* parent=0);

  /* virtual */    ~DataSourceTable    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& guide);

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
