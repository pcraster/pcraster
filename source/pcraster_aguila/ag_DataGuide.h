#ifndef INCLUDED_AG_DATAGUIDE
#define INCLUDED_AG_DATAGUIDE



// Library headers.

// PCRaster library headers.
#include "dev_Compiler.h"
#include "geo_dataguide.h"

// Module headers.
#include "ag_Configure.h"



namespace ag {
  // DataGuide declarations.
}



namespace ag {



//! The DataGuide class is for guide objects which are used in Aguila.
/*!
  This class adds aguila specific properties (eg: selection/deselection of data)
  to the standard geo::DataGuide class. The application is responsible to read
  the properties and take appropriate actions (eg: hide a data visualisation
  for data which is not visible).
*/
class PCR_AG_DECL DataGuide: public geo::DataGuide
{

private:

/*
  //! Data is selected or not.
  bool             d_selected;

  //! Data is visible or not.
  bool             d_visible;
*/

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataGuide           ();

                   DataGuide           (size_t index,
                                        Address address,
                                        geo::DataType type,
                                        CSF_VS valueScale);

  /* virtual */    ~DataGuide          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

/*
  void             select              ();

  void             unSelect            ();

  void             setSelected         (bool select);

  void             setVisible          (bool visible);
*/

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             equals              (const DataGuide& aDataGuide) const;

/*
  bool             isSelected          () const;

  bool             isVisible           () const;
*/

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (const DataGuide& lhs,
                                        const DataGuide& rhs);

bool               operator!=          (const DataGuide& lhs,
                                        const DataGuide& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
