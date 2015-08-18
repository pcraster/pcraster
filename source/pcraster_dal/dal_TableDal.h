#ifndef INCLUDED_DAL_TABLEDAL
#define INCLUDED_DAL_TABLEDAL



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DAL
#include "dal_Dal.h"
#define INCLUDED_DAL_DAL
#endif



namespace dal {
  // TableDal declarations.
  class Table;
  class TableDriver;
}



namespace dal {



//! This class represents the Data Abstraction Layer for Table datasets.
/*!
  Use an object of this class to read and write Table datasets.
*/
class PCR_DAL_DECL TableDal: public Dal
{

  friend class TableDalTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  TableDal&        operator=           (TableDal const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   TableDal            (TableDal const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TableDal            (bool addAllDrivers = true);

  /* virtual */    ~TableDal           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  boost::tuple<boost::shared_ptr<Table>, TableDriver*> open(
                                        std::string const& name,
                                        bool raiseException=false);

  Table*           read                (std::string const& name,
                                        TypeId typeId);

  void             read                (std::string const& name,
                                        Table& table);

  TableDriver*     driverByDataset     (std::string const& name);

  TableDriver*     driverByName        (std::string const& name);

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



} // namespace dal

#endif
