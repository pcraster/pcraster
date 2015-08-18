#ifndef INCLUDED_DAL_TABLEDRIVER
#define INCLUDED_DAL_TABLEDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DRIVER
#include "dal_Driver.h"
#define INCLUDED_DAL_DRIVER
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif



namespace dal {
  // TableDriver declarations.
}



namespace dal {



//! This class is a base class for i/o drivers for Table datasets.
/*!

  \todo      Refactor the TableDriver tree.
  \todo      GeoEASTableDriver::dataSpace contains code which is relevant for
             all table drivers.
*/
class PCR_DAL_DECL TableDriver: public Driver
{

  friend class TableDriverTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  TableDriver&     operator=           (TableDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   TableDriver         (TableDriver const& rhs);

  template<typename T>
  bool             extremes            (T& min,
                                        T& max,
                                        size_t col,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace space) const;

protected:

                   TableDriver         (Format const& format);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~TableDriver        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  virtual Table*   open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  virtual DataSpace dataSpace          (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Table*           read                (std::string const& name) const;

  void             read                (Table& table,
                                        std::string const& name) const;

  Table*           read                (std::string const& name,
                                        TypeId typeId) const;

  virtual Table*   read                (std::string const& name,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  virtual void     read                (dal::Table& table,
                                        std::string const& name,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             write               (Table const& table,
                                        std::string const& name) const;

  virtual void     write               (Table const& table,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const;

  void             append              (std::string const& name,
                                        Table const& table) const;

  virtual void     append              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        Table const& table) const;

  virtual bool     extremes            (boost::any& min,
                                        boost::any& max,
                                        size_t col,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
inline bool TableDriver::extremes(
         T& min,
         T& max,
         size_t col,
         TypeId typeId,
         std::string const& name,
         DataSpace space) const
{
  bool initialised = false;
  // boost::filesystem::path path;
  Table* table = 0;

  // Prevent iterating over time steps by removing the time dimension.
  if(space.hasTime()) {
    space.eraseDimension(space.indexOf(Time));
  }

  // Prevent iterating over individual cells by removing the space dimensions.
  space.eraseDimension(Space);

  if(space.isEmpty()) {
    // path = pathFor(boost::get<0>(splitNameAndSelection(name)));
    table = dynamic_cast<Table*>(Driver::open(name));

    if(table) {
      table->setTypeIds(TI_NR_TYPES);
      table->setTypeId(col, typeId);
      table->createCols();
      read(*table, name);
      Array<T> const& array(table->template col<T>(col));

      if(array.extremes(min, max)) {
        initialised = true;
      }

      delete table;
    }
  }
  else {
    T colMin, colMax;

    for(DataSpaceIterator it = space.begin(); it != space.end(); ++it) {
      // path = pathForDataSpaceAddress(boost::get<0>(splitNameAndSelection(name)),
      //    space, *it);
      table = open(name, space, *it);

      if(table) {
        table->setTypeIds(TI_NR_TYPES);
        table->setTypeId(col, typeId);
        table->createCols();
        read(*table, name, space, *it);
        Array<T> const& array(table->template col<T>(col));

        if(array.extremes(colMin, colMax)) {
          if(!initialised) {
            min = colMin;
            max = colMax;
            initialised = true;
          }
          else {
            min = std::min<T>(min, colMin);
            max = std::max<T>(max, colMax);
          }
        }

        delete table;
        table = 0;
      }
    }
  }

  return initialised;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
