#ifndef INCLUDED_DAL_GEOEASTABLEDRIVER
#define INCLUDED_DAL_GEOEASTABLEDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_TEXTTABLEDRIVER
#include "dal_TextTableDriver.h"
#define INCLUDED_DAL_TEXTTABLEDRIVER
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



namespace dal {
  // GeoEASTableDriver declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  - First line is a title.
  - Second line contains the number of variables (columns).
  - Next n lines (one per variable) each contain the variable name and (optionally) unit and format information.
  - The data table must contain one row per observation.
  - The data table must contain only numbers, in integer, decimal, or exponential (e.g. 5.6E6) format, separated by blanks or commas. Variable columns need not be aligned.
  - Missing values must be indicated by the code "1E31".
*/
class GeoEASTableDriver: public TextTableDriver
{

  friend class GeoEASTableDriverTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  GeoEASTableDriver& operator=         (GeoEASTableDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   GeoEASTableDriver   (GeoEASTableDriver const& rhs);

  bool             readHeader          (Table& table,
                                        std::istream& stream) const;

  /*
  template<typename T>
  bool             extremes            (T& min,
                                        T& max,
                                        size_t col,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace space) const;
                                        */

  template<typename T>
  void             writeValue          (Array<T> const& col,
                                        size_t rec,
                                        std::ofstream& stream) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GeoEASTableDriver   ();

  /* virtual */    ~GeoEASTableDriver  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  Table*           open                (boost::filesystem::path const& path) const;

  /*
  Table*           open                (std::string const& name) const;

  Table*           open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  DataSpace        dataSpace           (std::string const& name) const;
                                        */

  // DataSpace        dataSpace           (std::string const& name,
  //                                       DataSpace const& space,
  //                                       DataSpaceAddress const& address) const;

  /*
  bool             extremes            (boost::any& min,
                                        boost::any& max,
                                        size_t col,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space) const;

  void             read                (std::string const& name,
                                        Table& table) const;
                                        */

  void             read                (dal::Table& table,
                                        boost::filesystem::path const& path) const;

  /*
  void             read                (dal::Table& table,
                                        std::string const& name) const;
                                        */

  /*
  void             read                (dal::Table& table,
                                        std::string const& name,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;
                                        */

  Table*           read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             write               (Table const& table,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

/*
template<typename T>
inline bool GeoEASTableDriver::extremes(
         T& min,
         T& max,
         size_t col,
         TypeId typeId,
         std::string const& name,
         DataSpace space) const
{
  bool initialised = false;
  boost::filesystem::path path;
  Table* table = 0;

  // Prevent iterating over time steps by removing the time dimension.
  if(space.hasTime()) {
    space.eraseDimension(space.indexOf(Time));
  }

  // Prevent iterating over individual cells by removing the space dimensions.
  while(space.hasSpace()) {
    space.eraseDimension(space.indexOf(Space));
  }

  if(space.isEmpty()) {
    path = pathFor(name);
    table = open(path);

    if(table) {
      table->setTypeIds(TI_NR_TYPES);
      table->setTypeId(col, typeId);
      TextTableDriver::read(*table, path);
      Array<T> const& array(table->template col<T>(col));

      if(array.extremes(min, max)) {
        initialised = true;
      }
    }
  }
  else {
    T colMin, colMax;

    for(DataSpaceIterator it = space.begin(); it != space.end(); ++it) {
      path = pathForDataSpaceAddress(name, space, *it);
      table = open(path);

      if(table) {
        table->setTypeIds(TI_NR_TYPES);
        table->setTypeId(col, typeId);
        TextTableDriver::read(*table, path);
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
      }
    }
  }

  return initialised;
}
*/



template<typename T>
void GeoEASTableDriver::writeValue(
         Array<T> const& col,
         size_t rec,
         std::ofstream& stream) const
{
  if(pcr::isMV(col[rec])) {
    stream << "1E31";
  }
  else {
    stream << col[rec];
  }
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
