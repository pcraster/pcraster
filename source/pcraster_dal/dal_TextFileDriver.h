#ifndef INCLUDED_DAL_TEXTFILEDRIVER
#define INCLUDED_DAL_TEXTFILEDRIVER

#include "dal_Types.h"

#include <filesystem>
#include <fstream>


namespace dal {
  // TextFileDriver declarations.
}



namespace dal {



//! Base class for text formatted file drivers.
/*!
  In this class common stuff used by text file drivers is collected. No
  assumption is made about the type of data that is read or the layout of the
  files.

  \todo Rename to FileDriver? This class is also used by driver that read and
        write in binary mode.
*/
class TextFileDriver
{

  friend class TextFileDriverTest;

private:

  //! Types supported by this driver.
  Types            d_types;

  //! Assignment operator. NOT IMPLEMENTED.
  TextFileDriver&  operator=           (TextFileDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   TextFileDriver      (TextFileDriver const& rhs);

protected:

                   TextFileDriver      ();

  bool             open                (std::ifstream& stream,
                                        std::filesystem::path const& path,
                                        std::ios::openmode flags=std::ios::in) const;

  bool             open                (std::ofstream& stream,
                                        std::filesystem::path const& path,
                                        std::ios::openmode flags=std::ios::out) const;

  Types const&     types               () const;

  void             determineTypeId     (
                        std::vector<std::vector<std::string> > const& rows,
                        TypeId& typeId) const;

  void             determineTypeId     (std::vector<std::string> const& row,
                                        TypeId& typeId) const;

  void             determineTypeIds    (
                        std::vector<std::vector<std::string> > const& records,
                        std::vector<TypeId>& typeIds) const;

  void             determineTypeIds    (std::vector<std::string> const& record,
                                        std::vector<TypeId>& typeIds) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~TextFileDriver     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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



} // namespace dal

#endif
