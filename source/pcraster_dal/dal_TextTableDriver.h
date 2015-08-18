#ifndef INCLUDED_DAL_TEXTTABLEDRIVER
#define INCLUDED_DAL_TEXTTABLEDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_TABLEDRIVER
#include "dal_TableDriver.h"
#define INCLUDED_DAL_TABLEDRIVER
#endif

#ifndef INCLUDED_DAL_TEXTFILEDRIVER
#include "dal_TextFileDriver.h"
#define INCLUDED_DAL_TEXTFILEDRIVER
#endif



namespace dal {
  // TextTableDriver declarations.
}



namespace dal {



//! This class implements an i/o driver for text formatted tables datasets.
/*!
  This driver assumes the table is stored as text formatted in columns and
  separated by white space. Per column the type of the values must be constant.
  Between the columns the types can differ.

  \todo Support empty values -> missing values. See testColumnWithEmptyValues.
*/
class TextTableDriver: public TableDriver,
                       public TextFileDriver
{

  friend class TextTableDriverTest;

private:

  HeaderType       d_headerType;

  //! Assignment operator. NOT IMPLEMENTED.
  TextTableDriver& operator=           (TextTableDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   TextTableDriver     (TextTableDriver const& rhs);

  static Table*    markSelectedCols    (Table* table,
                                  std::vector<std::string> const& selectedCols);

  Table*           open                (
         boost::tuple<std::string, std::vector<std::string> > const& tuple) const;

  Table*           open                (
                   boost::tuple<std::string, std::vector<std::string> > const& tuple,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  // template<typename T>
  // bool             extremes            (T& min,
  //                                       T& max,
  //                                       size_t col,
  //                                       TypeId typeId,
  //                                       std::string const& name,
  //                                       DataSpace space) const;

protected:

                   TextTableDriver     (Format const& format,
                                        HeaderType headerType = AUTO_HEADER);

  bool             open                (Table& table,
                                        std::istream& stream) const;

  void             readValues          (Table& table,
                                        std::ifstream& stream,
                                        std::string const& name) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TextTableDriver     (HeaderType headerType=AUTO_HEADER);

  /* virtual */    ~TextTableDriver    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  virtual Table*   open                (boost::filesystem::path const& path) const;

  // Table*           open                (std::string const& name) const;

  Table*           open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  // DataSpace        dataSpace           (std::string const& name) const;

  // virtual DataSpace dataSpace          (std::string const& name,
  //                                       DataSpace const& space,
  //                                       DataSpaceAddress const& address) const;

  virtual void     read                (Table& table,
                                        boost::filesystem::path const& path) const;

  // void             read                (Table& table,
  //                                       std::string const& name) const;

  void             read                (Table& table,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  virtual Table*   read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  // bool             extremes            (boost::any& min,
  //                                       boost::any& max,
  //                                       size_t col,
  //                                       TypeId typeId,
  //                                       std::string const& name,
  //                                       DataSpace const& space) const;

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
