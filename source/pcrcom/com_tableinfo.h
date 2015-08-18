#ifndef INCLUDED_COM_TABLEINFO
#define INCLUDED_COM_TABLEINFO



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

// Module headers.






namespace com {



//! information about table contents such as format and column names
/*!
 * Stores information that is typically found in ascii formatted headers,
 * as in the Geo-EAS case, but could also come from other sources,
 * (e.g. database metatables). It does NOT hold info about the table
 * contents, such as the number of records (=rows) in the table.
 *
 *
 */
class TableInfo
{

public:
  typedef enum { PLAIN_TXT,
                 GEO_EAS,
                 EMPTY,
                 UNKNOWN
               } Layout;
private:
  com::PathName    d_fileName;



  Layout                   d_layout;

  bool                     d_columnNamesGenerated;

  //! Description of the table
  std::string              d_description;

  //! Names of the columns.
  std::vector<std::string> d_columnNames;


  //  Assignment operator. DEFAULT.
  // TableInfo&           operator=           (const TableInfo& rhs);

  //  Copy constructor. DEFAULT
  //                 TableInfo               (const TableInfo& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TableInfo               (const com::PathName& fileName);

  /* virtual */    ~TableInfo              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setFileName         (const com::PathName& fileName);
  void             setLayout      (Layout layout);
  void             generateColumnNames();
  void             setDescription      (const std::string& description);
  void             setColumnNames      (const std::vector<std::string>& columnNames);
  void             setNrColumns        (size_t nrColumns);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const com::PathName& fileName                 () const;
  Layout           layout                       () const;
  const bool&      columnNamesGenerated         () const;
  const std::string& description                () const;
  const std::vector<std::string>& columnNames   () const;
  size_t           nrColumns                    () const;

  size_t           nrHeaderLines                () const;


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



} // namespace com

#endif
