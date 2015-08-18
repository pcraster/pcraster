#ifndef INCLUDED_DAL_CONNECTIONINFO
#define INCLUDED_DAL_CONNECTIONINFO



// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.



namespace dal {
  // ConnectionInfo declarations.
}



namespace dal {



//! This utility class encapsulates database connection information.
/*!
  A ConnectionInfo object is created with a string which is parsed. When the
  parse succeeds member functions can be used to query for the connection
  information.

  The connection string has a specific layout: see the constructor
  documentation for the details.
*/
class ConnectionInfo
{

  friend class ConnectionInfoTest;

private:

  //! Username.
  std::string      _user;

  //! Password.
  std::string      _password;

  //! Hostname.
  std::string      _host;

  //! Databasename.
  std::string      _database;

  //! Tablename.
  std::string      _table;

  // //! Fields.
  std::vector<std::string> _fields;

  void             init                ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ConnectionInfo      ();

                   ConnectionInfo      (std::string const& name);

                   ConnectionInfo      (ConnectionInfo const& rhs);

  /* virtual */    ~ConnectionInfo     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  ConnectionInfo&  operator=           (ConnectionInfo const& rhs);

  void             setDatabase         (std::string const& database);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isValid             () const;

  std::string const& user              () const;

  std::string const& password          () const;

  std::string const& host              () const;

  std::string const& database          () const;

  std::string const& table             () const;

  std::vector<std::string> const& fields() const;

  std::string      dataSource          () const;

  std::string      name                () const;

  std::string      connection          (std::string const& driver) const;

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
