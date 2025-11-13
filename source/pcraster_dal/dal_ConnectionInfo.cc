#include "dal_ConnectionInfo.h"

#include <boost/spirit/include/classic_core.hpp>
#include <boost/spirit/include/classic_utility.hpp>

#include <cassert>
#include <format>


/*!
  \file
  This file contains the implementation of the ConnectionInfo class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CONNECTIONINFO MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CONNECTIONINFO MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  Creates an invalid object.
*/
ConnectionInfo::ConnectionInfo()
{
  init();
}



//! Constructor.
/*!
  \param     name Name with the database connection information.

  Database connection information consists of the folowing items:
  \li User name (optional).
  \li Password (optional).
  \li Hostname (optional).
  \li Databasename (required).
  \li Tablename (optional).

  This information should be present in \a name and layout in the folowing
  manner: user(password)\@hostname:databasename/tablename.

  Valid examples are:
  \li kor(348uk)\@colossal:randstad/households
  \li kor()\@colossal:randstad/households
  \li kor\@colossal:randstad/households
  \li \@colossal:randstad/households
  \li randstad/households
  \li kor(348uk):randstad/households
  \li kor:randstad
  \li kor\@randstad (database named kor\@randstad)
  \li kor\@ (database named kor\@)
  \li kor: (database named kor:)

  Use isValid() to test whether parsing \a name succeeded.
*/
ConnectionInfo::ConnectionInfo(
         std::string const& name)
{
  using namespace boost::spirit::classic;

  rule<> const userRule = (+(alnum_p|space_p))[assign_a(_user)];
  rule<> const passwordRule = (+alnum_p)[assign_a(_password)];
  rule<> const hostRule = (+alnum_p)[assign_a(_host)];
  // rule<> databaseRule = (+alnum_p)[assign_a(_database)];
  rule<> const databaseRule = (+~ch_p('/'))[assign_a(_database)];
  rule<> const tableRule = (+~ch_p('/'))[assign_a(_table)];
  rule<> const fieldsRule = ("{" >> list_p((+alnum_p)[push_back_a(_fields)], ",")
         >> "}") | (+alnum_p)[push_back_a(_fields)];

  rule<> const accountRule = userRule >> !("(" >> !passwordRule >> ")");
  rule<> const connectionRule = !accountRule >> !("@" >> hostRule)
         >> ":" >> databaseRule >> !("/" >> tableRule >> !("/" >> fieldsRule));

  if(!parse(name.c_str(), connectionRule).full) {
    init();

    if(!parse(name.c_str(), databaseRule
         >> !("/" >> tableRule >> !("/" >> fieldsRule))).full) {
      init();
    }
  }
}



//! Copy constructor.
/*!
*/
ConnectionInfo::ConnectionInfo(
         ConnectionInfo const& rhs)

  : _user(rhs._user),
    _password(rhs._password),
    _host(rhs._host),
    _database(rhs._database),
    _table(rhs._table),
    _fields(rhs._fields)

{
}



//! Destructor.
/*!
*/
ConnectionInfo::~ConnectionInfo()
{
}



//! Assignment operator.
/*!
*/
ConnectionInfo& ConnectionInfo::operator=(
         ConnectionInfo const& rhs)
{
  if (this != &rhs) {
    _user = rhs._user;
    _password = rhs._password;
    _host = rhs._host;
    _database = rhs._database;
    _table = rhs._table;
    _fields = rhs._fields;
  }

  return *this;
}



//! Initialises the object.
/*!
  isValid() will return false after calling this function.
*/
void ConnectionInfo::init()
{
  _user.clear();
  _password.clear();
  _host.clear();
  _database.clear();
  _table.clear();

  assert(!isValid());
}



//! Returns whether the object is in a valid state or not.
/*!
  \return    true or false
*/
bool ConnectionInfo::isValid() const
{
  return !_database.empty();
}



//! Sets the database name to \a database.
/*!
  \param     database New database name.
  \warning   \a database must be a non-empty string. The object must be in
             a valid state.
*/
void ConnectionInfo::setDatabase(
         std::string const& database)
{
  assert(isValid());
  assert(!database.empty());

  _database = database;

  assert(isValid());
}



//! Return the user name.
/*!
  \return    User name.
*/
std::string const& ConnectionInfo::user() const
{
  return _user;
}



//! Return the password.
/*!
  \return    Password.
*/
std::string const& ConnectionInfo::password() const
{
  return _password;
}



//! Return the host name.
/*!
  \return    Host name.
*/
std::string const& ConnectionInfo::host() const
{
  return _host;
}



//! Return the database name.
/*!
  \return    Database name.
*/
std::string const& ConnectionInfo::database() const
{
  return _database;
}



//! Return the table name.
/*!
  \return    Table name.
*/
std::string const& ConnectionInfo::table() const
{
  return _table;
}



std::vector<std::string> const& ConnectionInfo::fields() const
{
  return _fields;
}



//! Returns the data source string.
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  The string is formatted as follows:
  \code
  std::format("UID={0};PWD={1};DSN={2}", user(), password(), database()
  \endcode

  Useful for ODBC connections.
*/
std::string ConnectionInfo::dataSource() const
{
  return std::format("UID={0};PWD={1};DSN={2}",
         user(), password(), database());

}



//! Returns the name of the connection.
/*!
  \return    Name.

  The name of the connection is a string formatted according the the layout
  rules described in the documention of the constructor. There is no garantee
  that the string returned here equals the constructor argument given when
  the object was created. However, a ConnectionInfo object created with the
  name returned here will equal this object, except for the column selection.
*/
std::string ConnectionInfo::name() const
{
  std::string name;

  if(!user().empty()) {
    name += user();
  }

  if(!password().empty()) {
    name += "(" + password() + ")";
  }

  if(!host().empty()) {
    name += "@" + host();
  }

  assert(!database().empty());
  if(!name.empty()) {
    name += ":";
  }
  name += database();

  if(!table().empty()) {
    name += "/" + table();
  }

  return name;
}



std::string ConnectionInfo::connection(
         std::string const& driver) const
{
  return driver == "QODBC3" ? dataSource() : database();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


} // namespace dal
