#ifndef INCLUDED_DAL_SQLTABLEDRIVER
#define INCLUDED_DAL_SQLTABLEDRIVER


// Library headers.
#ifndef INCLUDED_QVARIANT
#include <QVariant>
#define INCLUDED_QVARIANT
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_TABLEDRIVER
#include "dal_TableDriver.h"
#define INCLUDED_DAL_TABLEDRIVER
#endif



#ifndef QT_NO_SQL

class QSqlDatabase;
namespace dal {
  // SQLTableDriver declarations.
  class ConnectionInfo;
}



namespace dal {



//! This class encapsulates the SQL table drivers from the Qt library.
/*!
  This class uses the Qt library to access tables in SQL databases. See the
  Qt SQL module documentation for the supported SQL database drivers.

  \todo Add support for column selections in the name of the table
        (splitNameAndSelection).
  \todo Implement empty functions.
*/
class SQLTableDriver: public TableDriver
{

  friend class SQLTableDriverTest;

private:

  ConnectionInfo   connectionInfoFor   (std::string const& name,
                                        DataSpace const& space) const;

  bool             databaseExists      (std::string const& name) const;

  QSqlDatabase     connectToDatabase   (ConnectionInfo const& info) const;

  static void      execQuery           (QSqlDatabase const& database,
                                        std::string const& query);

  static void      openDatabase        (QSqlDatabase& database);

  static TypeId    qtTypeId2DalTypeId  (QVariant::Type qtTypeId);

  static std::string typeId2SQLTypeName(TypeId typeId);

  static std::string fieldValues       (Table const& table,
                                        size_t rec);

public:

  static bool      driverIsAvailable   (std::string const& name);

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SQLTableDriver      (std::string const& driverName);

  /* virtual */    ~SQLTableDriver     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  DataSpace        dataSpace           (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Table*           open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Table*           read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             read                (dal::Table& table,
                                        std::string const& name,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  void             write               (Table const& table,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const;

  void             append              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        Table const& table) const;

  // bool             databaseExists      (std::string const& name) const;

  // void             removeDatabase      (std::string const& name) const;

  // void             addDatabase         (std::string const& name) const;

  void             grantReadAccess     (std::string const& name,
                                        std::string const& user="PUBLIC") const;

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

#endif // QT_NO_SQL
#endif
