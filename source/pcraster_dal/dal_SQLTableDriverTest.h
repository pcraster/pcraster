#ifndef INCLUDED_DAL_SQLTABLEDRIVERTEST
#define INCLUDED_DAL_SQLTABLEDRIVERTEST



// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



#ifndef QT_NO_SQL

namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // SQLTableDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the SQLTableDriver class.
class SQLTableDriverTest
{

private:

  std::string      d_user;

  void             createDatabase      (std::string const& driverName,
                                        std::string const& databaseName);

  void             removeDatabase      (std::string const& driverName,
                                        std::string const& databaseName);

  void             testODBC            ();

  void             testSQLite          ();

  void             testPostgreSQL      ();

  void             test                (std::string const& driverName);

public:

                   SQLTableDriverTest  ();

  void             setUp               ();

  void             tearDown            ();

  void             testUserVar         ();

  void             testDriverIsAvailable();

  void             testConstructor     ();

  void             testUnexisting      (std::string const& driverName);

  void             testWrite           (std::string const& driverName);

  void             testReadTemporal    (std::string const& driverName);

  void             testReadScenario    (std::string const& driverName);

  void             testReadScenarioTemporal(std::string const& driverName);

  void             testReadQuantile    (std::string const& driverName);

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif // QT_NO_SQL
#endif
