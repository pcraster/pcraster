    autoAddDriver(new TextTableDriver());
    autoAddDriver(new GeoEASTableDriver());

    // Add all drivers supported by the local qt installation.
    {
      QStringList list = QSqlDatabase::drivers();

      // First add dbms specific drivers.
      for(QStringList::Iterator it = list.begin(); it != list.end(); ++it) {
        if(*it != "QODBC3") {
          autoAddDriver(new SQLTableDriver((*it).toUtf8().constData()));
        }
      }

      // Than the odbc driver, if available.
      for(QStringList::Iterator it = list.begin(); it != list.end(); ++it) {
        if(*it == "QODBC3") {
          autoAddDriver(new SQLTableDriver((*it).toUtf8().constData()));
        }
      }
    }

// #ifndef QT_NO_SQL
//     // First add dbms specific drivers.
//     autoAddDriver(new SQLTableDriver("QPSQL7"));
//     // Than the odbc driver.
//     autoAddDriver(new SQLTableDriver("QODBC3"));
// #endif
