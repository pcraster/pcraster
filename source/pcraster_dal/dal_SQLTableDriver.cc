#ifndef INCLUDED_DAL_SQLTABLEDRIVER
#include "dal_SQLTableDriver.h"
#define INCLUDED_DAL_SQLTABLEDRIVER
#endif

// Library headers.
#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM
#include <boost/filesystem.hpp>
#define INCLUDED_BOOST_FILESYSTEM
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

#ifndef INCLUDED_BOOST_FUNCTION
#include <boost/function.hpp>
#define INCLUDED_BOOST_FUNCTION
#endif

#ifndef INCLUDED_QTSQL
#include <QtSql>
#define INCLUDED_QTSQL
#endif

#ifndef INCLUDED_QSTRINGLIST
#include <QStringList>
#define INCLUDED_QSTRINGLIST
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONNECTIONINFO
#include "dal_ConnectionInfo.h"
#define INCLUDED_DAL_CONNECTIONINFO
#endif

#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



/*!
  \file
  This file contains the implementation of the SQLTableDriver class.
*/


#ifndef QT_NO_SQL

namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SQLTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

void SQLTableDriver::execQuery(
         QSqlDatabase const& database,
         std::string const& query)
{
  QSqlQuery q(query.c_str(), database);
  if(!q.isActive()) {
    throw Exception(q.lastError().text().toUtf8().constData());
  }
}



TypeId SQLTableDriver::qtTypeId2DalTypeId(
         QVariant::Type qtTypeId)
{
  TypeId typeId = TI_NR_TYPES;

  switch(qtTypeId) {
    case QVariant::Bool:     { typeId = TI_UINT1; break; }
    case QVariant::Double:   { typeId = TI_REAL8; break; }
    case QVariant::Int:      { typeId = TI_INT4; break; }
    // case QVariant::LongLong: { typeId = INT8; break; }
    // case QVariant::ULongLong: { typeId = UINT8; break; }
    case QVariant::String:   { typeId = TI_STRING; break; }
    // case QVariant::CString:  { typeId = TI_STRING; break; }
    case QVariant::UInt:     { typeId = TI_UINT4; break; }
    default:                 { assert(false); break; }
  }

  return typeId;
}



std::string SQLTableDriver::typeId2SQLTypeName(
         TypeId typeId)
{
  std::string name;

  switch(typeId) {
    case TI_INT1:     { name = "tinyint"; break; }
    case TI_INT2:     { name = "smallint"; break; }
    case TI_INT4:     { name = "int"; break; }
    case TI_UINT1:    { name = "tinyint"; break; }
    case TI_UINT2:    { name = "smallint"; break; }
    case TI_UINT4:    { name = "int"; break; }
    case TI_REAL4:    { name = "float"; break; } // decimal
    case TI_REAL8:    { name = "double"; break; } // decimal
    case TI_STRING:   { name = "varchar"; break; }
    default:       { assert(false); break; }
  }

  return name;
}



std::string SQLTableDriver::fieldValues(
         Table const& table,
         size_t rec)
{
  std::string fieldValues;

  for(size_t col = 0; col < table.nrCols(); ++col) {
    if(table.typeId(col) == TI_STRING) {
      fieldValues = (boost::format("'%1%'")
            % table.asString(rec, col)).str();
    }
    else {
      fieldValues = table.asString(rec, col);
    }
    for(++col; col < table.nrCols(); ++col) {
      if(table.typeId(col) == TI_STRING) {
        fieldValues += (boost::format(", '%1%'")
          % table.asString(rec, col)).str();
      }
      else {
        fieldValues += ", " + table.asString(rec, col);
      }
    }
  }

  return fieldValues;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Have a look at isOpenError() and create a more useful error
             message.
*/
void SQLTableDriver::openDatabase(
         QSqlDatabase& database)
{
  if(!database.open()) {
    // TODO use database.lastError().text!
    throw Exception((boost::format(
       "Database %1%: Can not be opened")
       % std::string(database.databaseName().toUtf8().constData())).str());
  }
}



bool SQLTableDriver::driverIsAvailable(
         std::string const& name)
{
  bool result = false;
  QStringList list = QSqlDatabase::drivers();

  for(QStringList::Iterator it = list.begin(); it != list.end(); ++it) {
    if(*it == QString(name.c_str())) {
      result = true;
      break;
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF SQLTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

SQLTableDriver::SQLTableDriver(
         std::string const& driverName)

  : TableDriver(Format(driverName, "SQL table driver for " + driverName,
         TABLE, Format::Database))

{
  if(!driverIsAvailable(name())) {
    throw Exception((boost::format(
       "SQL table driver for %1%: Not available")
       % name()).str());
  }

  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;
  properties |= Writer;

  std::vector<std::string> extensions;

  if(name() == "QSQLITE") {
    extensions.push_back(".sql3");
    extensions.push_back(".sqlite3");
  }

  format().setExtensions(extensions);
}



SQLTableDriver::~SQLTableDriver()
{
}



bool SQLTableDriver::databaseExists(
         std::string const& name) const
{
  bool result = false;

  ConnectionInfo info(name);
  QSqlDatabase database = connectToDatabase(info);

  if(database.isValid()) {
    // TODO See remarks below (search for QSQLITE).
    if(this->name() != "QSQLITE" || boost::filesystem::exists(
         info.database())) {
      result = database.open();
    }
  }

  return result;
}



ConnectionInfo dal::SQLTableDriver::connectionInfoFor(
         std::string const& name,
         DataSpace const& space) const
{
  Properties& properties(this->properties(name, space));

  ConnectionInfo result(name);

  if(result.isValid()) {
    bool found = false;
    FilenameConvention convention;
    std::string extension;

    if(!format().extensions().empty()) {
      // Driver works with file based databases.

      if(properties.hasValue(DAL_FILENAME_CONVENTION)) {
        // The database has already been opened once and properties have been
        // set.
        found = true;
      }
      else {
        // Determine and cache database file properties.
        typedef boost::function<bool (std::string const&)> CallBack;
        CallBack callBack(boost::bind(&SQLTableDriver::databaseExists, this,
              _1));

        boost::tie(found, convention, extension) =
              dal::determineFilenameCharacteristics<CallBack>(callBack,
                   result.database(), DataSpace(), DataSpaceAddress(),
                   format().extensions());

        if(found) {
          properties.setValue<FilenameConvention>(DAL_FILENAME_CONVENTION,
              convention);
          properties.setValue<std::string>(DAL_DEFAULT_EXTENSION, extension);
        }
      }

      if(!found) {
        result = ConnectionInfo();
      }
      else {
        assert(result.isValid());

        boost::filesystem::path path(pathForDataSpaceAddress(
              result.database() + defaultExtension(name, space),
              DataSpace(), DataSpaceAddress(),
              filenameConvention(name, space)));
        result.setDatabase(path.string());
        assert(result.isValid());
      }
    }
  }

  return result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  This function does not open the database connection.
*/
QSqlDatabase SQLTableDriver::connectToDatabase(
         ConnectionInfo const& info) const
{
  // First see whether connection already exists.
  QSqlDatabase database = QSqlDatabase::database(
         QString(info.connection(name()).c_str()), false);

  if(std::string(database.driverName().toUtf8().constData()) != name()) {
    // Connection already known but different driver. Remove it first.
    database = QSqlDatabase();
    QSqlDatabase::removeDatabase(QString(info.connection(name()).c_str()));
  }

  if(!database.isValid()) {
    // Create new database connection.
    database = QSqlDatabase::addDatabase(QString(name().c_str()),
         QString(info.connection(name()).c_str()));

    if(database.isValid()) {
      database.setUserName(QString(info.user().c_str()));
      database.setPassword(QString(info.password().c_str()));
      database.setHostName(QString(info.host().c_str()));
      database.setDatabaseName(QString(info.database().c_str()));
      assert(database.isValid());
    }
  }

  // assert(database.isValid());
  assert(std::string(database.driverName().toUtf8().constData()) == name());

  return database;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
bool SQLTableDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return bool(boost::shared_ptr<Table>(open(name, space, address)));
}



DataSpace SQLTableDriver::dataSpace(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  boost::shared_ptr<Table> table(open(name, space, address));

  if(!table) {
    throwCannotBeOpened(name, TABLE);
  }

  // Otherwise change code below which sets column stuff explicitly.
  assert(!table->colsAreCreated());

  // Turn off all fields not related to data space dimensions.
  std::set<std::string> dimensionFieldNames;
  dimensionFieldNames.insert("scenario");
  dimensionFieldNames.insert("date");
  dimensionFieldNames.insert("quantile");

  for(size_t i = 0; i < table->nrCols(); ++i) {
    if(std::find(dimensionFieldNames.begin(), dimensionFieldNames.end(),
         table->title(i)) == dimensionFieldNames.end()) {
      table->setTypeId(i, TI_NR_TYPES);
    }
  }

  // Now create and read the dimensional columns.
  table->createCols();
  read(*table, name, space, address);

  // Translate the values to data space dimensions.
  DataSpace result;

  size_t quantileIndex = table->indexOf("quantile");

  if(quantileIndex < table->nrCols()) {
    // TODO make more generic.
    assert(table->typeId(quantileIndex) == TI_REAL4);

    Array<REAL4>& quantiles = table->col<REAL4>(quantileIndex);

    std::sort(quantiles.begin(), quantiles.end());

    REAL4 first, last, interval;

    if(isIncreasingRange(first, last, interval, quantiles.begin(),
         quantiles.end()) && first > REAL4(0.0)) {

      std::vector<REAL4> quantiles;
      quantiles.push_back(first);
      quantiles.push_back(last);
      quantiles.push_back(interval);

      result.addDimension(Dimension(CumulativeProbabilities, quantiles));
    }
  }

  size_t dateIndex = table->indexOf("date");

  if(dateIndex < table->nrCols()) {
    // TODO make more generic.
    assert(table->typeId(dateIndex) == TI_INT4);

    Array<INT4>& timeSteps = table->col<INT4>(dateIndex);

    std::sort(timeSteps.begin(), timeSteps.end());

    INT4 first, last, interval;

    if(isIncreasingRange(first, last, interval, timeSteps.begin(),
         timeSteps.end()) && first >= 1) {

      std::vector<size_t> timeSteps;
      timeSteps.push_back(first);
      timeSteps.push_back(last);
      timeSteps.push_back(interval);

      result.addDimension(Dimension(Time, timeSteps));
    }
  }

  return result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
Table* SQLTableDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const&
#ifdef DEBUG_BUILD
           address
#endif
         ) const
{
  assert(address.nrInvalidCoordinates() <= 1);

  Table* table = 0;

  ConnectionInfo info(connectionInfoFor(name, space));

  if(!info.isValid() || info.fields().empty()) {
    return 0;
  }

  // Default, the QSQLITE driver creates a database file for non-existing
  // databases. Since we are only probing here, we do not want this
  // behaviour. If the database does not exist, we need to return 0.
  // We don't want files to automagically be created.
  // This may be a general thing. A database is created when it is opened. (?)
  // TODO Better to use dbms specific master table property.
  if(this->name() == "QSQLITE" && !boost::filesystem::exists(
       info.database())) {
    return 0;
  }

  QSqlDatabase database = connectToDatabase(info);

  if(database.isValid() && database.open()) {
    QSqlRecord record = database.record(QString::fromUtf8(
         info.table().c_str()));

    if(record.isEmpty()) {
      return 0;
    }

    QSqlIndex index(database.primaryIndex(QString::fromUtf8(
         info.table().c_str())));

    // All tables need to have an index.
    if(index.isEmpty()) {
      return 0;
    }

    // Primary index contains the dimensional information and, optional, a
    // fid field. If there are other fields present, then we must stop
    // processing.
    std::vector<std::string> resultFieldNames;

    for(int i = 0; i < index.count(); ++i) {
      resultFieldNames.push_back(index.fieldName(i).toUtf8().constData());
    }

    {
      std::vector<std::string> indexFields;
      indexFields.push_back("scenario");
      indexFields.push_back("fid");
      indexFields.push_back("quantile");
      indexFields.push_back("date");

      BOOST_FOREACH(std::string const name, resultFieldNames) {
        if(std::find(indexFields.begin(), indexFields.end(), name) ==
              indexFields.end()) {
          // Unsupported field is part of index.
          return 0;
        }
      }
    }

    resultFieldNames.insert(resultFieldNames.end(), info.fields().begin(),
         info.fields().end());

    std::vector<std::string> titles;
    std::vector<TypeId> typeIds;
    QSqlField field;

    BOOST_FOREACH(std::string const name, resultFieldNames) {
      assert(record.contains(QString::fromUtf8(name.c_str())));

      field = record.field(QString::fromUtf8(name.c_str()));
      titles.push_back(std::string(field.name().toUtf8().constData()));
      typeIds.push_back(qtTypeId2DalTypeId(field.type()));

      if(this->name() == "QSQLITE") {
        if(typeIds.back() == TI_STRING) {
          if(name == "quantile") {
            typeIds[typeIds.size() - 1] = TI_REAL4;
          }
          else if(std::find(info.fields().begin(), info.fields().end(),
              name) != info.fields().end()) {
            typeIds[typeIds.size() - 1] = TI_REAL4;
          }
        }
      }
    }

    table = new Table(titles, typeIds);
  }

  return table;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement for case space is not empty.
*/
void SQLTableDriver::read(
         Table& table,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
// TODO uncomment
//   assert(!space.hasTime());
//   assert(!space.hasSpace());
//   assert(!space.hasCumProbabilities());

// TODO remove
  // assert(!space.isEmpty());

  assert(address.nrInvalidCoordinates() <= 1);

  if(!table.colsAreCreated()) {
    table.createCols();
  }

  ConnectionInfo info(connectionInfoFor(name, space));

  if(!info.isValid() || info.fields().empty()) {
    throw Exception((boost::format(
         "Database connection information %1%: not valid")
         % name).str());
  }

  QSqlDatabase database = connectToDatabase(info);

  if(!database.isValid()) {
    throw Exception((boost::format(
         "Database connection %1%: Can not be created")
         % info.connection(this->name())).str());
  }

  openDatabase(database);

  QSqlQuery query(QString::fromUtf8(dataSpaceAddressToSqlQuery(space, address,
         info.table(), table.titles()).c_str()), database);

  std::map<size_t, int> colIdToQueryId;

  for(size_t i = 0; i < table.nrCols(); ++i) {
    colIdToQueryId[i] = query.record().indexOf(QString::fromUtf8(
         table.title(i).c_str()));

    // Table / query mismatch.
    assert(colIdToQueryId[i] != -1);
  }

  query.setForwardOnly(true);
  assert(query.isSelect());
  assert(query.isActive());

  bool ok;

  while(query.next()) {
    table.appendRec();

    for(size_t col = 0, c = 0; col < table.nrCols(); ++col) {

      switch(table.typeId(col)) {
        case TI_UINT1: {
          table.col<UINT1>(col)[query.at()] = static_cast<UINT1>(query.value(
              colIdToQueryId[col]).toBool());
          ++c;
          break;
        }
        case TI_UINT4: {
          table.col<UINT4>(col)[query.at()] = query.value(
              colIdToQueryId[col]).toUInt(&ok);
          assert(ok);
          ++c;
          break;
        }
        case TI_INT4: {
          table.col<INT4>(col)[query.at()] = query.value(
              colIdToQueryId[col]).toInt(&ok);
          assert(ok);
          ++c;
          break;
        }
        case TI_REAL4: {
          table.col<REAL4>(col)[query.at()] = query.value(
              colIdToQueryId[col]).toDouble(&ok);
          assert(ok);
          ++c;
          break;
        }
        case TI_REAL8: {
          table.col<REAL8>(col)[query.at()] = query.value(
              colIdToQueryId[col]).toDouble(&ok);
          assert(ok);
          ++c;
          break;
        }
        case TI_STRING: {
          table.col<std::string>(col)[query.at()] = std::string(query.value(
              colIdToQueryId[col]).toString().toUtf8().constData());
          ++c;
          break;
        }
        case TI_NR_TYPES: {
          // Skip.
          break;
        }
        default: {
          // Should have been detected sooner.
          assert(false);
          break;
        }
      }
    }
  }
}



Table* SQLTableDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  Table* table = open(name, space, address);

  if(!table) {
    throwCannotBeOpened(name, space, TABLE);
  }

  read(*table, name, space, address);

  return table;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement for case space is not empty.
*/
void SQLTableDriver::write(
         Table const& table,
         DataSpace const&
#ifdef DEBUG_DEVELOP
         space
#endif
         ,
         DataSpaceAddress const&
#ifdef DEBUG_DEVELOP
         address
#endif
         ,
         std::string const& name) const
{
  assert(space.isValid(address));
  assert(space.isEmpty());

  ConnectionInfo info(name); // connectionInfoFor(name, space, address));

  if(!info.isValid()) {
    throw Exception((boost::format(
         "Database connection information %1%: not valid")
         % name).str());
  }

  QSqlDatabase database = connectToDatabase(info);

  if(!database.isValid()) {
    throw Exception((boost::format(
         "Database connection %1%: Can not be created")
         % info.connection(this->name())).str());
  }

  openDatabase(database);

  try {
    {
      // Drop table if it already exists.
      QStringList tableNames = database.tables(QSql::Tables);

      if(tableNames.contains(QString(info.table().c_str()))) {
        QSqlQuery query(QString((boost::format("DROP TABLE \"%1%\"")
               % info.table()).str().c_str()), database);

        if(!query.isActive()) {
          throwCannotBeDeleted(info.table(), TABLE,
              std::string(query.lastError().text().toUtf8().constData()));
        }
      }
    }

    std::string fieldSpecs, fieldNames;

    for(size_t col = 0; col < table.nrCols(); ++col) {
      assert(table.typeId(col) != TI_NR_TYPES);
      assert(!table.title(col).empty());
      fieldSpecs = (boost::format("\"%1%\" %2%")
         % table.title(col) % typeId2SQLTypeName(table.typeId(col))).str();
      fieldNames = "\"" + table.title(col) + "\"";

      for(++col; col < table.nrCols(); ++col) {
        assert(table.typeId(col) != TI_NR_TYPES);
        assert(!table.title(col).empty());
        fieldSpecs += (boost::format(", \"%1%\" %2%")
         % table.title(col) % typeId2SQLTypeName(table.typeId(col))).str();
        fieldNames += ", \"" + table.title(col) + "\"";
      }
    }

    {
      // Create new table.
      QSqlQuery query(QString((boost::format("CREATE TABLE \"%1%\" (%2%)")
           % info.table() % fieldSpecs).str().c_str()), database);

      if(!query.isActive()) {
        throwCannotBeCreated(info.table(), TABLE, std::string(
              query.lastError().text().toUtf8().constData()));
      }
    }

    {
      // Fill the new table.
      QSqlQuery query(database);
      for(size_t rec = 0; rec < table.nrRecs(); ++rec) {

        query.exec(QString((boost::format("INSERT INTO \"%1%\" (%2%) VALUES (%3%)")
           % info.table() % fieldNames % fieldValues(table, rec)).str().c_str()));

        if(!query.isActive()) {
          throwCannotWriteRecord(info.table(), TABLE, rec + 1,
              std::string(query.lastError().text().toUtf8().constData()));
        }
      }
    }

    // database.close();
  }
  catch(...) {
    // database.close();
    throw;
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement for case space is not empty.
*/
void SQLTableDriver::append(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         Table const& table) const
{
  assert(space.isValid(address));
  assert(space.isEmpty());

  ConnectionInfo info(connectionInfoFor(name, space));

  if(!info.isValid()) {
    throw Exception((boost::format(
         "Database connection information %1%: not valid")
         % name).str());
  }

  QSqlDatabase database = connectToDatabase(info);

  if(!database.isValid()) {
    throw Exception((boost::format(
         "Database connection %1%: Can not be created")
         % info.connection(this->name())).str());
  }

  try {

    std::string fieldSpecs, fieldNames;
    for(size_t col = 0; col < table.nrCols(); ++col) {
      assert(table.typeId(col) != TI_NR_TYPES);
      assert(!table.title(col).empty());
      fieldSpecs = (boost::format("\"%1%\" %2%")
         % table.title(col) % typeId2SQLTypeName(table.typeId(col))).str();
      fieldNames = "\"" + table.title(col) + "\"";

      for(++col; col < table.nrCols(); ++col) {
        assert(table.typeId(col) != TI_NR_TYPES);
        assert(!table.title(col).empty());
        fieldSpecs += (boost::format(", \"%1%\" %2%")
         % table.title(col) % typeId2SQLTypeName(table.typeId(col))).str();
        fieldNames += ", \"" + table.title(col) + "\"";
      }
    }

    boost::shared_ptr<Table> currentTable(open(name, space, address));
    openDatabase(database);

    if(!currentTable) {
      // Create new table if it doesn't already exist.
      QSqlQuery query(QString((boost::format("CREATE TABLE \"%1%\" (%2%)")
           % info.table() % fieldSpecs).str().c_str()), database);

      if(!query.isActive()) {
        throwCannotBeCreated(info.table(), TABLE, std::string(query.lastError().text().toUtf8().constData()));
      }

      currentTable.reset(open(name, space, address));
      assert(currentTable);
    }

    {
      // Append records from table to the database.
      QSqlQuery query(database);
      for(size_t rec = 0; rec < table.nrRecs(); ++rec) {

        query.exec(QString((boost::format("INSERT INTO \"%1%\" (%2%) VALUES (%3%)")
           % info.table() % fieldNames % fieldValues(table, rec)).str().c_str()));

        if(!query.isActive()) {
          throwCannotWriteRecord(info.table(), TABLE, rec + 1,
              std::string(query.lastError().text().toUtf8().constData()));
        }
      }
    }
  }
  catch(...) {
    // database.close();
    throw;
  }

   //database.close();
}



//! Grants read access to table \a name to \a user.
/*!
  \param     name Name of table to grant access on.
  \param     user Name of user to grant access to.
  \exception Exception When granting fails.

  The default user is PUBLIC which means that read access is granted to all
  database users.
*/
void SQLTableDriver::grantReadAccess(
         std::string const& name,
         std::string const& user) const
{
  ConnectionInfo info(connectionInfoFor(name, DataSpace()));

  if(!info.isValid()) {
    throw Exception((boost::format(
         "Database connection information %1%: not valid")
         % name).str());
  }

  QSqlDatabase database = connectToDatabase(info);

  if(!database.isValid()) {
    throw Exception((boost::format(
         "Database connection %1%: Can not be created")
         % info.connection(this->name())).str());
  }

  openDatabase(database);
  std::string query = (boost::format("GRANT SELECT ON \"%1%\" TO \"%2%\"")
         % info.table()
         % user).str();

  try {
    execQuery(database, query);
  }
  catch(Exception& exception) {
    throw Exception((boost::format(
         "Cannot grant read access on %1% to %2%: %3%")
         % name
         % user
         % exception.message()
         ).str());
  }
}



// bool SQLTableDriver::databaseExists(
//          std::string const& name) const
// {
//   ConnectionInfo info(name);
// 
//   return QSqlDatabase::contains(info.connection(this->name()).c_str());
// }
// 
// 
// 
// void SQLTableDriver::removeDatabase(
//          std::string const& name) const
// {
//   assert(databaseExists(name));
// 
//   ConnectionInfo info(name);
// 
//   QSqlDatabase database = QSqlDatabase::database(
//          QString(info.connection(this->name()).c_str()));
// 
//   if(database.isValid()) {
//     database.close();
//   }
// 
//   QSqlDatabase::removeDatabase(QString(info.connection(this->name()).c_str()));
// 
//   assert(!databaseExists(name));
// }
// 
// 
// 
// void SQLTableDriver::addDatabase(
//          std::string const& name) const
// {
//   assert(!databaseExists(name));
// 
//   ConnectionInfo info(name);
// 
//   QSqlDatabase database = QSqlDatabase::addDatabase(
//          QString(this->name().c_str()),
//          QString(info.connection(this->name()).c_str()));
// 
//   if(!database.isValid()) {
//     throw Exception((boost::format(
//        "Database connection %1%: Can not be created")
//        % info.connection(this->name())).str());
//   }
// 
//   database.setUserName(QString(info.user().c_str()));
//   database.setPassword(QString(info.password().c_str()));
//   database.setHostName(QString(info.host().c_str()));
//   database.setDatabaseName(QString(info.database().c_str()));
// 
//   assert(database.isValid());
//   assert(databaseExists(name));
// }



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif // QT_NO_SQL

