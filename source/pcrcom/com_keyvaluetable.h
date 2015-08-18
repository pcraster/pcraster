#ifndef INCLUDED_COM_KEYVALUETABLE
#define INCLUDED_COM_KEYVALUETABLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

// Module headers.



namespace com {
  // KeyValueTable declarations.
  template<typename T> class Interval;
}



namespace com {

//! Abstract class for key,value configurations used in class com::KeyValueTable
/*!
 * \todo
 *   Functionaliteit van klassen in KeyValueConfig hierarchie lijkt veel op wat
 *   in argument parsing in de class com::OptionValue gebeurt, en mischien ook
 *   class pcrxml::Attribute.
 *
 *  create class hierarchy
 *    sub classes
 *    - KeyValueNumber
 *       setInterval()
 *       subclasses
 *        KeyValueNumber
 *        KeyValueDouble
 */
class KeyValueConfig
{


private:
  //! name of key
  const std::string d_keyName;

  //! is key required
  bool d_required;

protected:
  //! Assignment operator. NOT IMPLEMENTED.
  KeyValueConfig&           operator=           (const KeyValueConfig&);

  // Copy constructor. DEFAULT
  //               KeyValueConfig               (const KeyValueConfig&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   KeyValueConfig              (const std::string& keyName);

  virtual         ~KeyValueConfig              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void setRequired(bool required);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const std::string&      keyName() const;

  //! return a copy of the object allocated by new
  virtual KeyValueConfig *createClone()const=0;

  //! validate the value
  /*!
   * \throws
   *   com::Exception is not a valid value
   */
  virtual void validate(const std::string& value) const=0;

  bool required() const;

};


//! Manage a set of key,value pairs as apparent in many file formats
/*!
 *  Many (GIS) formats provide an ASCII file with key, value pairs.
 *
 *  Like BIL (.HDR)
 *  <pre>
 *    nrows  200   comment here is allowed
 *    ncols  100
 *    layout bil
 *  </pre>
 *
 *  Like Idrisi (.DOC)
 *  <pre>
 *    rows:     200
 *    columns:  100
 *    data type: real
 *  </pre>
 *
 *  Like Esri Grid Ascii Header (part of data file)
 *  <pre>
 *    NCOLS 384
 *    NROWS 326
 *    XLLCORNER 362000.000000
 *    YLLCORNER 197000.000000
 *    CELLSIZE 1000.000000
 *    NODATA_VALUE 1e31
 *  </pre>
 *
 *  This class provides a generic interface for parsing and checking
 *  such files. Advantage is that the order of keywords is not relevant.
 *  Checks implemented are:
 *  <ul>
 *   <li>type checking, string, integer etc.</li>
 *   <li>range checking, &gt; 0</li>
 *   <li>unique: each key is only defined once</li>
 *  </ul>
 *
 * \sa class KeyValueTable::Error
 *
 *  Keys are matched case insensitive, NROWS, nrows and nRows all match as the
 *  same key. Note that the exception messages to the user use the word keyword
 *  instead of key.
 *
 *  use pattern (zo ongeveer):
 *  <pre>
 *  KeyValueNumber rows("rows",com::GreaterThan0());
 *  keyValueTable.insertKey(rows,true);
 *  try {
 *   while(std::getline(stream, line)) {
 *     tokens = com::split(line, ':'); // split as you like
 *     keyValueTable.add(tokens[0],tokens[1]);
 *  } catch (...) {
 *    // process errors
 *  }
 *  if (!keyValueTable.isSet(rows)) {
 *   // error. rows not set
 *  } else {
 *    // it knows rows is an integer, assertion will
 *    // fail otherwise
 *    int nrRows = rows.value(keyValueTable);
 *  }
 *  </pre>
 *
 */
class KeyValueTable
{

private:

  bool d_discardUnknownKeys;

  //! keys added key -> value
  typedef std::map<std::string,std::string, com::StringLessNoCase > KeyValues;
  //! keys added
  KeyValues d_keyValues;

  //! key configurations
  typedef std::map<std::string, KeyValueConfig *,com::StringLessNoCase > KeyConfigs;
  //! key configurations
  KeyConfigs d_keyConfigs;

  //! Assignment operator. NOT IMPLEMENTED.
  KeyValueTable&           operator=           (const KeyValueTable&);

  //! Copy constructor. NOT IMPLEMENTED.
                   KeyValueTable               (const KeyValueTable&);

public:
  //! errors associated with class KeyValueTable
  struct Error      :   public com::Exception {
  };

  //! unknown key error
  struct UnknownKey :   public Error {
     UnknownKey(const std::string& keyName, const std::string& message);
  };
  //! missing key error, see checkRequired()
  struct MissingKey :   public Error {
     MissingKey(const std::string& keyName, const std::string& message);
  };
  //! duplicate key error
  struct DuplicateKey : public Error {
     DuplicateKey( const std::string& key, const std::string& value,
                   const std::string& message);
  };
  //! illegal value error
  struct IllegalValue : public Error {
     IllegalValue( const std::string& key, const std::string& value,
                   const std::string& message);
  };

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    KeyValueTable              ();

  /* virtual */    ~KeyValueTable              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void add(const std::string& key, const std::string& value);

  void insertKey(const KeyValueConfig& kvc, bool required=false);

  void setDiscardUnknownKeys(bool discardUnknownKeys);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool isSet(const std::string& key) const;
  bool isSet(const KeyValueConfig& key) const;

  const std::string& value(const std::string& key) const;

  void throwIllegalValue(
      const KeyValueConfig& kv,
      const std::string&    message) const;


  void checkRequired() const;

};

class KeyValueString : public KeyValueConfig {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  KeyValueString&           operator=           (const KeyValueString&);

  //  Copy constructor. DEFAULT
  //               KeyValueString               (const KeyValueString&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   KeyValueString(const std::string& keyName);

  /* virtual */    ~KeyValueString              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  KeyValueString *createClone()const;
  const std::string& value(const KeyValueTable& kvt) const;
  void validate(const std::string& value) const;

};


class KeyValueEnum : public KeyValueConfig {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  KeyValueEnum&           operator=           (const KeyValueEnum&);

  //! Copy constructor.
                   KeyValueEnum               (const KeyValueEnum&);

  typedef std::set<std::string, com::StringLessNoCase> EnumValues;
  EnumValues d_enumValues;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   KeyValueEnum(const std::string& keyName);

  /* virtual */    ~KeyValueEnum              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void insert(const std::string& enumValue);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  KeyValueEnum *createClone()const;
  const std::string& value(const KeyValueTable& kvt) const;
  const std::string& configValue(const KeyValueTable& kvt) const;
  void validate(const std::string& value) const;
};

//! abstract class for numeric values
class KeyValueNumber : public KeyValueConfig {

private:
  const Interval<double> *d_iv;

protected:

  //! Assignment operator. NOT IMPLEMENTED.
  KeyValueNumber&           operator=           (const KeyValueNumber&);

  //! Copy constructor.
                   KeyValueNumber               (const KeyValueNumber&);


  /*! validate against type expected
   * \throws std::range_error as in com::fromString()
   * \returns value as double, to pass to validator
   */
  virtual double typeValidate(const std::string& value) const=0;
protected:
         KeyValueNumber(const std::string& keyName,
                        const Interval<double>* iv);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

     virtual       ~KeyValueNumber              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void setInterval(const Interval<double>& iv);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void validate(const std::string& value) const;

};

class KeyValueInteger : public KeyValueNumber {
 protected:
  double typeValidate(const std::string& value) const;
 public:
   KeyValueInteger(
       const std::string& keyName,
       const Interval<double>* iv=0);
   ~KeyValueInteger();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  KeyValueInteger *createClone()const;
  int    value(const KeyValueTable& kvt) const;

  void setConditional(int& v,   const KeyValueTable& kvt) const;
  void setConditional(size_t& v, const KeyValueTable& kvt) const;
};

class KeyValueDouble : public KeyValueNumber {
 protected:
  double typeValidate(const std::string& value) const;
 public:
   KeyValueDouble(
       const std::string& keyName,
       const Interval<double>* iv=0);
   ~KeyValueDouble();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  KeyValueDouble *createClone()const;
  double   value(const KeyValueTable& kvt) const;
  void     setConditional(double& v, const KeyValueTable& kvt) const;
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
