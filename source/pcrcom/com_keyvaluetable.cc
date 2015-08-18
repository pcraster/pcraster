#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_KEYVALUETABLE
#include "com_keyvaluetable.h"
#define INCLUDED_COM_KEYVALUETABLE
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif
#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"
#define INCLUDED_COM_STRCONV
#endif
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the KeyValueTable class.
*/



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC KEYVALUETABLE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF KEYVALUETABLE MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::KeyValueTable::KeyValueTable():
  d_discardUnknownKeys(false)
{
}

//! dtor
com::KeyValueTable::~KeyValueTable()
{
  for(KeyConfigs::iterator p=d_keyConfigs.begin(); p!=d_keyConfigs.end();++p)
    delete p->second;
}

//! set if unknown keys must be discarded
/*!
 * Default unknown keys do throw an UnknownKey exception if added, to change
 * that set this to true.
 *
 * Another option instead of discarding could be to save unknown
 * keys as KeyValueString objects. This is not implemented
 */
void com::KeyValueTable::setDiscardUnknownKeys(bool discardUnknownKeys)
{
 d_discardUnknownKeys=discardUnknownKeys;
}

/*!
 *  key is copied into the key configurations
 *  \param  kvc key configuration to copy into this table
 *  \param  required if this key is required, see checkRequired()
 * \pre
 *   key with that name is not yet present in the table
 */
void com::KeyValueTable::insertKey(const KeyValueConfig& kvc, bool required)
{
    PRECOND(!d_keyConfigs.count(kvc.keyName()));
    KeyValueConfig *k = kvc.createClone();
    k->setRequired(required);
    d_keyConfigs.insert(std::make_pair(kvc.keyName(),k));
}

//! add and validate  key,value pair to table
/*!
 * if validation fails an exception is thrown
 * \throws
 *   struct UnknownKey, struct DuplicateKey or struct IllegalValue
 */
void com::KeyValueTable::add(const std::string& key, const std::string& value)
{
  KeyConfigs::iterator kc=d_keyConfigs.find(key);
  if (kc == d_keyConfigs.end()) {
    if (d_discardUnknownKeys)
       return;
    throw UnknownKey(key,"unknown keyword");
  }
  if (isSet(key))
    throw DuplicateKey(key,value,"keyword defined twice");

  try { kc->second->validate(value);
  } catch (const com::Exception& e) {
    throw IllegalValue(key,value,e.messages());
  }

  d_keyValues.insert(std::make_pair(key,value));
}

//! is \a key set in the table?
bool com::KeyValueTable::isSet(const std::string& key) const
{
  KeyValues::const_iterator ka=d_keyValues.find(key);
  return ka!=d_keyValues.end();
}

bool com::KeyValueTable::isSet(const KeyValueConfig& key) const
{
  return isSet(key.keyName());
}

/*! value associated with key
 *   \pre isSet(key)
 */
const std::string& com::KeyValueTable::value(const std::string& key) const
{
  PRECOND(isSet(key));
  KeyValues::const_iterator ka=d_keyValues.find(key);
  PRECOND(ka!=d_keyValues.end());
  return ka->second;
}

//! perform check if table has all required keys
/*!
 *  \throws struct MissingKey() keyName "is not present"
 */
void com::KeyValueTable::checkRequired() const
{
  for(KeyConfigs::const_iterator p=d_keyConfigs.begin(); p!=d_keyConfigs.end();++p) {
      const KeyValueConfig* kc = p->second;
      if (kc->required() && !isSet(*kc))
        throw MissingKey(kc->keyName(), "is not present");
  }
}

//! ctor
com::KeyValueConfig::KeyValueConfig(const std::string& keyName):
  d_keyName(keyName),d_required(false)
{
}

com::KeyValueConfig::~KeyValueConfig()
{
}

void com::KeyValueConfig::setRequired(bool required)
{
  d_required=required;
}

bool com::KeyValueConfig::required() const
{
  return d_required;
}

//! name of key
const std::string& com::KeyValueConfig::keyName() const
{
  return d_keyName;
}

//------------------------------------------------------------------------------
// DEFINITION OF EXCEPTIONS
//------------------------------------------------------------------------------

com::KeyValueTable::UnknownKey::UnknownKey(
    const std::string& keyName,
    const std::string& message)
{
  std::ostringstream str;
  str << "keyword '" << keyName << "': " << message;
  append(str.str());
}

com::KeyValueTable::MissingKey::MissingKey(
    const std::string& keyName,
    const std::string& message)
{
  std::ostringstream str;
  str << "keyword '" << keyName << "': " << message;
  append(str.str());
}

com::KeyValueTable::DuplicateKey::DuplicateKey(
    const std::string& key,
    const std::string& value,
    const std::string& message)
{
  std::ostringstream str;
  str << "keyword '" << key   << "': "
      << "value '"   << value << "': " << message;
  append(str.str());
}

com::KeyValueTable::IllegalValue::IllegalValue(
    const std::string& key,
    const std::string& value,
    const std::string& message)
{
  std::ostringstream str;
  str << "keyword '" << key   << "': "
      << "value '"   << value << "': " << message;
  append(str.str());
}

/*!
 * \throws
 *   struct com::KeyValueTable::IllegalValue with value set for key
 * \pre
 *    isSet(kv)
 */
void com::KeyValueTable::throwIllegalValue(
    const KeyValueConfig& kv,
    const std::string& message) const
{
  PRECOND(isSet(kv));
  throw IllegalValue(kv.keyName(), value(kv.keyName()), message);
}

//------------------------------------------------------------------------------
// DEFINITION OF KEYVALUESTRING MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::KeyValueString::KeyValueString(const std::string& keyName):
 KeyValueConfig(keyName)
{
}

//! dtor
com::KeyValueString::~KeyValueString()
{
}

//! validate on string is a no-op assuming empty strings are allowed
void com::KeyValueString::validate(const std::string& /* value */) const
{
}

com::KeyValueString* com::KeyValueString::createClone() const
{
  return new KeyValueString(*this);
}

//! return value for key as stored in  \a kvt
/*!
 * \pre
 *   \a kvt must have the key, see com::KeyValueTable::isSet()
 */
const std::string& com::KeyValueString::value(const com::KeyValueTable& kvt) const
{
  return kvt.value(keyName());
}

//------------------------------------------------------------------------------
// DEFINITION OF KEYVALUEENUM MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::KeyValueEnum::KeyValueEnum(const std::string& keyName):
 KeyValueConfig(keyName)
{
}

//! dtor
com::KeyValueEnum::~KeyValueEnum()
{
}

//! Copy constructor.
com::KeyValueEnum::KeyValueEnum(const KeyValueEnum& kve):
   KeyValueConfig(kve.keyName()),
   d_enumValues(kve.d_enumValues)
{
}

com::KeyValueEnum* com::KeyValueEnum::createClone() const
{
  return new KeyValueEnum(*this);
}

//! check if \a value is on the inserted enums
/*!
 * \pre
 *   at least one enum value must be inserted
 */
void com::KeyValueEnum::validate(const std::string& value) const
{
  PRECOND(!d_enumValues.empty());
  EnumValues::const_iterator p = d_enumValues.find(value);
  if (p == d_enumValues.end())
    throw com::Exception("value not allowed");
}

//! insert an enum
/*! duplicate insertions are discarded
 */
void com::KeyValueEnum::insert(const std::string& enumValue)
{
  d_enumValues.insert(enumValue);
}

//! return value for key as stored in  \a kvt
/*!
 * \pre
 *   \a kvt must have the key, see com::KeyValueTable::isSet()
 */
const std::string& com::KeyValueEnum::value(const com::KeyValueTable& kvt) const
{
  return kvt.value(keyName());
}

//! return value for key \a kvt with casing as inserted with insert()
const std::string& com::KeyValueEnum::configValue(const KeyValueTable& kvt) const
{
  EnumValues::const_iterator p = d_enumValues.find(kvt.value(keyName()));
  PRECOND(p != d_enumValues.end());
  return *p;
}

//------------------------------------------------------------------------------
// DEFINITION OF KEYVALUENUMBER MEMBERS
//------------------------------------------------------------------------------

//! ctor
/*!
 *  \param keyName keyword, name of key
 *  \param iv      valid range, 0 if no range, a clone copy of iv is generated
 */
com::KeyValueNumber::KeyValueNumber(
    const std::string& keyName,
    const Interval<double>* iv):
 KeyValueConfig(keyName),d_iv(0)
{
 if (iv)
  d_iv=iv->createClone();
}

com::KeyValueNumber::KeyValueNumber(const KeyValueNumber& k):
 KeyValueConfig(k.keyName()),
 d_iv(0)
{
 if (k.d_iv)
  d_iv=k.d_iv->createClone();
}

com::KeyValueNumber&  com::KeyValueNumber::operator=(const KeyValueNumber& k)
{
  if (this != &k) {
    *this = k;
    delete this->d_iv;
    this->d_iv=0;
    if (k.d_iv)
      this->d_iv = k.d_iv->createClone();
  }
  return *this;
}

//! dtor
com::KeyValueNumber::~KeyValueNumber()
{
  delete d_iv;
}



void com::KeyValueNumber::setInterval(const Interval<double>& iv)
{
  delete d_iv;
  d_iv = iv.createClone();
}

//! validate on string is a no-op assuming empty strings are allowed
void com::KeyValueNumber::validate(const std::string& value ) const
{
  try {
   double numericValue = typeValidate(value);
   if (d_iv)
    if (!d_iv->valid(numericValue))
      throw com::Exception(d_iv->msg());
  } catch (const std::range_error& re) {
      throw com::Exception(re.what());
  }
}

/*!
 *  \param keyName keyword, name of key
 *  \param iv      valid range, 0 if no range, a clone copy of iv is generated
 */
com::KeyValueInteger::KeyValueInteger(
    const std::string& keyName,
    const Interval<double>* iv):
  KeyValueNumber(keyName,iv)
{
}

com::KeyValueInteger::~KeyValueInteger()
{
}

com::KeyValueInteger* com::KeyValueInteger::createClone() const
{
  return new KeyValueInteger(*this);
}

//! return value for key as stored in  \a kvt
/*!
 * \pre
 *   \a kvt must have the key, see com::KeyValueTable::isSet()
 */
int com::KeyValueInteger::value(const com::KeyValueTable& kvt) const
{
  return fromString<int>(kvt.value(keyName()));
}

//! set \a v to value() iff key is set
void com::KeyValueInteger::setConditional(int& v, const KeyValueTable& kvt) const
{
  if (kvt.isSet(keyName()))
    v = value(kvt);
}

//! set \a v to value() iff key is set
void com::KeyValueInteger::setConditional(size_t& v, const KeyValueTable& kvt) const
{
  if (kvt.isSet(keyName()))
    v = value(kvt);
}

double com::KeyValueInteger::typeValidate(const std::string& value) const
{
  return fromString<int>(value);
}

/*!
 *  \param keyName keyword, name of key
 *  \param iv      valid range, 0 if no range, a clone copy of iv is generated
 */
com::KeyValueDouble::KeyValueDouble(
    const std::string& keyName,
    const Interval<double>* iv):
  KeyValueNumber(keyName,iv)
{
}

com::KeyValueDouble::~KeyValueDouble()
{
}

com::KeyValueDouble* com::KeyValueDouble::createClone() const
{
  return new KeyValueDouble(*this);
}

//! return value for key as stored in  \a kvt
/*!
 * \pre
 *   \a kvt must have the key, see com::KeyValueTable::isSet()
 */
double com::KeyValueDouble::value(const com::KeyValueTable& kvt) const
{
  return fromString<double>(kvt.value(keyName()));
}

double com::KeyValueDouble::typeValidate(const std::string& value) const
{
  return fromString<double>(value);
}

//! set \a v to value() iff key is set
void com::KeyValueDouble::setConditional(double& v, const KeyValueTable& kvt) const
{
  if (kvt.isSet(keyName()))
    v = value(kvt);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



