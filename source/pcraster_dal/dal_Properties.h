#ifndef INCLUDED_DAL_PROPERTIES
#define INCLUDED_DAL_PROPERTIES



// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_BOOST_ANY
#include <boost/any.hpp>
#define INCLUDED_BOOST_ANY
#endif

// PCRaster library headers.
#ifndef INCLUDED_DEV_COMPILER
#include "dev_Compiler.h"
#define INCLUDED_DEV_COMPILER
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_PROPERTYKEYS
#include "dal_PropertyKeys.h"          // Only for convenience.
#define INCLUDED_DAL_PROPERTYKEYS
#endif



namespace dal {
  // Properties declarations.
}



namespace dal {



//! Class for storing all kinds/types of properties.
/*!
  Objects can be used to store properties of varying types. Convenience
  functions allow for setting, testing and retrieving property values.
  Properties are key referenced by name and can have any type.

  For convience a number of keys are predefined in \file dal_PropertyKeys.h

  \todo Afdwingen dat spullen die in d_values komen copyable zijn. Voor copy
        constructor.
*/
class PCR_DAL_DECL Properties
{

  friend class PropertiesTest;

private:

  //! Datastructure for storing the property values by name.
  std::map<std::string, boost::any> d_values;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Properties          ();

                   Properties          (Properties const& rhs);

  /* virtual */    ~Properties         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Properties&      operator=           (Properties const& rhs);

  template<typename T>
  void             setValue            (std::string const& name,
                                        T const& value);

  void             clear               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             hasValue            (std::string const& name) const;

  template<typename T>
  T const&         value               (std::string const& name) const;

  template<typename T>
  T&               value               (std::string const& name);

  template<typename T>
  T const&         value               (std::string const& name,
                                        T const& defaultValue) const;

  size_t           size                () const;

  bool             isEmpty             () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Returns whether a property value has been set for \a name.
/*!
  \param     name Name of property to check.
  \return    true or false
*/
inline bool Properties::hasValue(
         std::string const& name) const
{
  return d_values.find(name) != d_values.end();
}

//! Sets the value of property \a name to \a value.
/*!
  \param     name Name of property to set.
  \param     value Value of property to set.
  \warning   Any previous value is overwritten.
*/
template<typename T>
inline void Properties::setValue(
         std::string const& name,
         T const& value)
{
  d_values[name] = value;
}

//! Returns the property value for \a name.
/*!
  \param     name Name of property to return value for.
  \return    value
  \warning   A value for property \a name must have been set.
*/
template<typename T>
inline T const& Properties::value(
         std::string const& name) const
{
  assert(hasValue(name));

  return *boost::any_cast<T>(&(*d_values.find(name)).second);
}

//! Returns the property value for \a name.
/*!
  \param     name Name of property to return value for.
  \return    value
  \warning   A value for property \a name must have been set.
*/
template<typename T>
inline T& Properties::value(
         std::string const& name)
{
  assert(hasValue(name));

  return *boost::any_cast<T>(&(*d_values.find(name)).second);
}

//! Returns the property value for \a name.
/*!
  \param     name         Name of property to return value for.
  \param     defaultValue if (!hasValue(name) return defaultValue
  \return    value
  \warning   A value for property \a name must have been set.

  The default value will not be used to set a property value if no property
  value for \a name has been set yet.
*/
template<typename T>
inline T const & Properties::value(
         std::string const& name,
         T const& defaultValue) const
{
  if (hasValue(name))
    return value<T>(name);
  return defaultValue;
}


//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
