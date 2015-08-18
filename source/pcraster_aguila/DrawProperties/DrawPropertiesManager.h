#ifndef INCLUDED_DRAWPROPERTIESMANAGER
#define INCLUDED_DRAWPROPERTIESMANAGER



// External headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

// Project headers.

// Module headers.



namespace ag {
  // DrawPropertiesManager declarations.
  class DrawProperties;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
template<typename AttributeKey, typename PropertiesKey>
class DrawPropertiesManager: private boost::noncopyable,
         public std::map<AttributeKey,
              std::map<PropertiesKey, boost::shared_ptr<DrawProperties> > >
{

  friend class DrawPropertiesManagerTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DrawPropertiesManager();

  /* virtual */    ~DrawPropertiesManager();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename AttributeKey, typename PropertiesKey>
inline DrawPropertiesManager<AttributeKey, PropertiesKey>::
         DrawPropertiesManager()
  : std::map<AttributeKey,
         std::map<PropertiesKey, boost::shared_ptr<DrawProperties> > >()
{
}

template<typename AttributeKey, typename PropertiesKey>
inline DrawPropertiesManager<AttributeKey, PropertiesKey>::
         ~DrawPropertiesManager()
{
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

#endif
