#ifndef INCLUDED_COM_ANYPOINTERS
#define INCLUDED_COM_ANYPOINTERS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_BOOST_ANY
#include <boost/any.hpp>
#define INCLUDED_BOOST_ANY
#endif

// PCRaster library headers.

// Module headers.



namespace com {
  // AnyPointers declarations.
}



namespace com {

template<class Type>
bool isOfType(const boost::any& value)
{
  return value.type() == typeid(Type);
}

template<class Type>
class valueIsOfType
{
public:
  bool operator()(const std::pair<size_t, boost::any>& pair) const
  {
    return isOfType<Type>(pair.second);
  }
};

template<class Type>
class valueEquals
{
private:

  Type             d_value;

public:

  valueEquals(const Type& value)
    : d_value(value)
  {
  }

  bool operator()(const std::pair<size_t, boost::any>& pair) const
  {
    return isOfType<Type>(pair.second) &&
         boost::any_cast<Type>(pair.second) == d_value;
  }
};



//! Class to store pointers to objects of different types.
/*!
  AnyPointers objects are capable of storing pointers to objects of different
  types, like int rasters, double rasters and a collection with pointers. When
  the pointers are requested a type check is made to see if the requested
  type equals the stored one.
*/
class AnyPointers
{

private:

  //! Value used for generating unique id's.
  size_t           d_uniqueId;

  //! The collections of datasets.
  std::map<size_t, boost::any> d_objects;

  //! Assignment operator. NOT IMPLEMENTED.
  AnyPointers&     operator=           (const AnyPointers& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   AnyPointers         (const AnyPointers& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
  /*!
  */
                   AnyPointers         ()
  {}

  //! Destructor.
  /*!
  */
  /* virtual */    ~AnyPointers        ()
  {}

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Adds a pointer to the collection.
  /*!
    \param     pointer Data set to add.
    \return    Unique id for this pointer.
    \sa        erase(size_t), pointer(size_t)

    The id can be used to get at the pointer again later.

    Only one copy of a pointer is stored. If a pointer is added before, the
    id of the previously stored pointer is returned.
  */
  template<class ObjectType>
  size_t           insert              (ObjectType* pointer)
  {
    size_t id;

    std::map<size_t, boost::any>::const_iterator it = std::find_if(
         d_objects.begin(), d_objects.end(),
         valueEquals<ObjectType*>(pointer));
    if(it != d_objects.end()) {
      id = (*it).first;
    }
    else {
      d_objects[d_uniqueId] = pointer;
      id = d_uniqueId++;
    }

    return id;
  }

  //! Adds a pointer to the collection.
  /*!
    \param     pointer Data set to add.
    \param     id Id for this pointer.
    \warning   \a pointer must not have been added previously with another id.
               No pointer with id \a id must exist already.
    \sa        insert(ObjectType*), erase(size_t), pointer(size_t)
  */
  template<class ObjectType>
  void             insert              (ObjectType* pointer,
                                        size_t id)
  {
    std::map<size_t, boost::any>::const_iterator it = std::find_if(
         d_objects.begin(), d_objects.end(), valueEquals<ObjectType*>(pointer));
    if(it != d_objects.end()) {
      PRECOND((*it).first == id);
    }
    else {
      d_objects[id] = pointer;
      d_uniqueId = ++id;
    }
  }

  //! Removes pointer with id \a id.
  /*!
    \param     id Id of pointer to remove.
    \warning   A pointer with id \a id must exist.
    \sa        insert(ObjectType*), insert(ObjectType*, size_t), erase(size_t),
               pointer(size_t)
  */
  template<class ObjectType>
  void             erase               (size_t id)
  {
    PRECOND(d_objects.count(id) == 1);
    d_objects.erase(id);
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns whether the collection of pointers is empty.
  /*!
    \return    true or false
  */
  bool             empty               () const
  {
    return d_objects.empty();
  }

  //! Returns the number of pointers stored.
  /*!
    \return    Number of pointers.
  */
  size_t           size                () const
  {
    return d_objects.size();
  }

  //! Returns the number of pointers stored of a certain type.
  /*!
    \return    Number of pointers.
  */
  template<class ObjectType>
  size_t           size                () const
  {
    return std::count_if(d_objects.begin(), d_objects.end(),
         valueIsOfType<ObjectType*>());
  }

  //! Returns the pointer to an object with id \a id.
  /*!
    \param     id Id of pointer to return.
    \return    Pointer.
    \warning   Pointer with id \a id must be stored.
  */
  template<class ObjectType>
  ObjectType const* pointer            (size_t id) const
  {
    PRECOND(d_objects.count(id) == 1);
    return boost::any_cast<ObjectType*>((*d_objects.find(id)).second);
  }

  //! Returns the pointer to an object with id \a id.
  /*!
    \param     id Id of pointer to return.
    \return    Pointer.
    \warning   Pointer with id \a id must be stored.
  */
  template<class ObjectType>
  ObjectType*      pointer             (size_t id)
  {
    PRECOND(d_objects.count(id) == 1);
    return boost::any_cast<ObjectType*>((*d_objects.find(id)).second);
  }

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
