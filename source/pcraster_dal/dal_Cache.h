#pragma once
#include <cassert>
#include <map>
#include <boost/bimap.hpp>
#include <boost/noncopyable.hpp>


namespace dal {

//! Class for storing reference counted name - object relations.
/*!
  This class is for objects that should be shared. They are indexed by name,
  so you can ask whether an object with a certain name is already stored. If
  so, you can get a pointer to it.

  Users should call incrementUseCount(Object*) and decrementUseCount(Object*)
  at the appropriate times. When the use count of an object drops to zero, it
  is deleted from memory.

  \tparam    T Type of object whose pointers are to be stored.
*/
template<
   class T>
class Cache: private boost::noncopyable
{

  friend class CacheTest;

public:

                   Cache               ();

                   ~Cache              ();

  void             insert              (std::string const& name,
                                        T* object);

  void             incrementUseCount   (T const* object);

  void             decrementUseCount   (T const* object);

  void             clear               ();

  bool             empty               () const;

  size_t           size                () const;

  bool             contains            (std::string const& name) const;

  bool             contains            (T const* object) const;

  std::string const& name              (T const* object) const;

  T*               object              (std::string const& name);

private:

  //! Type for storing the name - object relations.
  typedef typename boost::bimap<std::string, T*> Map;

  //! Type for storing the use count per object.
  typedef std::map<T* , size_t> UseCount;

  //! Collection of name - object relations.
  Map              _map;

  //! Use count per object.
  UseCount         _useCount;

};


//! Default constructor.
/*!
*/
template<
    class T>
Cache<T>::Cache()
{
}


//! Destructor.
/*!
*/
template<
    class T>
Cache<T>::~Cache()
{
    // When used correctly, this object should not refer to any object anymore.
    assert(_map.empty());
    assert(_useCount.empty());
    clear();
}


//! Inserts the \a name - \a object relation into the collection.
/*!
  \param     name Name of object to store.
  \param     object Pointer to object to store.
  \warning   No other object with the same name may be present in the
             collection. Also, the object may not be already stored by a
             different name.

  The use count is not incremented. Calling code must increment and decrement
  it at the appropriate times.

  It is OK to call this function for name - object relations that are already
  in the collection. In that case, nothing happens.
*/
template<
    class T>
void Cache<T>::insert(
         std::string const& name,
         T* object)
{
    assert(_map.size() == _useCount.size());
    assert(!contains(name) || this->object(name) == object);
    assert(!contains(object) || this->name(object) == name);

    if(!contains(name)) {
        _map.insert(typename Map::value_type(name, object));
        _useCount.insert(typename UseCount::value_type(object, 0));
    }
    // else {
    //     incrementUseCount(object);
    // }

    assert(_map.size() == _useCount.size());
}


//! Increments the use count for \a object by one.
/*!
  \param     object Object to increase the use count for.
  \sa        decrementUseCount(T const*)
*/
template<
    class T>
void Cache<T>::incrementUseCount(
    T const* object)
{
    typename UseCount::iterator it = _useCount.find(const_cast<T*>(object));
    assert(it != _useCount.end());
    ++(*it).second;
}


//! Decrements the use count for \a object by one.
/*!
  \param     object Object to decrease the use count for.
  \warning   Don't use the object after calling this function. It may not
             exist anymore.
  \sa        incrementUseCount(T const*)

  When the use count for \a object reaches zero, the object will be deleted
  from memory.
*/
template<
    class T>
void Cache<T>::decrementUseCount(
         T const* object)
{
  assert(_map.size() == _useCount.size());
  assert(!_map.empty());

  typename UseCount::iterator it = _useCount.find(const_cast<T*>(object));
  assert(it != _useCount.end());
  assert((*it).second > 0);
  --(*it).second;

  if((*it).second == 0) {
      assert((*_map.right.find(const_cast<T*>(object))).first == object);

      // Delete records referring to object.
      _useCount.erase(it);
      _map.right.erase(const_cast<T*>(object));

      // Delete object.
      delete const_cast<T*>(object);
  }

  assert(_map.size() == _useCount.size());
}


//! Erases the contents of the cache.
/*!
*/
template<
    class T>
void Cache<T>::clear()
{
    assert(_map.size() == _useCount.size());

    for(typename Map::left_iterator it = _map.left.begin(); it !=
            _map.left.end(); ++it) {
        delete (*it).second;
    }

    _map.clear();
    _useCount.clear();
}


//! Returns whether the cache is empty or not.
/*!
  \return    true or false
*/
template<
    class T>
bool Cache<T>::empty() const
{
    assert(_map.size() == _useCount.size());

    return _map.empty();
}


//! Returns the number of name - object relations stored in the cache.
/*!
  \return    Number of pairs.
*/
template<
    class T>
size_t Cache<T>::size() const
{
    assert(_map.size() == _useCount.size());

    return _map.size();
}


//! Returns whether the cache contains an entry for \a name.
/*!
  \param     name Name of relation to find.
  \return    true or false
*/
template<
    class T>
bool Cache<T>::contains(
         std::string const& name) const
{
    return _map.left.find(name) != _map.left.end();
}


//! Returns whether the cache contains an entry for \a object.
/*!
  \param     object Object of relation to find.
  \return    true or false
*/
template<
    class T>
bool Cache<T>::contains(
         T const* object) const
{
    return _map.right.find(const_cast<T*>(object)) != _map.right.end();
}


//! Returns the name of the relation \a object is part of.
/*!
  \param     object Object of relation.
  \return    Name.
*/
template<
    class T>
std::string const& Cache<T>::name(
         T const* object) const
{
    assert(contains(object));

    return (*_map.right.find(const_cast<T*>(object))).second;
}


//! Returns the pointer to the object of the relation \a name is part of.
/*!
  \param     name Name of relation.
  \return    Pointer to object.
*/
template<
    class T>
T* Cache<T>::object(
         std::string const& name)
{
    assert(contains(name));

    return (*_map.left.find(name)).second;
}

} // namespace dal
