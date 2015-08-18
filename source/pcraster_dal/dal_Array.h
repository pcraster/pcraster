#ifndef INCLUDED_DAL_ARRAY
#define INCLUDED_DAL_ARRAY



// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.
// #ifndef INCLUDED_DAL_CONFIGURE
// #include "dal_Configure.h"
// #define INCLUDED_DAL_CONFIGURE
// #endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif

// #ifdef _MSC_VER // TODO link error otherwise
// namespace pcr {
//  template<>
//   inline bool isMV(const std::string&)
//   { return false; }
// }
// #endif



namespace dal {
  // Array declarations.
}



namespace dal {



//! The Array class is for arrays.
/*!
  This class is similar to the std::vector class. The difference is that Array
  objects are willing to release the pointer to the data they contain. The
  data is stored in a linear block of memory allocated with new[].

  \todo check if Boost.Iterator is useful here

  \sa        Documentation of std::vector.
*/
template<class T>
class /* PCR_DAL_DECL */ Array
{

  friend class ArrayTest;

private:

  //! Capacity of the array: how many values can be stored without reallocation.
  size_t           d_capacity;

  //! Number of values stored.
  size_t           d_size;

  //! Datastructure for actual values.
  T*               d_elements;

public:

  //! Const iterator type.
  typedef T const* const_iterator;

  //! Iterator type.
  typedef T* iterator;

  //! Value type.
  typedef T value_type;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Array               ();

                   Array               (size_t size);

                   Array               (size_t size,
                                        T const& value);

                   Array               (size_t size,
                                        T* values);

                   Array               (Array const& rhs);

  /* virtual */    ~Array              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Array&           operator=           (Array const& rhs);

  void             resize              (size_t size);

  void             reserve             (size_t capacity);

  void             push_back           (T const& value);

  // void             pop_back            ();

  void             clear               ();

  // void             resize              (size_t size,
  //                                       T const& value);

  T*               release             ();

  void             reset               (T* values);

  void             setAllMV            ();

  void             fill                (T const& value);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             equals              (Array const& array) const;

  iterator         begin               ();

  iterator         end                 ();

  const_iterator   begin               () const;

  const_iterator   end                 () const;

  size_t           size                () const;

  size_t           capacity            () const;

  bool             empty               () const;

  T&               operator[]          (size_t i);

  T const&         operator[]          (size_t i) const;

  T const*         elements            () const;

  T*               elements            ();

  /*
  T&               front               ();

  T const&         front               () const;

  T&               back                ();

  T const&         back                () const;
  */

  // T&               min                 () const;

  bool             extremes            (T& min,
                                        T& max) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

// #ifndef PCR_DAL_UNIT_TEST_TARGET

//! Default constructor.
/*!
  Initial capacity will be 100 values.
*/
template<typename T>
inline Array<T>::Array()

  : d_capacity(0), d_size(0), d_elements(0)

{
  reserve(100);

  assert(d_capacity > 0);
}

//! Constructor.
/*!
  \param     size Initial number of values in the array.

  Value of the elements in the array is undefined.
*/
template<typename T>
inline Array<T>::Array(size_t size)

  : d_capacity(0), d_size(0), d_elements(0)

{
  if(size > 0) {
    reserve(size);
  }
  else {
    reserve(100);
  }

  d_size = size;

  assert(d_capacity > 0);
}

//! Constructor.
/*!
  \param     size Initial number of values in the array.
  \param     value Value of the elements in the array.

  All elements in the array are set to \a value.
*/
template<typename T>
inline Array<T>::Array(size_t size, T const& value)

  : d_capacity(0), d_size(0), d_elements(0)

{
  reserve(size);
  d_size = size;
  std::fill(begin(), end(), value);

  assert(d_capacity > 0);
}

//! Constructor.
/*!
  \param     size Number of elements in \a values.
  \param     values C array to take ownership of.
  \warning   \a values is ours to delete. It is better not to use \a values
             anymore.
*/
template<typename T>
inline Array<T>::Array(size_t size, T* values)

  : d_capacity(0), d_size(0), d_elements(0)

{
  d_capacity = size;
  d_size = size;
  d_elements = values;
}

//! Copy constructor.
/*!
  \param     rhs Array to copy from.
*/
template<typename T>
inline Array<T>::Array(Array const& rhs)

  : d_capacity(0), d_size(0), d_elements(0)

{
  reserve(rhs.d_capacity);
  std::memcpy(static_cast<void*>(d_elements),
         static_cast<void const*>(rhs.d_elements), rhs.d_size * sizeof(T));
  d_size = rhs.d_size;

  assert(d_capacity > 0);
}

//! Destructor.
/*!
*/
template<typename T>
inline Array<T>::~Array()
{
  if(d_elements) {
    delete[] d_elements;
  }
}

//! Assignment operator.
/*!
  \param     rhs Array to assign from.
  \return    Reference to current array.
*/
template<typename T>
inline Array<T>& Array<T>::operator=(Array const& rhs)
{
  if(this != &rhs) {
    reserve(rhs.d_capacity);
    std::memcpy(static_cast<void*>(d_elements),
           static_cast<void const*>(rhs.d_elements), rhs.d_size * sizeof(T));
    d_size = rhs.d_size;
  }

  assert(d_capacity > 0);

  return *this;
}

//! Reserves memory for \a capacity values.
/*!
  \param     capacity Size of underlying data structure.

  Nothing happens if the current capacity is equal or larger than \a capacity.
  If a re-allocation of memory is needed, all values are copied to the new
  data structure.
*/
template<typename T>
inline void Array<T>::reserve(size_t capacity)
{
  if(capacity > d_capacity) {
    T* values = new T[capacity];
    if(d_elements) {
      std::memcpy(static_cast<void*>(values),
           static_cast<void const*>(d_elements), d_size * sizeof(T));
      delete[] d_elements;
    }

    d_elements = values;
    d_capacity = capacity;
  }
}

template<typename T>
inline void Array<T>::resize(size_t size)
{
  if(d_capacity < size) {
    reserve(size);
  }

  d_size = size;
}

//! Appends a new value to the array.
/*!
  \param     value New value.

  If the current capacity doesn't facilitate the addition of the new value,
  more room is reserved.
*/
template<typename T>
inline void Array<T>::push_back(T const& value)
{
  if(d_size == d_capacity) {
    reserve(2 * d_capacity);
  }

  d_elements[d_size++] = value;
}

template<typename T>
inline void Array<T>::clear()
{
  d_size = 0;
}

//! Releases the layered array. This object will not delete it anymore.
/*!
  \return    Pointer to layered array.
  \warning   The array is allocated with new, use delete to release the memory.
             The state of this object is undefined after this call. Don't use
             it anymore.
*/
template<typename T>
inline T* Array<T>::release()
{
  T* result = d_elements;
  d_elements = 0;

  return result;
}

//! Deletes the current contents and takes ownership of \a values.
/*!
  \tparam    T Value type.
  \param     values Values to take ownership over.
*/
template<typename T>
void Array<T>::reset(
         T* values)
{
  delete d_elements;
  d_elements = values;
}

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
template<typename T>
inline bool Array<T>::equals(Array const& array) const
{
  if(size() != array.size()) {
    return false;
  }

  for(size_t i = 0; i < d_size; ++i) {
    if(!(pcr::isMV(d_elements[i]) && pcr::isMV(array.d_elements[i])) &&
       !(!pcr::isMV(d_elements[i]) && !pcr::isMV(array.d_elements[i])) &&
       !(d_elements[i] == array.d_elements[i])) {
      return false;
    }
  }

  return true;
}

#ifndef _MSC_VER
// compile problem
//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
template<>
inline bool Array<std::string>::equals(Array const& array) const
{
  if(size() != array.size()) {
    return false;
  }

  for(size_t i = 0; i < d_size; ++i) {
    if(!(d_elements[i] == array.d_elements[i])) {
      return false;
    }
  }

  return true;
}
#endif

//! Returns an iterator to the first element.
/*!
  \return    Iterator
*/
template<typename T>
inline typename Array<T>::iterator Array<T>::begin()
{
  return d_elements;
}

//! Returns an iterator to the one-past-the-last element.
/*!
  \return    Iterator
*/
template<typename T>
inline typename Array<T>::iterator Array<T>::end()
{
  return d_elements + d_size;
}

//! Returns a const iterator to the first element.
/*!
  \return    Iterator
*/
template<typename T>
inline typename Array<T>::const_iterator Array<T>::begin() const
{
  return d_elements;
}

//! Returns a const iterator to the one-past-the-last element.
/*!
  \return    Iterator
*/
template<typename T>
inline typename Array<T>::const_iterator Array<T>::end() const
{
  return d_elements + d_size;
}

//! Returns the size of the array.
/*!
  \return    Size
  \sa        empty(), capacity()

  The size of an array is the number of elements stored.
*/
template<typename T>
inline size_t Array<T>::size() const
{
  return d_size;
}

//! Returns the capacity of the array.
/*!
  \return    Capacity
  \sa        size()

  The capacity is the size of the underlying data structure. When the size of
  an array equals the capacity and more values are added, the capacity needs
  to be enlarged (using reserve(size_t)).
*/
template<typename T>
inline size_t Array<T>::capacity() const
{
  return d_capacity;
}

//! Returns true if the array does not contain values.
/*!
  \return    true or false
  \sa        size()
*/
template<typename T>
inline bool Array<T>::empty() const
{
  return d_size == 0;
}

//! Subscript operator.
/*!
  \param     i Index of element to return.
  \return    Element
  \warning   Make sure i is smaller than the size of the array.
*/
template<typename T>
inline T& Array<T>::operator[](size_t i)
{
  assert(i < d_size);
  return d_elements[i];
}

//! Subscript operator.
/*!
  \param     i Index of element to return.
  \return    Element
  \warning   Make sure i is smaller than the size of the array.
*/
template<typename T>
inline T const& Array<T>::operator[](size_t i) const
{
  assert(i < d_size);
  return d_elements[i];
}

//! Returns a pointer to the layered C array.
/*!
  \return    Pointer to C array.
*/
template<typename T>
inline T const* Array<T>::elements() const
{
  return d_elements;
}

//! Returns a pointer to the layered C array.
/*!
  \return    Pointer to C array.
*/
template<typename T>
inline T* Array<T>::elements()
{
  return d_elements;
}

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      This should be a dal or csf/pcr algorithm.
*/
template<typename T>
inline bool Array<T>::extremes(T& min, T& max) const
{
  const_iterator it = begin();

  while(it != end() && pcr::isMV(*it)) {
    ++it;
  }

  if(it == end()) {
    return false;
  }
  else {
    min = *it;
    max = *it;
  }

  while(++it != end()) {
    if(!pcr::isMV(*it)) {
      min = std::min(min, *it);
      max = std::max(max, *it);
    }
  }

  return true;
}

template<typename T>
inline void Array<T>::setAllMV()
{
  for(iterator it = begin(); it != end(); ++it) {
    pcr::setMV(*it);
  }
}

template<typename T>
inline void Array<T>::fill(T const& value)
{
  for(size_t i = 0; i < size(); ++i) {
    d_elements[i] = value;
  }
}


template<typename T>
inline size_t indexOf(
    Array<T> const& array,
    T const& value)
{
  size_t i = 0;
  for(i = 0; i < array.size(); ++i) {
    if(array[i] == value) {
      break;
    }
  }

  return i;
}

// #endif // PCR_DAL_UNIT_TEST_TARGET



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

template<class T>
inline bool operator==(Array<T> const& lhs, Array<T> const& rhs)
{
  return lhs.equals(rhs);
}

template<class T>
inline bool operator!=(Array<T> const& lhs, Array<T> const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
