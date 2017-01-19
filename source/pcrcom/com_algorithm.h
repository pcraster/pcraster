#pragma once

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#include <algorithm>
#include <iterator>
#include <map>

/*!
  \file
  This file includes and extends <algorithm> with usefull templates
*/


namespace com {
  // Algorithm declarations.
}



namespace com {

//------------------------------------------------------------------------------
// TEMPLATES
//------------------------------------------------------------------------------


//! shorthand for std::for_each(c.begin(),c.end(),o)
template <class Container, class Operation>
 Operation forWhole(Container& c, Operation o) {
    return std::for_each(c.begin(),c.end(),o);
 }

//! shorthand for std::for_each(c.rbegin(),c.rend(),o)
template <class Container, class Operation>
 Operation forWholeReversed(Container& c, Operation o) {
    return std::for_each(c.rbegin(),c.rend(),o);
 }

//! on classic C array of Element type
template <class Element, class Operation>
 Operation forWhole(Element *a, size_t len, Operation o) {
    return std::for_each(a+0,a+len,o);
 }

//! shorthand for std::copy(c.begin(),c.end(),destBeg)
/*!
 * \sa com::insertTo
 */
template <class Container, class OutputIterator>
 OutputIterator copyWhole(Container& c, OutputIterator destBeg) {
    return std::copy(c.begin(),c.end(),destBeg);
 }

//! shorthand for std::copy(src.begin(),src.end(),std::insertor<dest,dest.begin())
/*!
 *  handy for inserting into maps or sets
 * \sa com::copyWhole
 * \todo
 *   implement Josuttis iter/assoiter to insert into map or set efficiently
 */
template <class SrcContainer, class DestContainer>
 void insertTo( SrcContainer& src, DestContainer& dest) {
      std::copy(src.begin(),src.end(),std::inserter(dest,dest.begin()));
 }

//! look if container \a c contains 1 or more elements \a k
/*!
 *  Uses std::find algorithm, do not use on associative containers.
 *  If you use hasElement heavily then consider an associative container
 */
template <typename Container, typename Key>
bool hasElement(const Container& c, const Key& k) {
      return std::find(c.begin(),c.end(),k) != c.end();
}

//! from \a eraseFrom erase all elements that are also in \a eraseThese
/*!
 * current implementation is only for maps: erase element if both
 * key and element are equal
 */
template <class Map>
 void erase(Map& eraseFrom, const Map& eraseThese) {
     typename Map::const_iterator i;
         for(i= eraseThese.begin(); i!=eraseThese.end(); ++i) {
             typename Map::iterator rF;
             rF = eraseFrom.find(i->first);
             if (rF != eraseFrom.end() && i->second == rF->second)
                 eraseFrom.erase(i->first);
         }
 }

/*!
 * find first element matching key and return value, if not
 * found return defValue, note that findValue returns a copy (NOT const T&)
 */
template <typename K,typename T>
class FindValue {
     T d_defValue;
  public:
   typedef std::map<K,T> Map;
   FindValue(const T& defValue):
     d_defValue(defValue) {};
   T find(const Map& m, const K& key) {
     typename std::map<K,T>::const_iterator i=m.find(key);
     if (i == m.end())
       return d_defValue;
     return i->second;
   }
 };

//! function object that deletes the pointer passed to operator()
/*! 
 *  handy to delete container of pointers (see Effective STL item 7)
 *  \begincode
 *   com::forWhole(container,com::Delete<T>());
 *  \endcode
 */
template <class T>
struct Delete:
  public std::unary_function<const T*,void> {
    void operator()(const T *item) const {
      delete item;
    }
};

//! function object that generates the sequence 0,1,2,3 by default etc.
template <typename T>
class SeqInc {
  T d_next;
  T d_inc;
  public:
    SeqInc(T begin=0, T inc=1): d_next(begin),d_inc(inc){};
    T operator()() {
     T v=d_next;
     d_next+=d_inc;
     return v;
    }
};

//! Sort two values in ascending order.
/*!
  \param   x1 First value to be sorted.
  \param   x2 Second value to be sorted.
  \param   min Lowest of \a x1 and \a x2.
  \param   max Highest of \a x1 and \a x2.
*/
template<class T>
inline void        sort2               (const T &x1,
                                        const T &x2,
                                        T &      min,
                                        T &      max)
{
  if(x2 < x1) {
    min    = x2;
    max    = x1;
  } else {
    min = x1;
    max = x2;
  }
}

//! Sort two values in ascending order.
/*!
  \param   x1 First value to be sorted.
  \param   x2 Second value to be sorted.

  On return x1 is garantueed to be the lowest value
  and x2 the highest.
*/
template<class T>
inline void        sort2               (T &x1,
                                        T &x2)
{
  if(x2 < x1)
    std::swap(x1,x2);
}




//! Returns a random number in the range [0, n).
/*!
  \param     n Upper boundary or range to choose from.
  \return    Random number >= 0 and < \a n.
*/
inline size_t randomNumber(size_t n) {
  return static_cast<size_t>(std::rand() % n);
}



//! Copies a random sample from the input range to the output range.
/*!
  \param     first Start of input range.
  \param     last End of input range.
  \param     outFirst Start of output range.
  \param     outLast End of output range.
  \return    \a outFirst + s with s is min(\a last - \a first, \a outLast - \a outFirst).

  Randomly copies a sample of the elements from the range
  [\a first, \a last) into the range [\a outFirst, \a outLast).  Each element in
  the input range appears at most once in the output range, and samples
  are chosen with uniform probability.  Elements in the output range
  might appear in any order: relative order within the input range is
  not guaranteed to be preserved.
*/
template <class InputIterator, class RandomAccessIterator>
inline RandomAccessIterator randomSample(
                   InputIterator first, InputIterator last,
                   RandomAccessIterator outFirst,
                   RandomAccessIterator outLast)
{
  return randomSample(first, last, outFirst, outLast - outFirst);
}



//! Copies a random sample from the input range to the output range.
/*!
  \param     first Start of input range.
  \param     last End of input range.
  \param     outFirst Start of output range.
  \param     n Size of output range.
  \return    \a outFirst + s with s is min(\a last - \a first, \a n).

  Randomly copies a sample of the elements from the range
  [first, last) into the range [outFirst, outFirst + n).  Each element in
  the input range appears at most once in the output range, and samples
  are chosen with uniform probability.  Elements in the output range
  might appear in any order: relative order within the input range is
  not guaranteed to be preserved.
*/
template <class InputIterator, class RandomAccessIterator, class Distance>
RandomAccessIterator randomSample(InputIterator first, InputIterator last,
                   RandomAccessIterator out, const Distance n)
{
  Distance m = 0;
  Distance t = n;

  for ( ; first != last && m < n; ++m, ++first) {
    out[m] = *first;
  }

  while (first != last) {
    ++t;

    Distance M = randomNumber(t);

    if(M < n) {
      out[M] = *first;
    }

    ++first;
  }

  return out + m;
}



} // namespace com
