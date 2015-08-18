#include "com_classclassifier.h"
#include <algorithm>
#include <set>
#include <vector>
#include "dev_ToString.h"
#include "com_legendclass.h"



//------------------------------------------------------------------------------

template<class T>
com_ClassClassifier<T>::com_ClassClassifier()
{
}



template<class T>
com_ClassClassifier<T>::~com_ClassClassifier()
{
}



template<class T>
void com_ClassClassifier<T>::clearClasses()
{
  d_classes.erase(d_classes.begin(), d_classes.end());
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
template<class T>
void com_ClassClassifier<T>::setClasses(int first, int last)
{
  assert(last >= first);

  d_classes.resize((last - first) + 1);

  for(size_t i = 0; i <= d_classes.size() - 1; ++i) {
    d_classes[i].setValue(static_cast<T>(first + i));
    d_classes[i].setDescr(dev::toString<T>(d_classes[i].value()));
    // d_classes[i].setDescr(com::doubleToStr(d_classes[i].value()));
  }
}



/*!
  \param c The classes to copy.
*/
template<class T>
void com_ClassClassifier<T>::setClasses(const std::vector<T> &c)
{
  //  1. Resize the d_classes datastructure.
  //  2. Copy the class numbers.
  //  3. Fill the description fields.

  d_classes.resize(c.size());                                              // 1.

  for(size_t i = 0; i < c.size(); i++)
  {
    d_classes[i].setValue(c[i]);                                           // 2.
    d_classes[i].setDescr(dev::toString<T>(c[i]));
    // d_classes[i].setDescr(com::doubleToStr(c[i]));                         // 3.
  }
}



/*!
  \param c The classes to copy.
*/
template<class T>
void com_ClassClassifier<T>::setClasses(const std::set<T> &c)
{
  //  1. Resize the d_classes datastructure.
  //  2. Copy the class numbers.
  //  3. Fill the description fields.

  d_classes.resize(c.size());                                              // 1.

  size_t i = 0;
  for(typename std::set<T>::const_iterator it = c.begin(); it != c.end();
         it++, i++)
  {
    d_classes[i].setValue(*it);                                            // 2.
    d_classes[i].setDescr(dev::toString<T>(*it));
    // d_classes[i].setDescr(com::doubleToStr(*it));                          // 3.
  }
}



/*!
  \param c The classes to copy.
  \param n The number of classes to copy.
*/
template<class T>
void com_ClassClassifier<T>::setClasses(const T *c, size_t n)
{
  //  2. Remove previously assigned class numbers.
  //  3. Resize the vector for storing the class numbers.
  //  4. Copy the class numbers.
  //  5. Fill the description fields.

  clearClasses();                                                          // 2.
  d_classes.resize(n);                                                     // 3.

  for(size_t i = 0; i < n; i++)
  {
    d_classes[i].setValue(c[i]);                                           // 4.
    d_classes[i].setDescr(dev::toString<T>(c[i]));
    // d_classes[i].setDescr(com::doubleToStr(c[i]));                         // 5.
  }
}



template<class T>
void com_ClassClassifier<T>::setClasses(
         T const* values,
         std::string const* labels,
         size_t size)
{
  clearClasses();
  d_classes.resize(size);

  for(size_t i = 0; i < size; i++) {
    d_classes[i].setValue(values[i]);
    d_classes[i].setDescr(labels[i]);
  }
}



template<class T>
void com_ClassClassifier<T>::setClasses(const std::vector<com_LegendClass<T> >
                                                                             &c)
{
  d_classes = c;
}



/*!
  \return The number of classes.
*/
template<class T>
size_t com_ClassClassifier<T>::nrClasses() const
{
  return d_classes.size();
}



/*!
  \param  v The value for which the class index needs to be calculated.
  \return The class index of class \a value.
*/
template<class T>
size_t com_ClassClassifier<T>::index(T v) const
{
//   // Doesn't work with msvs-8.0
//   // Find first occurance of v in d_classes.
//   const_iterator it = std::lower_bound(d_classes.begin(), d_classes.end(), v,
//                                        ::compClass<T>());
// 
// #ifdef DEBUG_DEVELOP
//   assert(it != d_classes.end());
// #endif
// 
//   return it - d_classes.begin();

  size_t i = 0;

  while(i < nrClasses() && d_classes[i].value() != v) {
    i++;
  }

#ifdef DEBUG_DEVELOP
  assert(d_classes[i].value() == v);
#endif

  return i;
}



/*!
  \return The classnumbers.
*/
template<class T>
std::vector<T> com_ClassClassifier<T>::classNumbers() const
{
  std::vector<T> c(nrClasses());

  for(size_t i = 0; i < nrClasses(); i++)
    c[i] = d_classes[i].value();

  return c;
}



template<class T>
const std::vector<com_LegendClass<T> > &com_ClassClassifier<T>::classes() const
{
  return d_classes;
}



template<class T>
T com_ClassClassifier<T>::value(size_t i) const
{
  assert(nrClasses());
  assert(i < nrClasses());

  return d_classes[i].value();
}



template<class T>
std::string com_ClassClassifier<T>::descr(size_t i) const
{
  assert(nrClasses());
  assert(i < nrClasses());

  return d_classes[i].descr();
}



template<class T>
typename com_ClassClassifier<T>::const_iterator
         com_ClassClassifier<T>::begin() const
{
  return d_classes.begin();
}



template<class T>
typename com_ClassClassifier<T>::const_iterator
         com_ClassClassifier<T>::end() const
{
  return d_classes.end();
}


