#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_CONTCLASSIFIER
#include "com_contclassifier.h"
#define INCLUDED_COM_CONTCLASSIFIER
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif



template <class T>
com_ContClassifier<T>::com_ContClassifier()

  : d_min(static_cast<T>(0)), d_max(static_cast<T>(0)),
    d_minCutOff(d_min), d_maxCutOff(d_max), d_nc(0)

{
}



/*!
  \param min The minimum value.
  \param max The maximum value.
  \param nc The number of classes.

  The minimum cutoff value and the maximum cutoff value will be set to
  \a min and \a max respectively.
*/
template <class T>
com_ContClassifier<T>::com_ContClassifier(T min, T max, size_t nrClasses)

  : d_min(min), d_max(max), d_minCutOff(d_min), d_maxCutOff(d_max),
    d_nc(nrClasses)

{
}



template <class T>
com_ContClassifier<T>::~com_ContClassifier()
{
}



/*!
  \param value The new minimum value.
*/
template <class T>
void com_ContClassifier<T>::setMin(T v)
{
  d_min = v;
}



/*!
  \param value The new maximum value.
*/
template <class T>
void com_ContClassifier<T>::setMax(T v)
{
  d_max = v;
}



/*!
  \param value The new minimum cutoff value.
*/
template <class T>
void com_ContClassifier<T>::setMinCutOff(T v)
{
  d_minCutOff = v;
}



/*!
  \param value The new maximum cutoff value.
*/
template <class T>
void com_ContClassifier<T>::setMaxCutOff(T v)
{
  d_maxCutOff = v;
}



/*!
  \param nrClasses The new number of classes.
*/
template <class T>
void com_ContClassifier<T>::setNrClasses(size_t nc)
{
  d_nc = nc;
}



/*!
  \return The minimum value.
*/
template <class T>
T com_ContClassifier<T>::min() const
{
  return d_min;
}



/*!
  \return The maximum value.
*/
template <class T>
T com_ContClassifier<T>::max() const
{
  return d_max;
}



/*!
  \return The minimum cutoff value.
*/
template <class T>
T com_ContClassifier<T>::minCutOff() const
{
  return d_minCutOff;
}



/*!
  \return The maximum cutoff value.
*/
template <class T>
T com_ContClassifier<T>::maxCutOff() const
{
  return d_maxCutOff;
}



/*!
  \return The number of classes.
*/
template <class T>
size_t com_ContClassifier<T>::nrClasses() const
{
  return d_nc;
}



/*!
  \param  v The value to classify.
  \return The class index of class \a value.

  Returned class indices range from 0 to nr_of_classes - 1.
*/
template <class T>
size_t com_ContClassifier<T>::classIndex(T v) const
{
  //  1. Check if the minimum value is smaller than the maximum value. In that
  //     case we can calculate the class number from the range in datavalues
  //     and the number of classes.
  //  2. Values smaller than the minimum value are assigned the lowest class.
  //     Values on a classborder are considered to be in that class.
  //  3. Values greater than the maximum value are assigned the highest class.
  //  4. In case the minimum value equals the maximum value there's only one
  //     class, which has index 0.

  PRECOND(d_minCutOff <= d_maxCutOff);
  PRECOND(d_nc > 0);

  size_t classIndex;

  if(d_minCutOff < d_maxCutOff)                                            // 1.
  {
    T cw  = classWidth();
    T cb = d_minCutOff + cw;

    for(classIndex = 0; classIndex < d_nc; classIndex++, cb += cw)         // 2.
    {
      if(v <= cb)
        break;
    }

    if(classIndex == d_nc)                                                 // 3.
      classIndex--;
  }
  else                                                                     // 4.
  {
    classIndex = 0;
  }

  return classIndex;
}



/*!
  \return  The classwidth.
  \warning The behaviour is undefined if the number of classes is zero.
*/
template <class T>
T com_ContClassifier<T>::classWidth() const
{
  PRECOND(d_nc > 0);

  return (d_maxCutOff - d_minCutOff) / d_nc;
}



/*!
  \param  i The class index for which the class border needs to be calculated.
  \return The upper class border.

  Class indices are whole numbers. The class index for the first class is 0, but
  it's allowed to use negative values or values greater than nr_of_classes - 1.

  examples:
  \code
    T border

    // Find lower border of first class. Equals minimum value of legend.
    border = legend.classBorder(-1);

    // Find upper border of first class. Equals lower border of second class.
    border = legend.classBorder(0);
  \endcode
*/
template <class T>
T com_ContClassifier<T>::classBorder(size_t i) const
{
  T cw = classWidth();
  return d_minCutOff + (i + 1) * cw;
}



template <class T>
bool com_ContClassifier<T>::equals(const com_ContClassifier &c) const
{
  return ((d_min         == c.min())       &&
          (d_max         == c.max())       &&
          (d_minCutOff   == c.minCutOff()) &&
          (d_maxCutOff   == c.maxCutOff()) &&
          (d_nc          == c.nrClasses()));
}



template <class T>
std::string com_ContClassifier<T>::descr(size_t i) const
{
  return com::doubleToStr(classBorder(i));
}



//------------------------------------------------------------------------------

template <class T>
bool operator==(const com_ContClassifier<T> &lhs,
                const com_ContClassifier<T> &rhs)
{
  return lhs.equals(rhs);
}



template <class T>
bool operator!=(const com_ContClassifier<T> &lhs,
                const com_ContClassifier<T> &rhs)
{
  return !lhs.equals(rhs);
}

