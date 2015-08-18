#include "com_logclassifier.h"
#include <stdexcept>
#include <vector>
#include "com_classifierimp.h"
#include "com_const.h"
#include "com_util.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

template<class T>
com_LogClassifier<T>::com_LogClassifier()

  : com::ClassifierImp<T>()

{
}



template<class T>
com_LogClassifier<T>::~com_LogClassifier()
{
}



/*!
  \exception std::range_error min <= 0 (in which case log10 is not defined).
*/
template<class T>
void com_LogClassifier<T>::classify(std::vector<T> &b, T min, T max, size_t n)
{
  assert(min < max);

  if(min <= 0)
    throw std::range_error("log10 of value <= 0 is undefined");

  n += 1;
  b.resize(n);
  double lmin  = std::log10(static_cast<double>(min));
  double lmax  = std::log10(static_cast<double>(max));
  double width = (lmax - lmin) / (n - 1);

  for(size_t i = 0; i < n - 1; i++)  // Calculate all but the last class border.
    b[i] = static_cast<T>(std::pow(10.0, lmin + i * width));
  b[n - 1] = max;                      // Set the last class border to \a max.
}



template<class T>
void com_LogClassifier<T>::autoClassify(std::vector<T> &b, T min, T max,
                                        size_t n)
{
  assert(min < max);

  if(min <= 0)
    throw std::range_error("log10 of value <= 0 is undefined");

  n += 1;

  // Calculate class width.
  T lmin  = static_cast<T>(std::log10(static_cast<double>(min)));
  T lmax  = static_cast<T>(std::log10(static_cast<double>(max)));
  T width = static_cast<T>(com_Util::ceil125((lmax - lmin) * 0.999999 / n));

  // Calculate new minimum, maximum, number of classes.
  T nmin, nmax;
  size_t nn;

  nmin = static_cast<T>(std::ceil(
                          (lmin - com_Const::StepEps * width) / width) * width);
  if(nmin > lmin) nmin -= width;
  nmax = static_cast<T>(std::floor(
                          (lmax + com_Const::StepEps * width) / width) * width);
  if(nmax < lmax) nmax += width;
  nn = dal::round<T, size_t>((nmax - nmin) / width) + 1;

  b.resize(nn);

  for(size_t i = 0; i < nn - 1; i++) // Calculate all but the last class border.
    b[i] = static_cast<T>(std::pow(10.0, nmin + i * width));
  b[nn - 1] = static_cast<T>(std::pow(10.0, nmax)); // Set last border to max.
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


