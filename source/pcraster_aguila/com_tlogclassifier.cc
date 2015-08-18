#include "com_tlogclassifier.h"
#include <cmath>
#include <stdexcept>
#include <vector>
#include "com_classifierimp.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

template<class T>
com_TLogClassifier<T>::com_TLogClassifier()

  : com::ClassifierImp<T>()

{
}



template<class T>
com_TLogClassifier<T>::~com_TLogClassifier()
{
}



/*!
  \exception std::range_error min == max (in which case log10 is not defined).
*/
template<class T>
void com_TLogClassifier<T>::classify(std::vector<T> &b, T min, T max, size_t n)
{
  assert(min < max);
  assert(n > 0);

  if(min == max)
    throw std::range_error("log10 of value == 0 is undefined");

  n += 1;
  b.resize(n);

  // Translate maximum value.
  // َ+ 1.0 to keep the value > 1.0
  double tmax  = static_cast<double>(max) - min + 1.0;

  double lmax  = std::log10(tmax);
  assert(lmax > 0.0);
  double width = lmax / (n - 1);
  assert(width > 0.0);

  b[0] = min;                          // Set the first class border to min.
  for(size_t i = 1; i < n - 1; i++)  // Calculate all but the last class border.
    b[i] = min + static_cast<T>(std::pow(10.0, i * width)) - 1.0;
  b[n - 1] = max;                      // Set the last class border to max.
}



template<class T>
void com_TLogClassifier<T>::autoClassify(std::vector<T> &b, T min, T max,
                                         size_t n)
{
  classify(b, min, max, n);
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


