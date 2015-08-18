#include "com_linclassifier.h"
#include <algorithm>
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
com_LinClassifier<T>::com_LinClassifier()

  : com::ClassifierImp<T>()

{
}



template<class T>
com_LinClassifier<T>::~com_LinClassifier()
{
}



/*!
  The algorithm devides the range \a min - \a max in \a n equaly sized classes
  and stores the class borders in \a b:

  \code
  T width = (max - min) / n;         // Calculate class width.

  for(size_t i = 1; i < n; i++)
    b[i - 1] = min + i * width;      // Calculate most class borders.
  b[n - 1] = max;                    // Set last class border to max.
  \endcode
*/
template<class T>
void com_LinClassifier<T>::classify(std::vector<T> &b, T min, T max, size_t n)
{
  assert(min < max);

  n += 1;
  b.resize(n);
  T width = (max - min) / (n - 1);     // Calculate class width.
  for(size_t i = 0; i < n - 1; i++) {
    b[i] = min + i * width;            // Calculate most class borders.
  }
  b[n - 1] = max;                      // Set last class border to max.
}



template<class T>
void com_LinClassifier<T>::autoClassify(std::vector<T> &b, T min, T max,
                                        size_t n)
{
  assert(min < max);

  REAL8 delta;
  REAL8 dec;       // Max power of ten which fits into the interval [min, max].
  REAL8 width;     // Class width.
  REAL8 nmin, nmax;
  size_t nn;

  n += 1;

  delta = max - min;
  dec   = std::pow(10.0, std::floor(std::log10(delta)));

  // Calculate class width such that:
  // - Number of intervals will not exceed the max number as specified.
  // - Class width fits {1, 2, 5}*10^n with a natural number n.
  width  = com_Util::ceil125(delta * 0.999999 / dec / n) * dec;

  // Adjust min and max such that both are integer multiples of the step size.
  nmin  = width * std::floor((min + com::minEps * width) / width);
  nmax  = width * std::ceil ((max - com::minEps * width) / width); 

  nn = dal::round<REAL8, size_t>((nmax - nmin) / width) + 1;
  b.resize(nn);

  for(size_t i = 0; i < nn - 1; i++)
    b[i] = nmin + i * width;
  b[nn - 1] = nmax;
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


