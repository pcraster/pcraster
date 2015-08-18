#include "com_util.h"
#include <cassert>
#include <cmath>
#include <iterator>
#include <vector>
#include "com_const.h"



/*!
  \param   x
  \return  The smallest value.
*/
double com_Util::ceil125(double x)
{
  double lx, rv;
  double p10, fr;
  double sign = ( x > 0) ? 1.0 : -1.0;

  if (x == 0.0) return 0.0;

  lx  = std::log10(std::fabs(x));
  p10 = std::floor(lx);
  fr  = std::pow(10.0, lx - p10);

  if      (fr <= 1.0) fr = 1.0;
  else if (fr <= 2.0) fr = 2.0;
  else if (fr <= 5.0) fr = 5.0;
  else                fr = 10.0;

  rv = fr * std::pow(10.0, p10);
  return sign * rv;
}



void com_Util::linSpace(std::vector<double>::iterator begin,
                        std::vector<double>::iterator end,
                        double min, double max)
{
  assert(end > begin);
  assert(max >= min);

  size_t imax(0);                  // Number of steps minus 1.
  double step;                     // Size of a step.
  std::vector<double>::iterator v; // Iterator to a value.

#ifdef BORLANDC
  // Borland only supports the old, deprecated, interface which will be removed
  // from the standard.
  std::distance(begin, end, imax);
  imax -= 1;
#else
  imax = std::distance(begin, end) - 1;
#endif

  if(imax > 0)     // More than one value to fill: step is important.
    step = (max - min) / static_cast<double>(imax);
  else
    step = 0;      // Only one value to fill (the first): step is not important.

/*
  cout << "distance : " << imax << endl;
  cout << "step     : " << step << endl;
*/

  size_t i;
  for(v = begin, i = 0; v != end; v++, i++)
  {
    *v = min + static_cast<double>(i) * step;
  }
}



void com_Util::logSpace(std::vector<double>::iterator begin,
                        std::vector<double>::iterator end,
                        double min, double max)
{
  size_t imax(0);             // Number of steps minus 1.
  double lmin;
  double lmax;
  double lstep;
  std::vector<double>::iterator v; // Iterator to a value.

#ifdef BORLANDC
  // Borland only supports the old, deprecated, interface which will be removed
  // from the standard.
  std::distance(begin, end, imax);
  imax -= 1;
#else
  imax = std::distance(begin, end) - 1;
#endif

  if((min <= 0.0) || (max <= 0.0) || (imax <= 0)) return;

  *begin     = min;
  *(end - 1) = max;
  lmin = std::log(min);
  lmax = std::log(max);
  lstep = (lmax - lmin) / static_cast<double>(imax);

  size_t i;
  for(v = begin + 1, i = 1; v != end - 1; v++, i++)
  {
    *v = std::exp(lmin + static_cast<double>(i) * lstep);
  }
}



// /*!
//   \param   array Double array.
//   \param   size Size of \a array.
//   \return  0 if the sequence is not strictly monotonic, 1 if the sequence
//            is strictly monotonically increasing, -1 if the sequence is
//            strictly monotonically decreasing.
// */
// int com_Util::monotonic(double *array, int size)
// {
//   int rv, i;
//   if(size < 2) return 0;
// 
//   rv = com::sign(array[1] - array[0]);
// 
//   for (i = 1; i < size - 1; i++)
//   {
//     if(com::sign(array[i+1] - array[i]) != rv )
//     {
//       rv = 0;
//       break;
//     }
//   }
// 
//   return rv;
// }



/*!
  \param   value Value to be scaled.
  \param   percentage Percentage!
  \return  Value scaled by \a percentage percent.

  \a percentage can be both negative and positive. A negative \a percentage will
  shrink \a value and vice versa. If \a percentage is 0 than \a percentage will
  be returned.
*/
double com_Util::scale(double value, double percentage)
{
  return (100.0 + percentage) * value / 100.0;
}



//! Returns the smallest number with which \a number can be int divided.
/*!
  \param     number Number to divide.
  \param     start Start number to divide with.
  \return    Smallest divisor.

  The result is undefined if \a number == 0 or if \a start > \a number or if
  \a start == 0.
*/
size_t com::smallestDivisor(size_t number, size_t start)
{
  assert(number > 0);
  assert(start <= number);
  assert(start > 0);

  for(size_t n = start; n < number; ++n) {
    if(number % n == 0) {
      return n;
    }
  }
  return number;
}



//! Returns the largest number with which \a number can be int divided.
/*!
  \param     number Number to divide.
  \param     start Start number to divide with.
  \return    Largest divisor.

  The result is undefined if \a number == 0 or if \a start > \a number or if
  \a start == 0.
*/
size_t com::largestDivisor(size_t number, size_t start)
{
  assert(number > 0);
  assert(start <= number);
  assert(start > 0);

  for(size_t n = start; n > 1; --n) {
    if(number % n == 0) {
      return n;
    }
  }
  return 1;
}

