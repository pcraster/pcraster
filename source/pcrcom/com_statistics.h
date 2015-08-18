#ifndef INCLUDED_COM_STATISTICS
#define INCLUDED_COM_STATISTICS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif



namespace com {
  // Statistics declarations.
}

/*!
  \file
  This file contains a number of small statistical function objects
  and algorithms.
*/

namespace com {

//! Sums all elements.
/*!
  \param  valueT  type of all elements to sum
  \param  sumT    type that can hold the sum
  \sa     std::accumulate
  \code
    int values[] = { 1, 2, 3, 4, 5 };
    int nr = 5;

    com::Sum<int> sum = std::for_each(value, value + nr, Sum<int>());

    std::cout << sum.value() << std::endl;       // Will print '15'.
  \endcode
*/

template<class valueT, class sumT=valueT>
struct Sum: public std::unary_function<valueT, void> {

  //! Running sum.
  sumT d_sum;

  //! Constructor.
  /*!
    \param     v Value to start with. The default is zero.
  */
  Sum(sumT v = sumT()) : d_sum(v) { }

  //! Function operator.
  /*!
    \param     value Value to add to sum.
  */
  void operator()(valueT value) {
    d_sum += value;
  }

  //! Returns the sum.
  /*!
    \returns   Sum.
  */
  sumT sum() const {
    return d_sum;
  }

                   operator sumT    () const
  {
    return sum();
  }
};

template<class valueT=double, class sumT=long double>
class SumNr: public Sum<valueT,sumT> {

private:
  size_t      d_nr;
public:
                   SumNr              (): d_nr(0) {};
  /* virtual */   ~SumNr              () {};

  void operator()(valueT value) {
    Sum<valueT,sumT>::operator()(value);
    d_nr++;
  }

  size_t nr() const {
    return d_nr;
  }
};

template<class valueT=double, class sumT=long double>
class Average : public SumNr<valueT,sumT> {
public:
  /*!
   *  \todo
   *    throw range error?
   */
  valueT average() const {
    PRECOND(this->nr());
    return (valueT)(this->sum()/this->nr());
  }

  valueT average(valueT returnWhenEmpty) const {
    if (!this->nr())
      return returnWhenEmpty;
    return average();
  }

  operator valueT  () const
  {
    return average();
  }

};

//! calculate average, minimum and maximum
/*!
 * \todo
 *   implement a policy class to init d_min and d_max at
 *   creation or as done now, when nr()==0
 */
template<class valueT=double, class sumT=long double>
class AverageMinMax : public Average<valueT,sumT> {
    valueT d_min, d_max;
public:
   void operator()(valueT value) {
     if (!this->nr()) // first one
       d_min=d_max=value;
     else {
       d_min=std::min(d_min,value);
       d_max=std::max(d_max,value);
     }
     Average<valueT,sumT>::operator()(value);
  }

   valueT minimum() const {
    PRECOND(this->nr());
    return d_min;
   }
   valueT maximum() const {
    PRECOND(this->nr());
    return d_max;
   }
   valueT middle() const {
     return (minimum()+maximum())/2;
   }
   valueT range() const {
     return maximum()-minimum();
   }
};

//! Function object for the calculation of the variance.
/*!
  This algorithm is taken from Press et al (Numerical Recipes in C, 2nd)
  and calculates the variance utilising the two-pass algorithm: it needs
  the mean to start off for calculating differences. It encorporates a way to
  minimize roundoff errors.

  s^2 = (sumOfSquaredDifferences - (sumOfDifferences^2 / N)) / (N - 1)
 
  \todo rename to Variance2Pass
*/
template<class ValueType = double, class SumType = long double>
class Variance1 {

private:

  //! Number of values.
  size_t           d_nr;

  //! Mean.
  ValueType        d_mean;

  //! Sum of differences from the mean.
  SumType          d_sumOfDifferences;

  //! Sum of squared differences from the mean.
  SumType          d_sumOfSquaredDifferences;

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
  /*!
  */
                   Variance1(ValueType mean)
    : d_nr(), d_mean(mean), d_sumOfDifferences(), d_sumOfSquaredDifferences()
  {
  }

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             operator()          (ValueType value)
  {
    ++d_nr;
    ValueType difference = value - d_mean;
    d_sumOfDifferences += difference;
    d_sumOfSquaredDifferences += (difference * difference);
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the variance (implicit type conversion).
  /*!
    \return    Variance.

    This function returns ValueType() if operator()(ValueType) has been called
    on an empty container.
  */
                   operator ValueType  () const
  {
    return d_nr > 0
      ? static_cast<ValueType>(
        (d_sumOfSquaredDifferences -
                   d_sumOfDifferences * d_sumOfDifferences / d_nr) /
                   (d_nr - 1))
      : ValueType();
  }

};



//! Function object for the calculation of the variance.
/*!
  This algorithm is taken from John C. Davis (Statistics and data analysis in
  geology, second edition) and calculates an unbiased value for the variance
  using the folowing formula:

  s^2 = (n * sumOfSquaredValues - sumOfValues^2) / (n * (n - 1))

  This algorithm doesn't need the mean to calculate the variance.

  \todo rename to Variance1Pass

*/
template<class ValueType = double, class SumType = long double>
class Variance2 {

private:

  //! Number of values.
  size_t           d_nr;

  //! Sum of values.
  SumType          d_sumOfValues;

  //! Sum of squared values.
  SumType          d_sumOfSquaredValues;

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
  /*!
  */
                   Variance2()
    : d_nr(), d_sumOfValues(), d_sumOfSquaredValues()
  {
  }

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             operator()          (ValueType value)
  {
    ++d_nr;
    d_sumOfValues += value;
    d_sumOfSquaredValues += value * value;
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the variance (implicit type conversion).
  /*!
    \return    Variance.

    This function returns ValueType() if operator()(ValueType) has been called
    on a container with size less than 2.
  */
  operator ValueType  () const
  {
    if (d_nr < 2)
      return ValueType();
    return static_cast<ValueType>(
         (d_nr * d_sumOfSquaredValues - d_sumOfValues * d_sumOfValues) /
         (d_nr * (d_nr - 1)));
  }
};



//! Function object for the calculation of the standard deviation.
/*!
  \sa Variance1, Variance2

  This object can be configured to the algorithm to calculate the variance.
  Default is Variance2 algorithm which doesn't need the mean to start with.
*/
template<class ValueType = double, class SumType = long double,
         class Variance = Variance2<ValueType, SumType> >
class StandardDeviation {

private:

  //! Function object to calculate variance.
  Variance         d_variance;

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
  /*!
  */
                   StandardDeviation(Variance variance = Variance())
    : d_variance(variance)
  {
  }

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             operator()          (ValueType value)
  {
    d_variance.operator()(value);
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the standard deviation (implicit type conversion).
  /*!
    \return    Standard deviation.

    This function returns std::sqrt(ValueType()) if operator()(ValueType) has
    been called on an empty container.
  */
                   operator ValueType  () const
  {
    return static_cast<ValueType>(std::sqrt(static_cast<double>(
                   d_variance.operator ValueType())));
  }

};

template<class valueT=double, class sumT=long double>
class AverageSdMinMax: public AverageMinMax<valueT,sumT> {
  StandardDeviation<valueT,sumT> d_sd;
 public:
   void operator()(valueT value) {
     AverageMinMax<valueT,sumT>::operator()(value);
     d_sd(value);
   }
  valueT sd() const {
    if (!this->nr() || 
        this->minimum()==this->maximum() // avoid numeric instabs close to 0
       ) return 0;
    return d_sd;
  }
};

/*!
 * \brief sort range [\a begin, \a end) as such that percentile \a p is returned
 *
 * operator < is used as sorting criteria on *Iter
 *
 * changes element order since std::nth_element is used. A value of 1 returns the last
 * element, and not past-the-end.
 * \returns
 *    \a end if \a p not in [0,1]
 */
template <class Iter>
 Iter percentile(Iter begin, Iter end, double p)
 {
   size_t n=std::distance(begin,end);
   if (com::lim(p,0.0,1.0)!=p||!n)
     return end;
   n=static_cast<size_t>(p*n);
   // p==1 might trigger this
   if (begin+n==end)
     n--; // last (max) element, not past-the-end
   std::nth_element(begin,begin+n,end);
   return begin+n;
 }

/*!
 * predicate version of com::percentile
 * \todo
 *  refactor common code of both percentile algorithms
 */
template <class Iter, class BinaryPredicate>
 Iter percentile(Iter begin, Iter end, double p, BinaryPredicate op)
 {
   size_t n=std::distance(begin,end);
   if (com::lim(p,0.0,1.0)!=p||!n)
     return end;
   n=static_cast<size_t>(p*n);
   // p==1 might trigger this
   if (begin+n==end)
     n--; // last (max) element, not past-the-end
   std::nth_element(begin,begin+n,end,op);
   return begin+n;
 }

/*!
 * \brief sort range [\a begin, \a end) and return median (DRAFT!)
 *
 * \todo it does not average on even-numbered set, what is the common approach!
 */
template <class Iter>
 Iter median(Iter begin, Iter end)
 {
  return percentile(begin, end, 0.5);
 }

template <class Iter, class BinaryPredicate>
 Iter median(Iter begin, Iter end, BinaryPredicate op)
 {
  return percentile(begin, end, 0.5, op);
 }





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
