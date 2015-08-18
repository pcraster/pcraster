#ifndef INCLUDED_COM_INTERVAL
#define INCLUDED_COM_INTERVAL

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.



namespace com {
  // Interval declarations.
}

/*!
  \file
  This file contains the interface for Interval class.
  file com_intervaltypes.h defines the derived concrete classes
*/


namespace com {

//! test value against an interval definition
/*!
   Derived classes implements the interval desired.
   Derived class name indicate test of one interval side.
   EqualTo: Interval is a single value.


   \warning
     It is common to use a double as template argument to store both float and all
     integer types, since double can hold 32 bit integers exact. some float's however
     lose their exact representation if upcasted to a double. As long as float values
     are also used to construct the interval there is no problem. Constructing from
     doubles (or constants) will yield this problem. see com::IntervalTest::testRoundError()
     for a example of the problem.

   \todo
     see warning, be able to parse for float while retaining double intervals.
     but also let large integers be represented correct
     see testRoundError()

   \todo
     make validators work for integers only as well, calcui needs this
 */
template <typename R=double>
class Interval
{

private:

  //  Assignment operator. DEFAULT
  // Interval&   operator=  (const Interval&);

  //  Copy constructor. DEFAULT
  //               Interval               (const Interval&);

protected:
                Interval               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


  virtual          ~Interval             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  //! returns true when class name test holds
  virtual bool valid(R v)const=0;
  //! valid() as unary predicate
  bool    operator()(R v)const {
    return valid(v);
  }
  bool    less(const Interval& rhs)const;

  bool    operator<(const Interval& rhs)const {
    return this->less(rhs);
  }
  bool    operator==(const Interval& rhs)const;
  bool    operator!=(const Interval& rhs)const {
    return ! this->operator==(rhs);
  }

  //! this is a single valued interval
  bool    equalTo() const {
    return min()==max();
  }

  //! is the largest value represented in the interval <b>less than</b> \a v?
  virtual bool    operator<(R v)const=0;
  //! is the smallest value represented in the interval <b>greater than</b> \a v?
  virtual bool    operator>(R v)const=0;

  //! maximum boundary value represented for interval
  /*!
   *  \sa min() for docs
   */
  virtual R  max()const=0;
  //! minimum boundary value represented for interval
  /*!
   *  a boundary value is part of the interval, except if
   *  the interval models an exclusive limit.
   *  For example GreaterThanEqualTo(0).min() will return 0
   *  and 0 is part of the interval.
   *  GreaterThan(0).min() will return 0
   *  but 0 is not part of the interval. Use
   *  Interval::valid(Interval::min()) for inclusive or
   *  exclusive:
   *  \code
   *     GreaterThanEqualTo ge(0);
   *     assert( ge.valid(ge.min()));
   *
   *     GreaterThan gt(0);
   *     assert(!gt.valid(gt.min()));
   *  \endcode
   *  For example GreaterThan(0).min() will return 0
   *  \returns
   *     the  minimum boundary value if defined,
   *     minLimit() otherwise.
   */
  virtual R  min()const=0;

  //! return centre of range
  virtual R  centre()const;

  //! text version of class name test
  /*!
      for example GreaterThanEqualTo(0):
           "greater than or equal to  0 (<=0)"
   */
  virtual std::string msg()const=0;

  virtual Interval *createClone()const=0;

  void    print(std::ostream& stream) const;

  static R minLimit();
  static R maxLimit();

};

// EqualTo is in interface -> see com_intervalmap.h

//! test value to be a certain value
template<typename R=double>
class EqualTo : public Interval<R> {
  //! value to compare against
  R d_thisValue;
  public:
   //! ctor
   EqualTo(R thisValue):d_thisValue(thisValue){}
   bool valid(R v)const       { return d_thisValue == v; };
   bool  operator<(R v) const { return d_thisValue <  v;};
   bool  operator>(R v) const { return d_thisValue >  v;};
   std::string msg() const;
   virtual EqualTo *createClone()const;
   R  min() const { return d_thisValue; };
   R  max() const { return d_thisValue; };
   R  centre()const{ return d_thisValue; };
};

//! may  be thrown when constructing an Interval
class BadIntervalFormat : public Exception {
 public:
   BadIntervalFormat(const std::string& m):
     Exception(m) {};
};

//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

template<typename R>
 std::ostream&      operator<<          (std::ostream& stream,
                                        const Interval<R>& i);


//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------
template<typename R>
  Interval<R> * createClone(Interval<R> const* c);
template<typename R>
  void deleteClone(Interval<R> const* c);

template<typename R>
Interval<R> * createIntervalFromLookupTableKey (const std::string& str);

//! instantiated in com_interval.cc
typedef Interval<double> IntervalD;
//! instantiated in com_interval.cc
typedef Interval<float>  IntervalF;

} // namespace com

#endif
