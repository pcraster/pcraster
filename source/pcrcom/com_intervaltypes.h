#ifndef INCLUDED_COM_INTERVALTYPES
#define INCLUDED_COM_INTERVALTYPES



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif


namespace com {
  // Interval declarations.
}



namespace com {


//! limits value to be greater (or equal) than something
template<typename R=double>
class LowerLimit : public Interval<R> {
 protected:
  //! value to compare against
  R d_minV;
  LowerLimit(R minV): d_minV(minV) {}
 public:
   bool    operator<(R /*v*/) const override { return false; }
   R  min() const override { return d_minV;     }
   R  max() const override { return this->maxLimit(); }
};

//! test value to be greater than \a minV, < l , inf
template<typename R=double>
class GreaterThan : public LowerLimit<R> {
  public:
   //! ctor
   GreaterThan(R minV):LowerLimit<R>(minV){}
   bool        valid(R v)const override { return v > this->d_minV; }
   bool    operator>(R v)const override { return this->d_minV >= v;}
   std::string msg() const override;
   GreaterThan *createClone()const override;

};

//! test value to be greater than or equal to \a minV, [ l , inf
template<typename R=double>
class GreaterThanEqualTo : public LowerLimit<R> {
  public:
   //! ctor
   GreaterThanEqualTo(R minV):LowerLimit<R>(minV){}
   bool valid(R v)        const override { return v >= this->d_minV; }
   bool    operator>(R v) const override { return this->d_minV >  v;}
   std::string msg() const override;
   GreaterThanEqualTo *createClone()const override;
};


//! limits value to be less (or equal) than \a d_maxV, inf, h ]&gt;
template<typename R=double>
class UpperLimit : public Interval<R> {
protected:
  //! value to compare against
  R d_maxV;
  UpperLimit(R maxV): d_maxV(maxV) {}
public:
   bool  operator>(R /*v*/) const override { return false;            }
   R  min() const override { return this->minLimit(); }
   R  max() const override { return d_maxV;     }
};

//! test value to be less than \a maxV, &lt; inf, h &gt;
template<typename R=double>
class LessThan : public UpperLimit<R> {
  public:
   //! ctor
   LessThan(R maxV):UpperLimit<R>(maxV){}
   bool valid(R v)const override       { return v < this->d_maxV;   }
   bool  operator<(R v) const override { return this->d_maxV <=  v; }
   std::string msg() const override;
   LessThan *createClone()const override;
};

//! test value to be less than or equal to \a maxV, &lt; inf, h ]
template<typename R=double>
class LessThanEqualTo : public UpperLimit<R> {
  public:
   //! ctor
   LessThanEqualTo(R maxV):UpperLimit<R>(maxV){}
   bool valid(R v)const override       { return v <= this->d_maxV; }
   bool  operator<(R v) const override { return this->d_maxV <  v; }
   std::string msg() const override;
   LessThanEqualTo *createClone()const override;
};


//! test value to be a certain value
template<typename R=double>
class AnythingInterval : public Interval<R> {
  public:
   //! ctor
   AnythingInterval(){}
   bool valid     (R /* v */) const override { return true; }
   bool  operator<(R /* v */) const override { return false;}
   bool  operator>(R /* v */) const override { return false;}
   std::string msg() const override;
   AnythingInterval *createClone()const override;
   R  min() const override { return this->minLimit(); }
   R  max() const override { return this->maxLimit(); }
};

//! defines open or closed interval
template<typename R=double>
class BetweenLimits : public Interval<R> {
  typedef  Interval<R>  IV;
  //! the lower limit
  /*!
   * just use Interval as long as ctor argument
   * is LowerLimit, make life easier
   */
  const IV*  d_lowerLimit;
  //! the upper limit
  /*!
   * d_lowerLimit remarks apply here also
   */
  const IV*  d_upperLimit;

  void             clean();
 public:
  BetweenLimits(
   const LowerLimit<R>&  lowerLimit,
   const UpperLimit<R>&  upperLimit);
  ~BetweenLimits() override;

  BetweenLimits&   operator=           (const BetweenLimits<R>& rhs);

                   BetweenLimits       (const BetweenLimits<R>& rhs);

  bool valid(R v) const override;
  bool  operator<(R v) const override;
  bool  operator>(R v) const override;
  std::string msg() const override;
  BetweenLimits<R> *createClone()const override;
  R  min() const override { return d_lowerLimit->min(); }
  R  max() const override { return d_upperLimit->max(); }
};


//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace com

#endif
