#ifndef INCLUDED_CALC_FOPOINTSPECIAL
#define INCLUDED_CALC_FOPOINTSPECIAL

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MATHX
#include "mathx.h"
#define INCLUDED_MATHX
#endif

#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif

#ifndef INCLUDED_API
#include "api.h" // Side
#define INCLUDED_API
#endif

/* special code, that is "too long" to put in operation.xml 
 * or specific classes such as aggregate
 */

namespace calc {
struct special {
  template<typename T>
   inline static bool fac_domainIll(const T& vIn) {
    // up to 34 is ok, 35 => infinite (on float)
    T v = std::floor(vIn);
    return v < 1 || v > 34 || v  != vIn;
  }

  template<typename T>
   inline static T fac_impl(const T& vIn) {
    T v = std::floor(vIn);
    T r = 1,n = 1;
    while (n <= v) {
        r *= n;
        n += 1;
    }
    return r;
  }
  template<typename T>
   inline static bool pow_domainIll(const T& l,const T& r) {
    return (l == 0 && r <= 0) ||
          (l < 0  && std::floor(r) != r);
   }
   inline static REAL4 impl_l_2_d( const UINT1& v) {
    const double ldd2Dir[10] = { 0, 0.625, 0.500, 0.375,
                                 0.750, -1   , 0.250,
                                 0.875, 0    , 0.125 };
    if (v == LDD_PIT) 
     return -1;
    PRECOND(v > 0 && v < ((UINT1)10) );
    return (REAL4)(ldd2Dir[v] * M_2PI);
  }
   inline static REAL4 impl_4_2_d( const UINT4& v) {
     double (* f)(double x)=
     (appDirection == APP_RADIANS) ? ScaleRad : Deg2Rad;
     return (REAL4)f(v);
  }
   inline static REAL4 impl_s_2_d(const REAL4& v) {
     double (* f)(double x)=
     (appDirection == APP_RADIANS) ? ScaleRad : Deg2Rad;
     return (REAL4)f(v);
  }
   inline static REAL4 impl_1_2_d( const UINT1& v) {
     return impl_4_2_d(v);
  }
   inline static UINT1 impl_4_2_l( const INT4& v) {
      UINT1   r = (UINT1)(ABS(v) % 10);
      if (!r) return 5; // pit who cares
      return r;// MISSING CODE: DO A LDD REPAIR
  }
   inline static UINT1 impl_s_2_l( const REAL4& v) {
      return impl_4_2_l((INT4)v);
  }
  inline static UINT1 impl_d_2_l( const REAL4& v) {
     const UINT1 lookup[8] = { 8, 9, 6, 3, 2, 1, 4, 7 }; 
     double dum,shift = 1.0/16.0; /* ((pi/8)/2pi) = 1/16 */
     if (v == -1)
       return LDD_PIT;
     /* shift a halfdir - eps. (0.0624999) */
     UINT1 r =(UINT1)( modf( (v/M_2PI)+shift, &dum)*8);
     POSTCOND(r < ((UINT1)8) );
     // MISSING CODE: DO A REPAIR
     return lookup[r];
   }
   inline static REAL4 impl_downstreamdist( const UINT1& v) {
      static double d[2] = {0,0};
      if (d[0]==0) {
        d[0] = Side();
        d[1] = d[0]*std::sqrt(2.0);
      }
      if (v==LDD_PIT) return 0;
       return (REAL4)d[((int)v)%2];
    }
};



template<typename A,
        typename I=A> /* REAL4 */
  struct MapTotal {
     typedef A AggregateType;
     typedef I Input;
     static A init() { return A(); }
     static void op(A &r, const I& v) {
       r+=v;
     }
  };

template<typename A,
        typename I=A> /* UINT1 */
  struct MapAnd {
     typedef A AggregateType;
     typedef I Input;
     static A init() { return A(1); /* true */ }
     static void op(A &r, const I& v) {
       r= r && v;
     }
  };

template<typename A,
        typename I=A> /* UINT1 */
  struct MapOr {
     typedef A AggregateType;
     typedef I Input;
     static A init() { return A(1); /* true */ }
     static void op(A &r, const I& v) {
       r= r || v;
     }
  };


template<typename A /* INT4/REAL4 */,
        typename I=A>
  struct MapMinimum {
     typedef A AggregateType;
     typedef I Input;
     static A init()
       { return com::NumericLimits<A>::maxValue(); }
     static void op(A &r, const I& v) {
       r=std::min<>(r,v);
     }
  };

template<typename A /* INT4/REAL4 */,
        typename I=A>
  struct MapMaximum {
     typedef A AggregateType;
     typedef I Input;
     static A init()
       { return com::NumericLimits<A>::minValue(); }
     static void op(A &r, const I& v) {
       r=std::max<>(r,v);
     }
  };

template<typename A /* UINT1/INT4/REAL4 */,
        typename I=A>
  struct MapArea {
     typedef A AggregateType;
     typedef I Input;
     static A init() { return A(); }
     static void op(A &r, const I& /* v */) {
       r+=(A)Area();
     }
  };


/*!
   <H2>doc on functions</H2>

   trigonometric and other mathematical functions :
     Function     Argument Range     Return Value Range

     acos         -1 to 1            0 to pi
     asin         -1 to 1            -pi/2 to pi/2
     atan         No limit           -pi/2 to pi/2

     cos, sin, and tan trigonometric functions return the cosine,
     sine, and tangent, respectively, of <x>:

     If <x> is large, a partial loss of significance in the result may
     occur in a trigonometric function. In this case, the function
     generates a PLOSS error. If <x> is so large that significance is
     completely lost, the function prints a TLOSS message to stderr and
     returns 0. In both cases, errno is set to ERANGE.

     The exp function returns the exponential function of its floating-
     point argument <x>.  _LHUGE_VAL for expl) on overflow and set errno
     to ERANGE; on underflow, they return 0 but do not set errno.

     The log and log10 functions calculate the natural logarithm and
     base-10 logarithm of <x>, respectively.
     These functions return the logarithm result. If <x> is negative,
     the functions print a DOMAIN error message to stderr, return the
     value -HUGE_VAL (or -_LHUGE_VAL for the long double functions),
     and set errno to EDOM. If <x> is 0, the functions print a SING
     error message to stderr, return the value -HUGE_VAL, and set errno
     to ERANGE.


 Power x**y
 The pow function return the value of <x> raised to the
 power of <y>. The result varies, depending on <x> and <y>:
                               case
 (x == 0.0 && y <= 0.0)        1, 2a
 (x < 0.0  && floor(y) != y)   2b
case:
   1. If <x> is 0.0 and <y> is negative, the functions set errno to
      EDOM and return 0.0.

   2. (a) If both <x> and <y> are 0.0, or if (b) <x> is negative and <y> is
      not an integer, the functions print a DOMAIN error message to
      stderr, set errno to EDOM, and return 0.0.
      (b) If an overflow results, the functions set errno to ERANGE
      and return .HUGE_VAL (for pow) or ._LHUGE_VAL (for powl). If
      an underflow results, errno is not set and 0.0 is returned.

  \todo also note powAsMultiply in c src in sn operation
 */
}

#endif
