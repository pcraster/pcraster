#ifndef INCLUDED_COM_RANGEMAP
#define INCLUDED_COM_RANGEMAP



// Library headers.

// PCRaster library headers.

// Module headers.



namespace com {
  // RangeMap declarations.
}



namespace com {



//! The RangeMap class is for scaling values from one range to another.
/*!
  \code

  // Map values from range 1 - 2 to range 0 - 100:
  RangeMap<double, double> mapper(1.0, 2.0, 0.0, 100.0);
  std::cout << mapper.map(1.5) << std::endl;     // 50

  \endcode
*/
template<class T, class U>
class RangeMap
{

private:

  //! Lower border of first range.
  T                d_lower1;

  //! Upper border of first range.
  T                d_upper1;

  //! Lower border of second range.
  U                d_lower2;

  //! Upper border of second range.
  U                d_upper2;

  //! Conversion factor from range 1 to range 2.
  double           d_scale;

  //! Assignment operator. NOT IMPLEMENTED.
  RangeMap&        operator=           (const RangeMap&);

  //! Copy constructor. NOT IMPLEMENTED.
                   RangeMap            (const RangeMap&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RangeMap            (const T& lower1,
                                        const T& upper1,
                                        const U& lower2,
                                        const U& upper2);

  /* virtual */    ~RangeMap           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setRanges           (const T& lower1,
                                        const T& upper1,
                                        const U& lower2,
                                        const U& upper2);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  U                map                 (const T& value);

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
