#ifndef INCLUDED_COM_DIMAP
#define INCLUDED_COM_DIMAP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



namespace com {



/*!
  \class DiMap
  \brief The DiMap class is for objects who need to map a double range to
         an integer range/interval.

  The DiMap class maps an interval of type double into an interval of type
  integer. It consists of two intervals D = [d1, d2] (double) and
  I = [i1, i2] (int), which are specified with the setDblRange() and
  setIntRange() members. The point d1 is mapped to the point i1, and d2 is
  mapped to i2. Any point inside or outside D can be mapped to a point inside
  or outside I using transform() or limTransform() or vice versa using
  invTransform(). D can be scaled linearly or logarithmically, as specified
  with setDblRange().

  Example:
  \code
  #include <qwt_dimap.h>

  DiMap map;
  int ival;
  double dval;

  map.setDblRange(0.0, 3.1415);   // Assign an interval of type double with
                                  // linear mapping.
  map.setIntRange(0,100);         // Assign an integer interval.

  ival = map.transform(1.0);      // Obtain integer value corresponding to 1.0.
  dval = map.invTransform(77);    // Obtain double value corresponding to 77.
  \endcode
*/
/*
//    com_DiMap::setIntRange -- Specify integer interval
//    com_DiMap::setDblRange -- Specify double interval
//    com_DiMap::transform   -- transform double -> int
//    com_DiMap::invTransform -- transform int -> double
//    com_DiMap::xTransform -- transform double -> double(int)
//    com_DiMap::limTransform(double x) -- transform double -> int
//    com_DiMap::d1 -- first border of double interval 
//    com_DiMap::d2 -- second border of double interval
//    com_DiMap::i1 -- first border of integer interval
//    com_DiMap::i2 -- second border of int interval
//    com_DiMap::logarithmic -- double interval scaled logarithmically?
*/
class DiMap
{

private:

  //! First double interval x boundary.
  double           d_x1;

  //! Second double interval x boundary.
  double           d_x2;

  //! First integer interval y boundary.
  int              d_y1;

  //! Second integer interval y boundary.
  int              d_y2;

  //! Conversion factor.
  double           d_cnv;

  //! Logarithmic scale?
  bool             d_log;
	
  //! Copy constructor. NOT IMPLEMENTED.
                   DiMap               (const DiMap &m);

  //! Re-calculate the conversion factor.
  void             newFactor           ();

public:

  static const double logMin;

  static const double logMax;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
                   DiMap              ();

  //! Constructor.
                   DiMap              (int i1,
                                       int i2,
                                       double d1,
                                       double d2,
                                       bool lg = false);

  //! Destructor.
                   ~DiMap              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Specify the borders of the integer interval.
  void             setIntRange        (int i1,
                                       int i2);

  //! Specify the borders of the double interval.
  void             setDblRange        (double d1,
                                       double d2,
                                       bool   lg = false);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns true if \a x lies inside or at the border of the double range.
  bool             contains           (double x) const;

  //! Returns true if \a x lies inside or at the border of the integer range.
  bool             contains           (int x) const;

  //! Transform a point in double interval into an point in the int interval.
  int              transform          (double x) const;

  //! Transform an integer value into a double value.
  double           invTransform       (int i) const;

  //! Transform and limit.
  int              limTransform       (double x) const;

  //! Exact transformation.
  double           xTransform         (double x) const;

  //! Returns the first border of the double interval.
  double           d1                 () const;

  //! Returns the second border of the double interval.
  double           d2                 () const;

  //! Returns the first border of the integer interval.
  int              i1                 () const;

  //! Returns the second border of the integer interval.
  int              i2                 () const;

  //! Returns true if the double interval is scaled logarithmically.
  bool             logarithmic        () const;

  //! Returns the scale factor (double * scale = int).
  double           scale              () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

inline bool DiMap::logarithmic() const
{ return d_log; }



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif

