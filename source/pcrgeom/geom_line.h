#ifndef INCLUDED_GEOM_LINE
#define INCLUDED_GEOM_LINE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



/*!
  \class geom_Line
  \brief The geom_Line class is for simple line objects with integer
         coordinates.

  geom_Line objects can be used for lines on a screen for example.
*/
class geom_Line
{

private:

  //! X-coordinate of start-point.
  int              d_xStart;

  //! Y-coordinate of start-point.
  int              d_yStart;

  //! X-coordinate of end-point.
  int              d_xEnd;

  //! Y-coordinate of end-point.
  int              d_yEnd;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   geom_Line          ();
                   geom_Line          (int xStart,
                                       int yStart,
                                       int xEnd,
                                       int yEnd);
  virtual          ~geom_Line         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setXStart          (int xStart);
  void             setYStart          (int yStart);
  void             setXEnd            (int xEnd);
  void             setYEnd            (int yEnd);
  void             setLine            (int xStart,
                                       int yStart,
                                       int xEnd,
                                       int yEnd);
  void             moveBy             (int dx,
                                       int dy);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  int              getXStart          () const;
  int              getYStart          () const;
  int              getXEnd            () const;
  int              getYEnd            () const;

};

#endif
