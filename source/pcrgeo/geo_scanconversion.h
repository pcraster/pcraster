#ifndef INCLUDED_GEO_SCANCONVERSION
#define INCLUDED_GEO_SCANCONVERSION



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif


// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.



namespace geo {
  // ScanConversion declarations.
}



namespace geo {

template<class T>
class RememberPoints {

private:

  std::vector<std::pair<T, T> > d_points;

public:

  typedef typename std::vector<std::pair<T, T> >::const_iterator const_iterator;

  bool operator()(T x, T y) {
    d_points.push_back(std::make_pair(x, y));
    return true;
  }

  std::pair<T, T>& operator[](size_t i) {
    return d_points[i];
  }

  void clear() {
    d_points.clear();
  }

  size_t size() {
    return d_points.size();
  }

  const_iterator begin() const {
    return d_points.begin();
  }

  const_iterator end() const {
    return d_points.end();
  }

};



struct Octant1 {
  template<class Integral>
  static void translateToFirstOctant(Integral& /* x1 */, Integral& /* y1 */,
         Integral& /* x2 */, Integral& /* y2 */) {
    // Do nothing.
  }

  template<class T, class Integral>
  static bool call(T& op, Integral x, Integral y) {
    return op(x, y);
  }
};

struct Octant2 {
  template<class Integral>
  static void translateToFirstOctant(Integral& x1, Integral& y1,
         Integral& x2, Integral& y2) {
    std::swap(x1, y1);
    std::swap(x2, y2);
  }

  template<class T, class Integral>
  static bool call(T& op, Integral x, Integral y) {
    return op(y, x);
  }
};

struct Octant3 {
  template<class Integral>
  static void translateToFirstOctant(Integral& x1, Integral& y1,
         Integral& x2, Integral& y2) {
    x1 *= -1;
    x2 *= -1;
    std::swap(x1, y1);
    std::swap(x2, y2);
  }

  template<class T, class Integral>
  static bool call(T& op, Integral x, Integral y) {
    return op(-y, x);
  }
};

struct Octant4 {
  template<class Integral>
  static void translateToFirstOctant(Integral& x1, Integral& /* y1 */,
         Integral& x2, Integral& /* y2 */) {
    x1 *= -1;
    x2 *= -1;
  }

  template<class T, class Integral>
  static bool call(T& op, Integral x, Integral y) {
    return op(-x, y);
  }
};

struct Octant5 {
  template<class Integral>
  static void translateToFirstOctant(Integral& x1, Integral& y1,
         Integral& x2, Integral& y2) {
    x1 *= -1;
    x2 *= -1;
    y1 *= -1;
    y2 *= -1;
  }

  template<class T, class Integral>
  static bool call(T& op, Integral x, Integral y) {
    return op(-x, -y);
  }
};

struct Octant6 {
  template<class Integral>
  static void translateToFirstOctant(Integral& x1, Integral& y1,
         Integral& x2, Integral& y2) {
    x1 *= -1;
    x2 *= -1;
    y1 *= -1;
    y2 *= -1;
    std::swap(x1, y1);
    std::swap(x2, y2);
  }

  template<class T, class Integral>
  static bool call(T& op, Integral x, Integral y) {
    return op(-y, -x);
  }
};

struct Octant7 {
  template<class Integral>
  static void translateToFirstOctant(Integral& x1, Integral& y1,
         Integral& x2, Integral& y2) {
    y1 *= -1;
    y2 *= -1;
    std::swap(x1, y1);
    std::swap(x2, y2);
  }

  template<class T, class Integral>
  static bool call(T& op, Integral x, Integral y) {
    return op(y, -x);
  }
};

struct Octant8 {
  template<class Integral>
  static void translateToFirstOctant(Integral& /* x1 */, Integral& y1,
         Integral& /* x2 */, Integral& y2) {
    y1 *= -1;
    y2 *= -1;
  }

  template<class T, class Integral>
  static bool call(T& op, Integral x, Integral y) {
    return op(x, -y);
  }
};

template<class Integral, class T>
T& midpointLineN(Integral x, Integral y1, Integral y2, T& op) {
  DEVELOP_PRECOND(y1 <= y2);
  while(y1 <= y2 && op(x, y1++)) {}
  return op;
}

template<class Integral, class T>
T& midpointLineS(Integral x, Integral y1, Integral y2, T& op) {
  DEVELOP_PRECOND(y1 >= y2);
  while(y1 >= y2 && op(x, y1--)) {}
  return op;
}

template<class Integral, class T>
T& midpointLineE(Integral x1, Integral x2, Integral y, T& op) {
  DEVELOP_PRECOND(x1 <= x2);
  while(x1 <= x2 && op(x1++, y)) {}
  return op;
}

template<class Integral, class T>
T& midpointLineW(Integral x1, Integral x2, Integral y, T& op) {
  DEVELOP_PRECOND(x1 >= x2);
  while(x1 >= x2 && op(x1--, y)) {}
  return op;
}

template<class Integral, class T>
T& midpointLineNE(Integral x1, Integral y1, Integral x2, T& op) {
  DEVELOP_PRECOND(x1 <= x2);
  while(x1 <= x2 && op(x1++, y1++)) {}
  return op;
}

template<class Integral, class T>
T& midpointLineNW(Integral x1, Integral y1, Integral x2, T& op) {
  DEVELOP_PRECOND(x1 >= x2);
  while(x1 >= x2 && op(x1--, y1++)) {}
  return op;
}

template<class Integral, class T>
T& midpointLineSW(Integral x1, Integral y1, Integral x2, T& op) {
  DEVELOP_PRECOND(x1 >= x2);
  while(x1 >= x2 && op(x1--, y1--)) {}
  return op;
}

template<class Integral, class T>
T& midpointLineSE(Integral x1, Integral y1, Integral x2, T& op) {
  DEVELOP_PRECOND(x1 <= x2);
  while(x1 <= x2 && op(x1++, y1--)) {}
  return op;
}

template<class Octant, class Integral, class T>
T& midpointLineENE(Integral x1, Integral y1, Integral x2, Integral y2, T& op) {

  Octant::translateToFirstOctant(x1, y1, x2, y2);
  DEVELOP_PRECOND(x2 > x1 && y2 > y1 && x2 - x1 > y2 - y1);

  // This algorithm might be improved once:
  // The start and end points of the line need not be integers as long as we
  // choose dx, dy and d to be integers.

  size_t dx = x2 - x1;
  size_t dy = y2 - y1;
  int d = 2 * dy - dx;

  size_t incrE = 2 * dy;
  size_t incrNE = 2 * (dy - dx);

  Integral x = x1;
  Integral y = y1;

  while(x <= x2 && Octant::call(op, x, y)) {

    if(d <= 0) {
      d += incrE;
      ++x;
    }
    else {
      d += incrNE;
      ++x;
      ++y;
    }
  }

  return op;
}

//! Determines which points lie on the line between x1, y1 and x2, y2.
/*!
  \param     x1 X-coordinate of the start point of the line.
  \param     y1 Y-coordinate of the start point of the line.
  \param     x2 X-coordinate of the end point of the line.
  \param     y2 Y-coordinate of the end point of the line.
  \param     op Function object which is called for each point on the line.
  \return    \a op

  For each point lying on the line op() is called with the coordinates of the
  point as its arguments. The first and last point of the line are processed
  too.

  For algorithm see Foley, Van Dam et. al., second ed.
*/
template<class Integral, class T>
T& midpointLine(Integral x1, Integral y1, Integral x2, Integral y2, T& op) {

  if(x1 == x2) {
    // Vertical line.
    if(y1 <= y2) {
      // North.
      return midpointLineN<Integral, T>(x1, y1, y2, op);
    }
    else {
      // South.
      return midpointLineS<Integral, T>(x1, y1, y2, op);
    }
  }
  else if(y1 == y2) {
    // Horizontal line.
    if(x1 <= x2) {
      // East.
      return midpointLineE<Integral, T>(x1, x2, y1, op);
    }
    else {
      // West.
      return midpointLineW<Integral, T>(x1, x2, y1, op);
    }
  }
  else if(x2 - x1 == y2 - y1) {
    // Line with slope = 1.
    if(x1 <= x2) {
      // North-East.
      return midpointLineNE<Integral, T>(x1, y1, x2, op);
    }
    else {
      // South-West.
      return midpointLineSW<Integral, T>(x1, y1, x2, op);
    }
  }
  else if(x2 - x1 == -(y2 - y1)) {
    // Line with slope = -1.
    if(x1 <= x2) {
      // South-East.
      return midpointLineSE<Integral, T>(x1, y1, x2, op);
    }
    else {
      // North-West.
      return midpointLineNW<Integral, T>(x1, y1, x2, op);
    }
  }
  else {
    int dx = x2 - x1;
    int dy = y2 - y1;

    if(dx > 0) {
      if(dy > 0) {
        if(dx > dy) {
          return midpointLineENE<Octant1, Integral, T>(x1, y1, x2, y2, op);
        }
        else {
          return midpointLineENE<Octant2, Integral, T>(x1, y1, x2, y2, op);
        }
      }
      else {
        if(dx > -dy) {
          return midpointLineENE<Octant8, Integral, T>(x1, y1, x2, y2, op);
        }
        else {
          return midpointLineENE<Octant7, Integral, T>(x1, y1, x2, y2, op);
        }
      }
    }
    else {
      if(dy > 0) {
        if(-dx > dy) {
          return midpointLineENE<Octant4, Integral, T>(x1, y1, x2, y2, op);
        }
        else {
          return midpointLineENE<Octant3, Integral, T>(x1, y1, x2, y2, op);
        }
      }
      else {
        if(-dx > -dy) {
          return midpointLineENE<Octant5, Integral, T>(x1, y1, x2, y2, op);
        }
        else {
          return midpointLineENE<Octant6, Integral, T>(x1, y1, x2, y2, op);
        }
      }
    }
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   Make sure this function is only called for
             x != 0 && y != 0 && x != y
  \sa        .
*/
template<class Integral, class T>
T& circlePoints(Integral xCenter, Integral yCenter,
         Integral x, Integral y, T& op) {

  DEVELOP_PRECOND(x != 0);
  DEVELOP_PRECOND(y != 0);
  DEVELOP_PRECOND(x != y);

  op(xCenter + x, yCenter + y);
  op(yCenter + y, xCenter + x);
  op(yCenter + y, xCenter - x);
  op(xCenter + x, yCenter - y);
  op(xCenter - x, yCenter - y);
  op(yCenter - y, xCenter - x);
  op(yCenter - y, xCenter + x);
  op(xCenter - x, yCenter + y);

  return op;
}



//! Determines which points lie on the circle \a xCenter, \a yCenter, \a radius.
/*!
  \param     xCenter X coordinate of center of circle.
  \param     yCenter Y coordinate of center of circle.
  \param     radius Radius of circle.
  \param     op Function object which is called for each point on the circle.
  \warning   The implementation of
             midpointCircle(Integral, Integral, size_t size_t, T&)
             depends on the internals of this function.
  \return    \a op

  For each point lying on the circle op() is called with the coordinates of the
  point as its arguments.

  For algorithm see Foley, Van Dam et. al., second ed.
*/
template<class Integral, class T>
T& midpointCircle(Integral xCenter, Integral yCenter, size_t radius, T& op) {

  if(radius == 0) {
    op(xCenter, yCenter);
    return op;
  }

  Integral x = 0;
  Integral y = radius;

  op(xCenter, yCenter + radius);
  op(xCenter, yCenter - radius);
  op(xCenter + radius, yCenter);
  op(xCenter - radius, yCenter);

  if(radius == 1) {
    return op;
  }

  int d = 1 - radius;
  size_t deltaE = 3;
  int deltaSE = -2 * radius + 5;

  while(y > (x + 1)) {
    if(d < 0) {
      d += deltaE;
      deltaE += 2;
      deltaSE += 2;
    }
    else {
      d += deltaSE;
      deltaE += 2;
      deltaSE += 4;
      --y;
    }
    ++x;

    if(x != y) {
      circlePoints(xCenter, yCenter, x, y, op);
    }
    else {
      // Last point in second octant.
      op(xCenter + x, yCenter + y);
      op(xCenter + x, yCenter - y);
      op(xCenter - x, yCenter - y);
      op(xCenter - x, yCenter + y);
      break;
    }
  }

  return op;
}



//! Determines which points lie on the two circles.
/*!
  \param     xCenter    X coordinate of center of circle.
  \param     yCenter    Y coordinate of center of circle.
  \param     fromRadius Radius of inner circle.
  \param     toRadius   Radius of outer circle.
  \param     op Function object which is called for each point on or between
             the circles.
  \warning   The implementation of this function depends on the internals of
             the implementation of
             midpointCircle(Integral, Integral, size_t, T&).
  \return    \a op

  This function determines which points lie on the inner circle with radius
  \a fromRadius, which points lie on the outer circle with radius \a toRadius
  and which points lie between those circles.

  For each point lying on or between the circles op() is called with the
  coordinates of the point as its arguments.

  For ideas see Foley, Van Dam et. al., second ed.
*/
template<class Integral, class T>
T& midpointCircle(Integral xCenter, Integral yCenter, size_t fromRadius,
         size_t toRadius, T& op) {

  PRECOND(fromRadius < toRadius);

  // Scan convert inner circle. Remember the points.
  RememberPoints<Integral> innerPoints;
  midpointCircle(xCenter, yCenter, fromRadius, innerPoints);

  // Call op for each point on the inner circle.
  for(typename RememberPoints<Integral>::const_iterator it =
         innerPoints.begin(); it != innerPoints.end(); ++it) {
    op((*it).first, (*it).second);
  }

  // Scan convert the outer circle.
  RememberPoints<Integral> outerPoints;
  midpointCircle(xCenter, yCenter, toRadius, outerPoints);

  // Call op for each point on the outer circle.
  for(typename RememberPoints<Integral>::const_iterator it =
         outerPoints.begin(); it != outerPoints.end(); ++it) {
    op((*it).first, (*it).second);
  }

  // Now, handle the points between the inner and outer circle.

  // Start with x = xCenter.
  for(size_t radius = fromRadius + 1; radius < toRadius; ++radius) {
    op(xCenter, yCenter + radius);
    op(xCenter, yCenter - radius);
    op(xCenter + radius, yCenter);
    op(xCenter - radius, yCenter);
  }

  // Determine number of points in second octant, excluding x = xCenter column
  // and x = y.
  bool innerXIsYPresent = false;
  bool outerXIsYPresent = false;

  size_t nrInnerPointsOctant, nrOuterPointsOctant;
  if(fromRadius == 0) {
    PRECOND(innerPoints.size() == 1.0);
    innerXIsYPresent = false;
    nrInnerPointsOctant = 0;
  }
  else if(fromRadius == 1) {
    PRECOND(innerPoints.size() == 4.0);
    innerXIsYPresent = false;
    nrInnerPointsOctant = 0;
  }
  else {
    // Determine whether points lie on x=y lines.
    innerXIsYPresent = (innerPoints.size() - 4) % 8 == 4;

    // Substract 4 points which lie on the axes.
    nrInnerPointsOctant = innerPoints.size() - 4;

    if(innerXIsYPresent) {
      // Substract 4 points which lie on the x=y lines.
      nrInnerPointsOctant -= 4;
    }

    // Devide by 8 to get points per octant.
    PRECOND(nrInnerPointsOctant % 8 == 0.0);
    nrInnerPointsOctant = static_cast<size_t>(static_cast<double>(
         nrInnerPointsOctant) / 8.0);
  }

  if(toRadius == 1) {
    PRECOND(outerPoints.size() == 4.0);
    nrOuterPointsOctant = 0;
  }
  else {
    outerXIsYPresent = (outerPoints.size() - 4) % 8 == 4;
    nrOuterPointsOctant = outerPoints.size() - 4;

    if(outerXIsYPresent) {
      nrOuterPointsOctant -= 4;
    }

    PRECOND(nrOuterPointsOctant % 8 == 0.0);
    nrOuterPointsOctant = static_cast<size_t>(static_cast<double>(
         nrOuterPointsOctant) / 8.0);
  }

  // We assume that midPointCircle calls op in a certain order:
  // - First 4 coordinates:
  //    op(xCenter, yCenter + radius);
  //    op(xCenter, yCenter - radius);
  //    op(xCenter + radius, yCenter);
  //    op(xCenter - radius, yCenter);
  // - Than n * 8 octants points untill x == y.
  // - In some cases 4 coordinates on x=y lines:
  //    op(xCenter + x, yCenter + y);
  //    op(xCenter + x, yCenter - y);
  //    op(xCenter - x, yCenter - y);
  //    op(xCenter - x, yCenter + y);

  // Continue with columns excluding x == y.
  Integral x = 0;
  Integral yFrom, yTo;
  size_t i;

  for(i = 0; i < nrInnerPointsOctant; ++i) {

    x++;
    yFrom = innerPoints[4 + i * 8].second + 1 - yCenter;
    yTo = outerPoints[4 + i * 8].second - yCenter;

    while(yFrom < yTo) {

      circlePoints(xCenter, yCenter, x, yFrom, op);
      yFrom++;
    }
  }

  if(innerXIsYPresent) {

    x++;
    yFrom = innerPoints[4 + i * 8].second + 1 - yCenter;
    yTo = outerPoints[4 + i * 8].second - yCenter;

    for(; yFrom < yTo; ++yFrom) {
      circlePoints(xCenter, yCenter, x, yFrom, op);
    }

    i++;
  }

  for(; i < nrOuterPointsOctant; ++i) {

    x++;
    yFrom = x;
    yTo = outerPoints[4 + i * 8].second - yCenter;

    // Point x, yFrom lies on x = y.
    op(xCenter + x, yCenter + yFrom);
    op(xCenter + x, yCenter - yFrom);
    op(xCenter - x, yCenter - yFrom);
    op(xCenter - x, yCenter + yFrom);

    // Handle triangle above x = y line and between inner and outer circles.
    for(++yFrom; yFrom < yTo; ++yFrom) {
      circlePoints(xCenter, yCenter, x, yFrom, op);
    }
  }

  return op;
}



} // namespace geo

#endif
