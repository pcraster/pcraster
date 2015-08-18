#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#include <boost/math/special_functions/round.hpp>
#define INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#endif

#ifndef INCLUDED_GEOM_SIZE
#include "geom_size.h"
#define INCLUDED_GEOM_SIZE
#endif

#ifndef INCLUDED_GEOM_RECTANGLE
#include "geom_rectangle.h"
#define INCLUDED_GEOM_RECTANGLE
#endif

#ifndef INCLUDED_GEOM_UTIL
#include "geom_util.h"
#define INCLUDED_GEOM_UTIL
#endif



/*!
  This function returns a rectangle based on the size of \a maxSize and the
  ratio of \a width and \a height. The new rectangle will have the same aspect
  as \a maxSize, but might be different in size.
*/
geom_Size<int> geom_Util::adjustWidthOrHeight(const geom_Size<int> &maxSize,
                                         double width, double height)
{
  geom_Size<int> adjustedSize(maxSize);

  double widthRatio  = maxSize.getWidth()  / width;
  double heightRatio = maxSize.getHeight() / height;

  widthRatio < heightRatio
    ? adjustedSize.setHeight(static_cast<int>(
                   maxSize.getWidth() * height / width))
    : adjustedSize.setWidth(static_cast<int>(
                   maxSize.getHeight() * width / height));

  return adjustedSize;
}



/*!
  This function returns a rectangle based on the size of \a size and the ratio
  of \a width and \a height. The new rectangle will have the same aspect as
  \a widht and \a height. The size of \a size will be adjusted (enlarged) to
  meet these requirements.
*/
geom::Rectangle<int> geom_Util::reAspect(const geom::Rectangle<int> &size,
                                        double width, double height)
{
  geom::Rectangle<int> newSize = size;
  double widthRatio           = width  / (double)size.width();
  double heightRatio          = height / (double)size.height();

  if(widthRatio < heightRatio)
  {
    int newHeight = boost::math::iround((size.width() * height) / width);
    newSize.setHeight(newHeight); 
  }
  else
  {
    int newWidth = boost::math::iround((size.height() * width) / height);
    newSize.setWidth(newWidth); 
  }

  return newSize;
}

