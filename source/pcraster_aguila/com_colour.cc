#include "dal_MathUtils.h"
#include "com_colour.h"



/*!
  \sa      com_Colour(int red, int green, int blue)

  The red, green and blue colour values are initialised by 0.
*/
com_Colour::com_Colour()

  : d_red(0), d_green(0), d_blue(0)
{
}



/*!
  \param   red The red colour value.
  \param   green The red colour value.
  \param   blue The red colour value.
  \sa      com_Colour()

  The red, green and blue colour values are initialised by \a red, \a green and
  \a blue.
*/
com_Colour::com_Colour(int red, int green, int blue)

  : d_red(red), d_green(green), d_blue(blue)

{
}



/*!
  \param   factor The scalefactor.

  If, for example, the rgb values are currently scaled from 0 to 100 they can
  be rescaled to the range 0 to 255 by issuing the following command:

  \code
    colour.scale(255.0 / 100);
  \endcode
*/
void com_Colour::scale(double factor)
{
  d_red   = dal::round<double, int>(d_red   * factor);
  d_green = dal::round<double, int>(d_green * factor);
  d_blue  = dal::round<double, int>(d_blue  * factor);
}



/*!
  \return  The red colour value.
  \sa      getGreen(), getBlue()
*/
int com_Colour::getRed() const
{
  return d_red;
}



/*!
  \return  The green colour value.
  \sa      getRed(), getBlue()
*/
int com_Colour::getGreen() const
{
  return d_green;
}



/*!
  \return  The blue colour value.
  \sa      getRed(), getGreen()
*/
int com_Colour::getBlue() const
{
  return d_blue;
}

