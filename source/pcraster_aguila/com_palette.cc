#include "com_palette.h"
#include "com_colour.h"



enum { RED, GREEN, BLUE };



/*!
  \param   colourRange  A two dimensional array with RGB colour values.
  \param   colourMaxVal The maximum colour value.
  \param   nrColours  The number of colours in \a colourRange.
*/
com_Palette::com_Palette(const int colourRange[][3], int colourMaxVal,
                         size_t nrColours)
{
  size_t colourNr;

  for(colourNr = 0; colourNr < nrColours; colourNr++)
  {
    d_colourRange.push_back(com_Colour(colourRange[colourNr][RED],
                                       colourRange[colourNr][GREEN],
                                       colourRange[colourNr][BLUE]));
  }

  // Scale the colour values to a range from 0 to 256.
  double factor = 255.0 / colourMaxVal;
  scale(factor);
}



/*!
  \param   factor The scaling factor.
  \sa      com_Colour::scale(double factor);
*/
void com_Palette::scale(double factor)
{
  com_Palette::iterator colourHandle;

  for(colourHandle = begin(); colourHandle != end(); colourHandle++)
  {
    (*colourHandle).scale(factor);
  }
}



/*!
  \return  An iterator to the first colour in the palette. If there are no
           colours in the palette, end() is returned.
  \sa      end(), rbegin(), rend()
*/
com_Palette::const_iterator com_Palette::begin() const
{
  return d_colourRange.begin();
}



/*!
  \return  An iterator to the one-past-the-last colour in the palette.
  \warning Don't folow this iterator; it points to an undefined value. Use
           the returned iterator for loop control.
  \sa      begin(), rbegin(), rend()
*/
com_Palette::const_iterator com_Palette::end() const
{
  return d_colourRange.end();
}



/*!
  \return  A reverse iterator to the first colour in the palette or rend().
  \sa      rend(), begin(), end()
*/
com_Palette::const_reverse_iterator com_Palette::rbegin() const
{
  return d_colourRange.rbegin();
}



/*!
  \return  A reverse iterator to the one-past-the-last colour in the palette.
  \warning Don't folow this iterator; it points to an undefined value. Use
           the returned iterator for loop control.
  \sa      rbegin(), begin(), end()
*/
com_Palette::const_reverse_iterator com_Palette::rend() const
{
  return d_colourRange.rend();
}



/*!
  \return  An iterator to the first colour in the palette. If there are no
           colours in the palette, end() is returned.
  \sa      end(), rbegin(), rend()
*/
com_Palette::iterator com_Palette::begin()
{
  return d_colourRange.begin();
}



/*!
  \return  An iterator to the one-past-the-last colour in the palette.
  \warning Don't folow this iterator; it points to an undefined value. Use
           the returned iterator for loop control.
  \sa      begin(), rbegin(), rend()
*/
com_Palette::iterator com_Palette::end()
{
  return d_colourRange.end();
}



/*!
  \return  A reverse iterator to the first colour in the palette or rend().
  \sa      rend(), begin(), end()
*/
com_Palette::reverse_iterator com_Palette::rbegin()
{
  return d_colourRange.rbegin();
}



/*!
  \return  A reverse iterator to the one-past-the-last colour in the palette.
  \warning Don't folow this iterator; it points to an undefined value. Use
           the returned iterator for loop control.
  \sa      rbegin(), begin(), end()
*/
com_Palette::reverse_iterator com_Palette::rend()
{
  return d_colourRange.rend();
}



/*!
  \return  The number of colours in the palette.
*/
size_t com_Palette::getNrOfColours() const
{
  return d_colourRange.size();
}

