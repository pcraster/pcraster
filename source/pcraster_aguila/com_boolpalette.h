#ifndef COM_BOOLPALETTE
#define COM_BOOLPALETTE



#include "csftypes.h"


/*!
  Number of colours in palette with colours for boolean classes.
*/
static const size_t boolPaletteSize = 2;



/*!
  The maximum of the colour values.
*/
static const UINT2 boolColourMaxVal = 100;



/*!
  Palette with colours for boolean classes.
*/
static UINT2 boolPalette[][3] =
{
  { 100,  25,   0 },
  {   0, 100,  25 }
};

#endif

