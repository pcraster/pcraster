#ifndef COM_CLASSPALETTE
#define COM_CLASSPALETTE



#include "csftypes.h"



/*!
  The number of colours in the palette with colours for classified classes.
*/
static const size_t classPaletteSize = 9; // 6; // 15;



/*!
  The maximum of the colour values.
*/
static const UINT2 classColourMaxVal = 255;



/*!
  The palette with colours for classified classes.
*/
static UINT2 classPalette[][3] =
{
  // set19 color scheme from http://www.graphviz.org/doc/info/colors.html
  { 228,  26,  29 }, // Red
  {  55, 128, 184 }, // Blue
  {  77, 175,  74 }, // Green
  { 152,  78, 163 }, // Purple
  { 255, 123,   0 }, // Orange
  { 244, 244,  48 }, // Yellow
  { 166,  84,  39 }, // Brown
  { 247, 129, 192 }, // Pink
  { 153, 153, 153 }, // Grey

  // { 255,   0,   0 }, // Red.
  // { 255, 153,   0 }, // Orange.
  // {  15, 173,   0 }, // Green.
  // {   0, 102, 181 }, // Blue.
  // { 197,   0, 125 }, // Magenta.
  // {  99,   0, 165 }, // Purple.

  // { 255,   0,   0 },
  // // { 255, 255,   0 }, Yellow is hard to see.
  // {   0, 255,   0 },
  // {   0, 255, 255 },
  // {   0,   0, 255 },
  // { 255,   0, 255 },
  // { 200, 200, 200 },
  // {   0, 128,   0 },
  // { 255, 128,   0 },
  // { 128,   0, 128 },
  // {   0,   0, 128 },
  // { 255,   0, 128 },
  // {   0, 128, 128 },
  // { 150, 100,  50 },
  // { 128, 128, 128 }

/*
  { 100,   0,   0 },
  {   0, 100,   0 },
  {   0,   0, 100 },
  {   0, 100, 100 },
  { 100,   0, 100 },
  {  50, 100, 100 },
  {  50,   0, 100 },
  { 100, 100,   0 }
*/
};

#endif

