#include "ag_FileIconProvider.h"
#include "icons/pcr_16x16.xpm"



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

ag::FileIconProvider::FileIconProvider(QObject *p, const char *n)

  : Q3FileIconProvider(p, n),
    d_pcrIcon((const char **)pcr_16x16_xpm)

{
}



ag::FileIconProvider::~FileIconProvider()
{
}



const QPixmap *ag::FileIconProvider::pixmap(const QFileInfo &i)
{
  QPixmap *p = 0;
  QString e = i.extension(false).lower();

  if(e == "csf" || e == "map" || e == "pcr" || e == "tss") {
    p = &d_pcrIcon;
  }
  else if(e.length() == 3 && e[0].isDigit() && e[1].isDigit() &&
                   e[2].isDigit()) {
    p = &d_pcrIcon;
  }

  return p;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


