#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_STRINGCONV
#include "pcrxml_stringconv.h"
#endif

#ifndef  INCLUDED_QSTRING
#include <qstring.h>
#define  INCLUDED_QSTRING
#endif

/*!
  \file
  This file contains the implementation of a number of string conversion routines
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC STRING MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STRING MEMBERS
//------------------------------------------------------------------------------


//! convert
std::string pcrxml::asString(const QString& s)
{
  if (s.isEmpty())
   return std::string();

  QByteArray asciiData = s.toLatin1();
  return std::string(asciiData.constData());
}

/*
//! convert
std::string pcrxml::asString(const XMLCh *s)
{
    // Copied from xerces memparse sample
   //  This is a simple way that lets us do easy (though not terribly efficient)
   //  trancoding of XMLCh data to local code page for display.
   // Call the private transcoding method
   char *fLocalForm = XMLString::transcode(s);
   std::string str(fLocalForm);
// delete [] fLocalForm;
   return str;
}
*/

//! convert
QString pcrxml::asQString(const std::string& s)
{
  return QString(s.c_str());
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

