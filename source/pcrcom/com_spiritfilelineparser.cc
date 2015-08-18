#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_SPIRITFILELINEPARSER
#include "com_spiritfilelineparser.h"
#define INCLUDED_COM_SPIRITFILELINEPARSER
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
// Module headers.



/*!
  \file
  This file contains the implementation of the SpiritFileLineParser class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class SpiritFileLineParserPrivate
{
public:

  SpiritFileLineParserPrivate()
  {
  }

  ~SpiritFileLineParserPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPIRITFILELINEPARSER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SPIRITFILELINEPARSER MEMBERS
//------------------------------------------------------------------------------

com::SpiritFileLineParser::SpiritFileLineParser(
    const com::PathName& pn,
    size_t nrHeaderLinesToSkip):
  d_pn(pn),
  d_nrNewLinesRead(0)
{
  open(d_ifs,pn);
  skipLines(nrHeaderLinesToSkip);
}

com::SpiritFileLineParser::~SpiritFileLineParser()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
com::SpiritFileLineParser& com::SpiritFileLineParser::operator=(const SpiritFileLineParser& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
com::SpiritFileLineParser::SpiritFileLineParser(const SpiritFileLineParser& rhs)
{
}
*/
//! advance by filling the buffer
/*!
 * the buffer will be filled including a terminating newline, except
 * in the case of end of file
 */
void com::SpiritFileLineParser::advance()
{
  bool allWhiteSpace=true;
  while(allWhiteSpace) {
    char c;
    d_end=d_buffer;
    while(d_ifs.get(c)) {
      if (d_end == begin()+CAPACITY)
        throw FileFormatError(d_pn,"exceeds buffer");
      *d_end = c;
      d_end++;
      allWhiteSpace = allWhiteSpace && (std::isspace(c));
      if (c == '\n') {
         d_nrNewLinesRead++; 
         break;
      }
    }
    if (d_ifs.eof()) {
      d_nrNewLinesRead++; // let eof also be a terminator
      return;
    } else {
       if (!d_ifs)
        throw FileFormatError(d_pn,"input error");
    }
  }
}

void com::SpiritFileLineParser::skipLines(size_t nrLinesToSkip)
{
  size_t nrNewLinesNowRead(0);

  char c;
  while( nrNewLinesNowRead != nrLinesToSkip && d_ifs.get(c)) {
    if (c == '\n') {
       d_nrNewLinesRead++;
       ++nrNewLinesNowRead;
    }
  }
  advance();
}

//! we do have a full match if there is only white space left
bool com::SpiritFileLineParser::fullMatch() const {
  for(iterator i=pi.stop; i != end(); ++i)
    if (!std::isspace(*i))
      return false;
  return true;
}

//! throw com::FilePositionError for expect format of current stop
/*! \todo
 *  this requires a patch to boost in
 *    &lt;boost/spirit/iterator/position_iterator.hpp>
 *    <pre>
 *    120a121,123
 *    >     IteratorT pos () const {
 *    >       return m_Iter;
 *    >     }
 *    </pre>
 *
 *  forthcoming spirit 1.7 has position_iterator2 that can give us the 
 *  current line data we hack here
 */
void com::SpiritFileLineParser::errorAtStop() const {
  // vreet bcc32 niet
  // std::string(pi.stop.pos(),d_fend).substr(0,5)+"'");

  // eat away trailing space
  iterator colPos=pi.stop;
  while(std::isspace(*colPos))
    ++colPos;
  std::string s;
  for(iterator i=colPos; i != end(); ++i) {
      if (*i=='\n')
        break;
      s+=*i;
      if (s.size()==5)
        break;
  }

  throw com::FilePositionError(d_pn,d_nrNewLinesRead,colPos-begin()+1,
   "unexpected format: '"+s+",");
}

//! get current line nr of file position
size_t com::SpiritFileLineParser::lineNr() const
{
  return d_nrNewLinesRead;
}

bool com::SpiritFileLineParser::eof() const
{
  return d_ifs.eof();
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
