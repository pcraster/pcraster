#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_SPIRITFILEPARSER
#include "com_spiritfileparser.h"
#define INCLUDED_COM_SPIRITFILEPARSER
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
#ifndef INCLUDED_COM_FILEMAP
#include "com_filemap.h"
#define INCLUDED_COM_FILEMAP
#endif
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
// Module headers.



/*!
  \file
  This file contains the implementation of the SpiritFileParser class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class SpiritFileParserPrivate
{
public:

  SpiritFileParserPrivate()
  {
  }

  ~SpiritFileParserPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPIRITFILEPARSER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SPIRITFILEPARSER MEMBERS
//------------------------------------------------------------------------------

/*!
 * \throws
 *   com::OpenFileError if file is larger than 512 Mb
 */
com::SpiritFileParser::SpiritFileParser(const com::PathName& pn)
//  :d_fbegin(pn.toString().c_str()),
//   d_fend(d_fbegin.make_end()),
//   d_begin(d_fbegin,d_fend,pn.toString()),
//   d_current(d_begin)
{
    com::testOpenForReading(pn);
    if (com::size(pn) > megaByte<size_t>(512))
      throw com::OpenFileError(pn,"File too large for reading");
    d_fileMap = new FileMap(pn);
    d_fbegin  = d_fileMap->begin();
    d_fend    = d_fileMap->end();
    d_begin   = iterator(d_fbegin,d_fend,pn.toString());
    d_current = d_begin;
}

/*!
 * NOTE
 *  bcc32 crashes with file_iterator version
 *  if spirit/iterator/impl/file_iterator.ipp
 *  is not hacked:  comment the line
 *    close(m_File);
 *   into
 *   ; //close(m_File);
 *   but then file handles remain open
 */
com::SpiritFileParser::~SpiritFileParser()
{
  delete d_fileMap;
}

/* NOT IMPLEMENTED
//! Assignment operator.
com::SpiritFileParser& com::SpiritFileParser::operator=(const SpiritFileParser& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
com::SpiritFileParser::SpiritFileParser(const SpiritFileParser& rhs)
{
}
*/
//! advance by setting current after the just parsed part
void com::SpiritFileParser::advance()
{
  d_current = pi.stop;
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
void com::SpiritFileParser::errorAtStop() const {
  boost::spirit::classic::file_position fp(pi.stop.get_position());
  std::string s;
  /*
  for(fit_t i=pi.stop.pos(); i != d_fend; ++i) {
      if (*i=='\n')
        break;
      s+=*i;
      if (s.size()==5)
        break;
  }
  */

  throw com::FilePositionError(fp.file,fp.line,fp.column,
   "unexpected format: '"+s+",");
}

//! get current line nr of file position
size_t com::SpiritFileParser::lineNr() const
{
  boost::spirit::classic::file_position fp(d_current.get_position());
  return fp.line;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
