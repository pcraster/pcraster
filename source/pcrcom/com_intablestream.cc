#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_INTABLESTREAM
#include "com_intablestream.h"
#define INCLUDED_COM_INTABLESTREAM
#endif

// Library headers.
#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif
#include <boost/spirit/dynamic/while.hpp>

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the InTableStream class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class InTableStreamPrivate
{
public:

  InTableStreamPrivate()
  {
  }

  ~InTableStreamPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC INTABLESTREAM MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF INTABLESTREAM MEMBERS
//------------------------------------------------------------------------------

//! open the stream, init tableInfo, ready for streaming records with >>
/*!
 * \throws
 *   FileFormatError if the file format is not recognized
 */
com::InTableStream::InTableStream(const com::PathName& pn):
  TableInfo(pn),
  d_parser(pn,nrHeaderLines()),
  d_lineNrStartLastRead(0)
{
  switch(layout()) {
    case UNKNOWN: throw FileFormatError(pn,"Format of file not recognized");
    default:      ;
  }
}

com::InTableStream::~InTableStream()
{
}

/* DEFAULT
//! Assignment operator.
com::InTableStream& com::InTableStream::operator=(const InTableStream& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. DEFAULT
com::InTableStream::InTableStream(const InTableStream& rhs)
{
}
*/
bool com::InTableStream::moreHeaderLinesToSkip()
{
  if (d_nrHeaderLinesParsed == d_nrHeaderLinesToSkip)
    return false;
  d_nrHeaderLinesParsed++;
  return true;
}

void com::InTableStream::parseHeader()
{
  d_parser.skipLines(nrColumns()+2);
}

//! the line nr of the table stream before the last >> is done
/*!
 * \todo
 *   line nr are less than actual start if first blank lines scanned
 */
size_t com::InTableStream::lineNrStartLastRead() const
{
  return d_lineNrStartLastRead;
}

/*!
 * \throws 
 *   com::FilePositionError for the position the last read (>>) started.
 */
void com::InTableStream::throwForLastRead(const std::string& msg) const
{
  throw FilePositionError(fileName(),d_lineNrStartLastRead,0,msg);
}

//! read next line
/*! read a line of data that is terminated by a new-line. Lines with only
 *  white space or empty lines are ignored. There is no check if the number
 *  of values read into \a record equals nrColumns()
 *
 * \param record  is assigned all values read
 * \returns
 *    wether \a record is not empty
 * \throws
 *    com::FilePositionError if not all data read is convertible to a double
 *
 * typical use:
 * <pre>
 * std::vector<double> d;
 * while (tab >> d) {
 *  // do something
 * }
 */
bool com::InTableStream::operator>>(std::vector<double>& record)
{
  record.clear();
  using namespace boost::spirit;
  d_lineNrStartLastRead = d_parser.lineNr();
  d_parser.pi = parse(d_parser.begin(),d_parser.end(),
      // line of numbers followed by a newline
      *real_p[append(record)],space_p);
  if (!d_parser.fullMatch())
    d_parser.errorAtStop();

  d_parser.advance();
  return !record.empty();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



