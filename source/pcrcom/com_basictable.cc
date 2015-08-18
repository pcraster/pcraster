#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_BASICTABLE
#include "com_basictable.h"
#define INCLUDED_COM_BASICTABLE
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_CCTYPE
#include <cctype>
#define INCLUDED_CCTYPE
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif


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

com::BasicTable::BasicTable()

  : d_nrRecs(0)

{
}



com::BasicTable::BasicTable(size_t nrCols, size_t nrRecs)
{
  resize(nrCols, nrRecs);
}



com::BasicTable::~BasicTable()
{
  clean();
}



void com::BasicTable::clean()
{
  for(col_iterator it = d_columns.begin(); it != d_columns.end(); it++) {
    delete *it;
  }

  d_columns.erase(d_columns.begin(), d_columns.end());
}



void com::BasicTable::resize(size_t n)
{
  int dn = n - d_columns.size();

  if(dn < 0) {
    for(size_t i = n; i != d_columns.size(); i++) {
      delete d_columns[i];
    }

    d_columns.erase(d_columns.begin() + n, d_columns.end());
  }
  else if(dn > 0) {
    d_columns.resize(n);

    for(size_t i = d_columns.size() - dn; i != d_columns.size(); i++) {
      d_columns[i] = new std::vector<double>;
    }
  }
}



void com::BasicTable::resize(size_t nrCols, size_t nrRecs)
{
  resize(nrCols);

  for(col_iterator it = d_columns.begin(); it != d_columns.end(); ++it) {
    (*it)->resize(nrRecs);
  }

  d_nrRecs = nrRecs;
}



void com::BasicTable::setValue(size_t col, size_t rec, double value) const
{
  PRECOND(col < nrCols());
  PRECOND(rec < nrRecs());

  (*d_columns[col])[rec] = value;
}



void com::BasicTable::setMV(size_t col, size_t rec)
{
  PRECOND(col < nrCols());
  PRECOND(rec < nrRecs());

  pcr::setMV((*d_columns[col])[rec]);
}



/*!
  \return  Number of columns in the table.
  \sa      nrRecs()
*/
size_t com::BasicTable::nrCols() const
{
  return d_columns.size();
}



/*!
  \return  Number of records in the table.
  \warning You can't be sure that every column is of the same length. You can
           be sure that no column has more than nrRecs() records.
  \sa      nrCols()
*/
size_t com::BasicTable::nrRecs() const
{
  return d_nrRecs;
}



bool com::BasicTable::isMV(size_t col, size_t rec)
{
  PRECOND(col < nrCols());
  PRECOND(rec < nrRecs());

  return pcr::isMV((*d_columns[col])[rec]);
}



//! Returns true if all values in column \a i are missing values.
/*!
  \param     i Column number.
  \return    true or false.

  This function also returns true if there are no records at all.
*/
bool com::BasicTable::allMV(size_t i) const
{
  for(const_iterator it = begin(i); it != end(i); ++it) {
    if(!pcr::isMV(*it)) {
      return false;
    }
  }

  return true;
}



double com::BasicTable::min(size_t i) const
{
  double min;
  pcr::setMV(min);

  for(const_iterator it = begin(i); it != end(i); ++it) {
    if(!pcr::isMV(*it)) {
      if(pcr::isMV(min)) {
        min = *it;
      }
      else {
        min = MIN(min, *it);
      }
    }
  }

  return min;
}



double com::BasicTable::max(size_t i) const
{
  double max;
  pcr::setMV(max);

  for(const_iterator it = begin(i); it != end(i); ++it) {
    if(!pcr::isMV(*it)) {
      if(pcr::isMV(max)) {
        max = *it;
      }
      else {
        max = MAX(max, *it);
      }
    }
  }

  return max;
}



double com::BasicTable::value(size_t col, size_t rec) const
{
  PRECOND(col < nrCols());
  PRECOND(rec < nrRecs());

  return (*d_columns[col])[rec];
}



double& com::BasicTable::value(size_t col, size_t rec)
{
  PRECOND(col < nrCols());
  PRECOND(rec < nrRecs());

  return (*d_columns[col])[rec];
}



/*!
  \param   i Column number/index.
  \return  Iterator to the first record of column \a i.
  \sa      end(size_t)

  Column numbers range between [0 - (nrCols() - 1)].
*/
com::BasicTable::const_iterator com::BasicTable::begin(size_t i) const
{
  if(i > d_columns.size() - 1) {
    throw std::range_error("com::BasicTable::begin");
  }

  return d_columns[i]->begin();
}



/*!
  \param   i Column number/index.
  \return  Iterator to the one-past-the-last record of column \a i.
  \sa      begin(size_t)

  Column numbers range between [0 - (nrCols() - 1)].
*/
com::BasicTable::const_iterator com::BasicTable::end(size_t i) const
{
  if(i > d_columns.size() - 1) {
    throw std::range_error("com::BasicTable::end");
  }

  return d_columns[i]->end();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

namespace com {

/*!
  \param   s Output stream to write to.
  \param   t Table to write.
  \return  A reference to stream \a s.
  \sa      operator>>(std::istream &, BasicTable &)

  A BasicTable object will be written in the folowing format:
  \code
  1 2 3
  4 5 6
  7 8 9
  10 11 12
  13 14 15
  ...
  \endcode
*/
std::ostream &operator<<(std::ostream &s, const BasicTable &t)
{
  if(t.nrCols())
  {
    size_t r, c;
    for(r = 0; r < t.nrRecs(); r++)
    {
      for(c = 0; c < t.nrCols() - 1; c++)
        s << (*(t.d_columns[c]))[r] << ' ';
      s << (*(t.d_columns[c]))[r] << '\n';
    }
  }

  return s;
}

/*! Read the table \a t from \a s
  \param   s Input stream to read from.
  \param   t Table to read.
  \return  A reference to stream \a s.
  \sa      operator<<(std::istream &, const BasicTable &)
  \throws  BadStreamFormat if incorrect format
*/
std::istream &operator>>(std::istream &s, BasicTable &t)
{
  //  1. Clean and initialise t.
  //  2. Determine number of columns.
  //     Get current position.
  //     Split the line into tokens, separated by whitespace.
  //     The number of tokens is the number of columns.
  //     Create space for the column values.
  //     Reset s to saved position. (NO BUGS on VS2005)
  //  3. Read the columns.
  // 10. Return the stream.

  t.clean();                                                               // 1.

  // std::pos_type on Borland?
  // std::streampos p = s.tellg();                                            // 2.
  std::string line;
  std::getline(s, line);
  // Adjust this if separator is variable.
  std::vector<std::string> tokens = split(line);
  size_t n = tokens.size();
  t.resize(n);


  size_t i;
  size_t l = 1;    // Line number.                                         // 3.
  double v;        // Value read.
  char c;          // Character read.

//  s.seekg(p); Bugs on VS2005
// instead add the line read and start at line 2
  std::istringstream firstLine(line);
  for(i = 0; i < n; i++) {
      firstLine >> v;
      t.d_columns[i]->push_back(v);
  }

  while(s)
  {
    l++;
    for(i = 0; i < n; i++)
    {
      s >> v;

      if(!s)
      {
        // Check if this is a real error situation.
        if(!s.eof() || (s.eof() && i > 0))
        {
          std::string m = createMessage(
            "bad format: line %d of table, token %d", l, i + 1);
          throw BadStreamFormat(m);
        }
        break;
      }

      t.d_columns[i]->push_back(v);
    }

    // Check for remaining tokens.
    while(s.get(c))
    {
      if(c == '\n')                    // Ok, just some optional whitespace and
      {                                // the newline.
        break;
      }
      else if(!std::isspace(c))             // Not a newline, space or tab.
      {
        std::string m = createMessage(
                                     "bad format, line %d, token %d", l, i + 1);
        throw BadStreamFormat(m);
      }
    }
  }

  // Update number of records variable.
  for(i = 0; i < n; i++)
    t.d_nrRecs = MAX(t.d_nrRecs, t.d_columns[i]->size());

  return s;                                                               // 10.
}



} // namespace com



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


