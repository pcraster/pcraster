#include "stddefx.h"
#include "com_table.h"
#include "com_basictable.h"
#include "com_exception.h"
#include "com_file.h"
#include "com_pathname.h"
#include "com_strconv.h"
#include "com_strlib.h"

#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

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
com::Table::Table()

    : BasicTable()

{
}

com::Table::Table(size_t nrCols, size_t nrRecs)

    : BasicTable(nrCols, nrRecs)

{
  d_names.resize(nrCols);

  for (size_t i = 0; i < d_names.size(); ++i) {
    // Default name for column is the column number.
    d_names[i] = com::toString(i + 1);
  }
}

com::Table::Table(com::PathName const &fileName)
{
  std::ifstream stream;
  open(stream, fileName);
  stream >> *this;
}

com::Table::~Table()
{
  clean();
}

void com::Table::clean()
{
  d_names.erase(d_names.begin(), d_names.end());
}

const std::string &com::Table::descr() const
{
  return d_descr;
}

/*!
  \param     i Column index.
  \return    Name of the column with index \a i.
  \exception std::range_error Column index out of range.
  \deprecated Use label(size_t).

  Column indices range between [0 - (nrCols() - 1)]
*/
const std::string &com::Table::name(size_t i) const
{
  if (i >= d_names.size()) {
    throw std::range_error("com::Table::name");
  }
  return d_names[i];
}

void com::Table::setLabel(size_t col, std::string const &label)
{
  PRECOND(col < d_names.size());
  d_names[col] = label;
}

std::string const &com::Table::label(size_t col) const
{
  PRECOND(col < d_names.size());

  return d_names[col];
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

namespace com
{


//! Output operator for writing Table objects to an output stream.
/*!
  \param   s Output stream to write to.
  \param   t Table to write.
  \return  A reference to the stream.

  The table is written in "simplified" geo-eas format:
  \code
  description
  nr-of-cols
  name1
  name2
  ...
  1 2 3 ...
  4 5 6 ...
  7 8 9 ...
  ...
  \endcode
*/
std::ostream &operator<<(std::ostream &s, const Table &t)
{
  s << t.d_descr << '\n';
  s << t.nrCols() << '\n';
  for (size_t i = 0; i < t.nrCols(); i++) {
    s << t.d_names[i] << '\n';
  }
  s << (BasicTable &)t;
  return s;
}

//! Input operator for reading Table objects from an input stream.
/*!
  \param   s Output stream to read from.
  \param   t Table to read.
  \return  A reference to the stream.
  \throws  com::BadStreamFormat if contents is not in correct format

  \todo
    Table en BasicTable moeten alle functionaliteit van apps/readcols.c bevatten, die
    zowel tss als xyz data  inleest, maar niet lookuptables.
    Zie ook geo_util.h: geoEasFormat en isTimeSeriesFile.
    De grote wijziging zit in het inlezen van de tabellen, vereisten
    <ul>
     <li>foutmelding met regelnr. ( kolomnr ook wenselijk)
     <li>validator per kolom, voor geldig scalair, boolean of ldd waarde b.v.
     <li>input transformator per kolom, b.v. directional input moet afh.
          van global setting naar radians worden omgezet
     <li>scheidingsteken, default whitespace, instelbaar, b.v. , of \\t
    </ul>

    Uitdaging: ontwerp een algemene structuur voor het inlezen van ascii
    geformatteerde tabellen, flexibel maar met juiste foutmeldingen: de data
    moet on-the-fly tijdens het inlezen gechecked kunnen worden ( vandaar de
    validator en transformator) zodat we niet de regel en kolom nrs. hoeven
    te onthouden.

    Kunnen we het met boost::spirit en ERRUG goede foutmeldingen het oplossen?

    Plekken  met te vervangen parse code:
     <ol>
      <li>apps/readcols.c
      <li>tab/readltab.c
      <li>com::createIntervalFromLookupTableKey (nu boost::spirit)
     </ol>

     ook op later tijdstip inhoud tss checken
  \endtodo

*/
std::istream &operator>>(std::istream &s, Table &t)
{
  //  1. Clean and initialise t.

  t.clean();  // 1.

  // Read the description.
  std::getline(s, t.d_descr);
  if (!s) {
    throw com::BadStreamFormat("bad format: header corrupt");
  }


  // Read the number of variables.
  size_t n = 0;
  s >> n;
  if (!s) {
    throw com::BadStreamFormat("bad format: header corrupt");
  }
  t.d_names.resize(n);
  char c = 0;
  while (s.get(c) && c != '\n') {
    ;
  }

  // Read the column names.
  for (size_t i = 0; i < n; i++) {
    std::getline(s, t.d_names[i]);
  }
  if (!s) {
    throw com::BadStreamFormat("bad format: header corrupt");
  }

  // Read the cells.
  s >> (BasicTable &)t;

  // Perform some checks.
  if (t.nrRecs() == 0) {
    // Empty body. Create datastructure for number of columns in header.
    t.resize(t.nrCols());
  } else if (n != t.nrCols()) {  // Check if header is consistent with header.
    throw com::BadStreamFormat("bad format: wrong number of columns");
  }

  return s;
}

}  // namespace com

//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------
