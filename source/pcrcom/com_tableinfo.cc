#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_TABLEINFO
#include "com_tableinfo.h"
#define INCLUDED_COM_TABLEINFO
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_SPIRITFILEPARSER
#include "com_spiritfileparser.h"
#define INCLUDED_COM_SPIRITFILEPARSER
#endif
#include <boost/spirit/dynamic/while.hpp>
#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
// Module headers.



/*!
  \file
  This file contains the implementation of the TableInfo class.
*/



//------------------------------------------------------------------------------

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

namespace com {


/*!
 * Goal: detect how many columns an ascii formatted table has and if it
 * has an GeoEas formatted header.
 * GeaEas MUST have a single integer > 0 on line 2. This may conflict with
 * a 1 column-sized table with no header, that happens to have such an
 * integer on line 2.
 *
 * An example of such a conflict is
 * <pre>
 *   0.5
 *   1
 *   8
 *   3
 * </pre>
 * This could be a GeoEas file, with description "0.5", 1 column with title
 * "8" and a single record with value 3. Or it is a 1 column-no-header
 * file with 4 records.
 *
 * FTTB GeoEas format is not recognized as such if there are no 
 *  value records below
 *  the header and both the description and column names are all whitespace
 *   or numbers, example not recognized as GeoEas:
 * <pre>
 *   
 *  2
 *   
 *  0
 * </pre>
 * but this is recognized as GeoEas:
 * <pre>
 *   
 *  2
 *   
 *  title for column 2
 * </pre>
 * and this is recognized as GeoEas:
 * <pre>
 *  1
 *  2
 *  0
 *  0
 *  2 3
 * </pre>
 */


class TableInfoParser
{
public:
  typedef SpiritFileParser::iterator iterator;

  TableInfo::Layout        d_layout;
  std::string              d_description;
  std::vector<std::string> d_columnNames;

private:
  const com::PathName&     d_pn;

  std::vector<double>      d_values;
  bool                     d_continueParsing;
  bool                     d_lastColumnEmpty;
  void endLine() {
    // stop when parsed some values before this record terminator
    d_continueParsing = d_values.empty();
   }
  //! are we still not sure if it is valid plain text?
  bool continueParsing() {
    return d_continueParsing;
  }



 void parsePlainText() {
   using namespace boost::spirit;

   SpiritFileParser sfp(d_pn);

   d_continueParsing=true;
   d_values.clear();
   sfp.pi = boost::spirit::parse(sfp.begin(),sfp.end(),
    while_p(boost::bind(&com::TableInfoParser::continueParsing,this))
     [ *(eol_p[boost::bind(&com::TableInfoParser::endLine,this)])
                                  >> // allow new lines
       // some values on a line, 0 is ok, empty file
       +(real_p[append(d_values)])>> 
        (+(eol_p[boost::bind(&com::TableInfoParser::endLine,this)])
         |end_p[boost::bind(&com::TableInfoParser::endLine,this)]
         )          // terminated by some new lines or eof
     ] // [ handler() ]
    , blank_p);
   d_description.clear();
   d_columnNames.clear();
   if (sfp.pi.stop == sfp.end() && d_values.empty()) {
       d_layout = TableInfo::EMPTY;
       return;
   }
   d_layout = TableInfo::UNKNOWN;
   if (!d_continueParsing) {
     // full match, set nr of columns, with empty titles
     d_layout = TableInfo::PLAIN_TXT;
     d_columnNames.resize(d_values.size());
   }
   // else not a full match, due to parse, d_columNames remains empty
 }

 void add(iterator first,
          iterator last)
 {
    // bcc32 does not link this
    // std::string s(first,last);
    std::string s;
    for(iterator i=first; i != last; ++i)
      s+=*i;
    d_lastColumnEmpty=s.empty();
    com::removeFrontEndSpace(s);
    d_columnNames.push_back(s);
 }

 //! are there still columns to parse?
 bool columnNamesToParse() {
  return d_columnNames.size() < d_nrColsOnLine2;
 }

 size_t                   d_nrColsOnLine2;

 bool parseGeoEasHeader() {
   using namespace boost::spirit;

   SpiritFileParser sfp(d_pn);

   d_nrColsOnLine2=0;
   d_lastColumnEmpty=true;
   sfp.pi = boost::spirit::parse(sfp.begin(),sfp.end(),
      //  Line 1:
      lexeme_d[*(anychar_p[append(d_description)]-eol_p)]>> eol_p >>
      //  Line 2, a single integer:
      uint_p[assign(d_nrColsOnLine2)] >> eol_p >>
      //  Line 3 and on for column names
      while_p(boost::bind(&com::TableInfoParser::columnNamesToParse,this))
        [((*(anychar_p-eol_p))
          [boost::bind(&com::TableInfoParser::add,this,_1,_2)]
           >> eol_p )] >>
      // try first value line
      *(real_p[append(d_values)])>> !eol_p
   ,blank_p);
   if (d_lastColumnEmpty && ! d_columnNames.empty())
     d_columnNames.resize(d_columnNames.size()-1);

   removeFrontEndSpace(d_description);

   bool allTextNumeric=d_description.empty() || isDouble(d_description);
   for(size_t c=0;c<d_columnNames.size(); c++) {
     if (!d_columnNames[c].empty())
      allTextNumeric &= isDouble(d_columnNames[c]);
   }

   //! short on column names
   if (d_nrColsOnLine2 != d_columnNames.size())
     return false;

   switch(d_nrColsOnLine2) {
    case 0: return false; // not geoEas
    case 1: // only possible if some description is non-empty-numeric
             return !allTextNumeric;
    default:
            if (d_nrColsOnLine2 == d_values.size())
               return true;
              // possible no data, empty geo-Eas if some non-empty numeric
            return !allTextNumeric;
   }

   if (d_nrColsOnLine2 == d_values.size())
     return true;
   // Not recognized ??
   return false;
 }

public:
  TableInfoParser(const com::PathName& pn):
    d_pn(pn)
  {
   if (parseGeoEasHeader())
     d_layout = TableInfo::GEO_EAS;
   else
     parsePlainText();
  }
};

};

//! input constructor
/*!
 * detects the layout and the number of columns. There is no
 * garantuee that the whole table format is valid, since only the
 * first few lines are parsed.
 * \param pn an existing file of whom the info distilled
 * \throws what com::testOpenForReading() may throw
 */
com::TableInfo::TableInfo(const com::PathName& fileName):
  d_fileName(fileName),
  d_columnNamesGenerated(false)
{
  testOpenForReading(d_fileName);

  size_t twoGb=1;
  twoGb <<=16;
  twoGb--;
  if (size(d_fileName) > twoGb) {
    // > 2Gb spirit will fail,
    std::ifstream ifs;
    open(ifs,d_fileName);
    std::string s;
    char c;
    size_t nrTokens=0;
    while (ifs.get(c) && !nrTokens) {
      s += c;
      if (c == '\n') {
        // detect nr on this line
        // if empty continue
       nrTokens = com::split(s).size();
       s = "";
      }
    }
    setNrColumns(nrTokens);
  } else {
   TableInfoParser tip(d_fileName);
   d_layout      = tip.d_layout;
   d_description = tip.d_description;
   d_columnNames = tip.d_columnNames;
  }
}


com::TableInfo::~TableInfo()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
com::TableInfo& com::TableInfo::operator=(const TableInfo& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
com::TableInfo::TableInfo(const TableInfo& rhs)
{
}
*/

//! set value of layout
void com::TableInfo::setLayout(Layout layout)
{
  d_layout=layout;
}

//! get value of layoyt
com::TableInfo::Layout com::TableInfo::layout() const
{
  return d_layout;
}

//! set value of columnNamesGenerated
void com::TableInfo::generateColumnNames()
{
  POSTCOND(FALSE); // TODO
  d_columnNamesGenerated=true;
}

//! set value of description
void com::TableInfo::setDescription(const std::string& description)
{
  d_description=description;
}

//! set value of columnNames
void com::TableInfo::setColumnNames(const std::vector<std::string>& columnNames)
{
  d_columnNames=columnNames;
}

//! get value of columnNamesGenerated
const bool& com::TableInfo::columnNamesGenerated() const
{
  return d_columnNamesGenerated;
}

//! get value of description
const std::string& com::TableInfo::description() const
{
  return d_description;
}

//! get value of columnNames
const std::vector<std::string>& com::TableInfo::columnNames() const
{
  return d_columnNames;
}

//! set the nr of columns, switch to PLAIN_TXT, clear description and names
void com::TableInfo::setNrColumns(size_t nrColumns)
{
  d_description.clear();
  d_columnNames.clear();
  d_columnNames.resize(nrColumns);
  d_layout = PLAIN_TXT;
}

//! get value of nrColumns
size_t com::TableInfo::nrColumns() const
{
  return d_columnNames.size();
}

//! set value of fileName
void com::TableInfo::setFileName(const com::PathName& fileName)
{
  d_fileName=fileName;
}

//! get value of fileName
const com::PathName& com::TableInfo::fileName() const
{
  return d_fileName;
}

// nr of ascii fmt lines to skip as header before possible data
size_t com::TableInfo::nrHeaderLines() const
{
  if (layout()==GEO_EAS)
    return nrColumns()+2;
  return 0;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

