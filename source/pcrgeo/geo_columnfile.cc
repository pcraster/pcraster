#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_COLUMNFILE
#include "geo_columnfile.h"
#define INCLUDED_GEO_COLUMNFILE
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_ITERATOR
#include <iterator>
#define INCLUDED_ITERATOR
#endif

#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
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

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------



geo_ColumnFile::geo_ColumnFile(bool autoDelete)

  : d_state(INITIAL), d_nrVars(0), d_ad(autoDelete)

{
}



geo_ColumnFile::geo_ColumnFile(const std::string &fn, bool autoDelete)

  : d_state(INITIAL), d_fn(fn), d_nrVars(0), d_ad(autoDelete)

{
}



geo_ColumnFile::~geo_ColumnFile()
{
}



/*!
  \warning All data will be erased!
*/
void geo_ColumnFile::initialise()
{
  d_fn = "";
  d_fs.close();
  d_nrVars = 0;
  d_varNames.erase(d_varNames.begin(), d_varNames.end());
  d_selVars.erase(d_selVars.begin(), d_selVars.end());
  d_title = "";

  if(d_ad)
  {
    std::vector<std::vector<double> *>::iterator it;
    for(it = d_data.begin(); it != d_data.end(); it++)
      delete *it;
  }

  d_data.erase(d_data.begin(), d_data.end());

  d_state = INITIAL;
}



/*!
  \param   fn New filename to set.
  \warning If a previous filename has been set it will be replaced. All
           settings and data will be erased (see initialise()).
*/
void geo_ColumnFile::setFilename(const std::string &fn)
{
  if(d_state != INITIAL)
    initialise();

  d_fn = fn;
}



/*!
  \warning The result is undefined if open() is called more than once.
  \sa      The filename must be known to the object.
*/
void geo_ColumnFile::open()
{
  PRECOND(!d_fn.empty());
  PRECOND(d_state == INITIAL);

  com::open(d_fs, d_fn);
}



/*!
  \sa      readHeader(), readData()

  Use this function if you want to read the whole column file in one time.
*/
void geo_ColumnFile::read()
{
  PRECOND(d_state == INITIAL);

  readHeader();
  readData();
}



/*!
  \sa      read(), readData()

  If the file has a simple column format without a header, than the filename
  will be used as the title of the file and the variable names will be the
  numbers 1, 2, 3, ..., nrVars().
*/
void geo_ColumnFile::readHeader()
{
  PRECOND(d_state == INITIAL);
  PRECOND(!d_fn.empty());

  // Open the file for reading.
  open();

  if(geoEasFormat())
  {
    getline(d_fs, d_title);
    d_fs >> d_nrVars;
    d_fs.ignore(1000, '\n');

    d_varNames.resize(d_nrVars);

    for(size_t i = 0; i < d_nrVars; i++)
    {
      getline(d_fs, d_varNames[i]);
    }
  }
  else // Standard column file.
  {
    // Use filename as title.
    d_title = d_fn;

    // Number of vars equals the number of doubles on the first line.
    std::string buf;
    std::ifstream fs;
    com::open(fs, d_fn);

    // Read the first line.
    getline(fs, buf);

    // Split the line into tokens, separated by whitespace.
    std::vector<std::string> t = com::split(buf);

    // Check if the tokens are doubles.
    std::vector<std::string>::const_iterator it;
    for(it = t.begin(); it != t.end(); it++)
    {
     try {
      (void)com::strToDouble(*it);
     } catch (std::range_error)  {
        throw com::FileFormatError(d_fn, "expected a number");
      }
    }

    d_nrVars = t.size();
    d_varNames.resize(d_nrVars);

    for(size_t i = 0; i < d_nrVars; i++)
      d_varNames[i] = com::intToStr(i + 1);
  }

  d_state = HEADER_READ;
}



/*!
  \sa      read(), readHeader()

  If no columns are selected for reading, all columns will be read.
*/
void geo_ColumnFile::readData()
{
  PRECOND(d_state == HEADER_READ);

  //  1. Create a buffer for the values.
  //  2. If no selection is made of the columns to read, every column will be
  //     read into memory.
  //  3. Reserve memory for values in d_data.
  //  4. Read the expected number of doubles into memory.
  //  5. Unable to read the expected amount of doubles.
  //  6. Copy values for selected columns.

  size_t i = 0;
  std::vector<double> buffer;                                              // 1.
  buffer.resize(d_nrVars);

  if(d_selVars.empty())                                                    // 2.
  {
    for(i = 1; i <= d_nrVars; i++)
      d_selVars.insert(i);
  }

  if(d_ad)
  {
    std::vector<std::vector<double> *>::iterator it;
    for(it = d_data.begin(); it != d_data.end(); it++)
      delete *it;
  }

  d_data.resize(d_selVars.size());                                         // 3.

  std::vector<std::vector<double> *>::iterator it;
  for(it = d_data.begin(); it != d_data.end(); it++)
    *it = new std::vector<double>;

  std::set<size_t>::const_iterator setIt;

  size_t n = 0;             // Number of lines.

  while(d_fs)               // Do for the rest of the file...
  {
    for(i = 0; i < d_nrVars && d_fs; i++)                                  // 4.
      d_fs >> buffer[i];

    if(d_fs.eof()) break;  // Possibly eof reached.

    n++;                   // We've read a line.

    if(!d_fs)                                                              // 5.
    {
      std::string m = com::createMessage(
                     "row %Zd: bad format; floating point number expected", n);
      throw com::FileFormatError(d_fn,m);
    }

    for(setIt = d_selVars.begin(), i = 0; setIt != d_selVars.end();        // 6.
        setIt++, i++)
    {
      d_data[i]->push_back(buffer[(*setIt - 1)]);
    }
  }

  d_state = DATA_READ;
}



/*!
  \param   n Variable/column number to select.
  \throws com::FileError exception will be thrown if \a n is not within the
           valid range of 1 - nrVars().
*/
void geo_ColumnFile::selVar(size_t n)
{
  if(n > d_nrVars)
  {
    std::string m = com::createMessage(
                          "only %Zd variables in %s and variable %Zd requested",
                          d_nrVars, d_fn.c_str(), n);
    throw com::FileError(d_fn,m);
  }

  d_selVars.insert(n);
}



void geo_ColumnFile::setAutoDelete(bool d)
{
  d_ad = d;
}



const std::string &geo_ColumnFile::title() const
{
  return d_title;
}



size_t geo_ColumnFile::nrVars() const
{
  return d_nrVars;
}



size_t geo_ColumnFile::nrSelVars() const
{
  return d_selVars.size();
}



/*!
  \param   n Column/variable number for which the name should be returned.
  \return  The name of variable \a n.
  \warning The result is undefined if \n is outside the range of possible
           numbers (1 - nrVars()).
*/
const std::string &geo_ColumnFile::varName(size_t n) const
{
  if(n > d_varNames.size())
    throw std::range_error("geo_ColumnFile::varName(size_t)");

  return d_varNames[n - 1];
}



/*!
  \return  true if the file has the geo-eas format, false if not.
*/
bool geo_ColumnFile::geoEasFormat() const
{
  PRECOND(!d_fn.empty());

  std::ifstream fs;
  com::open(fs, d_fn);
  return geoEasFormat(fs);
}



/*!
  \overload
*/
bool geo_ColumnFile::geoEasFormat(std::ifstream &fs) const
{
  // We asume that a file has the geo-eas format, if the second line of the
  // file contains one integer.

  // Ignore the first line.
  fs.ignore(1000, '\n');
  if(!fs) return false;
  std::string buf;

  // Read the second line.
  getline(fs, buf);
  if(!fs) return false;

  // Check if the string contains an integer and nothing more.
  try {
  com::strToInt(buf);
	} catch (std::range_error) {
		return false;
	}
  return true;
}



/*!
  \return  The number of records in the file.
  \warning This function returns only useful information if the readData()
           member has been called and data has been actually read.
*/
size_t geo_ColumnFile::nrRecs() const
{
  return d_data.empty() ? 0 : d_data[0]->size();
}



/*!
  \param   n The number of the column for which an iterator has to be
             returned.
  \return  An iterator to the first value of column \a n.
  \warning The result is undefined if column \a n is not read.
  \sa      end()
*/
geo_ColumnFile::const_iterator geo_ColumnFile::begin(size_t n) const
{
  std::set<size_t>::const_iterator it = d_selVars.find(n);

  if(it == d_selVars.end())
    throw std::range_error("geo_ColumnFile::begin(size_t)");

  size_t i(0);
#ifdef BORLANDC
  std::distance(d_selVars.begin(), it, i);
#else
  i = std::distance(d_selVars.begin(), it);
#endif

  return d_data[i]->begin();
}



/*!
  \param   n The number of the column for which an iterator has to be
             returned.
  \return  An iterator to the 'one past the last' value of column \a n.
  \warning The result is undefined if column \a n is not read.
  \sa      begin()
*/
geo_ColumnFile::const_iterator geo_ColumnFile::end(size_t n) const
{
  std::set<size_t>::const_iterator it = d_selVars.find(n);

  if(it == d_selVars.end())
    throw std::range_error("geo_ColumnFile::end(size_t)");

  size_t i(0);
#ifdef BORLANDC
  std::distance(d_selVars.begin(), it, i);
#else
  i = std::distance(d_selVars.begin(), it);
#endif

  return d_data[i]->end();
}



std::vector<double> *geo_ColumnFile::data(size_t n) const
{
  std::set<size_t>::const_iterator it = d_selVars.find(n);

  if(it == d_selVars.end())
    throw std::range_error("geo_ColumnFile::data(size_t)");

  size_t i(0);
#ifdef BORLANDC
  std::distance(d_selVars.begin(), it, i);
#else
  i = std::distance(d_selVars.begin(), it);
#endif

  return d_data[i];
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------

/*!
  \var geo_ColumnFile::INITIAL

  Header and data part of file have not been read yet.
*/

/*!
  \var geo_ColumnFile::HEADER_READ

  Header of column file has been read. Information about the title, number of
  variables and the name of the variables is available.
*/

/*!
  \var geo_ColumnFile::DATA_READ

  Data of the column file has been read. Information about the data values and
  the number of records in the file is available.
*/



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


