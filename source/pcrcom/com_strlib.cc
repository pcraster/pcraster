#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_CSTDARG
#include <cstdarg>
#define INCLUDED_CSTDARG
#endif

#ifndef INCLUDED_CSTDIO
#include <cstdio>
#define INCLUDED_CSTDIO
#endif

#ifndef INCLUDED_CSTDLIB
#include <cstdlib>
#define INCLUDED_CSTDLIB
#endif

#ifndef INCLUDED_IOMANIP
#include <iomanip>
#define INCLUDED_IOMANIP
#endif

#ifndef INCLUDED_STDEXCEP
#include <stdexcept>
#define INCLUDED_STDEXCEP
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

#ifndef INCLUDED_CCTYPE
#include <cctype>
#define INCLUDED_CCTYPE
#endif

#ifndef INCLUDED_COM_STRINGFO
#include "com_stringfo.h"
#define INCLUDED_COM_STRINGFO
#endif
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif

#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"
#define INCLUDED_COM_STRCONV
#endif

//! Converts the printf-like format argument to a string.
/*! Warning: result message should be smaller than 250 chars
 */
std::string com::createMessage(const char *format, ...)
{
  std::va_list arguments;
  char message[250];

  va_start(arguments, format);
  vsprintf(message, format, arguments);
  va_end(arguments);
  return std::string(message);
}

#define NOT_A_INT    "not an integer"
#define NOT_A_SIZET  "not a positive integer"
#define NOT_A_NUMBER "not a number"
// real is most generic
#define NOT_A_REAL   NOT_A_NUMBER

static void rangeErrorIf(bool test,const char *msg)
{
  if (test)
    throw std::range_error(msg);
}

//! Converts \a s to a size_t.
/*! \exception std::range_error if \a  s is not a valid size_t
 *  \warning implementation limits valid result to 2^31 not 2^31
 */
size_t com::strToSize_t(const std::string &s)
{
  int v = strToInt(s);
  rangeErrorIf(v < 0,NOT_A_SIZET);
  return (size_t)v;
}

//! Converts \a s to a bool
/*!
   \param s  a string containing true or false
   \exception std::range_error if \a  s is not a valid bool
 */
bool com::strToBool(const std::string &s)
{
   if (s == "true")
      return true;
   if (s == "false")
      return false;
   throw std::range_error("not a boolean");
}

static char *strToInit(const std::string &s)
{
  rangeErrorIf(s.empty(),NOT_A_NUMBER);
  char *str = com::createAsCStr(s);
  errno=0;
  return str;
}

static void strToCheck(
         char *str,
         char const* endPtr,
         char const* errMsg)
{
  std::string endRemaining(endPtr);
  bool test= (endPtr == str); // nothing parsed, all space
  // then delete since rangeErrorIf may throw
  delete [] str;
  rangeErrorIf(test,errMsg);
  com::removeAllSpace(endRemaining);

  rangeErrorIf(errno == ERANGE || !endRemaining.empty(),errMsg);
}

//! Converts \a s to an  int.
/*! \exception std::range_error if \a s is not a valid int
 */
int com::strToInt(const std::string &s)
{
  char *str = strToInit(s);
  char *endPtr;
  long int v = strtol(str, &endPtr,10);
  strToCheck(str, endPtr, NOT_A_INT);
  return (int)v;
}


//! Converts \a s to a double.
/*! \exception std::range_error if \a s is not a valid double
 */
double com::strToDouble(const std::string &s)
{
  char *str = strToInit(s);
  char *endPtr;
  double v = strtod(str, &endPtr);
  strToCheck(str, endPtr, NOT_A_REAL);
  return v;
}

//! Converts \a s to a double.
/*! \exception std::range_error if \a s is not a valid double
 */
bool com::isDouble(const std::string &s)
{
  try {
    (void)strToDouble(s);
  } catch(const std::range_error&) {
    return false;
  }
  return true;
}

//! Converts \a v to a string
std::string com::size_tToStr(size_t v)
{
  return toString<size_t>(v);
}

//! Converts \a v to a string containing "true" or "false"
std::string com::boolToStr(bool v)
{
  if (v)
    return "true";
  return "false";
}

//! Converts \a v to a string
std::string com::intToStr(int v)
{
  return toString<int>(v);
}

//! Converts \a v to a string
std::string com::longToStr(long v)
{
  return toString<long>(v);
}

//! Converts \a v to a string with no spaces
std::string com::doubleToStr(double n)
{
  char aBuf[128];
  sprintf(aBuf,"%g",n);
  return std::string(aBuf);
}

//! Converts \a v to a specific formatted string
/*!
 *
 *  \bug
 *     seach google discussions on  keywords:
 *       std::setprecision gcc 4096
 *     it tells that fixed is missing
 *     At least write good tests
 *
 *  KDJ: This function has been ported to gcc-3.2.1. Check if bug is still
 *       present. Write tests!
 */
std::string com::doubleToFmtStr(double n, int w, int p)
{
  std::ostringstream s;
  s.setf(std::ios::fixed, std::ios::floatfield);
#ifndef ALPHA_OSF
  // both setw and setprecision generate a link error
  // with gcc ( search for smanip in www.deja.com/usenet )
  s << std::setw(w) << std::setprecision(p) << n;
  return s.str();
#else
  {
  char aBuf[128];
  sprintf(aBuf,"%*.*g",w,p,n);
  return std::string(aBuf);
  }
#endif
}


//! Splits the string \a s into multiple strings
/*! Each  substring of \a s
  delimited by one or more whitespace characters
  yields another entry in the result vector, each result entry
  has no whitespace at start or end of the string.
  \sa        split(const std::string&, char)
  \sa join(const std::vector<std::string>& s, const std::string& delim)
 */
std::vector<std::string> com::split(const std::string &s)
{
  std::vector<std::string> t;
  std::string::const_iterator it;

  for(it = s.begin(); it != s.end(); it++)
  {
    if(!std::isspace(*it))
    {
      std::string tmp;
      while(it != s.end() && !std::isspace(*it)) {
        tmp += *it;
        it++;
      }
      it--;

      t.push_back(tmp);
    }
  }

  return t;
}

//! join all strings in \a s into a string delimeted by \a delim
/*!
 * \param s vector of strings
 * \param delim delimeter if empty then no delimeters is used
 * \sa    split(const std::string&)
 */
std::string com::join(const std::vector<std::string>& s,
                      const std::string& delim)
{
  return com::forWhole(s,Concatenate(delim));
}


//! Splits a string into tokens at occurences of \a aCharacter.
/*!
  \param     aString String to split.
  \param     aCharacter Character to use for splitting at.
  \return    The tokens.
  \sa        split(const std::string&)
*/
std::vector<std::string> com::split(const std::string &aString, char aCharacter)
{
  std::vector<std::string> tokens;
  std::string::const_iterator it;

  for(it = aString.begin(); it != aString.end(); it++) {

    if(*it != aCharacter) {

      std::string tmp;

      while(it != aString.end() && *it != aCharacter) {
        tmp += *it;
        ++it;
      }

      --it;

      tokens.push_back(tmp);
    }
  }

  return tokens;
}



//! compare 2 string case insensitive
/*!
 * See Item 35 of Effective STL, use stricmp if avail.
 * \sa com::LessNoCase
 * \return values like the string.compare() methods
 */
int com::compareNoCase(
  const std::string& s,
  const std::string& s2)
{
#if defined(GCC)
    return strcasecmp(s.c_str(),s2.c_str());
#elif defined(BORLANDC)
    return stricmp(s.c_str(),s2.c_str());
#elif defined(_MSC_VER)

    return _stricmp(s.c_str(),s2.c_str());
#else
#error stricmp
#endif
/*
#error IS THIS OK? check Item 35 of Eff. STL
  using namespace std;
  string::const_iterator p = s.begin();
  string::const_iterator p2= s2.begin();

  while (p!=s.end() && p2 != s2.end()) {
    if ( toupper(*p) != toupper(*p2))
       return ( toupper(*p) < toupper(*p2)) ? -1 : 1;
    ++p;
    ++p2;
  }
  return s2.size()-s.size();
*/
}

//! tests 2 strings on case insensitive equality
/*! return true if equal, false if not
 */
bool com::equalNoCase(
  const std::string& s,
  const std::string& s2)
{
  return compareNoCase(s,s2)==0;
}

//! remove all space characters, incl. newline, at begin and end of string
/*!
  \sa        removeFrontEndSpace(std::string const&)
*/
void com::removeFrontEndSpace(
  std::string& str)
{
  std::string::iterator it(str.begin());
  while ( it != str.end() && std::isspace(*it))
    it++;
  str.erase(str.begin(),it);
  if (str.empty())
   return;
  it = str.end();
  do {
    it--;
  } while (it != str.begin() && std::isspace(*it));
  it++;
  str.erase(it,str.end());
}



//! Removes all space characters, incl. newline, at begin and end of string.
/*!
  \param     str Input string.
  \return    Resulting string.
  \sa        removeFrontEndSpace(std::string&)
*/
std::string com::removeFrontEndSpace(std::string const& str)
{
  std::string result(str);
  removeFrontEndSpace(result);
  return result;
}



//! remove all space characters, incl. newline, in the whole string
void com::removeAllSpace(std::string& str)
{
  std::string newStr;
  for(size_t i=0; i < str.size(); i++)
    if (!std::isspace(str[i]))
      newStr += str[i];
  str = newStr;
}



//! Removes all occurences of \a c from the start and the end of \a str.
/*!
  \param     str String to edit.
  \param     c Characters to remove.
  \sa        removeFrontEndSpace(std::string&)
*/
void com::removeFrontEndChar(std::string& str, char c)
{
  // Front of string.
  std::string::iterator it = str.begin();
  while(it != str.end() && *it == c)
    ++it;
  str.erase(str.begin(), it);

  // End of string.
  std::string::reverse_iterator rit = str.rbegin();
  while(rit != str.rend() && *rit == c)
    ++rit;
  str.erase(str.length() - (rit - str.rbegin()),
                   str.length() - (str.length() - (rit - str.rbegin())));
}



//! Removes all occurences of \a strToRemove from the start and the end of \a str.
/*!
  \param     str String to edit.
  \param     strToRemove String to remove.
  \sa        removeFrontEndSpace(std::string&),
             removeFrontEndChar(std::string&, char)
*/
void com::removeFrontEndString(std::string& str, const std::string& strToRemove)
{
  // Front of string.
  if(!strToRemove.empty()) {
    while(str.length() >= strToRemove.length() && str.substr(0,
                   strToRemove.length()) == strToRemove) {
      str.erase(0, strToRemove.length());
    }

    while(str.length() >= strToRemove.length() && str.substr(str.length() -
                   strToRemove.length(), strToRemove.length()) == strToRemove) {
      str.erase(str.length() - strToRemove.length(), strToRemove.length());
    }
  }
}



//! test 2 strings on equality discarding all isspace characters
bool com::equalNoSpace(const std::string& s1, const std::string& s2)
{
  std::string c1(s1);
  std::string c2(s2);
  removeAllSpace(c1);
  removeAllSpace(c2);
  return c1 == c2;
}

//! duplicate a std::string as new allocated C string, e.g a char[]
/*! copies \a str into a <i>new char[]</i> allocated 0-terminated
    C string. Caller is responsible for deletion, with delete []
 */
char *com::createAsCStr(const std::string& str)
{
  char *buf = new char[str.size()+1]; // +1 for \0
  return ::strcpy(buf,str.c_str());
}

std::string com::format(const std::string& str, size_t offset, size_t width)
{
  std::string formattedString;

  std::string::const_iterator begin = str.begin(), end;

  while(begin != str.end()) {

    // Add the offset.
    formattedString += std::string(offset, ' ');

    // Determine string of chars to consider.
    if(std::distance(begin, str.end()) <= static_cast<int>(width - offset)) {

      end = str.end();
      formattedString += std::string(begin, end);
    }
    else {

      end = begin + (width - offset);
      POSTCOND(end > begin && end < str.end());

      // Find first whitespace character, starting from the end.
      while((end != begin) && !std::isspace(*end)) {
        --end;
      }

      // end is equal to begin or end points to a whitespace char.
      if(end == begin) {
        // It is not possible to find a suitable place to write a newline.
        end = begin + (width - offset);
        formattedString += std::string(begin, end);
      }
      else {
        // end is on a whitespace character.
        formattedString += std::string(begin, end);
        ++end;
      }

      formattedString += std::string("\n");
    }

    begin = end;
  }

  return formattedString;
}



/*!
 * \brief
 *   replace all characters in \a str that are in \a charsToBeReplaced by
 *   the character \a resultChar
 *
 *   \code
 *  str = "b.b.b";
 *  replaceChars(str,'_',".");
 *  cu_assert(str == "b_b_b");
 *   \endcode
 *
 *   \code
 * str = "b.b.b";
 * replaceChars(str,'_',".b");
 * cu_assert(str == "_____");
 *   \endcode
 */
void com::replaceChars(std::string& str, char resultChar,
                         const std::string& charsToBeReplaced)
{
  std::set<char> replaceSet;
  com::insertTo(charsToBeReplaced, replaceSet);
  for(size_t i=0; i < str.size(); i++)
    if (replaceSet.count(str[i]))
      str[i]=resultChar;
}

/*!
 * \brief
 *   replace each occurence of character \a c in \a str with the
 *   \a with argument
 *
 *   \code
 *  str = "b.b.b";
 *  std::string result(replaceCharByStr(str,'.',"XX"));
 *  cu_assert(result == "bXXbXXb");
 *   \endcode
 */
std::string com::replaceCharByStr(const std::string& str,char c,
                               const std::string& with)
{
  std::string d;
  for(size_t i=0; i<str.size(); i++) {
    if (str[i] == c)
      d+=with;
    else
      d+=str[i];
  }
  return d;
}

/*!
 * \brief
 *   replace each occurence of \a subStr in \a str with the
 *   \a newSubStr argument
 *
 *   \code
 *  str = "b..b..b";
 *  std::string result(replaceCharByStr(str,"..","XX"));
 *  cu_assert(result == "bXXbXXb");
 *   \endcode
 */
std::string com::replaceStrByStr(const std::string& str,const std::string& subStr,
                                 const std::string& newSubStr)
{
  std::string d;

  if(subStr.empty()) {
    d = str;
  }
  else {
    for(size_t i=0; i<str.size(); /* inc in loop */ )
      if (str.find(subStr,i) == i) {
        d+=newSubStr;
        i+=subStr.size();
      } else {
        d+=str[i];
        i++;
      }
  }

  return d;
}
