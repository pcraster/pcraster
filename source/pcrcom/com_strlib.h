#ifndef INCLUDED_COM_STRLIB
#define INCLUDED_COM_STRLIB

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

namespace com
{
  int  compareNoCase(const std::string& s, const std::string& s2);

  //! function object as needed in std::set criterion and other STL containers
  /*!
   * identical to std::less<std::string> but then case insensitive
   * \sa int compareNoCase(const std::string& s, const std::string& s2);
   */
  struct StringLessNoCase {
    bool operator()(const std::string& s, const std::string& s2) const {
        return compareNoCase(s,s2) < 0;
    }
  };

  bool equalNoCase(const std::string& s, const std::string& s2);

  bool equalNoSpace(const std::string& s, const std::string& s2);

  std::string      createMessage       (const char *format, ...);

  std::vector<std::string> split       (const std::string &s);

  std::string              join        (const std::vector<std::string>& s,
                                        const std::string& delim=" ");

  std::vector<std::string> split       (const std::string &aString,
                                        char aCharacter);

  size_t           strToSize_t         (const std::string &s);
  bool             strToBool           (const std::string &s);
  int              strToInt            (const std::string &s);
  double           strToDouble         (const std::string &s);

  bool             isDouble            (const std::string &s);

  std::string      longToStr           (long l);
  std::string      size_tToStr         (size_t n);
  std::string      boolToStr           (bool   n);
  std::string      intToStr            (int i);
  std::string      doubleToFmtStr      (double n, int  w = 0, int   p = 6);
  std::string      doubleToStr         (double n);

  void        replaceChars             (std::string& str, char resultChar,
                                        const std::string& charsToBeReplaced);

  std::string replaceCharByStr         (const std::string& str,char c,
                                        const std::string& with);

  std::string replaceStrByStr          (const std::string& str,const std::string& subStr,
                                        const std::string& newSubStr);

  void             removeFrontEndSpace (std::string& str);

  std::string      removeFrontEndSpace (std::string const& str);

  void             removeAllSpace      (std::string& str);

  void             removeFrontEndChar  (std::string& str,
                                        char c);

  void             removeFrontEndString(std::string& str,
                                        const std::string& strToRemove);

  char*            createAsCStr        (const std::string& str);

  std::string      format              (const std::string& str,
                                        size_t offset,
                                        size_t width);


} // namespace com

#endif
