#ifndef INCLUDED_COM_STRINGFO
#define INCLUDED_COM_STRINGFO

#include "stddefx.h"

#include <string>
#include <functional>



/*!
  \file
  This file contains a number of small function objects related to
   text string manipulations.
*/

namespace com {

/*!
 * For an alternative aproach read:
 * "Easily write and format ranges and containers in C++"
 * at http://builder.com.com/article.jhtml?id=u00220020805gcn01.htm&fromtm=e601-2
 */
class Concatenate {
  std::string d_delim;
  //! Running string concatenation
  std::string d_str;
public:
  typedef const std::string& argument_type;
  typedef void result_type;

  //! Constructor.
  /*!
    \param delim Delimeter string. Empty string result in no
                 delimeter. Default Delimeter is a single space
  */
  Concatenate(const std::string& delim): d_delim(delim) { }
  //! Constructor with single space as Default Delimeter
  Concatenate(): d_delim(" ") { }

  //! Function operator.
  /*!
    \param     value Value to add to string.
  */
  void operator()(const std::string& value) {
    if (!d_str.empty())
     d_str += d_delim;
    d_str += value;
  }

  /*!
    \returns the concatenated string
  */
  const std::string& result() const {
    return d_str;
  }
  operator const std::string& () const {
    return result();
  }
};


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
