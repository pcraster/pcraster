#ifndef INCLUDED_DAL_REGULAREXPRESSIONS
#define INCLUDED_DAL_REGULAREXPRESSIONS



// External headers.
#include <string>
#ifndef INCLUDED_BOOST_REGEX
#include <boost/regex.hpp>
#define INCLUDED_BOOST_REGEX
#endif

// Project headers.

// Module headers.



namespace dal {

extern std::string const attributeNamePattern;

//! Time steps are positive integers.
extern std::string const timeStepPattern;

//! Quantiles are floating points between 0.0 and 1.0.
extern std::string const quantilePattern;

//! Extension are strings that can be empty.
extern std::string const extensionPattern;

//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Not bullet proof yet, Google for official pattern.
*/
extern std::string const dosFileNamePattern;

extern std::string const pcrStackPattern;

extern boost::regex const temporalVectorXRegex;

extern boost::regex const vectorXRegex;

extern boost::regex const quantileOfTemporalRasterRegex;

extern boost::regex const temporalRasterRegex;

extern boost::regex const quantileOfRasterRegex;

extern boost::regex dosRegex;

extern boost::regex stackRegex;

} // namespace dal

#endif
