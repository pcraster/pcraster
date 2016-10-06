#ifndef INCLUDED_DAL_REGULAREXPRESSIONS
#define INCLUDED_DAL_REGULAREXPRESSIONS



// External headers.
#include <string>
#include <regex>

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

extern std::regex const temporalVectorXRegex;

extern std::regex const vectorXRegex;

extern std::regex const quantileOfTemporalRasterRegex;

extern std::regex const temporalRasterRegex;

extern std::regex const quantileOfRasterRegex;

extern std::regex dosRegex;

extern std::regex stackRegex;

} // namespace dal

#endif
