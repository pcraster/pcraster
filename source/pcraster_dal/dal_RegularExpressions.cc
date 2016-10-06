#ifndef INCLUDED_DAL_REGULAREXPRESSIONS
#include "dal_RegularExpressions.h"
#define INCLUDED_DAL_REGULAREXPRESSIONS
#endif

// External headers.
#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

// Project headers.

// Module headers.



namespace {

} // Anonymous namespace



namespace dal {

std::string const attributeNamePattern =
       "[[:alpha:]](?:[[:alnum:]]|_)*"; // [[:alnum:]]";

std::string const timeStepPattern = "[[:digit:]]+";

std::string const quantilePattern = "0\\.[[:digit:]]+";

std::string const extensionPattern = "\\.[[:alnum:]]*";

std::string const dosFileNamePattern = "[[:alnum:]]{8}\\.[[:alnum:]]{3}";

//! PCRaster stack pattern, given DOS file name convention and dot removed.
std::string const pcrStackPattern =
       "([[:alpha:]](?:[[:alnum:]]*[[:alpha:]])?)([[:digit:]]+)";



//! Regular expression for matching members of a file based temporal vector attribute.
/*!
  \code
  <name>_<x|y>_<timestep>{.extension}
  \endcode

  Groups created:
  - attribute name
  - time step
  - extension
*/
std::regex const temporalVectorXRegex(
       (boost::format("(%1%)_x_(%2%)(%3%)?")
        % attributeNamePattern
        % timeStepPattern
        % extensionPattern).str());

std::regex const vectorXRegex(
       (boost::format("(%1%)_x(%2%)?")
       % attributeNamePattern
       % extensionPattern).str());

std::regex const quantileOfTemporalRasterRegex(
       (boost::format("(%1%)_(%2%)_(%3%)(%4%)?")
       % attributeNamePattern
       % timeStepPattern
       % quantilePattern
       % extensionPattern).str());

std::regex const temporalRasterRegex(
       (boost::format("(%1%)_(%2%)(%3%)?")
       % attributeNamePattern
       % timeStepPattern
       % extensionPattern).str());

std::regex const quantileOfRasterRegex(
       (boost::format("(%1%)_(%2%)(%3%)?")
       % attributeNamePattern
       % quantilePattern
       % extensionPattern).str());

std::regex dosRegex(dosFileNamePattern);

std::regex stackRegex(pcrStackPattern);

} // namespace dal

