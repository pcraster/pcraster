#include "dal_RegularExpressions.h"

#include <format>



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
       std::format("({0})_x_({1})({2})?",
        attributeNamePattern,
        timeStepPattern,
        extensionPattern));

std::regex const vectorXRegex(
       std::format("({0})_x({1})?",
       attributeNamePattern,
       extensionPattern));

std::regex const quantileOfTemporalRasterRegex(
       std::format("({0})_({1})_({2})({3})?",
       attributeNamePattern,
       timeStepPattern,
       quantilePattern,
       extensionPattern));

std::regex const temporalRasterRegex(
       std::format("({0})_({1})({2})?",
       attributeNamePattern,
       timeStepPattern,
       extensionPattern));

std::regex const quantileOfRasterRegex(
       std::format("({0})_({1})({2})?",
       attributeNamePattern,
       quantilePattern,
       extensionPattern));

std::regex dosRegex(dosFileNamePattern);

std::regex stackRegex(pcrStackPattern);

} // namespace dal

