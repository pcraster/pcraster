#include "value_scale_traits.h"


namespace pcraster {

std::string const ValueScaleTraits<VS_B>::name("Boolean");
std::string const ValueScaleTraits<VS_L>::name("LDD");
std::string const ValueScaleTraits<VS_N>::name("Nominal");
std::string const ValueScaleTraits<VS_O>::name("Ordinal");
std::string const ValueScaleTraits<VS_S>::name("Scalar");
std::string const ValueScaleTraits<VS_D>::name("Directional");

ValueScaleTraits<VS_S>::Type const ValueScaleTraits<VS_S>::minimum = -FLT_MAX;
ValueScaleTraits<VS_S>::Type const ValueScaleTraits<VS_S>::maximum = FLT_MAX;

ValueScaleTraits<VS_D>::Type const ValueScaleTraits<VS_D>::minimum = -FLT_MAX;
ValueScaleTraits<VS_D>::Type const ValueScaleTraits<VS_D>::maximum = FLT_MAX;

} // namespace pcraster
