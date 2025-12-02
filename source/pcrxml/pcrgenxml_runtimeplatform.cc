/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_runtimeplatform.h"
#include "pcrxml_enumnmtoken.h"

namespace pcrxml
{
template <> const char *EnumNmToken<RuntimePlatform>::d_strings[] = {"RP_WIN32", "RP_LINUX"};

//! generated from DTD
template <>
const size_t
    EnumNmToken<RuntimePlatform>::d_nrStrings(ARRAY_SIZE(EnumNmToken<RuntimePlatform>::d_strings));
//! generated from DTD
static const EnumNmToken<RuntimePlatform> classRuntimePlatform;
}  // namespace pcrxml

pcrxml::RuntimePlatform::RuntimePlatform(const QDomNode &owningElement, const std::string &nameOfAttr,
                                         bool req)
    : Attribute(owningElement, nameOfAttr, req)
{
  if (present())
    d_value = classRuntimePlatform.value(owningElement, nameOfAttr);
}

pcrxml::RuntimePlatform::RuntimePlatform() : Attribute(false)
{
}

pcrxml::RuntimePlatform::RuntimePlatform(EnumType value) : Attribute(true), d_value(value)
{
}

std::string pcrxml::RuntimePlatform::attrValueStr() const
{
  return classRuntimePlatform.attrValueStr(value());
}
