/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_migrationdirection.h"
#include "pcrxml_enumnmtoken.h"

namespace pcrxml
{
template <> const char *EnumNmToken<MigrationDirection>::d_strings[] = {"E2W", "W2E", "N2S", "S2N"};

//! generated from DTD
template <>
const size_t
    EnumNmToken<MigrationDirection>::d_nrStrings(ARRAY_SIZE(EnumNmToken<MigrationDirection>::d_strings));
//! generated from DTD
static const EnumNmToken<MigrationDirection> classMigrationDirection;
}  // namespace pcrxml

pcrxml::MigrationDirection::MigrationDirection(const QDomNode &owningElement,
                                               const std::string &nameOfAttr, bool req)
    : Attribute(owningElement, nameOfAttr, req)
{
  if (present())
    d_value = classMigrationDirection.value(owningElement, nameOfAttr);
}

pcrxml::MigrationDirection::MigrationDirection() : Attribute(false)
{
}

pcrxml::MigrationDirection::MigrationDirection(EnumType value) : Attribute(true), d_value(value)
{
}

std::string pcrxml::MigrationDirection::attrValueStr() const
{
  return classMigrationDirection.attrValueStr(value());
}
