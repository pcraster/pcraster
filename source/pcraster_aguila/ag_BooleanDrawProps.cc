#include "ag_BooleanDrawProps.h"
#include <cassert>
#include "pcrtypes.h"
#include "com_classclassifier.h"
#include "ag_ColourSelector.h"



/*!
  \file
  This file contains the implementation of the BooleanDrawProps class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     p Palette.
  \param     c Class info object.
*/
ag::BooleanDrawProps::BooleanDrawProps(const std::string& title,
         const com::RawPalette* p, const com_ClassClassifier<UINT1>* c)

  : ClassDrawProps(title, p),
    _classifier(c)

{
  assert(c);

  reMapColours();
  _nrClasses = _classifier->nrClasses();
  for(size_t i = 0; i < _classifier->nrClasses(); ++i) {
    _labels.push_back(_classifier->descr(i));
  }
}



ag::BooleanDrawProps::BooleanDrawProps(const BooleanDrawProps& properties)

  : ClassDrawProps(properties),
    _classifier(properties._classifier)

{
}



//! Destructor.
/*!
  The classifier is for use only and is not deleted here.
*/
ag::BooleanDrawProps::~BooleanDrawProps()
{
}



void ag::BooleanDrawProps::reMapColours()
{
  _colours = mapSequential(*palette(), _classifier->nrClasses());
}



//! Returns the class info object.
/*!
  \return    Class info object.
*/
const com_ClassClassifier<UINT1>& ag::BooleanDrawProps::classifier()
                   const
{
  assert(_classifier);
  return *_classifier;
}



std::string ag::BooleanDrawProps::label(UINT1 const& value)
{
  std::string result = "mv";

  if(!pcr::isMV(value)) {
    size_t index = classifier().index(value);
    result = classifier().descr(index);
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


