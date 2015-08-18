#ifndef INCLUDED_DAL_STEPMAPPER
#include "dal_StepMapper.h"
#define INCLUDED_DAL_STEPMAPPER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif



/*!
  \file
  This file contains the implementation of the StepMapper class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC STEPMAPPER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STEPMAPPER MEMBERS
//------------------------------------------------------------------------------

StepMapper::StepMapper()

  : d_sourceFirstStep(),
    d_sourceLastStep(),
    d_destinationFirstStep(),
    d_destinationLastStep()

{
}



StepMapper::StepMapper(
         double sourceFirstStep,
         double sourceLastStep,
         double destinationFirstStep,
         double destinationLastStep)

  : d_sourceFirstStep(sourceFirstStep),
    d_sourceLastStep(sourceLastStep),
    d_destinationFirstStep(destinationFirstStep),
    d_destinationLastStep(destinationLastStep)

{
  // TODO make these size_t arguments?
  assert(comparable(fmod(sourceFirstStep, 1.0)      + 1.0, 0.0 + 1.0));
  assert(comparable(fmod(sourceLastStep, 1.0)       + 1.0, 0.0 + 1.0));
  assert(comparable(fmod(destinationFirstStep, 1.0) + 1.0, 0.0 + 1.0));
  assert(comparable(fmod(destinationLastStep, 1.0)  + 1.0, 0.0 + 1.0));

  determineConversionFactor();
}



/* NOT IMPLEMENTED
//! Copy constructor.
StepMapper::StepMapper(
         StepMapper const& rhs)

  : Base(rhs)

{
}
*/



StepMapper::~StepMapper()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
StepMapper& StepMapper::operator=(
         StepMapper const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



void StepMapper::determineConversionFactor()
{
  if(comparable(d_sourceLastStep, d_sourceFirstStep)) {
    d_conversionFactor = 0.0;
  }
  else {
    d_conversionFactor = (d_destinationLastStep - d_destinationFirstStep) /
         (d_sourceLastStep - d_sourceFirstStep);
  }
}



StepMapper& StepMapper::operator|=(
         StepMapper const& rhs)
{
  assert(
  (smallerOrComparable(d_sourceFirstStep, d_sourceLastStep) &&
   smallerOrComparable(rhs.d_sourceFirstStep, rhs.d_sourceLastStep))
  ||
  (!smallerOrComparable(d_sourceFirstStep, d_sourceLastStep) &&
   !smallerOrComparable(rhs.d_sourceFirstStep, rhs.d_sourceLastStep))
  );
  assert(
  (smallerOrComparable(d_destinationFirstStep, d_destinationLastStep) &&
   smallerOrComparable(rhs.d_destinationFirstStep, rhs.d_destinationLastStep))
  ||
  (!smallerOrComparable(d_destinationFirstStep, d_destinationLastStep) &&
   !smallerOrComparable(rhs.d_destinationFirstStep, rhs.d_destinationLastStep))
  );

  if(smallerOrComparable(d_sourceFirstStep, d_sourceLastStep)) {
    d_sourceFirstStep = std::min(d_sourceFirstStep, rhs.d_sourceFirstStep);
    d_sourceLastStep = std::max(d_sourceLastStep, rhs.d_sourceLastStep);
  }
  else {
    d_sourceFirstStep = std::max(d_sourceFirstStep, rhs.d_sourceFirstStep);
    d_sourceLastStep = std::min(d_sourceLastStep, rhs.d_sourceLastStep);
  }

  if(smallerOrComparable(d_destinationFirstStep, d_destinationLastStep)) {
    d_destinationFirstStep =
         std::min(d_destinationFirstStep, rhs.d_destinationFirstStep);
    d_destinationLastStep =
         std::max(d_destinationLastStep, rhs.d_destinationLastStep);
  }
  else {
    d_destinationFirstStep =
         std::max(d_destinationFirstStep, rhs.d_destinationFirstStep);
    d_destinationLastStep =
         std::min(d_destinationLastStep, rhs.d_destinationLastStep);
  }

  determineConversionFactor();

  return *this;
}



double StepMapper::sourceFirstStep() const
{
  return d_sourceFirstStep;
}



double StepMapper::sourceLastStep() const
{
  return d_sourceLastStep;
}



double StepMapper::destinationFirstStep() const
{
  return d_destinationFirstStep;
}



double StepMapper::destinationLastStep() const
{
  return d_destinationLastStep;
}



double StepMapper::destination(
         double sourceStep) const
{
  return d_destinationFirstStep +
         (sourceStep - d_sourceFirstStep) * d_conversionFactor;
}



double StepMapper::source(
         double destinationStep) const
{
  if(comparable(d_conversionFactor, 0.0)) {
    return 0.0;
  }
  else {
    return d_sourceFirstStep +
         (destinationStep - d_destinationFirstStep) / d_conversionFactor;
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

#ifdef DEBUG_DEVELOP
std::ostream& operator<<(
         std::ostream& stream,
         StepMapper const& mapper)
{
  stream
    << mapper.sourceFirstStep() << ' ' << mapper.sourceLastStep()
    << " -> "
    << mapper.destinationFirstStep() << ' ' << mapper.destinationLastStep();

  return stream;
}
#endif



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

