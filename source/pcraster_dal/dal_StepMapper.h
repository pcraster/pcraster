#ifndef INCLUDED_DAL_STEPMAPPER
#define INCLUDED_DAL_STEPMAPPER



// Library headers.
#ifdef DEBUG_DEVELOP
  #ifndef INCLUDED_IOSTREAM
  #include <iostream>
  #define INCLUDED_IOSTREAM
  #endif
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif



namespace dal {
  // StepMapper declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class PCR_DAL_DECL StepMapper
{

  friend class StepMapperTest;

private:

  double           d_sourceFirstStep;

  double           d_sourceLastStep;

  double           d_destinationFirstStep;

  double           d_destinationLastStep;

  double           d_conversionFactor;

  void             determineConversionFactor();

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   StepMapper          ();

                   StepMapper          (double sourceFirstStep,
                                        double sourceLastStep,
                                        double destinationFirstStep,
                                        double destinationLastStep);

  virtual          ~StepMapper         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  StepMapper&      operator|=          (StepMapper const& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  double           sourceFirstStep     () const;

  double           sourceLastStep      () const;

  double           destinationFirstStep() const;

  double           destinationLastStep () const;

  double           destination         (double sourceStep) const;

  double           source              (double destinationStep) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

#ifdef DEBUG_DEVELOP
std::ostream&      operator<<          (std::ostream& stream,
                                        StepMapper const& mapper);
#endif



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
