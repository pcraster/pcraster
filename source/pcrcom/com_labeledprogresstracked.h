#ifndef INCLUDED_COM_LABELEDPROGRESSTRACKED
#define INCLUDED_COM_LABELEDPROGRESSTRACKED

#include "stddefx.h"
#include "com_labeledprogresstracker.h"
#include "com_progresstracked.h"



namespace com {
  // LabeledProgressTracked declarations.
}



namespace com {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
template<typename StreamProgressTracker>
class LabeledProgressTracked:
         public ProgressTracked<LabeledProgressTracker<StreamProgressTracker> >
{

  friend class LabeledProgressTrackedTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  LabeledProgressTracked& operator=    (LabeledProgressTracked const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   LabeledProgressTracked(LabeledProgressTracked const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LabeledProgressTracked();

  /* virtual */    ~LabeledProgressTracked() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setLabel            (const std::string& label);

  void             init                (const std::string& label,
                                        size_t nrSteps);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif

