#ifndef INCLUDED_COM_PROGRESSTRACKED
#define INCLUDED_COM_PROGRESSTRACKED



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace com {
  // ProgressTracked declarations.
  class ProgressTracker;
}



namespace com {



//! The ProgressTracked class is for objects whose progress is tracked.
/*!
  For some objects it is useful to track the progress of (one of) its processes.
  This can be useful, for example, if you want to give the user some sort of
  feedback about the current progress.
*/
template<typename ProgressTracker>
class ProgressTracked
{

  friend class ProgressTrackedTest;

private:

  //! Pointer to ProgressTracker object.
  ProgressTracker* d_tracker;

  //! Assignment operator. NOT IMPLEMENTED.
  ProgressTracked& operator=           (ProgressTracked const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ProgressTracked     (ProgressTracked const& rhs);

protected:

  void             setNrSteps          (size_t nrSteps);

  // virtual void     clear               ();

  // virtual void     init                ();

  void             init                (size_t nrSteps);

  void             finishedStep        ();

  void             finishedSteps       ();

  void             finishedSteps       (size_t nrSteps);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ProgressTracked     ();

  virtual          ~ProgressTracked    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setTracker          (ProgressTracker* tracker);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  ProgressTracker* tracker             ();

  size_t           nrFinishedSteps     () const;

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
