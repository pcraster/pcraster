#ifndef INCLUDED_COM_PROGRESSTRACKER
#define INCLUDED_COM_PROGRESSTRACKER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace com {
  // ProgressTracker declarations.
}



namespace com {



//! Base class for all progress trackers.
/*!
  A progress tracker is an object which provides the user with some kind of
  feedback about the progress of a computing task.

  Progress tracker objects have information about the current status of a
  task. They keep track of the number of steps to be performed and the
  last finished step. Based on this information, more specialized classes can
  provide a visual feedback to the user (e.g. progress bars, countdown timers,
  etc) by implementing the virtual function update().

  Subclasses may provide graphical and/or textual implementations.

  \sa ProgressTracked
*/
class ProgressTracker
{

  friend class ProgressTrackerTest;

  //! Total number of steps to be performed.
  size_t           d_nrSteps;

  //! Number of finished steps.
  size_t           d_nrFinishedSteps;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ProgressTracker& operator=          (ProgressTracker const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ProgressTracker    (ProgressTracker const& rhs);

  // void             reset               ();

protected:

                   ProgressTracker    ();

                   ProgressTracker    (size_t nrSteps);

  bool             finished            () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~ProgressTracker   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setNrSteps          (size_t nrSteps);

  void             setNrFinishedSteps  (size_t nrSteps);

  virtual void     clear               ();

  virtual void     init                ();

  void             finishedStep        ();

  void             finishedSteps       (size_t nrSteps);

  void             finishedSteps       ();

  //! Updates the progress tracker.
  /*!
    This function is called automatically whenever relevant information for
    the subclasses changes. It is public because some ProgressTracker
    implementations layer each other and need to call update on each other.

    \warning This function might be called zillion of times so check whether
             the visualisation actualy needs an update.
  */
  virtual void     update              () = 0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           nrSteps             () const;

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
