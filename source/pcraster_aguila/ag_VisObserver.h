#ifndef INCLUDED_AG_VISOBSERVER
#define INCLUDED_AG_VISOBSERVER





namespace ag {



//! The VisObserver class is for objects who are observing a VisSubject object.
/*!
  A VisObserver object can be attached to a VisSubject object after which
  rescan(), process() and visualise() are called in order every time the
  VisSubject object changes. First all rescan is called on all observers,
  than process() and lastly visualise(). This way the observers will perform
  better and give the user the impression that all observers are drawn at the
  same time, during an animation for example.

  Simple, fast observers might choose to only implement visualise().

  This class is modeled after the Observer pattern.

  \sa VisSubject
*/
class VisObserver
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  VisObserver &    operator=           (const VisObserver &);

  //! Copy constructor. NOT IMPLEMENTED.
                   VisObserver         (const VisObserver &);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VisObserver         ();

  virtual          ~VisObserver        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  virtual void     rescan              ();

  virtual void     process             ();

  virtual void     visualise           ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
