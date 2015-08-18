#ifndef INCLUDED_QT_ANIMATION
#define INCLUDED_QT_ANIMATION



#include <set>
#include <QObject>



class QTimer;



namespace qt {



/*!
  \class Animation
  \brief The Animation class is for manager objects controlling animations.

  \code
  // Create an animation manager for timesteps [1, 1000].
  Animation a(1, 1000, 1000);

  // Start the manager. This will take 1 sec * 1000 steps = 1000 sec to finish.
  // 1000 process() signals will be emitted, folowed by a stopped() signal.
  a.start();
  \endcode

  \code
  // Create an animation manager.
  Animation a(1000);

  // Configure the manager to iterate over steps 1 500 and 1000. Set the
  // time interval to 1 sec.
  a.addStep(1);
  a.addStep(500);
  a.addStep(1000);

  // Start the manager. This will take 1 sec * 3 steps = 3 sec to finish.
  // 3 process() signals will be emitted, folowed by a stopped() signal.
  a.start();
  \endcode
*/
class Animation: public QObject
{

private:

  typedef std::set<size_t>::const_iterator const_iterator;

  // Qt-specific.
  Q_OBJECT

  //! First step.
  size_t           d_first;

  //! Last step.
  size_t           d_last;

  //! Steps size between time steps.
  size_t           d_stepInterval;

  //! Current step.
  size_t           d_current;

  //! Step which has to be processed once the timer times out.
  size_t           d_stepToProcess;

  //! Steps to visit. If empty, visit all steps between d_first and d_last.
  std::set<size_t> d_steps;

  //! Interval in milliseconds.
  size_t           d_interval;

  //! Loop over the timesteps.
  bool             d_loop;

  //! Timer.
  QTimer *         d_timer;

  //! Frees dynamically allocated memory.
  void             clean               ();

private Q_SLOTS:

  //! Gets called every time the timer times out.
  void             timedOut            ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
                   Animation           ();

  //! Constructor.
                   Animation           (size_t i);

  //! Constructor.
                   Animation           (size_t f,
                                        size_t l,
                                        size_t i = 0);

  //! Copy constructor.
                   Animation           (const Animation &animation);

  //! Destructor.
                   ~Animation          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Assignment operator.
  Animation &      operator=           (const Animation &);

  //! Sets the first timestep to \a f.
  void             setFirst            (size_t f);

  //! Sets the last timestep to \a f.
  void             setLast             (size_t l);

  //! Sets the current timestep to \a t.
  void             setCurrent          (size_t t);

  //! Sets the timer interval to \a i msec.
  void             setInterval         (size_t i);

  //! Adds step \a i to the set of timesteps to visit.
  void             addStep             (size_t i);

  void             addSteps            (const std::set<size_t>& steps);

  void             setSteps            (const std::set<size_t>& steps);

  void             setRangeOfSteps     (size_t first,
                                        size_t last,
                                        size_t stepInterval);

  //! If \a l is true then the manager will loop over the timesteps forever.
  void             setLoop             (bool l);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the first timestep.
  size_t           firstStep           () const;

  //! Returns the last timestep.
  size_t           lastStep            () const;

  //! Returns the previous timestep.
  size_t           prevTimeStep        () const;

  //! Returns the next timestep.
  size_t           nextTimeStep        () const;

  //! Returns the current timestep.
  size_t           currentStep         () const;

  size_t           closestStep         (size_t step) const;

  //! Returns the timer interval.
  size_t           interval            () const;

  //! Returns the number of timesteps.
  size_t           nrSteps             () const;

  size_t           timeSpan            () const;

  //! Returns true if the animation is running.
  bool             isRunning           () const;

  //! Returns true if looping is enabled.
  bool             loop                () const;

public Q_SLOTS:

  //! Starts the animation.
  void             start               ();

  //! Stops the animation.
  void             stop                ();

  //! Pauses the animation.
  void             pause               ();

Q_SIGNALS:

  //! Gets emitted if the outside world should process a timestep (\a i).
  void             process             (size_t i);

  //! Gets emitted if the animation is finished or if stop() is called.
  void             stopped             ();

  //! Gets emitted as soon if the animation is started.
  void             started             ();

  //! Gets emitted as soon if the animation is paused.
  void             paused              ();

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace qt

#endif
