#include "qt_Animation.h"
#include <QTimer>
#include <algorithm>
#include <iostream>
#include "dev_MathUtils.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

/*!
  \sa      Animation(size_t), Animation(size_t, size_t, size_t)
*/
qt::Animation::Animation()

  : QObject(),
    d_first(0), d_last(0), d_stepInterval(0),
    d_current(0), d_stepToProcess(0), d_interval(0),
    d_loop(false), d_timer(new QTimer(this))

{
  d_timer->setSingleShot(false);
  connect(d_timer, SIGNAL(timeout()), this, SLOT(timedOut()));
}



/*!
  \param   i Timer interval.
  \sa      Animation(), Animation(size_t, size_t, size_t)
*/
qt::Animation::Animation(size_t i)

  : QObject(),
    d_first(0), d_last(0), d_stepInterval(0),
    d_current(0), d_stepToProcess(0), d_interval(i),
    d_loop(false), d_timer(new QTimer(this))

{
  d_timer->setSingleShot(false);
  connect(d_timer, SIGNAL(timeout()), this, SLOT(timedOut()));
}



/*!
  \param   f First timestep.
  \param   l Last timestep.
  \param   i Animation interval in msec.
  \sa      Animation(), Animation(size_t)
*/
qt::Animation::Animation(size_t f, size_t l, size_t i)

  : QObject(),
    d_first(f), d_last(l),d_stepInterval(1),
    d_current(0), d_stepToProcess(0), d_interval(i),
    d_loop(false), d_timer(new QTimer(this))

{
  d_timer->setSingleShot(false);
  connect(d_timer, SIGNAL(timeout()), this, SLOT(timedOut()));
}



qt::Animation::Animation(const Animation &animation)

  : QObject(),
    d_first(animation.d_first), d_last(animation.d_last),
    d_stepInterval(animation.d_stepInterval),
    d_current(animation.d_current),
    d_stepToProcess(animation.d_stepToProcess),
    d_steps(animation.d_steps),
    d_interval(animation.d_interval),
    d_loop(animation.d_loop), d_timer(new QTimer(this))

{
  d_timer->setSingleShot(false);
  connect(d_timer, SIGNAL(timeout()), this, SLOT(timedOut()));
}



qt::Animation::~Animation()
{
}



qt::Animation& qt::Animation::operator=(const Animation &animation)
{
  assert(!isRunning());

  if(this != &animation) {
    d_first = animation.d_first;
    d_last = animation.d_last;
    d_stepInterval = animation.d_stepInterval;
    d_current = animation.d_current;
    d_stepToProcess = animation.d_stepToProcess;
    d_steps = animation.d_steps;
    d_loop = animation.d_loop;

    assert(d_timer);
    setInterval(animation.d_interval);
  }

  return *this;
}



/*!
  \sa      stop(), pause()

  If the animation is pause()-ed and if timesteps have been added using
  addStep(), then the current timestep is set to the lowest step added. Else if
  the last timestep is larger or equal to first, the current timestep is set to
  first.

  If the animation is paused()-ed, than start() continuous at the point were
  it was stopped.
*/
void qt::Animation::start()
{
  if(!isRunning()) {
    d_stepToProcess = nextTimeStep();
    d_timer->start(static_cast<int>(d_interval)); // Start the timer.
    Q_EMIT started();
  }
}



/*!
  \sa      start(), pause()

  Stops the manager and emits the stopped() signal.
*/
void qt::Animation::stop()
{
  if(isRunning()) {
    d_timer->stop();
    Q_EMIT stopped();
  }
}



/*!
  \sa      start(), stop()

  Pauses the animation and emits the paused() signal. By calling start() you
  can resume it again.
*/
void qt::Animation::pause()
{
  if(isRunning()) {
    d_timer->stop();
    Q_EMIT paused();
  }
}



/*!
  \param   f New first timestep.
  \sa      setLast(), setInterval()
*/
void qt::Animation::setFirst(size_t f)
{
  d_first = f;
}



/*!
  \param   l New last timestep.
  \sa      setFirst(), setInterval()
*/
void qt::Animation::setLast(size_t l)
{
  d_last = l;
}



void qt::Animation::setCurrent(size_t t)
{
#ifdef DEBUG
  if(!d_steps.empty()) {
    assert(d_steps.find(t) != d_steps.end());
  }
  else {
    assert(t >= d_first || t <= d_last);
    assert((t - d_first) % d_stepInterval == 0);
  }
#endif

  d_current = t;
  // pause();
}



/*!
  \param   i New interval in msec.
  \sa      setFirst(), setLast()

  This function can also be called while the animation is running.
*/
void qt::Animation::setInterval(size_t i)
{
  if(d_interval != i) {
    d_interval = i;

    if(isRunning()) {
      d_timer->setInterval(static_cast<int>(i));
    }
  }
}



/*!
  \param   s Timestep to add.
  \sa        addSteps(size_t)

  After timesteps are added, the timesteps set by calling setFirst() and/or
  setLast() don't matter anymore. By calling this function you explicitly
  state which timesteps should be processed.

  If the same timestep is added more than once, only one copy is remembered.
*/
void qt::Animation::addStep(size_t s)
{
  d_steps.insert(s);
}



//! Add timesteps.
/*!
  \param     steps Timesteps to add.
  \sa        addStep(size_t)
*/
void qt::Animation::addSteps(const std::set<size_t>& steps)
{
  std::set_union(d_steps.begin(), d_steps.end(), steps.begin(), steps.end(),
         std::inserter(d_steps, d_steps.begin()));
}



void qt::Animation::setSteps(const std::set<size_t>& steps)
{
  // Crash for now. Might have to do something smart here.
  assert(!isRunning());

  d_steps = steps;
}



void qt::Animation::setRangeOfSteps(size_t first, size_t last, size_t stepInterval)
{
  assert(d_last >= d_first);
  assert(stepInterval > 0);

  d_steps.clear();
  d_first = first;
  d_last = last;
  d_stepInterval = stepInterval;
}



/*!
  \return  First timestep of the manager.
  \sa      lastStep()

  The first timestep of the manager is either the lowest step added with
  addStep() or, if no timestep is added, the timestep set with setFirst().
*/
size_t qt::Animation::firstStep() const
{
  if(!d_steps.empty())
    return *(d_steps.begin());
  else
    return d_first;
}



/*!
  \return  Last timestep of the manager.
  \sa      firstStep()

  The last timestep of the manager is either the highest step added with
  addStep() or, if no timestep is added, the timestep set with setLast().
*/
size_t qt::Animation::lastStep() const
{
  if(!d_steps.empty())
    return *(--d_steps.end());
  else
    return d_last;
}



size_t qt::Animation::currentStep() const
{
  return d_current;
}



/*!
  \return  The timer interval in msec.
*/
size_t qt::Animation::interval() const
{
  return d_interval;
}



/*!
  \return  Number of timesteps to process.
  \warning Make shure you understand the difference between the *number* of
           timesteps and the *span* in time. These two values mean different
           things.
  \sa      timeSpan()

  The number of timesteps is equal to the number of times the process() signal
  is emitted.

  The code:
  \code
  qt::Animation a1(100, 150, 1000);
  qt::Animation a2(1000);
  a2.addStep(100);
  a2.addStep(150);

  std::cout << a1.nrSteps  << " =? " << a1.timeSpan << std::endl;
  std::cout << a2.nrSteps  << " =? " << a2.timeSpan << std::endl;
  \endcode
  will print:
  \code
  50 =? 50
  2 =? 50
  \endcode
*/
size_t qt::Animation::nrSteps() const
{
  if(!d_steps.empty()) {
    return d_steps.size();
  }
  else {
    assert(d_last >= d_first);
    return 1 + (d_last - d_first) / d_stepInterval;
  }
}



//! Returns the time span between the first and the last step.
/*!
  \return  The span in time between last() and first().
  \warning Make shure you understand the difference between the *number* of
           timesteps and the *span* in time. These two values mean different
           things.
  \sa      nrSteps()
*/
size_t qt::Animation::timeSpan() const
{
  if(!d_steps.empty())
    return *(--d_steps.end()) - *(d_steps.begin());
  else
    return d_last - d_first;
}



/*!
  \return  True if the animation is running right now.
*/
bool qt::Animation::isRunning() const
{
  return d_timer->isActive();
}



/*!
  This function gets called each time the timer times out. First it emits a
  process() signal and than it checks if there're more timesteps to process.
  If so, it continuous to wait for a time time interval. If not, it calls
  stop().

  The new timestep to process depends on wether timesteps are added with
  addStep() or not. If so, than the next higher timestep is searched in the
  collection with added timesteps. If not, than d_stepInterval is added to the
  current timestep.
*/
void qt::Animation::timedOut()
{
  Q_EMIT process(d_stepToProcess);       // Finished waiting for the current step.

  d_current = d_stepToProcess;
  d_stepToProcess = nextTimeStep();

  if(d_stepToProcess == d_current) {
    stop();
  }
}



void qt::Animation::setLoop(bool l)
{
  d_loop = l;
}



bool qt::Animation::loop() const
{
  return d_loop;
}



size_t qt::Animation::prevTimeStep() const
{
  size_t t;

  if(!d_steps.empty()) {

    // Whe have a fixed set of steps.
    std::set<size_t>::const_iterator it = d_steps.find(d_current);

    if(it != d_steps.begin()) {

      // Another step to wait for.
      t = *(--it);
    }
    else {

      if(d_loop) {
        // Return to last step.
        t = *(--d_steps.end());
      }
      else {
        // Previous time step is the current time step.
        t = d_current;
      }
    }
  }
  else {

    // We have a range of steps.
    if(d_current >= d_first + d_stepInterval) {

      // Another step to wait for.
      t = d_current - d_stepInterval;
    }
    else {

      if(d_loop) {
        // Return to last step.
        t = d_last;
      }
      else {
        // Next time step is the current time step.
        t = d_current;
      }
    }
  }

  return t;
}



size_t qt::Animation::nextTimeStep() const
{
  size_t t;

  if(!d_steps.empty()) {

    // We have a fixed set of steps.
    std::set<size_t>::const_iterator it = d_steps.find(d_current);
    assert(it != d_steps.end());

    if(++it != d_steps.end()) {

      // Another step to wait for.
      t = *it;
    }
    else {

      if(d_loop) {
        // Return to first step.
        t = *(d_steps.begin());
      }
      else {
        // Next time step is the current time step.
        t = d_current;
      }
    }
  }
  else {

    // We have a range of steps.
    if(d_current + d_stepInterval <= d_last) {

      // Another step to wait for.
      t = d_current + d_stepInterval;
    }
    else {

      if(d_loop) {
        // Return to first step.
        t = d_first;
      }
      else {
        // Next time step is the current time step.
        t = d_current;
      }
    }
  }

  return t;
}



//! Returns the existing time step equal or closest to \a step.
/*!
  \param     step Step to find existing closest for.
  \return    Time step.

  \a step is clamped to the first and last timestep set. If the distance
  to the lower existing time step equals the distance to the upper existing
  time step the lower is returned. \a step is returned if no time steps are
  found.
*/
size_t qt::Animation::closestStep(size_t step) const
{
  step = dev::limit(step, firstStep(), lastStep());

  if(d_steps.empty()) {
    return step;
  }

  if(d_steps.find(step) != d_steps.end()) {
    return step;
  }

  std::set<size_t>::const_iterator it = d_steps.begin();
  while(*it < step) {
    ++it;
  }

  // it points to the first step which is larger than step
  size_t previous = *--it;
  size_t next = *++it;
  assert(previous < step);
  assert(next > step);
  return step - previous <= next - step ? previous : next;
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


