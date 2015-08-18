#ifndef INCLUDED_CALC_PROGRESSCALLBACK
#define INCLUDED_CALC_PROGRESSCALLBACK



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_PROGRESSPULSE
#include "calc_progresspulse.h"
#define INCLUDED_CALC_PROGRESSPULSE
#endif


namespace calc {
  // ProgressCallBack declarations.
}





namespace calc {


//! info passed into a ProgressCallBack::update method
/*!
 *  \todo
 *    add ParsedCorrectly status having time step info
 *     to improve different post messages in calcui.
 */
struct ProgressInfo
{
  //! at what timestep execution is
  /*! >=0, if 0 then start of initial,
   *  if greater than nrTimeSteps
   *  then model is finished else at start of that timestep
   */
  int  inTimeStep;
  //! line nr of next statement executed
  /*! >=0, if 0 then start of timestep,
   *  if > 0 at start of statement at this line
   */
  int  inStatementLineNr;
  //! nr of timesteps in model
  /*! >= 0 if 0 then model is static,
   *  if greater,then  model is dynamic
   */
  int  nrTimeSteps;

  //! Where is execution now?
  enum Status {
    Error=0,         /*!< Not Used, normal operation result must be
                          checked for errors, not the callback
                      */
    Static=1,          /*!< in the static model */
    InitialSection=2,  /*!< in the initial of a dynamic model */
    DynamicSection=3,  /*!< in the dynamic of a dynamic model */
    Finished=4       /*!< finished the model computations
                          \warning
                            when Finished is status, postmodel actions
                            must still be done, e.g. writing timeseries etc.
                      */
  };
  Status status() const;

};

//! progress call back function
class ProgressCallBack
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ProgressCallBack&           operator=           (const ProgressCallBack&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ProgressCallBack               (const ProgressCallBack&);

public:


  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ProgressCallBack               ();

  virtual         ~ProgressCallBack              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  //! the call back
  /*!
      \returns if non-zero then quit running script by throwing
               QuitForProgressCallBack
   */
  virtual int update(const struct ProgressInfo& p);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  virtual ProgressPulse callAtPulse() const;
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



} // namespace calc

#endif
