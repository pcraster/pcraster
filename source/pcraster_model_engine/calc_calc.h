#ifndef INCLUDED_CALC_CALC
#define INCLUDED_CALC_CALC



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_EXECUTESCRIPTSTATUS
#include "calc_executescriptstatus.h"
#define INCLUDED_CALC_EXECUTESCRIPTSTATUS
#endif

namespace com {
  class PathName;
}

namespace calc {

class  ProgressCallBack;

class Calc
{
private:

  //! Assignment operator. NOT IMPLEMENTED.
  Calc&           operator=           (const Calc&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Calc               (const Calc&);
protected:

   //! set in executeScript
  ExecuteScriptStatus  d_executeScriptStatus;

  //! stream where std out message are written to
  std::ostream& d_stdOut;
  //! stream where error message and progress are written to
  std::ostream& d_stdErr;



protected:
  //! implement here what to call in run();
  /*!
      \returns preffered 'exit' code
   */
  virtual int execute()=0;

  void parse();
  int  executeScript();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Calc     ( std::ostream& stdOut,
                              std::ostream& stdErr);

    virtual       ~Calc               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void setProgressCallBack(ProgressCallBack* pcb);

  int  run(int returnValueOnException=1);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ExecuteScriptStatus executeScriptStatus() const;


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

} // namespace calc

#endif
