#ifndef INCLUDED_COM_ICOMMANDMODE
#define INCLUDED_COM_ICOMMANDMODE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_LABELEDPROGRESSTRACKED
#include "com_labeledprogresstracked.h"
#define INCLUDED_COM_LABELEDPROGRESSTRACKED
#endif

#ifndef INCLUDED_COM_PROGRESSBAR
#include "com_progressbar.h"
#define INCLUDED_COM_PROGRESSBAR
#endif



namespace com {
  // ICommandMode declarations.
  class CommandModeArgument;
}



namespace com {



//! Base class for application command modes.
/*!
*/
class ICommandMode: public LabeledProgressTracked<ProgressBar>
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ICommandMode&    operator=           (const ICommandMode&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ICommandMode        (const ICommandMode&);

protected:

                   ICommandMode        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~ICommandMode       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Executes the command mode.
  /*!
  */
  virtual void     exec                () = 0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns true if the command line arguments have been parsed.
  /*!
    \return    true or false.
    \sa        arguments()
  */
  virtual bool     isParsed            () const = 0;

  //! Returns the command line arguments.
  /*!
    \return    Command line arguments.
    \sa        isParsed()
  */
  virtual com::CommandModeArgument* arguments() = 0;

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
