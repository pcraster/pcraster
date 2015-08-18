#ifndef INCLUDED_COM_APPHELPER
#define INCLUDED_COM_APPHELPER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace com {
  // AppHelper declarations.
  class App;
}



namespace com {



//! Base class for application helper objects.
/*!
  Most applications consist of a collection of classes working together. There
  is one class which inherits from com::App (by com::CommandLineApp or
  qt::GuiApp, for example) and a bunch of classes assisting this application
  class. Often you want to be able to print warning messages or similar from
  a helper class, but you don't have access to the showWarning member of
  the com::App class. This is where AppHelper comes in handy: just let the
  application helper classes inherit from AppHelper and provide a pointer to
  the one application object. This way the application object is alway nearby
  to call functions upon.
*/
class AppHelper
{

  friend class AppHelperTest;

private:

  //! The application object.
  App*             d_application;

  //! Assignment operator. NOT IMPLEMENTED.
  AppHelper&       operator=           (AppHelper const& rhs);

protected:

   App*            application         ();

   App const*      application         () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AppHelper           (App* application);

                   AppHelper           (AppHelper const& rhs);

  /* virtual */    ~AppHelper          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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
