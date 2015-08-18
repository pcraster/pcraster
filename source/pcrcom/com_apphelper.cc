#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_APPHELPER
#include "com_apphelper.h"
#define INCLUDED_COM_APPHELPER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the AppHelper class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class AppHelperPrivate
{
public:

  AppHelperPrivate()
  {
  }

  ~AppHelperPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC APPHELPER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF APPHELPER MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     application Application object.
  \warning   \a application must not be 0.
*/
com::AppHelper::AppHelper(App* application)

  : d_application(application)

{
  DEVELOP_PRECOND(application);
}



//! Copy constructor.
com::AppHelper::AppHelper(AppHelper const& rhs)

  : d_application(rhs.d_application)

{
}



//! Destructor.
/*!
  The layered application object is for use only and won't be deleted.
*/
com::AppHelper::~AppHelper()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
com::AppHelper& com::AppHelper::operator=(AppHelper const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



//! Returns the layered application object.
/*!
  \return    Application object.
*/
com::App* com::AppHelper::application()
{
  return d_application;
}



//! Returns the layered application object.
/*!
  \return    Application object.
*/
com::App const* com::AppHelper::application() const
{
  return d_application;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



