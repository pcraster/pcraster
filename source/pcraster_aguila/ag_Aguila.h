#ifndef INCLUDED_AG_AGUILA
#define INCLUDED_AG_AGUILA



#include "dev_GDalClient.h"
#include "dal_Client.h"
#include "qt_Def.h"
#include "qt_GuiApp.h"

namespace qt {
  class AppWindowProperties;
}
namespace ag {
  class Viewer;
}
namespace pcrxsd {
  class Library;
}



namespace ag {



//! The Aguila class is the main application class.
/*!

  It creates all visualisation groups.
*/
class PCR_AG_DECL Aguila: public qt::GuiApp,
                          public dev::GDalClient,
                          public dal::Client
{

  friend class AguilaGuiTest;

private:

  pcrxsd::Library* d_xsdLib;

  int&             d_argc;

  char**           d_argv;

  Viewer*          d_viewer;

  void             init                (qt::AppWindowProperties const& awp);


  // void             createLockFile      ();

  // void             parseConfigurationFile();


protected:

  void             setup               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   // Aguila              ();

                   Aguila              (int& argc,
                                        char** argv,
                                        qt::ApplicationRole role=qt::StandAlone);

  /* virtual */    ~Aguila             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  Viewer&          viewer              ();

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
