#ifndef INCLUDED_QT_GUIAPP
#define INCLUDED_QT_GUIAPP



#include <string>
#include <boost/filesystem/path.hpp>
#include <QObject>
#include "dev_CommandLineApplication.h"
#include "dev_QtClient.h"
#include "com_exception.h"
#include "ag_Configure.h"
#include "qt_Def.h"
#include "ag_QApplication.h"



namespace qt {



/*!
  \class GuiApp
  \brief The GuiApp class is for handling qt-applications.

  By subclassing your application from the GuiApp class you can skip a lot of
  exception catching code. You just need to implement the virtual setup()
  function. This class puts exception catching code around the calls to setup()
  and QApplication::exec(). This way you only need to catch application specific
  exceptions in your subclass.

  \sa AppWindow
*/
class PCR_AG_DECL GuiApp: public QObject,
                          public dev::QtClient<ag::QApplication>,
                          public dev::CommandLineApplication
{

private:

  //! File name of lock file.
  boost::filesystem::path d_lockFilename;

  virtual void     setup               () = 0;

protected:

                   GuiApp              (int& argc,
                                        char** argv,
                                        // const com::CommandLine& cmdLine,
                                        // com::License = com::UNKNOWN,
                                        ApplicationRole role = StandAlone);

  void             createLockFile      (std::string const& filename);

  void             deleteLockFile      ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~GuiApp             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  int              run                 ();


  void             quit                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             showInfo            (const std::string& message) const;

  void             showWarning         (const std::string& message) const;

  void             showError           (const std::string& message) const;

  void             showError           (com::Exception::const_iterator begin,
                                        com::Exception::const_iterator end) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace qt

#endif
