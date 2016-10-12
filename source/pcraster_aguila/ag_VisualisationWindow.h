#ifndef INCLUDED_AG_VISUALISATIONWINDOW
#define INCLUDED_AG_VISUALISATIONWINDOW



// Library headers.
#include <cassert>

// PCRaster library headers.
#include "qt_AppWindow.h"

// Module headers.
#include "ag_IVisualisation.h"



class QAction;
namespace ag {
  // VisualisationWindow declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/

// Qt comments on BCC bug:
// It shouldn't be necessary to put "qt::" in front, since namespaces are
// inherited. In other words, if C::D inherits A::B, D can use objects in the
// namespaces A and C without the "A::" or "C::" prefix. We do not put the
// prefix because it confuses MSVC 6, which then somehow believes that the
// function is a static function and prevents compilation.
#if defined WIN32 && defined BORLANDC
typedef qt::AppWindow AppWindow;
#endif

class VisualisationWindow: public qt::AppWindow,
                           public IVisualisation
{

private:

  Q_OBJECT

  //! all menus accesible by derived classes
  QMenu            *d_fileMenu, *d_editMenu, *d_viewMenu, *d_helpMenu;

  //! all toolbars accesible by derived classes
  QToolBar*        d_toolBar;

  QAction*         d_animateAction;

  QAction*         d_saveAsAction;

  QAction*         d_preferencesAction;

  //! Assignment operator. NOT IMPLEMENTED.
  VisualisationWindow& operator=       (const VisualisationWindow& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   VisualisationWindow (const VisualisationWindow& rhs);

protected:

                   VisualisationWindow (const qt::AppWindowProperties& props,
                                        const std::string& visualisationName,
                                        ag::DataObject* object,
                                        Qt::WindowFlags flags);

  void             createInterface     ();

  void             insertAddAndNewVisualisationsMenu();

  // void             insertOpenAction    ();

  void             insertSaveAsMenuItem();

  void             insertPreferencesMenuItem();

  void             insertAnimateAction ();

  void             insertCloseAndExitMenuItems();

  void             createFileMenu ();

  void             createEditMenu      ();

  void             createViewMenu      ();

  void             insertShowCursorAction();

  void             createHelpMenu      ();

  void             insertWhatsThisMenuItem();

  void             insertAboutMenuItem ();

  void             addToMenuAndToolBar (QMenu* menu,
                                        QAction* action,
                                        bool toggle);

  void             saveAs              (com::FileFormatInfo const& format,
                                        std::string name,
                                        dal::DataSpace iterationSpace);

  // void             createFullScreenToolButton(QToolBar* toolBar);

  QMenu*           fileMenu            () const
  { assert(d_fileMenu);
    return d_fileMenu; };

  QMenu*           editMenu            () const
  { assert(d_editMenu);
    return d_editMenu; };

  QMenu*           viewMenu            () const
  { assert(d_viewMenu);
    return d_viewMenu; };

  QMenu*           helpMenu            () const
  { assert(d_helpMenu);
    return d_helpMenu; };

  QToolBar*        toolBar             ()  const
  { assert(d_toolBar);
    return d_toolBar;  };

  void             rescan              ();

  void             setEnableAnimation  (bool enable);

  void             setEnableSaveAs     (bool enable);

  virtual std::string windowName       () const;

  virtual bool     dataVisualised      () const;

  virtual void     addAttribute        (DataGuide const& guide) = 0;

protected Q_SLOTS:

  // void             fileMenuControlCenter();

  void             fileMenuAnimationControl();

  void             fileMenuNewMap2D    ();

  void             fileMenuNewMap3D    ();

  void             fileMenuNewTimePlot ();

  void             fileMenuAddMap2D    ();

  void             fileMenuAddMap3D    ();

  void             fileMenuAddTimePlot ();

  // void             fileMenuCopy        ();

  void             fileMenuOpen        ();

  void             fileMenuClose       ();

  void             fileMenuCloseGroup  ();

  void             fileMenuSaveAs      ();

  void             editMenuPreferences ();

  void             viewMenuShowCursor  ();

  void             quit                ();

  // void             toggleFullScreen    ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~VisualisationWindow();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  bool             close               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

Q_SIGNALS:

  void             showControlCenter   ();

  // void             showAnimationControl();

  void             newMap2DWindow      (ag::VisualisationWindow *v);

  void             newMap3DWindow      (ag::VisualisationWindow *v);

  void             newTimePlotWindow   (ag::VisualisationWindow *v);

  void             addMap2DWindow      (ag::VisualisationWindow *v);

  void             addMap3DWindow      (ag::VisualisationWindow *v);

  void             addTimePlotWindow   (ag::VisualisationWindow *v);

  // void             open                (ag::VisualisationWindow *v);

  void             closeGroup          ();

  void             closeAll            ();

/*
  void             addDataPropertiesDialog(
                                        ag::DataObject& dataObject,
                                        const ag::DataGuide& dataGuide);
*/

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



} // namespace ag

#endif
