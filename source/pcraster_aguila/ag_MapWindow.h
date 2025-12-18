#ifndef INCLUDED_AG_MAPWINDOW
#define INCLUDED_AG_MAPWINDOW

#include "ag_VisualisationWindow.h"



namespace ag {
}



namespace ag {



/*!
  \class MapWindow
  \brief short_description

  longer_description
*/
class MapWindow: public ag::VisualisationWindow
{

private:

  Q_OBJECT

  /// QAction*         d_queryAction;

  /// QAction*         d_panAction;

  /// QAction*         d_zoomAction;

  QAction*         d_zoomAllAction{nullptr};

  //! Assignment operator. NOT IMPLEMENTED.
  MapWindow &      operator=           (const MapWindow &);

  //! Copy constructor. NOT IMPLEMENTED.
                   MapWindow           (const MapWindow &);

  bool     dataVisualised      () const override = 0;

  /// void             rescanMapAction     ();

  void             setEnableDataInterfaceElements(bool enable);

protected:

  //! Constructor.
                   MapWindow           (const qt::AppWindowProperties& props,
                                        const std::string& windowName,
                                        ag::DataObject* dataObject);

  void             createInterface     ();

  void             rescan              () override;

  /// void             addMapActionGroup   ();

  void             addZoomAllAction    (QObject* receiver);

protected:

protected Q_SLOTS:

  // void             startSelectMode     ();

  // virtual void     resetMapView        () = 0;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Destructor.
           ~MapWindow                () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void     addAttribute        (DataGuide const& guide) override = 0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
