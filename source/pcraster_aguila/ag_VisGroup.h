#ifndef INCLUDED_AG_VISGROUP
#define INCLUDED_AG_VISGROUP

#include <memory>
#include <string>
#include <vector>
#include <QObject>
#include "ag_DataGuide.h"

// #ifdef DEBUG_DEVELOP
// #ifndef INCLUDED_AG_MAP3DWINDOW3
// #include "ag_map3dwindow3.h"
// #define INCLUDED_AG_MAP3DWINDOW3
// #endif
// #endif



namespace dal {
  class DataSpace;
}
namespace pcrxml {
  class VisualisationGroup;
}
namespace geo {
  class CSFStackName;
}
namespace qt {
  class AppWindowProperties;
}
namespace ag {
  class AnimationControl;
  class BufferedVisualisation;
  class CumDistributionFunctionWindow;
  class CursorWindow;
  class DataObject;
  // class DataPropertiesDialog;
  class GLVisualisation;
  class MapWindow;
  class Map2D;
  class Map2DView;
  class Map2DWindow;
  class Map3DWindow;
  class MultiMap2DWindow;
  class TimePlotWindow;
  class VisGroupPrivate;
  class VisGroupManager;
  class IVisualisation;
  class PlotVisualisation;
  class VisualisationWindow;
}



namespace ag {



//! Class for objects managing a data subject and its observers.
/*!
  A visualisation group is a data subject and its connected visualisation
  observers.
*/
class PCR_AG_DECL VisGroup: public QObject
{

private:

  Q_OBJECT

  std::auto_ptr<VisGroupPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  VisGroup &       operator=           (const VisGroup &);

  void             connectVisSignals   (VisualisationWindow* visualisation);

  void             addVisualisation    (VisualisationWindow* visualisation);

  // void             createAnimationControl();

private Q_SLOTS:

  // void             detach              (ag::IVisualisation* v);

  // void             open                (ag::VisualisationWindow* window);

/*
  void             updateTime          (size_t t);
*/

/*
  void             copy                (ag::Visualisation *v);
*/

public:

  typedef std::vector<IVisualisation *>::iterator vis_iterator;
  typedef std::vector<IVisualisation *>::const_iterator const_vis_iterator;

  typedef std::vector<VisualisationWindow *>::iterator viswin_iterator;
  typedef std::vector<VisualisationWindow *>::const_iterator const_viswin_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VisGroup            (const qt::AppWindowProperties& props,
                                        VisGroupManager *m);

                   VisGroup            (const qt::AppWindowProperties& props,
                                        VisGroupManager *m,
                                        const ag::DataObject &o);

                   VisGroup            (const qt::AppWindowProperties& props,
                                        VisGroupManager *m,
                                        const pcrxml::VisualisationGroup& vg);

  /* virtual */    ~VisGroup           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  ag::DataGuide    addData             (std::string const& name,
                                        dal::DataSpace const& space);

  Map2DView*       addMap2DView        (QWidget* parent = 0);

  Map2D*           addMap2D            (QWidget* parent = 0);

  Map2DWindow*     addMap2DWindow      ();

  Map2DWindow*     addMap2DWindow      (ag::VisualisationWindow *v);

  MultiMap2DWindow* addMultiMap2DWindow(size_t nrRows,
                                        size_t nrCols);

  Map3DWindow*     addMap3DWindow      ();

  Map3DWindow*     addMap3DWindow      (ag::VisualisationWindow *v);

// #ifdef DEBUG_DEVELOP
//   TestVisualisation* addTestVisualisation();
// #endif

  TimePlotWindow*  addTimePlotWindow   ();

  TimePlotWindow*  addTimePlotWindow   (ag::VisualisationWindow *visualisation);

  CumDistributionFunctionWindow* addProbabilityGraphWindow();

  AnimationControl* addAnimationDialog ();

  CursorWindow*    addCursorWindow     ();

  // ag::DataPropertiesDialog* addDataPropertiesDialog(
  //                                       const ag::DataGuide& dataGuide);

  void             show                ();

  // void             open                (ag::MapWindow* mapWindow = 0);

  // void             close               (ag::VisualisationWindow *visualisation);

  void             loadData            ();

  void             reloadData          ();

  void             sync                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  ag::DataObject & dataObject          ();

  size_t           nrVisualisations    () const;

/*
  vis_iterator     vis_begin           ();

  vis_iterator     vis_end             ();

  const_vis_iterator vis_begin         () const;

  const_vis_iterator vis_end           () const;

  viswin_iterator  viswin_begin        ();

  viswin_iterator  viswin_end          ();

  const_viswin_iterator viswin_begin   () const;

  const_viswin_iterator viswin_end     () const;
  */

  bool             contains            (IVisualisation const* visualisation) const;

public Q_SLOTS:

  // void             showAnimationControl();

  // void             copy                (ag::IVisualisation *v);

  void             close               ();

Q_SIGNALS:

  void             changed             (ag::VisGroup *g);

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
