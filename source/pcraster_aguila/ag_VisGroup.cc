#include "ag_VisGroup.h"
#include <iomanip>
#include <string>
#include <boost/format.hpp>
#include <QProgressDialog>
#include <QWidget>
#include "dev_Algorithm.h"
#include "dal_DataSpace.h"
#include "dal_Exception.h"
#include "com_const.h"
#include "com_exception.h"
#include "com_fileformatinfo.h"
#include "qt_Animation.h"
#include "qt_AppWindowProperties.h"
#include "qt_Util.h"
#include "ag_AnimationControl.h"
#include "ag_BufferedVisualisation.h"
#include "ag_CumDistributionFunctionWindow.h"
#include "ag_CursorWindow.h"
#include "ag_DataObject.h"

// #ifndef INCLUDED_AG_DATAPROPERTIESDIALOG
// #include "ag_DataPropertiesDialog.h"
// #define INCLUDED_AG_DATAPROPERTIESDIALOG
// #endif
#include "ag_GLVisualisation.h"
#include "ag_Map2D.h"
#include "ag_Map2DView.h"
#include "ag_Map2DWindow.h"
#include "ag_MultiMap2DWindow.h"
#include "ag_Map3DWindow.h"
#include "ag_PlotVisualisation.h"
#include "ag_TimePlotWindow.h"
#include "ag_VisGroupManager.h"



//------------------------------------------------------------------------------

namespace ag {

class VisGroupPrivate
{
public:
  static size_t    d_nrCreated;        // Number of objects created.

  qt::AppWindowProperties d_winProps;

  DataObject       d_dataObject;     // Data object of the group.
  std::vector<IVisualisation *> d_visualisations; // Observing visualisations.
  std::vector<VisualisationWindow *> d_visualisationWindows;
  // AnimationControl* d_animationControl;
  // TODO shouldn't this be a static?
  VisGroupManager *d_manager;

  VisGroupPrivate(const qt::AppWindowProperties& props)
    : d_winProps(props) // , d_animationControl(0)
  {
    init();
  }

/*
  VisGroupPrivate(const qt::AppWindowProperties& props,
                   const pcrxml::DataObject& dataObject)
    : d_winProps(props), d_dataObject(dataObject), d_animationControl(0)
  {
    init();
  }
*/

/*
  VisGroupPrivate(const qt::AppWindowProperties& props, const ag::DataObject& o)
    : d_winProps(props), d_dataObject(o), d_animationControl(0)
  {
    init();
  }
*/

  void init()
  {
    ++d_nrCreated;
  }

  ~VisGroupPrivate()
  {
  }

};

size_t VisGroupPrivate::d_nrCreated = 0;

}



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

/*!
  \brief     Default constructor.
*/
ag::VisGroup::VisGroup(const qt::AppWindowProperties& props,
                   VisGroupManager *m)

  : QObject(0),
    d_data(new VisGroupPrivate(props))

{
  assert(m);

  d_data->d_manager = m;

  std::string n = (boost::format("group %1%") % d_data->d_nrCreated).str();
  setObjectName(n.c_str());

  // createAnimationControl();

  // connect(this, SIGNAL(changed(ag::VisGroup*)),
  //                  m, SLOT(updateControlCenter()));
}



/*
ag::VisGroup::VisGroup(const qt::AppWindowProperties& props,
                   VisGroupManager *m, const ag::DataObject &o)

  : QObject(0, "vis group object"),
    d_data(new VisGroupPrivate(props, o))

{
  assert(m);

  d_data->d_manager = m;

  std::string n = "group ";
  n += com::size_tToStr(d_data->d_nrCreated);
  setName(n.c_str());

  createAnimationControl();

  connect(this, SIGNAL(changed(ag::VisGroup*)),
                   m, SLOT(updateControlCenter()));
}



ag::VisGroup::VisGroup(const qt::AppWindowProperties& props,
                   VisGroupManager *m, const pcrxml::VisualisationGroup& vg)
  : QObject(0, "vis group object"),
    d_data(new VisGroupPrivate(props, *vg.dataObject))

{
  assert(m);

  d_data->d_manager = m;

  // yepyep visualisations maken

  std::string name = "group ";
  name += com::size_tToStr(d_data->d_nrCreated);
  setName(name.c_str());

  createAnimationControl();

  connect(this, SIGNAL(changed(ag::VisGroup*)),
                   m, SLOT(updateControlCenter()));
}
*/



/*!
  \brief     Destructor.

  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Handle closing of visualisations.

  This will close the animation dialog and all visualisations.
*/
ag::VisGroup::~VisGroup()
{
  /*
  if(d_data->d_animationControl) {
    d_data->d_animationControl->close(true);
  }
  */

  bool closed;

  while(d_data->d_dataObject.isObserved()) {
    IVisualisation* visualisation =
         dynamic_cast<IVisualisation*>(*(d_data->d_dataObject.begin()));
    assert(visualisation);
    closed = visualisation->close();
    assert(closed);
    delete visualisation;
  }

/*
  std::vector<IVisualisation *>& visualisations = d_data->d_visualisations;

  while(!visualisations.empty()) {
    (*visualisations.begin())->close(true);
  }
*/

  assert(d_data->d_visualisations.empty());
}



void ag::VisGroup::connectVisSignals(VisualisationWindow* visualisation)
{
  // Logic for new visualisations. Uses the VisGroupManager object.
  connect(visualisation, SIGNAL(newMap2DWindow(ag::VisualisationWindow *)),
         d_data->d_manager, SLOT(newMap2DWindow(ag::VisualisationWindow *)));
  connect(visualisation, SIGNAL(newMap3DWindow(ag::VisualisationWindow *)),
         d_data->d_manager, SLOT(newMap3DWindow(ag::VisualisationWindow *)));
  connect(visualisation, SIGNAL(newTimePlotWindow(ag::VisualisationWindow *)),
         d_data->d_manager, SLOT(newTimePlotWindow(ag::VisualisationWindow *)));

  connect(visualisation, SIGNAL(closeAll()), d_data->d_manager, SLOT(close()));

/*
  connect(visualisation, SIGNAL(showControlCenter()),
         d_data->d_manager, SLOT(showControlCenter()));
*/

  // Logic for adding visualisations to the group. Through manager.
  connect(visualisation, SIGNAL(addMap2DWindow(ag::VisualisationWindow *)),
         d_data->d_manager, SLOT(addMap2DWindow(ag::VisualisationWindow *)));
  connect(visualisation, SIGNAL(addMap3DWindow(ag::VisualisationWindow *)),
         d_data->d_manager, SLOT(addMap3DWindow(ag::VisualisationWindow *)));
  connect(visualisation, SIGNAL(addTimePlotWindow(ag::VisualisationWindow *)),
         d_data->d_manager, SLOT(addTimePlotWindow(ag::VisualisationWindow *)));
  /*
  connect(visualisation, SIGNAL(addDataPropertiesDialog(ag::DataObject&,
         const ag::DataGuide&)),
         d_data->d_manager, SLOT(addDataPropertiesDialog(
         ag::DataObject&, const ag::DataGuide&)));
  */

  // connect(visualisation, SIGNAL(showAnimationControl()),
  //        this, SLOT(showAnimationControl()));
  // connect(visualisation, SIGNAL(open(ag::VisualisationWindow *)),
  //        this, SLOT(open(ag::VisualisationWindow *)));
  connect(visualisation, SIGNAL(closeGroup()),
         this, SLOT(close()));

/*
  connect(visualisation, SIGNAL(closed(ag::VisualisationWindow *)),
         this, SLOT(detach(ag::VisualisationWindow *)));
*/
}



ag::DataGuide ag::VisGroup::addData(
         std::string const& name,
         dal::DataSpace const& space)
{
  return d_data->d_dataObject.add(name, space);
}



ag::Map2DView* ag::VisGroup::addMap2DView(QWidget* parent)
{
  Map2DView* view = new Map2DView(&(d_data->d_dataObject), parent);

  return view;
}



ag::Map2D* ag::VisGroup::addMap2D(QWidget* parent)
{
  Map2D* map = new Map2D(&(d_data->d_dataObject), parent);

  return map;
}



/*!
  \brief     Adds a new Map View to the group.
*/
ag::Map2DWindow* ag::VisGroup::addMap2DWindow()
{
  //  1. A visualisation is automagicaly deleted from memory if it is closed.
  //     Have a look at qt::AppWindow's constructor.
  //  2. Let's make a deal: the data object will notify us if it changes its
  //     state.
  //  3. Connect the signals from the visualisation to our slots.
  //  4. Add the newly created visualisation to the collection.
  Map2DWindow *m = new Map2DWindow(d_data->d_winProps, &(d_data->d_dataObject));
  addVisualisation(m);

  return m;
}



ag::Map2DWindow* ag::VisGroup::addMap2DWindow(VisualisationWindow *v)
{
  assert(v);

  Map2DWindow *m = new Map2DWindow(d_data->d_winProps, &(d_data->d_dataObject));
  addVisualisation(m);
  m->resize(v->size());                                                   // 10.
  if(v->isVisible())
    m->show();
  return m;
}


ag::MultiMap2DWindow* ag::VisGroup::addMultiMap2DWindow(
         size_t nrRows, size_t nrCols)
{
  MultiMap2DWindow* window = new MultiMap2DWindow(
         d_data->d_winProps, &(d_data->d_dataObject), nrRows, nrCols);
  addVisualisation(window);
  return window;
}



/*!
  \brief     Adds a new Drape View to the group.
*/
ag::Map3DWindow* ag::VisGroup::addMap3DWindow()
{
  //  1. A visualisation is automagicaly deleted from memory if it is closed.
  //     Have a look at qt::AppWindow's constructor.
  //  2. Let's make a deal: the data object will notify us if it changes its
  //     state.
  //  3. Connect the signals from the visualisation to our slots.
  //  4. Add the newly created visualisation to the collection.

  Map3DWindow *m = new Map3DWindow(d_data->d_winProps, &(d_data->d_dataObject));
  addVisualisation(m);
  // m->resize(600, 400);
  return m;
}



ag::Map3DWindow* ag::VisGroup::addMap3DWindow(VisualisationWindow *v)
{
  assert(v);

  Map3DWindow *m = new Map3DWindow(d_data->d_winProps, &(d_data->d_dataObject));
  addVisualisation(m);
  m->resize(v->size());                                                   // 10.
  if(v->isVisible())
    m->show();
  return m;
}



ag::TimePlotWindow* ag::VisGroup::addTimePlotWindow()
{
  TimePlotWindow* timePlot = new TimePlotWindow(d_data->d_winProps,
                   &(d_data->d_dataObject));
  addVisualisation(timePlot);
  // timePlot->resize(600, 400);
  return timePlot;
}



ag::TimePlotWindow* ag::VisGroup::addTimePlotWindow(ag::VisualisationWindow *visualisation)
{
  assert(visualisation);

  TimePlotWindow* timePlot = new TimePlotWindow(d_data->d_winProps,
                   &(d_data->d_dataObject));
  addVisualisation(timePlot);
  timePlot->resize(visualisation->size());
  if(visualisation->isVisible()) {
    timePlot->show();
  }
  return timePlot;
}



ag::CumDistributionFunctionWindow* ag::VisGroup::addProbabilityGraphWindow()
{
  CumDistributionFunctionWindow* window = new CumDistributionFunctionWindow(
         d_data->d_winProps, &(d_data->d_dataObject));
  addVisualisation(window);
  return window;
}



// ag::DataPropertiesDialog* ag::VisGroup::addDataPropertiesDialog(
//          const ag::DataGuide& dataGuide)
// {
//   assert(d_data->d_dataObject.isValid(dataGuide));
// 
//   ag::DataPropertiesDialog* dialog =
//                    new ag::DataPropertiesDialog(d_data->d_winProps,
//                    &(d_data->d_dataObject), dataGuide);
//   addVisualisation(dialog);
//   dialog->show();
//   return dialog;
// }



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Make sure the visualisation is removed from d_visualisationWindows when it is detached.
*/
void ag::VisGroup::addVisualisation(VisualisationWindow* visualisation)
{
  connectVisSignals(visualisation);
  // d_data->d_visualisations.push_back(visualisation);
  // d_data->d_visualisationWindows.push_back(visualisation);
}



//! Show all visualisation windows in the group
void ag::VisGroup::show()
{
  dev::forWhole(d_data->d_visualisationWindows,
         std::mem_fun(&VisualisationWindow::show));
}



/*!
  \brief     Detaches visualisation \a v from the group.
  \param     v Visualisation to detach.
  \sa        close()

  The visualisation will be detached from the layered data object.
*/
/*
void ag::VisGroup::detach(ag::IVisualisation *visualisation)
{
  assert(visualisation);
  std::vector<IVisualisation *>& visualisations = d_data->d_visualisations;

  d_data->d_dataObject.detach(visualisation);
  assert(com::hasElement(visualisations, visualisation));
  visualisations.erase(std::find(visualisations.begin(), visualisations.end(),
         visualisation));
  assert(!com::hasElement(visualisations, visualisation));

  Q_EMIT changed(this);
}
*/



// /*!
//  * \todo
//  *   open method do  not differ  that much, refactor!
//  *   And removing open(ag::Map* m) will still compile, DA HORROR!
//  */
// void ag::VisGroup::open(ag::VisualisationWindow* window)
// {
//   MapWindow* mapWindow = dynamic_cast<MapWindow*>(window);
// 
//   // mapWindow might be 0.
//   open(mapWindow);
// }
// 
// 
// 
// //!
// /*!
//   \param     map Map visualisation, possibly 0.
//   \exception .
//   \warning   .
//   \sa        .
// */
// void ag::VisGroup::open(ag::MapWindow* window)
// {
//   bool opened = false;
// 
//   std::string filename = qt::getOpenFileName(com::FileFormatInfo::csf(),
//          window, 0);
// 
//   if(!filename.empty()) {
// 
//     try {
// 
//       boost::tuple<std::string, dal::DataSpace> tuple =
//          dal::oldStackName2NameSpaceTuple(filename);
//       tuple.get<1>() |= d_data->d_dataObject.dataSpace();
//       tuple.get<1>().eraseDimension(dal::Space);
// 
//       ag::DataGuide guide = addData(tuple.get<0>(), tuple.get<1>());
// 
//       if(window) {
//         window->addAttribute(guide);
//       }
// 
//       /*
//       if(dataObject.cursorPos().isValid()) {
//         dataObject.notify();
//       }
//       else {
//         dataObject.setCursorPos(CursorPos(0.0, 0.0, 0.0,
//               dataObject.firstTimeStep()));
//       }
//       */
// 
//       opened = true;
//     }
//     catch(dal::Exception const& exception) {
//       qt::AppWindow::showError("Aguila", exception.message());
//     }
//     catch(const com::Exception &exception) {
//       qt::AppWindow::showError("Aguila", exception.messages());
//     }
//   }
// }




/*!
  \brief     Closes the visualisation group.
  \sa        detach(IVisualisation *)

  All visualisations of the group will be closed.
*/
void ag::VisGroup::close()
{
  d_data->d_manager->erase(this);
}



// void ag::VisGroup::createAnimationControl()
// {
//   assert(!d_data->d_animationControl);
//   ag::DataObject& dataObject = d_data->d_dataObject;
// 
//   d_data->d_animationControl = new AnimationControl(&dataObject);
// 
//   connect(&dataObject.animationManager(), SIGNAL(started()),
//          d_data->d_animationControl, SLOT(updateInterface()));
//   connect(&dataObject.animationManager(), SIGNAL(paused()),
//          d_data->d_animationControl, SLOT(updateInterface()));
//   connect(&dataObject.animationManager(), SIGNAL(stopped()),
//          d_data->d_animationControl, SLOT(updateInterface()));
// }


/*!
 * \todo
 *   are we sure we want a different action on menu (dialog) and toolbar
 *   (start)? Maybe a separate menu item to show the "Animation Control"
 */
// void ag::VisGroup::showAnimationControl()
// {
//   assert(d_data->d_animationControl);
// 
//   if(d_data->d_animationControl->isVisible()) {
//     d_data->d_animationControl->raise();
//   }
//   else {
//     d_data->d_animationControl->show();
//   }
// }



/*
void ag::VisGroup::updateTime(size_t t)
{
  const CursorPos& cp(d_data->d_dataObject.cursorPos());
  CursorPos cpNew(cp.x(), cp.y(), cp.z(), t);
  d_data->d_dataObject.setCursorPos(cpNew);
}
*/



/*
void ag::VisGroup::copy(IVisualisation *v)
{
  assert(v);

  VisGroup* vg = d_data->d_manager->newGroup(d_data->d_dataObject);
  IVisualisation* vCopy = v->copy(&vg->d_data->d_dataObject);
  vg->addVisualisation(vCopy);
  vg->d_data->d_dataObject.notify();
  vCopy->resize(v->size());
  if(v->isVisible())
    vCopy->show();
  Q_EMIT changed(this);
}
*/



ag::DataObject& ag::VisGroup::dataObject()
{
  return d_data->d_dataObject;
}



size_t ag::VisGroup::nrVisualisations() const
{
  return d_data->d_dataObject.nrObservers();
}



/*
ag::VisGroup::vis_iterator ag::VisGroup::vis_begin()
{
  return d_data->d_visualisations.begin();
}



ag::VisGroup::vis_iterator ag::VisGroup::vis_end()
{
  return d_data->d_visualisations.end();
}



ag::VisGroup::const_vis_iterator ag::VisGroup::vis_begin() const
{
  return d_data->d_visualisations.begin();
}



ag::VisGroup::const_vis_iterator ag::VisGroup::vis_end() const
{
  return d_data->d_visualisations.end();
}



ag::VisGroup::viswin_iterator ag::VisGroup::viswin_begin()
{
  return d_data->d_visualisationWindows.begin();
}



ag::VisGroup::viswin_iterator ag::VisGroup::viswin_end()
{
  return d_data->d_visualisationWindows.end();
}



ag::VisGroup::const_viswin_iterator ag::VisGroup::viswin_begin() const
{
  return d_data->d_visualisationWindows.begin();
}



ag::VisGroup::const_viswin_iterator ag::VisGroup::viswin_end() const
{
  return d_data->d_visualisationWindows.end();
}
*/



bool ag::VisGroup::contains(IVisualisation const* visualisation) const
{
  return std::find(d_data->d_dataObject.begin(), d_data->d_dataObject.end(),
         visualisation) != d_data->d_dataObject.end();
}



/*
void ag::VisGroup::close(ag::VisualisationWindow* visualisation)
{
  assert(visualisation);
  visualisation->close(true);
  if(!nrVisualisations()) {
    close();
  }
}
*/



void ag::VisGroup::reloadData()
{
  // cout << "reload data" << endl;
  /*
    for all visgroups {
      group->reload()
      . lees alleen die datasets die nieuwer zijn dan de eerste keer
      . update alleen die visualisaties waar datasets in voorkomen die gereload
        zijn
    }
  */

}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Handle initial quantile if CUM_PROBABILITIES is available.
*/
void ag::VisGroup::sync()
{
  /*
  // Check if group has an invalid cursor.
  if(!dataObject().cursorPos().isValid()) {

    // Inititialize the cursor position. This will also load needed data and
    // notify all visualisations in the group.
    dataObject().setCursorPos(CursorPos(0.0, 0.0, 0.0,
                   dataObject().firstTimeStep()));
  }
  else {

    // New datasets can have an effect on the extent of the time axis.
    // For example, when a static map is loaded first the first and last
    // time step are set to 0. If a dynamic map is loaded afterwards the
    // cursor position should be reset.
    if(dataObject().cursorPos().time() == 0 &&
                   dataObject().firstTimeStep() > 0) {

      // The current cursor position was valid.

      dataObject().setCursorPos(CursorPos(0.0, 0.0, 0.0,
                   dataObject().firstTimeStep()));
    }
    else {

      // The current cursor position is still valid.

      // New data doesn't have valid data loaded yet.
      // dataObject().loadData();

      // Force a rescan of all data objects. Visualisations check if they
      // should update, so this won't result in redundant redraws.
      dataObject().notify();
    }
  }
    */

  dataObject().notify();
}



ag::AnimationControl* ag::VisGroup::addAnimationDialog()
{
  return AnimationControl::instance(&(d_data->d_dataObject));
}



ag::CursorWindow* ag::VisGroup::addCursorWindow()
{
  return CursorWindow::instance(&(d_data->d_dataObject));
}



// namespace ag {
// 
// #ifdef DEBUG_DEVELOP
// TestVisualisation* VisGroup::addTestVisualisation()
// {
//   TestVisualisation* visualisation = new TestVisualisation(
//          d_data->d_winProps, &(d_data->d_dataObject));
//   addVisualisation(visualisation);
// 
//   return visualisation;
// }
// #endif
// 
// } // namespace



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


