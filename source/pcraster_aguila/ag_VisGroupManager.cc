#include "ag_VisGroupManager.h"
#include <ctime>
#include <string>
#include <QApplication>
#include <QObject>
#include "dev_Algorithm.h"
#include "com_exception.h"
#include "qt_AppWindowProperties.h"
#include "ag_DataObject.h"
#include "ag_VisGroup.h"
#include "ag_VisualisationWindow.h"



//------------------------------------------------------------------------------

namespace ag {

class VisGroupManagerPrivate
{
public:

  qt::AppWindowProperties d_winProps;
  std::vector<ag::VisGroup *> d_groups;
  // ControlCenter *d_controlCenter;

  VisGroupManagerPrivate(const qt::AppWindowProperties& props)
    : d_winProps(props) // , d_controlCenter(0)
  {
  }

  ~VisGroupManagerPrivate()
  {
  }
};

}

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

ag::VisGroupManager::VisGroupManager(const qt::AppWindowProperties& props)

  : QObject(0),
    // ag::Configurable(),
    d_data(new VisGroupManagerPrivate(props))

{
}



ag::VisGroupManager::~VisGroupManager()
{
  clean();
}



void ag::VisGroupManager::clean()
{
  std::vector<VisGroup *>& groups(d_data->d_groups);

  while(!groups.empty()) {
    erase(*groups.begin());
  }
}



ag::VisGroup *ag::VisGroupManager::newGroup()
{
  VisGroup *vg = new VisGroup(d_data->d_winProps, this);
  add(vg);
  // updateControlCenter();
  return vg;
}



/*
ag::VisGroup *ag::VisGroupManager::newGroup(const ag::DataObject &o)
{
  VisGroup *vg = new VisGroup(d_data->d_winProps, this, o);
  add(vg);
  updateControlCenter();
  return vg;
}



ag::VisGroup* ag::VisGroupManager::newGroup(const pcrxml::VisualisationGroup& vgXml)
{
  VisGroup* vg = new VisGroup(d_data->d_winProps, this, vgXml);
  add(vg);
  updateControlCenter();
  return vg;
}
*/



void ag::VisGroupManager::add(VisGroup *g)
{
  std::vector<VisGroup *>& gs = d_data->d_groups;
  assert(!dev::hasElement(gs, g));
  gs.push_back(g);
}



void ag::VisGroupManager::erase(VisGroup *group)
{
  // Erase the group from the manager. No objects which refer to the manager
  // will know about the group anymore.
  std::vector<VisGroup *>& groups = d_data->d_groups;
  assert(dev::hasElement(groups, group));
  groups.erase(std::find(groups.begin(), groups.end(), group));
  assert(!dev::hasElement(groups, group));

/*
  if(controlCenterIsAlive()) {
    // Remove the group from the list view in the control center widget. As
    // a side effect, this will call ControlCenter::updateContents().
    d_data->d_controlCenter->removeVisGroupItem(group);
  }
  */

  delete group;

  // Removing g from the control center updated the view already.
  // updateControlCenter();
}



ag::Map2DView* ag::VisGroupManager::addMap2DView(ag::VisGroup* group,
         QWidget* parent)
{
  assert(group);
  Map2DView* view = group->addMap2DView(parent);
  return view;
}



ag::Map2D* ag::VisGroupManager::addMap2D(ag::VisGroup* group, QWidget* parent)
{
  assert(group);
  Map2D* map = group->addMap2D(parent);
  return map;
}



ag::Map2DWindow* ag::VisGroupManager::addMap2DWindow(
         ag::VisGroup* group)
{
  assert(group);
  Map2DWindow* map = group->addMap2DWindow();

  return map;
}



ag::Map2DWindow* ag::VisGroupManager::addMap2DWindow(ag::VisualisationWindow* v)
{
  assert(v);
  VisGroup *g = findGroup(v);
  assert(g);
  Map2DWindow* map = g->addMap2DWindow(v);
  return map;
}



ag::MultiMap2DWindow* ag::VisGroupManager::addMultiMap2DWindow(
         ag::VisGroup* group, size_t nrRows, size_t nrCols)
{
  assert(group);
  MultiMap2DWindow* window = group->addMultiMap2DWindow(nrRows, nrCols);
  return window;
}



ag::Map3DWindow* ag::VisGroupManager::addMap3DWindow(ag::VisGroup* g)
{
  assert(g);
  Map3DWindow* map = g->addMap3DWindow();

  return map;
}



ag::Map3DWindow* ag::VisGroupManager::addMap3DWindow(ag::VisualisationWindow *v)
{
  assert(v);
  VisGroup *g = findGroup(v);
  assert(g);
  Map3DWindow* map = g->addMap3DWindow(v);
  return map;
}



ag::Map3DWindow* ag::VisGroupManager::addMap3DWindow(ag::VisGroup *g,
                   ag::VisualisationWindow *v)
{
  assert(v && g);
  Map3DWindow* map = g->addMap3DWindow(v);
  return map;
}



ag::TimePlotWindow* ag::VisGroupManager::addTimePlotWindow(ag::VisGroup* visGroup)
{
  assert(visGroup);
  TimePlotWindow* plot = visGroup->addTimePlotWindow();
  return plot;
}



ag::TimePlotWindow* ag::VisGroupManager::addTimePlotWindow(
                   ag::VisualisationWindow* visualisation)
{
  assert(visualisation);
  VisGroup *visGroup = findGroup(visualisation);
  assert(visGroup);
  TimePlotWindow* plot = visGroup->addTimePlotWindow(visualisation);
  return plot;
}



ag::TimePlotWindow* ag::VisGroupManager::addTimePlotWindow(ag::VisGroup* visGroup,
                   ag::VisualisationWindow* visualisation)
{
  assert(visGroup && visualisation);
  TimePlotWindow* plot = visGroup->addTimePlotWindow(visualisation);
  return plot;
}



ag::CumDistributionFunctionWindow*
ag::VisGroupManager::addProbabilityGraphWindow(
         VisGroup* group)
{
  assert(group);
  CumDistributionFunctionWindow* window =
         group->addProbabilityGraphWindow();
  return window;
}


void ag::VisGroupManager::newMap2DWindow(ag::VisualisationWindow *v)
{
  VisGroup* vg = newGroup();
  (void)vg->addMap2DWindow(v);
}



void ag::VisGroupManager::newMap3DWindow(ag::VisualisationWindow *v)
{
  VisGroup* vg = newGroup();
  (void)vg->addMap3DWindow(v);
  // updateControlCenter();
}



void ag::VisGroupManager::newTimePlotWindow(ag::VisualisationWindow *visualisation)
{
  VisGroup* visGroup = newGroup();
  (void)visGroup->addTimePlotWindow(visualisation);
}



void ag::VisGroupManager::close()
{
  clean();
  qApp->quit();
}



/*
void ag::VisGroupManager::showControlCenter()
{
  if(!controlCenterIsAlive()) {
    d_data->d_controlCenter = new ControlCenter(d_data->d_winProps, this, 0, 0);
    d_data->d_controlCenter->resize(200, 300);
  }

  if(!d_data->d_controlCenter->isVisible()) {
    d_data->d_controlCenter->show();
  }
  else {
    d_data->d_controlCenter->raise();
  }
}
*/




// Returns true if the control center is alive and kicking.
/*
  \return    true or false.
  \warning   The d_data->d_controlCenter pointer can't be trusted if this
             function returns false. This is because the control center is a
             top level widget which gets deleted if the user closes the
             widget.
*/
/*
bool ag::VisGroupManager::controlCenterIsAlive() const
{
  return QObject::objectTrees()->containsRef(d_data->d_controlCenter);
}
*/



// ag::DataPropertiesDialog* ag::VisGroupManager::addDataPropertiesDialog(
//                    ag::DataObject& dataObject,
//                    const ag::DataGuide& dataGuide)
// {
//   assert(dataObject.isValid(dataGuide));
//   ag::VisGroup* group = findGroup(dataObject);
//   assert(group);
//   return group->addDataPropertiesDialog(dataGuide);
// }



ag::VisGroupManager::const_iterator ag::VisGroupManager::begin() const
{
  return d_data->d_groups.begin();
}



ag::VisGroupManager::const_iterator ag::VisGroupManager::end() const
{
  return d_data->d_groups.end();
}



ag::VisGroupManager::iterator ag::VisGroupManager::begin()
{
  return d_data->d_groups.begin();
}



ag::VisGroupManager::iterator ag::VisGroupManager::end()
{
  return d_data->d_groups.end();
}



ag::VisGroupManager::const_reverse_iterator ag::VisGroupManager::rbegin() const
{
  return d_data->d_groups.rbegin();
}



ag::VisGroupManager::const_reverse_iterator ag::VisGroupManager::rend() const
{
  return d_data->d_groups.rend();
}



ag::VisGroupManager::reverse_iterator ag::VisGroupManager::rbegin()
{
  return d_data->d_groups.rbegin();
}



ag::VisGroupManager::reverse_iterator ag::VisGroupManager::rend()
{
  return d_data->d_groups.rend();
}



bool ag::VisGroupManager::exists(const VisGroup* group) const
{
  return dev::hasElement(*this,group);
}



ag::VisGroup* ag::VisGroupManager::findCompatibleGroup(
         std::string const& name,
         dal::DataSpace const& space)
{
  VisGroup* group = 0;

  // Test group is reverse order.
  for(std::vector<VisGroup*>::reverse_iterator it = d_data->d_groups.rbegin();
                   it != d_data->d_groups.rend(); ++it)
  {
    if((*it)->dataObject().compatibleData(name, space)) {
      group = *it;
      break;
    }
  }

  // Possibly 0.
  return group;
}



void ag::VisGroupManager::show()
{
/*
  if(nrVisualisations() == 0) {
    showControlCenter();
  }
  else {
  */
    for(iterator it = begin(); it != end(); ++ it) {
      (*it)->show();
    }
    /*
  }
  */
}



// void ag::VisGroupManager::open(VisGroup *g)
// {
//   assert(g);
//   g->open();
// }



/*
void ag::VisGroupManager::close(ag::VisGroup* g, ag::VisualisationWindow* v)
{
  assert(g && v);
  g->close(v);
  // updateControlCenter();
}
*/



/*
void ag::VisGroupManager::copy(ag::VisGroup* g, ag::IVisualisation* v)
{
  assert(g && v);
  g->copy(v);

// VisGroup* vg = newGroup(g->dataObject());
// IVisualisation* vCopy = v->copy(&vg->dataObject());
// vg->addIVisualisation(vCopy);
// vCopy->resize(v->size());
// if(v->isVisible())
//   vCopy->show();
// updateControlCenter();
}
*/



size_t ag::VisGroupManager::nrGroups() const
{
  return d_data->d_groups.size();
}



size_t ag::VisGroupManager::nrVisualisations() const
{
  size_t n = 0;
  for(const_iterator it = begin(); it != end(); ++ it) {
    n += (*it)->nrVisualisations();
  }
  return n;
}



/*
void ag::VisGroupManager::updateControlCenter()
{
  if(controlCenterIsAlive() && d_data->d_controlCenter->isVisible()) {
    d_data->d_controlCenter->updateContents();
  }
}
*/



ag::VisGroup *ag::VisGroupManager::findGroup(IVisualisation *v)
{
  VisGroup* group = 0;
  for(iterator groupIt = begin(); groupIt != end(); ++ groupIt) {
    if((*groupIt)->contains(v)) {
      group = *groupIt;
      break;
    }
  }

  assert(group);
  return group;
}



ag::VisGroup *ag::VisGroupManager::findGroup(ag::DataObject& dataObject)
{
  VisGroup* group = 0;
  for(iterator groupIt = begin(); groupIt != end(); ++ groupIt) {
    if(&(*groupIt)->dataObject() == &dataObject) {
      group = *groupIt;
      break;
    }
  }

  assert(group);
  return group;
}



//void ag::VisGroupManager::configure(const QDomElement& /* n */)
// {
//   assert(false);
// /*
//   const pcrxml::VisualisationConfiguration vc(n);
// 
//   // We have the main node and checked it. Let's find some contents.
//   // The 'pcrviscfg' has attributes: 'date', 'version', 'os' and 'cwd'.
//   assert(vc.date.present());
//   assert(vc.version.present()); // version is not needed
//   assert(vc.os.present());
//   assert(vc.cwd.present());
// 
//   for(size_t i = 0; i < vc.visualisationGroup.size(); i++) {
//     assert(vc.visualisationGroup[i]);
//     newGroup(*vc.visualisationGroup[i]);
//   }
// */
// }



//! save configuration to file \a fName
/*!
   \return DOM tree of pcrxml::VisualisationConfiguration()
 */
// QDomElement ag::VisGroupManager::configuration(const std::string& fName) const
// {
//   assert(!fName.empty());
//   pcrxml::VisualisationConfiguration vc;
//
//   std::time_t t = std::time(NULL);
//   std::string ts = std::string(std::ctime(&t));
//   assert(!ts.empty());
//   ts.erase(ts.length() - 1);           // Last character is '\n'.
//   vc.date = pcrxml::String(ts);
//
//   assert(false);
//   // yepyep:
//   // vc.version = pcrxml::String(APPVERSION);
//ifdef WIN32
//   vc.os = pcrxml::RuntimePlatform(
//                   pcrxml::RuntimePlatform::RP_WIN32);
//elif __linux__
//   vc.os = pcrxml::RuntimePlatform(
//                   pcrxml::RuntimePlatform::RP_LINUX);
//else
//   #error unknown target platform
//endif
//
//   vc.cwd = pcrxml::String(com::currentWorkingDirectory().toString());
//
//   // groupen dumpen
//
//   // Add main, top level, element to the main document.
//
//   vc.writeToFile(fName.c_str());  // <-- change this to PathName.
//   QDomDocument doc(vc.asDomDocument());
//   return doc.documentElement();
// }



// void ag::VisGroupManager::loadSettings(const com::PathName& pn)
// {
//   ag::Configuration conf(pn);
//   configure(conf.documentElement());
// }



// void ag::VisGroupManager::saveSettings(const com::PathName& pn) const
// {
//   // Easy!
//   ag::Configuration conf(pn, *this);
// }



//! Syncs the settings of nested data objects.
/*!
  \sa        DataObject::sync()
*/
void ag::VisGroupManager::sync()
{
  for(iterator it = begin(); it != end(); ++it) {
    (*it)->sync();
  }
}



ag::VisGroup* ag::VisGroupManager::group(IVisualisation const* visualisation)
{
  VisGroup* group = 0;

  for(iterator it = begin(); it != end(); ++ it) {
    if((*it)->contains(visualisation)) {
      group = *it;
      break;
    }
  }

  assert(group);

  return group;
}



ag::CursorWindow* ag::VisGroupManager::addCursorWindow(
         VisGroup* group)
{
  assert(group);

  return group->addCursorWindow();
}



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


