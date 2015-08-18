#ifndef INCLUDED_AG_VIEWER
#define INCLUDED_AG_VIEWER

// Library headers.
#include <cassert>
#include <vector>
#include <string>
#include <boost/noncopyable.hpp>
#include <boost/tuple/tuple.hpp>

// PCRaster library headers.
#include "dal_Driver.h" // DataSpaceQueryResult

// Module headers.
#include "ag_VisGroupManager.h"

namespace pcrxml {
  class Aguila;
}
namespace dal {
  class DataSpace;
}
namespace qt {
  class AppWindowProperties;
}
namespace ag {
  class DataGuide;
  class IVisualisation;
  class ViewerPrivate;
  class VisGroup;
}



namespace ag {



//! Viewer class to add Aguila functionality to an existing app.
/*!
  This class contains high level functionality for visualising files with data.
  If you know what kind of data a file contains (e.g.: stack data or a time
  series), than you can use the specific display functions to create
  visualisations (e.g.: displayMap2D(const std::string&, VisGroup*, bool) or
  displayTimePlot(const std::string&, VisGroup*, bool). If you don't know the
  data type or if you want default visualisations you can call
  display(const std::string&, VisGroup*, bool).

  Each display functions has two kinds of interface: with std::string or with
  std::vector<std::string>. The first is to create a visualisation with data from one file
  visualised and the second is for visualising data from more than one data
  set in one visualisation.

  If you know to which visualisation group you want to add a visualisation you
  can use the VisGroup* argument. Make sure that the data is compatible with
  the data already present in the group. If you want to add the visualisation
  to the default group (the first group which accepts the data, starting with
  the last one created) you can pass 0 (the default) for the group argument.

  The last steps taken by all display functions is to sync the data in
  the data object of the visualisation group used and to show the visualisation.
  These are expensive operations so whenever possible try to display all
  data files you want in one visualisation using the interface with the
  PathNameVector argument. Alternatively, you might want to pass false for the
  sync parameter of the various display functions. This way none of the
  datasets passed in will be sunken, and hence shown. The visualisation will
  be shown (show() is called on the widget), but none of the datasets passed
  in will be shown. Only after you call sync() yourself will all the datasets
  be visualised.

  Each display function returns the visualisation group to which the
  visualisation was added.

  Default visualisations: stack: mapView, time series: timeGraphView.

  \todo Gebruik dal voor i/o. - hele pathname spul uit de interface, dataset heeft een naam en dat zou een filename kunnen zijn, maar hoeft niet (zie dal) - testen van type data achteraf doen: open data, bekijk wat voor data het is en lees door als het ok is (map vis wil raster data, timeplot wil tijdserie, etc) - misschien moet dal aangepast dat er functies komen die een stream krijgen ipv bestandsnaam, anders moet elk bestand twee keer geopend worden: een keer in open(), waarna getest wordt en een keer in read(). - als je weet wat voor dat het moet zijn dan kan je specialistische dal objecten gebruiken voor het lezen: rasterdal, tabledal, etc: scheelt een test. - addData in de visgroup moet dan worden uitgesplitst in addRaster (addStack?), addTable, addScript. Scripts (textfiles toevoegen aan dal) - add Stackname class to dal which replaces CSFStackName, but than without the PathName and CSF specific stuff, maybe call it StackInfo
*/
class PCR_AG_DECL Viewer: private boost::noncopyable
{

  friend class Aguila;

public:
  struct QueryResult: public dal::DataSpaceQueryResult {
    std::string name;
    QueryResult(std::string const& _name,
                DataSpaceQueryResult const& q):
      dal::DataSpaceQueryResult(q),
      name(_name) {}
  };

  typedef std::vector<QueryResult>    QueryResults;

  typedef VisGroup*  (ag::Viewer::*CreateView)   (std::vector<DataGuide> const& guides,
                                      VisGroup* group, bool sync);

private:

  static Viewer*   d_instance;

  /// dal::Dal         d_dal;

  ViewerPrivate*   d_data;

  size_t           d_nrViewRows;

  size_t           d_nrViewCols;

  bool             exists              (const VisGroup* group) const;

  VisGroup*        newGroup            ();

  QueryResults     querySearchDataSpace(
                                     dal::DataSpace           const& searchSpace,
                                     std::vector<std::string> const& names);

  VisGroup*        groupFor            (QueryResults const& results);

  VisGroup*        groupFor            (dal::DataSpaceQueryResult const& result);

  template<class T>
  VisGroup*        groupFor            (T const& dataset,
                                        dal::DataSpace const& space);

  template<class T>
  VisGroup*        findCompatibleGroup (T const& dataset,
                                        dal::DataSpace const& space);

  VisGroup*        groupFor            (std::string const& name);

  VisGroup*        groupFor            (std::string const& name,
                                        dal::DataSpace const& space);

  VisGroup*        findCompatibleGroup (std::string const& name,
                                        dal::DataSpace const& space);

  VisGroup*        displayMap2D        (QueryResults const& results,
                                        VisGroup* group,
                                        bool sync);

  VisGroup*        displayMap3D        (QueryResults const& results,
                                        VisGroup* group,
                                        bool sync);

  VisGroup*        displayMultiMap2D   (
                                  QueryResults const& queryResults,
                                        VisGroup* group,
                                        size_t nrRows=1,
                                        size_t nrCols=1,
                                        bool sync=true);
  VisGroup*        displayValueView    (QueryResults const& queryResults,
                                        VisGroup* group,
                                        bool sync);

  VisGroup*        displayMap3D        (dal::DataSpace const& searchSpace,
                                        std::vector<std::string> const& names,
                                        VisGroup* group=0,
                                        bool sync=true);

// #ifdef DEBUG_DEVELOP
// 
//   VisGroup*        displayTestVisualisation(
//                                         dal::DataSpace const& space,
//                                         std::vector<std::string> const& names,
//                                         VisGroup* group=0,
//                                         bool sync=true);
// #endif

  // VisGroup*        display             (dal::DataSpace const& space,
  //                                       std::vector<std::string> const& names,
  //                                       VisGroup* group=0,
  //                                       bool sync=true);

  VisGroup*        displayMap2D        (dal::DataSpace const& space,
                                        std::vector<std::string> const& names,
                                        VisGroup* group=0,
                                        bool sync=true);

  VisGroup*        displayMultiMap2D   (dal::DataSpace const& space,
                                        std::vector<std::string> const& names,
                                        VisGroup* group=0,
                                        size_t nrRows=1,
                                        size_t nrCols=1,
                                        bool sync=true);

  // VisGroup*        displayTimePlot     (dal::DataSpace const& space,
  //                                       std::vector<std::string> const& names,
  //                                       VisGroup* group=0,
  //                                       bool sync=true);

  VisGroup*        displayProbabilityGraphWindow(
                                        dal::DataSpace const& space,
                                        std::vector<std::string> const& names,
                                        VisGroup* group=0,
                                        bool sync=true);

  VisGroup*        displayValueView    (dal::DataSpace const& space,
                                        std::vector<std::string> const& names,
                                        VisGroup* group=0,
                                        bool sync=true);

  // VisGroup*        display             (std::string const& name,
  //                                       VisGroup* group=0,
  //                                       bool sync=true);

  // VisGroup*        display             (std::vector<std::string> const& names,
  //                                       VisGroup* group=0,
  //                                       bool sync=true);

  VisGroup*        displayMap2D        (std::string const& name,
                                        VisGroup* group=0,
                                        bool sync=true);

  VisGroup*        displayMap2D        (std::vector<std::string> const& names,
                                        VisGroup* group=0,
                                        bool sync=true);

  VisGroup*        displayMap3D        (std::string const& name,
                                        VisGroup* group=0,
                                        bool sync=true);

  VisGroup*        displayMap3D        (std::vector<std::string> const& names,
                                        VisGroup* group=0,
                                        bool sync=true);

  // VisGroup*        displayTimePlot     (std::string const& fileName,
  //                                       VisGroup* group=0,
  //                                       bool sync=true);

  // VisGroup*        displayTimePlot     (std::vector<std::string> const& names,
  //                                       VisGroup* group=0,
  //                                       bool sync=true);
  VisGroup*        displayMultiMap2D   (std::vector<std::string> const& names,
                                        VisGroup* group=0,
                                        size_t nrRows=1,
                                        size_t nrCols=1,
                                        bool sync=true);

  template<typename T>
    static std::vector<T> vectorizer   (T item);

  VisGroup*        createView          (std::vector<std::vector<DataGuide> > const& guideCollections,
                                        VisGroup* group,
                                        bool sync);

  VisGroup*        createMapView       (std::vector<std::vector<DataGuide> > const& guideCollections,
                                        VisGroup* group,
                                        bool sync);

  VisGroup*        createDrapeView     (std::vector<std::vector<DataGuide> > const& guideCollections,
                                        VisGroup* group,
                                        bool sync);

  VisGroup*        createTimeGraphView (std::vector<std::vector<DataGuide> > const& guideCollections,
                                        VisGroup* group,
                                        bool sync);

  VisGroup*        createProbabilityGraphView(
                        std::vector<std::vector<DataGuide> > const& collections,
                        VisGroup* group,
                        bool sync);

  VisGroup*        createMultiMapView  (std::vector<std::vector<DataGuide> > const& guideCollections,
                                        VisGroup* group,
                                        bool sync);

  bool             multiView           () const;

  static void      resetInstance       ();

protected:

                   Viewer              (qt::AppWindowProperties const& awp);

public:

  static Viewer*   instance            (
                                  qt::AppWindowProperties const& properties);

  static Viewer*   instance            ();

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~Viewer             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             createViews         (pcrxml::Aguila const &cfg);

  VisGroup*        createView          (std::vector<DataGuide> const& guides,
                                        VisGroup* group,
                                        bool sync);

  VisGroup*        createMapView       (std::vector<DataGuide> const& guides,
                                        VisGroup* group,
                                        bool sync=true);

  VisGroup*        createMultiMapView  (std::vector<DataGuide> const& guides,
                                        VisGroup* group,
                                        bool sync);

  VisGroup*        createDrapeView     (std::vector<DataGuide> const& guides,
                                        VisGroup* group,
                                        bool sync);

  VisGroup*        createTimeGraphView (std::vector<DataGuide> const& guides,
                                        VisGroup* group,
                                        bool sync=true);

  VisGroup*        createProbabilityGraphView(
                                        std::vector<DataGuide> const& guides,
                                        VisGroup* group,
                                        bool sync=true);








  VisGroup*        displayMap2D        (DataGuide const& guide,
                                        VisGroup* group,
                                        bool sync=true);


  VisGroup*        displayTimePlot     (DataGuide const& guide,
                                        VisGroup* group,
                                        bool sync=true);

  // VisGroup*        createValueView     (std::vector<DataGuide> const& guides,
  //                                       VisGroup* group,
  //                                       bool sync);

  VisGroup*        displayProbabilityGraphWindow(
                                        DataGuide const& guide,
                                        VisGroup* group,
                                        bool sync=true);

  VisGroup*        displayAnimationDialog(VisGroup* group,
                                        bool sync=true);

  VisGroup*        displayCursor       (VisGroup*   group,
                                        std::string const& cursorValueMonitorFile="",
                                        std::string const& fileToGetCursorValue="",
                                        bool sync=true);

  void             sync                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           nrVisualisations    () const;

  VisGroupManager& manager             ();

  VisGroup*        group               (IVisualisation const* visualisation);

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class T>
inline ag::VisGroup* Viewer::groupFor(
         T const& dataset,
         dal::DataSpace const& space)
{
  VisGroup* group = findCompatibleGroup(dataset, space);

  if(!group) {
    group = newGroup();
  }

  assert(group && exists(group));

  return group;
}



template<class T>
inline ag::VisGroup* Viewer::findCompatibleGroup(
         T const& dataset,
         dal::DataSpace const& space)
{
  return manager().findCompatibleGroup(dataset, space);
}



template<typename T>
inline  std::vector<T> Viewer::vectorizer(T item)
{
  std::vector<T> v;
  v.push_back(item);
  return v;
}



inline VisGroup* Viewer::displayMap2D(
         DataGuide const& guide,
         VisGroup* group,
         bool sync)
{
  assert(group);
  return createMapView(vectorizer(guide), group, sync);
}

inline VisGroup* Viewer::displayProbabilityGraphWindow(
         DataGuide const& guide,
         VisGroup* group,
         bool sync)
{
  assert(group);
  return createProbabilityGraphView(vectorizer(guide), group, sync);
}

inline VisGroup* Viewer::displayTimePlot(
         DataGuide const& guide,
         VisGroup* group,
         bool sync)
{
  assert(group);
  return createTimeGraphView(vectorizer(guide), group, sync);
}

// inline VisGroup* Viewer::displayTimePlot(
//          std::string const& name,
//          VisGroup* group,
//          bool sync)
// {
//   return displayTimePlot(dal::DataSpace(), vectorizer(name), group, sync);
// }
inline VisGroup* Viewer::displayMap3D(
         std::string const& name,
         VisGroup* group,
         bool sync)
{
  return displayMap3D(dal::DataSpace(), vectorizer(name), group, sync);
}
inline VisGroup* Viewer::displayMap2D(
         std::string const& name,
         VisGroup* group,
         bool sync)
{
  return displayMap2D(dal::DataSpace(), vectorizer(name), group, sync);
}
// inline VisGroup* Viewer::display(std::string const& name,
//          VisGroup* group, bool sync)
// {
//   return display(dal::DataSpace(), vectorizer(name), group, sync);
// }
// inline VisGroup* Viewer::displayTimePlot(
//          std::vector<std::string> const& names,
//          VisGroup* group,
//          bool sync)
// {
//   return displayTimePlot(dal::DataSpace(), names, group, sync);
// }
inline VisGroup* Viewer::displayMap3D(
         std::vector<std::string> const& names,
         VisGroup* group,
         bool sync)
{
  return displayMap3D(dal::DataSpace(), names, group, sync);
}
inline VisGroup* Viewer::displayMultiMap2D(
         std::vector<std::string> const& names,
         VisGroup* group,
         size_t nrRows,
         size_t nrCols,
         bool sync)
{
  return displayMultiMap2D(dal::DataSpace(), names, group,
         nrRows, nrCols, sync);
}
inline VisGroup* Viewer::displayMap2D(
         std::vector<std::string> const& names,
         VisGroup* group,
         bool sync)
{
  return displayMap2D(dal::DataSpace(), names, group, sync);
}
// inline VisGroup* Viewer::display(
//          std::vector<std::string> const& names,
//          VisGroup* group, bool sync)
// {
//   return display(dal::DataSpace(), names, group, sync);
// }





//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
