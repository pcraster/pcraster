#include "ag_Viewer.h"
#include <QApplication>
#include "dal_Client.h"
#include "dal_Dal.h"
#include "dal_DataSpace.h"
#include "dal_Table.h"
#include "com_exception.h"
#include "ag_AnimationControl.h"
#include "ag_CumDistributionFunctionWindow.h"
#include "ag_CursorWindow.h"
#include "ag_DataObject.h"
#include "ag_Map2DWindow.h"
#include "ag_Map3DWindow.h"
#include "ag_MultiMap2DWindow.h"
#include "ag_TimePlotWindow.h"
#include "ag_VisGroup.h"
#include "ag_VisGroupManager.h"
#include "AguilaXSD.h"
#include "ag_DataConfiguration.h"
#include "ag_XMLViewItems.h"



namespace ag {

class ViewerPrivate
{
public:

  // Visualisation manager.
  VisGroupManager* d_manager;

  ViewerPrivate(const qt::AppWindowProperties& awp)
    : d_manager(new VisGroupManager(awp))
  {
  }

  ~ViewerPrivate()
  {
    delete d_manager;
  }
};



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

Viewer* Viewer::d_instance = 0;

Viewer* Viewer::instance(
         qt::AppWindowProperties const& properties)
{
  assert(!d_instance);
  d_instance = new Viewer(properties);

  return d_instance;
}



Viewer* Viewer::instance()
{
  assert(d_instance);

  // if(!d_instance) {
  //   d_instance = new Viewer(qt::AppWindowProperties("Viewer", "", __DATE__,
  //        com::DEVELOPERMESSAGE_KOR, (char const **)pcr_180x39_xpm));
  // }

  // assert(d_instance);

  return d_instance;
}



void Viewer::resetInstance()
{
  if(d_instance) {
    delete d_instance;
    d_instance = 0;
  }

  assert(!d_instance);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      See dal::dataSpaceWithNarrowedDimension()
*/
static dal::DataSpace fixForScenario(
    dal::DataSpace const& dataSpace,
    std::vector<std::string> const &scenarios,
    size_t                        j)
{
   // subSpace copy is modified if scenarios are used
   dal::DataSpace subSpace(dataSpace);
   size_t indexOfScenarios = subSpace.indexOf(dal::Scenarios);

   if(indexOfScenarios < dataSpace.rank()) {
     subSpace.dimension(indexOfScenarios).setValue<std::string>(scenarios[j]);
    }
   return subSpace;
}


//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructs an Viewer application object.
/*!
 * \param awp the properties of the app that uses Viewer
 */
Viewer::Viewer(
         qt::AppWindowProperties const& awp)

  : /// d_dal(true),
    d_data(new ViewerPrivate(awp)),
    d_nrViewRows(1),
    d_nrViewCols(1)

{
}



//! Destructor.
/*!
*/
Viewer::~Viewer()
{
  delete d_data;
}



VisGroupManager& Viewer::manager()
{
  return *d_data->d_manager;
}



/*! \brief add views from cfg
 *  \throws if cfg does not have a views element (nothing to do!)
 */
void Viewer::createViews(
         pcrxml::Aguila const &cfg)
{
  if(!cfg.visualisationGroup().view().size()) {
    com::Exception exception("Nothing to do!");
    exception.append("Use -h or --help for usage information");
    throw exception;
  }

  if(cfg.multiView().present()) {
    d_nrViewRows =
         pcrxsd::fundamentalBaseCast<size_t>(cfg.multiView()->nrRows());
    d_nrViewCols =
         pcrxsd::fundamentalBaseCast<size_t>(cfg.multiView()->nrCols());
  }

  // Currently, the XML contains just one group. In the future: loop.
  VisGroup* group = newGroup();
  DataConfiguration dc(/* d_dal, */ group, cfg.visualisationGroup());

  typedef pcrxml::VisualisationGroup::view_sequence Views;
  Views const& xmlViews(cfg.visualisationGroup().view());

  // Collect in for Views loop, then display after loop.
  // std::vector<std::string> multiMap2DArgs;

  for(Views::const_iterator v = xmlViews.begin(); v != xmlViews.end(); ++v) {
    if(!v->valueOnly().present()) {
      std::vector<std::vector<DataGuide> > guideCollections(
         dc.guidesOfView2(*v));

      if(v->default_().present()) {
        createView(guideCollections, group, false);
      }
      else if(v->map().present()) {
        createMapView(guideCollections, group, false);
      }
      else if(v->drape().present()) {
        createDrapeView(guideCollections, group, false);
      }
      else if(v->timeGraph().present()) {
        createTimeGraphView(guideCollections, group, false);
      }
      else if(v->probabilityGraph().present()) {
        createProbabilityGraphView(guideCollections, group, false);
      }
#ifdef DEBUG_DEVELOP
      else if(v->test().present()) {
        assert(false);
      }
#endif
    }
  }

  assert(manager().nrGroups() == 1);
  manager().sync();
}



VisGroup* Viewer::createView(
         std::vector<std::vector<DataGuide> > const& guideCollections,
         VisGroup* group,
         bool sync)
{
  // Base view type on data set type of first guide.
  std::vector<DataGuide> const& guides(guideCollections[0]);

  switch(guides.front().type()) {
    case geo::STACK:
    case geo::FEATURE:
    case geo::VECTOR: {
      group = createMapView(guideCollections, group, sync);
      break;
    }
    case geo::TIMESERIES: {
      group = createTimeGraphView(guideCollections, group, sync);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return group;
}



VisGroup* Viewer::createMapView(
         std::vector<std::vector<DataGuide> > const& guideCollections,
         VisGroup* group,
         bool sync)
{
  if(multiView()) {
    group = createMultiMapView(guideCollections, group, sync);
  }
  else {
    for(size_t i = 0; i < guideCollections.size(); ++i) {
      if(!guideCollections[i].empty()) {
        group = createMapView(guideCollections[i], group, sync);
      }
    }
  }

  return group;
}



VisGroup* Viewer::createDrapeView(
         std::vector<std::vector<DataGuide> > const& guideCollections,
         VisGroup* group,
         bool sync)
{
  for(size_t i = 0; i < guideCollections.size(); ++i) {
    if(!guideCollections[i].empty()) {
      group = createDrapeView(guideCollections[i], group, sync);
    }
  }

  return group;
}



VisGroup* Viewer::createTimeGraphView(
         std::vector<std::vector<DataGuide> > const& guideCollections,
         VisGroup* group,
         bool sync)
{
  for(size_t i = 0; i < guideCollections.size(); ++i) {
    if(!guideCollections[i].empty()) {
      group = createTimeGraphView(guideCollections[i], group, sync);
    }
  }

  return group;
}



VisGroup* Viewer::createProbabilityGraphView(
         std::vector<std::vector<DataGuide> > const& collections,
         VisGroup* group,
         bool sync)
{
  BOOST_FOREACH(std::vector<DataGuide> const& collection, collections) {
    if(!collection.empty()) {
      group = createProbabilityGraphView(collection, group, sync);
    }
  }

  return group;
}



VisGroup* Viewer::createView(
         std::vector<DataGuide> const& guides,
         VisGroup* group,
         bool sync)
{
  assert(!guides.empty());
  assert(group);

  // Base view type on data set type of first guide.
  switch(guides.front().type()) {
    case geo::STACK:
    case geo::FEATURE: {
      group = createMapView(guides, group, sync);
      break;
    }
    case geo::TIMESERIES: {
      group = createTimeGraphView(guides, group, sync);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return group;
}



VisGroup* Viewer::createMapView(
         std::vector<DataGuide> const& guides,
         VisGroup* group,
         bool sync)
{
  assert(!guides.empty());
  assert(group);

  Map2DWindow* window = d_data->d_manager->addMap2DWindow(group);

  for(std::vector<DataGuide>::const_iterator it = guides.begin();
         it != guides.end(); ++it) {
    window->addAttribute(*it);
  }

  if(sync) {
    group->sync();
  }

  window->show();

  return group;
}



VisGroup* Viewer::createMultiMapView(
         std::vector<std::vector<DataGuide> > const& guideCollections,
         VisGroup* group,
         bool sync)
{
  // for(size_t i = 0; i < guideCollections.size(); ++i) {
  //   std::vector<DataGuide> const& guides(guideCollections[i]);

  //   for(size_t j = 0; j < guides.size(); ++j) {
  //     std::cout << group->dataObject().description(guides[j]) << ' ';
  //   }
  //   std::cout << std::endl;
  // }

  // assert(!guides.empty());
  assert(group);

  MultiMap2DWindow* window = d_data->d_manager->addMultiMap2DWindow(
       group, d_nrViewRows, d_nrViewCols);

  DataGuide guide;

  // if(guideCollections.size() == 1) {
  //   DataGuide mergeGuide;
  //   std::vector<DataGuide> const& guides(guideCollections[0]);
  //   size_t guideIndex = 0;

  //   for(size_t row = 0; row < d_nrViewRows &&
  //        guideIndex < guides.size(); ++row) {
  //     for(size_t col = 0; col < d_nrViewCols &&
  //           guideIndex < guides.size(); ++col) {
  //       guide = guides[guideIndex];

  //       if(row == 0 && col == 0) {
  //         // First data to add.
  //         mergeGuide = guide;
  //       }
  //       else {
  //         // Subsequent data to add, merge properties with first one. This
  //         // way these datasets will be drawn with the same
  //         // colours / classes / ...
  //         group->dataObject().mergeDataProperties(guide, mergeGuide);
  //       }

  //       window->addAttribute(row, col, guide);
  //       ++guideIndex;
  //     }
  //   }
  // }
  // else {
    // This block assumes that a previous stack of guides contains more or
    // the same amount of guides as subsequent ones. Furthermore guides with
    // the same index in different stacks should point to 'compatable' data
    // of which the properties can be merged.
    assert(guideCollections.size() <= d_nrViewRows * d_nrViewCols);
    std::vector<DataGuide> mergeGuides(guideCollections[0].size());
    size_t viewId;

    for(size_t row = 0; row < d_nrViewRows; ++row) {
      for(size_t col = 0; col < d_nrViewCols; ++col) {
        viewId = row * d_nrViewCols + col;

        if(viewId < guideCollections.size()) {
          std::vector<DataGuide> const& guides(guideCollections[viewId]);
          assert(guides.size() <= mergeGuides.size());

          for(size_t guideIndex = 0; guideIndex < guides.size(); ++guideIndex) {
            guide = guides[guideIndex];

            if(row == 0 && col == 0) {
              mergeGuides[guideIndex] = guide;
            }
            else {
              group->dataObject().mergeDataProperties(guide,
                   mergeGuides[guideIndex]);
            }

            window->addAttribute(row, col, guide);
          }
        }
      }
    }
  // }


/*
        else {
          size_t indexOfScenarios =
            subSpace.indexOf(dal::Scenarios);
          size_t scenarioIndex = 0;

          for(size_t row = 0; row < nrRows; ++row) {
            for(size_t col = 0; col < nrCols &&
                 scenarioIndex < scenarios.size(); ++col) {
              dal::DataSpace subSubSpace(subSpace);
              std::string const& scenario = scenarios[scenarioIndex];
              subSubSpace.dimension(indexOfScenarios).clear();
              subSubSpace.dimension(indexOfScenarios).addValue(scenario);
              guide = group->addData(name, subSubSpace);

              if(scenarioIndex == 0) {
                mergeGuide = guide;
              }
              else {
                group->dataObject().mergeDataProperties(guide, mergeGuide);
              }

              windows[0]->addAttribute(row, col, guide);

              ++scenarioIndex;
            }
          }
        }

        ++guideIndex;
      }
    }
  }
  */

/*
    while(queryResultsIndex < queryResults.size()) {
      std::string const& name = queryResults[queryResultsIndex].name;
      dal::DataSpace const& subSpace = queryResults[queryResultsIndex].dataSpace;
      assert(!subSpace.hasScenarios());

      for(size_t row = 0; row < nrRows; ++row) {
        for(size_t col = 0; col < nrCols; ++col) {
          windows[0]->addAttribute(row, col, group->addData(name, subSpace));
        }
      }

      ++queryResultsIndex;
    }

    if(sync) {
      group->sync();
    }
    */
// ----------------------------------------------

  if(sync) {
    group->sync();
  }

  window->show();

  return group;
}



VisGroup* Viewer::createDrapeView(
         std::vector<DataGuide> const& guides,
         VisGroup* group,
         bool sync)
{
  assert(group);
  assert(!guides.empty());

  // Create windows.
  std::vector<Map3DWindow*> windows;
  std::vector<std::string> scenarios; // EMPTY FTTB

  // 1 window Or for each scenario one window. (max( ))
  for(size_t i = 0; i < std::max<size_t>(1,scenarios.size()); ++i) {
     windows.push_back(d_data->d_manager->addMap3DWindow(group));
     // Use the first data source for height.
     windows.back()->setHeight(guides[0]);
  }

  // Add data to windows.
  { // i == 0 already done as height
    for(size_t i=1; i < guides.size(); ++i) {
      for(size_t j = 0; j < windows.size(); ++j) {
        windows[j]->addAttribute(guides[i]);
      }
    }
  }

  if(sync) {
    group->sync();
  }
  assert(!windows.empty());

  for(size_t i = 0; i < windows.size(); ++i) {
    windows[i]->show();
  }

  return group;
}



VisGroup* Viewer::createTimeGraphView(
         std::vector<DataGuide> const& guides,
         VisGroup* group,
         bool sync)
{
  assert(!guides.empty());
  assert(group);

  TimePlotWindow* window = d_data->d_manager->addTimePlotWindow(group);

  for(std::vector<DataGuide>::const_iterator it = guides.begin();
         it != guides.end(); ++it) {
    window->addAttribute(*it);
  }

  if(sync) {
    // What if the constructor of IVisualisation calls setNotifyNeeded(true).
    group->dataObject().setNotifyNeeded(true);
    group->sync();
  }

  window->show();

  return group;
}



VisGroup* Viewer::createProbabilityGraphView(
         std::vector<DataGuide> const& guides,
         VisGroup* group,
         bool sync)
{
  assert(!guides.empty());
  assert(group);

  CumDistributionFunctionWindow* window =
         d_data->d_manager->addProbabilityGraphWindow(group);

  for(std::vector<DataGuide>::const_iterator it = guides.begin();
         it != guides.end(); ++it) {
    window->addAttribute(*it);
  }

  if(sync) {
    group->dataObject().setNotifyNeeded(true);
    group->sync();
  }

  window->show();

  return group;
}



//! See VisGroupManager::exists(const VisGroup*)
/*!
*/
bool Viewer::exists(const VisGroup* group) const
{
  return d_data->d_manager->exists(group);
}



//! See VisGroupManager::findCompatibleGroup(const com::PathName&)
/*!
*/
VisGroup* Viewer::findCompatibleGroup(
         std::string const& name,
         dal::DataSpace const& space)
{
  return d_data->d_manager->findCompatibleGroup(name, space);
}



//!
/*!
  Determine if scenarios are present in \a v and how they are named
  \param     v "list of dataspaces"
  \return    list of scenario name, empty if no scenarios
  \exception .
  \warning   .
  \sa        .

*/
template<typename C>
static std::vector<std::string>
   findScenarios(C const& v)
{
  dal::DataSpace space;

  for(size_t i = 0; i < v.size(); ++i) {
    // No, resultSpace is the enclosing space of the data.
    // if(result.dataSpace.isEmpty()) {
    //   dal::throwDataSourceError(resultName, resultDataset->type(),
    //      "data source is empty");
    // }

    space |= v[i].space();
  }
  return dal::scenarios(space);
}

//! See VisGroupManager::newGroup()
/*!
*/
VisGroup* Viewer::newGroup()
{
  return d_data->d_manager->newGroup();
}



Viewer::QueryResults Viewer::querySearchDataSpace(
         dal::DataSpace const& searchSpace,
         std::vector<std::string> const& names)
{
  QueryResults  queryResults;

  for(size_t i = 0; i < names.size(); ++i) {
    dal::DataSpaceQueryResult result;
    boost::tie(result, boost::tuples::ignore) = dal::Client::dal().search(
         names[i], searchSpace, dal::NarrowSpaceWhenNeeded,
         dal::SearchForAllItems);

    if(!result) {
      dal::throwCannotBeOpened(names[i], searchSpace);
    }

    queryResults.push_back(QueryResult(names[i], result));
  }

  return queryResults;
}



VisGroup* Viewer::groupFor(
         QueryResults const& results)
{
  return groupFor(results.front());
}



VisGroup* Viewer::groupFor(
         dal::DataSpaceQueryResult const& result)
{
  VisGroup* group = 0;

  dal::Driver const* driver(dal::Client::dal().driver(result));
  assert(driver);

  std::auto_ptr<dal::Dataset> dataset(driver->open(result.name(),
         result.space(), result.address()));

  switch(dataset->type()) {
    case dal::RASTER: {
      group = groupFor<dal::Raster>(
            dynamic_cast<dal::Raster const&>(*dataset.get()), result.space());

      break;
    }
    case dal::TABLE: {
      group = groupFor<dal::Table>(
            dynamic_cast<dal::Table const&>(*dataset.get()), result.space());

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  assert(group && exists(group));

  return group;
}



//! Returns a group which can be used for visualising data from \a fileName.
/*!
  \param     fileName File name of data to visualise.
  \return    Visualisation group.
  \sa        findCompatibleGroup(const com::PathName&), newGroup()

  First a compatible group is searched for and when not available a new group
  is created.
*/
VisGroup* Viewer::groupFor(
         std::string const& name)
{
  return groupFor(name, dal::DataSpace());
}



VisGroup* Viewer::groupFor(
         std::string const& name,
         dal::DataSpace const& space)
{
  VisGroup* group = findCompatibleGroup(name, space);

  if(!group) {
    group = newGroup();
  }

  assert(group && exists(group));

  return group;
}



// //! Creates a default visualisation for the data in \a fileNames.
// /*!
//   \param     searchSpace search name here
//   \param     fileNames File names of files with data.
//   \param     group Visualisation group to add visualisation to.
//   \param     sync Whether the data sets will be sunken in the group.
//   \return    Visualisation group to which the visualisation is added.
//   \sa        display(const com::PathName&, VisGroup*, bool)
// */
// VisGroup* Viewer::display(
//          dal::DataSpace const& searchSpace,
//          std::vector<std::string> const& names,
//          VisGroup* group,
//          bool sync)
// {
//   assert(!names.empty());
// 
//   dal::DatasetType type = dal::NR_DATASET_TYPES;
//   std::auto_ptr<dal::Dataset> dataset;
// 
//   // determine DataSpace for first item in view
//   dal::DataSpaceQueryResult result = d_dal.open(names.front(), searchSpace);
//   dataset.reset(result.dataset);
//   if(dataset.get()) {
//     type = dataset->type();
//   }
// 
//   switch(type) {
//     case dal::RASTER: {
//       if(!group) {
//         group = groupFor<dal::Raster>(
//               *dynamic_cast<dal::Raster*>(dataset.get()), searchSpace);
//       }
// 
//       group = displayMap2D(searchSpace, names, group, sync);
// 
//       break;
//     }
//     case dal::TABLE: {
//       if(!group) {
//         group = groupFor<dal::Table>(
//               *dynamic_cast<dal::Table*>(dataset.get()), searchSpace);
//       }
// 
//       group = displayTimePlot(searchSpace, names, group, sync);
// 
//       break;
//     }
//     case dal::MATRIX: case dal::FEATURE: {
//       dal::throwCannotBeOpened(names.front(), searchSpace, type);
//       break;
//     }
//     default: {
//       dal::throwCannotBeOpened(names.front(), searchSpace);
//       break;
//     }
//   }
// 
//   return group;
// }

//! Creates a Map2D visualisation for the data in \a fileNames.
/*!
  \param     searchSpace space to search names in
  \param     names File names of file with data.
  \param     group Visualisation group to add visualisation to.
  \param     sync Whether the data sets will be sunken in the group.
  \return    Visualisation group to which the visualisation is added.
  \warning   \a fileNames must contain stack data. \a fileNames must be
             absolute.
  \sa        displayMap2D(const com::PathName&, VisGroup*, bool)
*/

VisGroup* Viewer::displayMap2D(
         dal::DataSpace const& searchSpace,
         std::vector<std::string> const& names,
         VisGroup* group,
         bool sync)
{
  return displayMap2D(querySearchDataSpace(searchSpace,names), group, sync);
}



VisGroup* Viewer::displayMap2D(
         QueryResults const& results,
         VisGroup* group,
         bool sync)
{
  if(results.empty())
    return group;
  if(!group)
    group = groupFor(results);
  assert(group);

  // Create windows.
  std::vector<Map2DWindow*> windows;
  std::vector<std::string> scenarios(findScenarios(results));

  // 1 window Or for each scenario one window. (max( ))
  for(size_t i = 0; i < std::max<size_t>(1,scenarios.size()); ++i) {
     windows.push_back(d_data->d_manager->addMap2DWindow(group));
  }

  // Add data to windows.
  for(QueryResults::const_iterator i=results.begin(); i != results.end(); ++i) {
    for(size_t j = 0; j < windows.size(); ++j) {
      // subSpace copy is modified if scenarios are used
      dal::DataSpace subSpace(fixForScenario(i->space(), scenarios, j));
      windows[j]->addAttribute(group->addData(i->name, subSpace));
    }
  }

  if(sync) {
    group->sync();
  }

  assert(!windows.empty());
  for(size_t i = 0; i < windows.size(); ++i) {
    windows[i]->show();
  }

  return group;
}



VisGroup* Viewer::displayMultiMap2D(
         dal::DataSpace const& space,
         std::vector<std::string> const& names,
         VisGroup* group,
         size_t nrRows,
         size_t nrCols,
         bool sync)
{
  return displayMultiMap2D(querySearchDataSpace(space,names),
                           group,nrRows,nrCols,sync);
}



VisGroup* Viewer::displayMultiMap2D(
         QueryResults const& queryResults,
         VisGroup* group,
         size_t nrRows,
         size_t nrCols,
         bool sync)
{
  if(!queryResults.empty()) {
    if(!group) {
      group = groupFor(queryResults);
    }

    std::vector<std::string> scenarios(findScenarios(queryResults));

    // Create windows.
    std::vector<MultiMap2DWindow*> windows;
    windows.push_back(d_data->d_manager->addMultiMap2DWindow(
         group, nrRows, nrCols));

    DataGuide guide, mergeGuide;
    size_t queryResultsIndex = 0;

    if(scenarios.empty()) {
      for(size_t row = 0; row < nrRows &&
              queryResultsIndex < queryResults.size(); ++row) {
        for(size_t col = 0; col < nrCols &&
              queryResultsIndex < queryResults.size(); ++col) {
          std::string const& name =
              queryResults[queryResultsIndex].name;
          dal::DataSpace const& subSpace = queryResults[queryResultsIndex].space();

          guide = group->addData(name, subSpace);

          if(row == 0 && col == 0) {
            // First data to add.
            mergeGuide = guide;
          }
          else {
            // Subsequent data to add, merge properties with first one. This
            // way these datasets will be drawn with the same
            // colours / classes / ...
            group->dataObject().mergeDataProperties(guide, mergeGuide);
          }

          windows[0]->addAttribute(row, col, guide);
          ++queryResultsIndex;
        }
      }
    }
    else {
      assert(scenarios.size() <= nrRows * nrCols);

      for(size_t row = 0; row < nrRows &&
              queryResultsIndex < queryResults.size(); ++row) {
        for(size_t col = 0; col < nrCols &&
              queryResultsIndex < queryResults.size(); ++col) {
          std::string const& name =
                queryResults[queryResultsIndex].name;
          dal::DataSpace const& subSpace = 
                queryResults[queryResultsIndex].space();

          // Current data item is constant for all scenarios.
          if(!subSpace.hasScenarios()) {
            for(size_t row = 0; row < nrRows; ++row) {
              for(size_t col = 0; col < nrCols; ++col) {
                guide = group->addData(name, subSpace);

                if(row == 0 && col == 0) {
                  mergeGuide = guide;
                }
                else {
                  group->dataObject().mergeDataProperties(guide, mergeGuide);
                }

                windows[0]->addAttribute(row, col, guide);
              }
            }
          }
          else {
            size_t indexOfScenarios =
              subSpace.indexOf(dal::Scenarios);
            size_t scenarioIndex = 0;

            for(size_t row = 0; row < nrRows; ++row) {
              for(size_t col = 0; col < nrCols &&
                   scenarioIndex < scenarios.size(); ++col) {
                dal::DataSpace subSubSpace(subSpace);
                std::string const& scenario = scenarios[scenarioIndex];
                subSubSpace.dimension(indexOfScenarios).setValue<std::string>(
                   scenario);
                guide = group->addData(name, subSubSpace);

                if(scenarioIndex == 0) {
                  mergeGuide = guide;
                }
                else {
                  group->dataObject().mergeDataProperties(guide, mergeGuide);
                }

                windows[0]->addAttribute(row, col, guide);

                ++scenarioIndex;
              }
            }
          }

          ++queryResultsIndex;
        }
      }
    }

    while(queryResultsIndex < queryResults.size()) {
      std::string const& name = queryResults[queryResultsIndex].name;
      dal::DataSpace const& subSpace = queryResults[queryResultsIndex].space();
      assert(!subSpace.hasScenarios());

      for(size_t row = 0; row < nrRows; ++row) {
        for(size_t col = 0; col < nrCols; ++col) {
          windows[0]->addAttribute(row, col, group->addData(name, subSpace));
        }
      }

      ++queryResultsIndex;
    }

    if(sync) {
      group->sync();
    }

    assert(!windows.empty());

    for(size_t i = 0; i < windows.size(); ++i) {
      windows[i]->show();
    }
  }

  return group;
}


//! Creates a Map3D visualisation for the data in \a fileNames.
/*!
  \param     searchSpace data space to search in
  \param     fileNames File names of file with data.
  \param     group Visualisation group to add visualisation to.
  \param     sync Whether the data sets will be sunken in the group.
  \return    Visualisation group to which the visualisation is added.
  \warning   \a fileNames must contain stack data. \a fileNames must be
             absolute. The first file in \a fileNames must contain scalar
             data to be used as height.
  \sa        displayMap3D(const com::PathName&, VisGroup*, bool)
*/
VisGroup* Viewer::displayMap3D(
         dal::DataSpace const& searchSpace,
         std::vector<std::string> const& names,
         VisGroup* group,
         bool sync)
{
  return displayMap3D(querySearchDataSpace(searchSpace,names), group, sync);
}



VisGroup* Viewer::displayMap3D(
         QueryResults const& results,
         VisGroup* group,
         bool sync)
{
  if(results.empty())
    return group;
  if(!group)
      group = groupFor(results);

  std::string const&   firstName    = results[0].name;
  dal::DataSpace const& firstSpace = results[0].space();

  // Create windows.
  std::vector<Map3DWindow*> windows;
  std::vector<std::string> scenarios(findScenarios(results));

  // 1 window Or for each scenario one window. (max( ))
  for(size_t i = 0; i < std::max<size_t>(1,scenarios.size()); ++i) {
     windows.push_back(d_data->d_manager->addMap3DWindow(group));
     // Use the first data source for height.
     windows.back()->setHeight(group->addData(firstName, firstSpace));
  }

  // Add data to windows.
  {
    QueryResults::const_iterator i=results.begin();
    ++i; // skip height layer
    for(; i != results.end(); ++i) {
      for(size_t j = 0; j < windows.size(); ++j) {
        // subSpace copy is modified if scenarios are used
        dal::DataSpace subSpace(fixForScenario(i->space(), scenarios,j));

        windows[j]->addAttribute(group->addData(i->name, subSpace));
      }
    }
  }

  if(sync) {
    group->sync();
  }

  assert(!windows.empty());

  for(size_t i = 0; i < windows.size(); ++i) {
    windows[i]->show();
  }

  return group;
}



// #ifdef DEBUG_DEVELOP
// //! not used hack only ?
// VisGroup* Viewer::displayTestVisualisation(
//          dal::DataSpace const& space,
//          std::vector<std::string> const& names,
//          VisGroup* group,
//          bool sync)
// {
//   assert(!names.empty());
// 
//   if(!group) {
//     group = groupFor(names.front(), space);
//   }
// 
//   TestVisualisation* window = group->addTestVisualisation();
//   window->setHeight(group->addData(names.front(), space));
// 
//   for(std::vector<std::string>::const_iterator it = ++names.begin();
//          it != names.end(); ++it) {
//     window->addAttribute(group->addData(*it, space));
//   }
// 
//   if(sync) {
//     group->sync();
//   }
// 
//   window->show();
// 
//   return group;
// }
// #endif



//! Creates a TimePlot visualisation for the data in \a names.
/*!
  \param     searchSpace dataspace to find the names in
  \param     names "File names or selections" of file with data.
  \param     group Visualisation group to add visualisation to.
  \param     sync Whether the data sets will be sunken in the group.
  \return    Visualisation group to which the visualisation is added.
  \warning   \a fileNames must contain stack data. \a fileNames must be
             absolute.
*/

// VisGroup* Viewer::displayTimePlot(
//          dal::DataSpace const& searchSpace,
//          std::vector<std::string> const& names,
//          VisGroup* group,
//          bool sync)
// {
//   assert(!names.empty());
// 
//   if(!group) {
//     group = groupFor(names.front(), searchSpace);
//   }
//   DataConfiguration dc(d_dal,searchSpace,group);
//   std::vector<DataGuide> allGuides;
// 
//   for(std::vector<std::string>::const_iterator it = names.begin();
//          it != names.end(); ++it) {
//     std::vector<DataGuide> dg(dc.tableGuides(*it));
//     allGuides.insert(allGuides.end(),dg.begin(),dg.end());
//   }
//   return createTimeGraphView(allGuides,group,sync);
// }



VisGroup* Viewer::displayProbabilityGraphWindow(
         dal::DataSpace const& space, std::vector<std::string> const& names,
         VisGroup* group, bool sync)
{
  assert(!names.empty());

  if(!group) {
    group = groupFor(names.front(), space);
  }

  CumDistributionFunctionWindow* window =
         d_data->d_manager->addProbabilityGraphWindow(group);

  for(std::vector<std::string>::const_iterator it = names.begin();
         it != names.end(); ++it) {
    window->addAttribute(group->addData(*it, space));
  }

  if(sync) {
    group->sync();
  }

  window->show();

  return group;
}



VisGroup* Viewer::displayAnimationDialog(
         VisGroup* group,
         bool sync)
{
  assert(group);

  AnimationControl* window = group->addAnimationDialog();

  if(sync) {
    group->sync();
  }

  window->show();

  return group;
}



ag::VisGroup* ag::Viewer::displayCursor(
         VisGroup* group,
         std::string const& cursorValueMonitorFile,
         std::string const& fileToGetCursorValue,
         bool sync)
{
  assert(group);

  CursorWindow* window = d_data->d_manager->addCursorWindow(group);
  window->setCursorIO(cursorValueMonitorFile, fileToGetCursorValue);

  if(sync) {
    group->sync();
  }

  window->show();

  return group;
}



VisGroup* Viewer::displayValueView(
         dal::DataSpace const& space,
         std::vector<std::string> const& names,
         VisGroup* group,
         bool sync)
{
  return displayValueView(querySearchDataSpace(space, names), group, sync);
}




VisGroup* Viewer::displayValueView(
         QueryResults const& queryResults,
         VisGroup* group,
         bool sync)
{
  if(!queryResults.empty()) {

    if(!group) {
      group = groupFor(queryResults);
    }

    std::vector<std::string> scenarios(findScenarios(queryResults));

    for(size_t i = 0; i < queryResults.size(); ++i) {
      std::string const& name = queryResults[i].name;
      dal::DataSpace const& subSpace = queryResults[i].space();

      if(scenarios.empty()) {
        group->addData(name, subSpace);
      }
      else {
        for(size_t j = 0; j < scenarios.size(); ++j) {
          dal::DataSpace subSubSpace(fixForScenario(subSpace,scenarios,j));
          group->addData(name, subSubSpace);
        }
      }
    }

    if(sync) {
      group->sync();
    }
  }

  return group;
}



// VisGroup* Viewer::createValueView(
//          std::vector<DataGuide> const& guides,
//          VisGroup* group,
//          bool sync)
// {
//   assert(!guides.empty());
//   assert(group);
// 
//   Map2DWindow* window = d_data->d_manager->addMap2DWindow(group);
// 
//   for(std::vector<DataGuide>::const_iterator it = guides.begin();
//          it != guides.end(); ++it) {
//     window->addAttribute(*it);
//   }
// 
//   if(sync) {
//     group->sync();
//   }
// 
//   window->show();
// 
//   return group;
// }



void Viewer::sync()
{
  d_data->d_manager->sync();
}



size_t Viewer::nrVisualisations() const
{
  return d_data->d_manager->nrVisualisations();
}



VisGroup* Viewer::group(IVisualisation const* visualisation)
{
  return d_data->d_manager->group(visualisation);
}



bool Viewer::multiView() const
{
  return d_nrViewRows > 1 || d_nrViewCols > 1;
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

} // namespace ag

