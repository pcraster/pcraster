#include "ag_DataConfiguration.h"

// Library headers.
#include <boost/format.hpp>

// PCRaster library headers.
#include "dal_Client.h"
#include "dal_Dal.h"
#include "dal_TableDriver.h"

// Module headers.
#include "ag_DataGuide.h"
#include "ag_VisGroup.h"
#include "ag_DataObject.h"
#include "ag_XMLViewItems.h"

/*!
  \file
  This file contains the implementation of the DataConfiguration class.
*/


namespace ag {

template<typename T>
struct DimensionMaker {
  dal::Meaning            meaning;
  dal::DiscretisationType setType;
  dal::DiscretisationType rangeType;

  dal::Dimension create(
         typename pcrxsd::RangeSetTypeTrait<T>::RangeOrSet const& f) {

    if(f.set().present()) {
      return dal::Dimension(meaning, pcrxsd::items<T>(f.set().get()));
    }
    else {
      assert(f.range().present());
      return dal::Dimension(meaning,
         T(f.range()->begin()),
         T(f.range()->end()),
         T(f.range()->increment()));
    }
  }
};



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATACONFIGURATION MEMBERS
//------------------------------------------------------------------------------

/*!
 * \todo should go into dal, but first must have definite XML Schema
 */
dal::DataSpace DataConfiguration::dataSpace(
         pcrxml::DataSpace const& xml)
{
  dal::DataSpace space;

  if(!xml.scenarios().empty()) {
    std::set<std::string> set;
    set.insert(xml.scenarios()[0].item().begin(),
         xml.scenarios()[0].item().end());
    dal::Dimension dimension(dal::Scenarios, set);

    for(size_t i = 1; i < xml.scenarios().size(); ++i) {
      set.clear();
      set.insert(xml.scenarios()[i].item().begin(),
           xml.scenarios()[i].item().end());
      dimension |= dal::Dimension(dal::Scenarios, set);
    }

    space.addDimension(dimension);
  }

  if(!xml.quantiles().empty()) {
    DimensionMaker<float> m;
    m.meaning  = dal::CumulativeProbabilities;
    m.setType  = dal::ExactDiscretisation;
    m.rangeType= dal::RegularDiscretisation;

    dal::Dimension dimension(m.create(xml.quantiles()[0]));

    for(size_t i = 1; i < xml.quantiles().size(); ++i) {
      dimension |= m.create(xml.quantiles()[i]);
    }

    space.addDimension(dimension);
  }

// if(!xml.samples().empty()) {
//   DimensionMaker m;
//   m.meaning    = dal::Samples;
//   m.setType    = dal::RegularDiscretisation; // ???? default
//   m.rangeType  = dal::RegularDiscretisation;
//    BLA BLA
//   space.addDimension(dimension);
// }

  if(!xml.timesteps().empty()) {
    DimensionMaker<size_t> m;
    m.meaning  = dal::Time;
    m.setType  = dal::RegularDiscretisation;
    m.rangeType= dal::RegularDiscretisation;

    dal::Dimension dimension(m.create(xml.timesteps()[0]));
    for(size_t i = 1; i < xml.timesteps().size(); ++i) {
      dimension |= m.create(xml.timesteps()[i]);
    }
    space.addDimension(dimension);
  }

  return space;
}


//------------------------------------------------------------------------------
// DEFINITION OF DATACONFIGURATION MEMBERS
//------------------------------------------------------------------------------

DataConfiguration::DataConfiguration(
         /// dal::Dal& dal,
         VisGroup* group,
         pcrxml::VisualisationGroup const& xmlGroup)

  : /// d_dal(dal),
    d_group(group),
    d_nrScenarios(0)

{
  // Search space is optional.
  if(xmlGroup.searchSpace().present()) {
    d_searchSpace = dataSpace(xmlGroup.searchSpace().get());
  }

  // For each data set mentioned store:
  // - name
  // - data space
  // - properties
  // - data guide

  // Loop over all data items and get data space and property information.
  // If a data space is not set, but the search space is, then this is set
  // initially.

  dal::DataSpace space;

  for(size_t i = 0; i < xmlGroup.data().size(); ++i) {
    pcrxml::AguilaData const& configuration = xmlGroup.data()[i];
    std::string name = configuration.name();
    space = configuration.dataSpace().present()
        ? dataSpace(configuration.dataSpace().get())
        : d_searchSpace;

    add(name, space, configuration);
  }

  // Loop over all views and get name / data space information.
  for(pcrxml::VisualisationGroup::view_const_iterator view =
         xmlGroup.view().begin(); view != xmlGroup.view().end(); ++view) {
    XMLViewItems const& items(*view);

    for(XMLViewItems::const_iterator name = items.begin();
         name != items.end(); ++name) {

      // If not yet added, add now
      // TODO KDJ name is not a good identifier for a data set, the name
      // TODO space combination is. This works for as long as no two datasets
      // TODO with the same name but different space are mentioned in xmlGroup.
      if(!d_dataMap2.count(*name)) {
        space = d_searchSpace;
        add(*name, space, pcrxml::AguilaData(*name));
      }
    }
  }

  // Overall space.
  space = group->dataObject().dataSpace();

  d_nrScenarios = space.hasScenarios()
         ? space.dimension(space.indexOf(dal::Scenarios)).nrCoordinates()
         : 1;

  // Send properties and datemappers found to the data object.
  for(DataMap2::const_iterator d = d_dataMap2.begin();
         d != d_dataMap2.end(); ++d) {
    DataGuide const& guide((*d).second.guide);
    pcrxml::AguilaData const& configuration((*d).second.configuration);

    if(configuration.drawProperties().present()) {
      d_group->dataObject().setXML(
         guide, configuration.drawProperties().get(), false);
    }

    // Set date mapper if needed/present.
    if(d_group->dataObject().dataSpace(guide).hasTime()) {
      // Data space stuff in data item takes precedence
      if(guide.type() == geo::STACK) {
        // TODO only supported for stacks at the moment.
        if(configuration.dataSpace().present() &&
              configuration.dataSpace()->timesteps().size() &&
              configuration.dataSpace()->timesteps()[0].dateMapper().present()) {
          d_group->dataObject().setDateMapper(guide,
              configuration.dataSpace()->timesteps()[0].dateMapper().get(),
                   false);
        }
        else if(xmlGroup.searchSpace().present() &&
              xmlGroup.searchSpace()->timesteps().size() &&
              xmlGroup.searchSpace()->timesteps()[0].dateMapper().present()) {
          d_group->dataObject().setDateMapper(guide,
              xmlGroup.searchSpace()->timesteps()[0].dateMapper().get(), false);
        }
      }
    }
  }
}



// DataConfiguration::DataConfiguration(
//     dal::Dal&     dal,
//     dal::DataSpace const& searchSpace,
//     VisGroup* group):
//      d_dal(dal),
//      d_searchSpace(searchSpace),
//      d_group(group)
// {
// }



/* NOT IMPLEMENTED
//! Copy constructor.
DataConfiguration::DataConfiguration(
         DataConfiguration const& rhs)

  : Base(rhs)

{
}
*/



DataConfiguration::~DataConfiguration()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
DataConfiguration& DataConfiguration::operator=(
         DataConfiguration const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



void DataConfiguration::add(
         std::string const& name,
         dal::DataSpace space,
         pcrxml::AguilaData const& configuration)
{
  dal::DataSpaceQueryResult result;
  boost::tie(result, boost::tuples::ignore) = dal::Client::dal().search(name,
      space, dal::NarrowSpaceWhenNeeded, dal::SearchForAllItems);

  if(!result) {
    dal::throwCannotBeOpened(name, space);
  }

  space = result.space();

  if(!space.hasScenarios()) {
    add(name, space, result, configuration);
  }
  else {
    size_t index = space.indexOf(dal::Scenarios);
    dal::Dimension const& dimension(space.dimension(index));
    // d_nameMap.insert(std::make_pair(name, name));
    dal::DataSpace narrowSpace;

    for(size_t i = 0; i < dimension.nrCoordinates(); ++i) {
      add(name, dal::dataSpaceWithNarrowedDimension(space, index, i),
         result, configuration);
    }
  }
}



void DataConfiguration::add(
         std::string const& name,
         dal::DataSpace space,
         dal::DataSpaceQueryResult const& result,
         pcrxml::AguilaData const& configuration)
{
  // TODO Use result to help
  // - VisGroup::addData(name, space)     This only forwards to:
  // - DataObject::add(name, space)
  //   - Needs:
  //     - DatasetType (in query result)
  //     - 
  //   - Forwards to addStack(name, space), etc.
  //     - Forwards to DataObjectBase<Raster>.add(name, space)
  //       - Forwards to RasterDataSources::openData(name, space)
  //         - Forwards to Raster(name, space)
  //           - Forwards to DataSource(name, space)
  // to find the data faster.
  // Check whether DataObject::add(name, space) can be changed, or whether we
  // need more overloads of that one.
  assert(result);

  if(result.datasetType() != dal::TABLE) {
    d_nameMap[name].insert(name);
    d_dataMap2.insert(std::make_pair(name, DataItemInformation(
       space, configuration, d_group->addData(name, space))));
  }
  else {
    std::vector<std::string> selection;
    boost::tie(boost::tuples::ignore, selection) =
         dal::splitNameAndSelection(name);

    if(!selection.empty()) {
      // Name IS selection.
      d_nameMap[name].insert(name);
      d_dataMap2.insert(std::make_pair(name, DataItemInformation(
         space, configuration, d_group->addData(name, space))));
    }
    else {
      // Name is not a selection, but an "alias" for all columns
      // Create for each colum, a selection specification:
      //   name{1,n}, name{1,n-1}, ...,name{1,2}
      dal::Driver const* driver(dal::Client::dal().driverByDataset(name,
         space));
      assert(driver);
      std::auto_ptr<dal::Table const> table(dynamic_cast<dal::Table const*>(
         driver->open(name, result.space(), result.address())));
      assert(table.get());

      for(size_t i = table->nrCols(); i > 1; --i) {
        std::string colName =
           (boost::format("%1%{1, %2%}") % (name) % i).str();
        d_nameMap[name].insert(colName);
        d_dataMap2.insert(std::make_pair(colName, DataItemInformation(
           space, configuration, d_group->addData(colName, space))));
      }
    }
  }
}



// dal::DataSpace DataConfiguration::dataSpaceFor(
//          std::string const& name) const
// {
//    DataMap::const_iterator i=d_dataMap.find(name);
// 
//    if (i != d_dataMap.end() && i->second.dataSpace().present()) {
//      return dataSpace(i->second.dataSpace().get());
//    }
// 
//    dal::DataSpaceQueryResult result = d_dal.open(name, d_searchSpace);
// 
//    if(!result.dataset) {
//       dal::throwCannotBeOpened(name, d_searchSpace);
//    }
// 
//    return result.dataSpace;
// };



// //! if not found return a pcrxml::DrawProperties() (empty)
// pcrxml::DrawProperties DataConfiguration::drawProperties(
//          std::string const& name) const
// {
//    DataMap::const_iterator i=d_dataMap.find(name);
//    if (i != d_dataMap.end() && i->second.drawProperties().present()) {
//      return i->second.drawProperties().get();
//    }
//    return pcrxml::DrawProperties();
// }



// dal::DataSpace const& DataConfiguration::searchSpace() const
// {
//   return d_searchSpace;
// }



std::vector<std::vector<DataGuide> > DataConfiguration::guidesOfView2(
         pcrxml::AguilaView const& view) const
{
  // For each scenario a bunch of guides.
  std::vector<std::vector<DataGuide> > result(d_nrScenarios);

  XMLViewItems viewItems(view);

  // Loop over views items: names of data sets.
  for(XMLViewItems::const_iterator i = viewItems.begin(); i != viewItems.end();
         ++i) {
    // Data set names can be an alias for a set of data set names. First
    // translate the name to this set and collect the guides for each of them.
    assert(d_nameMap.find(*i) != d_nameMap.end());
    std::set<std::string> names((*d_nameMap.find(*i)).second);
    assert(!names.empty());

    std::pair<NameMap::const_iterator, NameMap::const_iterator> nameRange(
         d_nameMap.equal_range(*i));
    assert(nameRange.first != nameRange.second);

    // For this one name we now have the data set names it corresponds to.
    // Most of the time this is the same name, but it may be a set of other
    // names. A table name is unpacked to column selections, for example.
    // Aguila is an attribute visualizer and does not handle blobs of data
    // with more than one attributes.
    for(std::set<std::string>::const_iterator it = names.begin();
         it != names.end(); ++it) {
      // This is the name to handle.
      // Get data item information.
      std::pair<DataMap2::const_iterator, DataMap2::const_iterator> range(
         d_dataMap2.equal_range(*it));
      assert(range.first != range.second);

      if(std::distance(range.first, range.second) == 1) {
        // No scenarios available in this data set. Add this one data set to
        // all collections of guides.
        for(size_t i = 0; i < result.size(); ++i) {
          result[i].push_back((*range.first).second.guide);
        }
      }
      else {
        assert(static_cast<size_t>(std::distance(range.first, range.second)) ==
              d_nrScenarios);

        for(size_t i = 0; range.first != range.second; ++range.first, ++i) {
          result[i].push_back((*range.first).second.guide);
        }
      }
    }
  }

  return result;
}



// //! return the list of guides needed for \a view
// std::vector<DataGuide> DataConfiguration::guidesOfView(
//          pcrxml::AguilaView const& view) const
// {
//   XMLViewItems avi(view);
//   std::vector<DataGuide> dg;
//   for(XMLViewItems::const_iterator i=avi.begin(); i != avi.end(); ++i) {
//       GuideMap::const_iterator g=d_guideMap.find(*i);
//       assert(g!= d_guideMap.end());
//       dg.push_back(g->second);
//   }
//   return dg;
// }



// std::vector<DataGuide> DataConfiguration::tableGuides(
//          std::string const& name) const
// {
//   const dal::DataSpace tableDataSpace(dataSpaceFor(name));
// 
//   std::vector<std::string> ts(tableSelections(name));
//   std::vector<DataGuide> dg;
// 
//   for (size_t i=0; i < ts.size(); ++i)
//     dg.push_back(d_group->addData(ts[i],tableDataSpace));
// 
//   return dg;
// }



// std::vector<std::string> DataConfiguration::tableSelections(
//          std::string const& name) const
// {
//   std::vector<std::string> ts;
// 
//   // We assume that the first column in the table contains the time steps
//   // and the subsequent ones contain the time series. Each time series is
//   // treated as a separate attribute.
// 
//   dal::Table* table;
//   boost::tie(table,boost::tuples::ignore,boost::tuples::ignore) =
//          d_dal.open<dal::Table>(name, dal::TABLE, dataSpaceFor(name));
// 
//   if(!table || !(table->nrCols() >= 2)) {
//     // The name does not refer to a table but some other dataset. Selections
//     // are only supported for tables so we just add this name to the
//     // collection.
//     ts.push_back(name);
//   } else {
//     std::vector<std::string> selection;
//     boost::tie(boost::tuples::ignore,selection)= dal::splitNameAndSelection(name);
// 
//     if(!selection.empty()) {
//       // name IS A selection
//       ts.push_back(name);
//     } else {
//       // name IS not a selection, but "alias" for all collumns
//       // create for each colum, a selection specification:
//       //    name{1,n}, name{1,n-1}, ...,name{1,2}
//       for(size_t i = table->nrCols(); i > 1; --i) {
//         std::string colName = (boost::format("%1%{1, %2%}") % (name) % i).str();
//         ts.push_back(colName);
//       }
//     }
//   }
// 
//   return ts;
// }



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

