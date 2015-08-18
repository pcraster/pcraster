#ifndef INCLUDED_AG_DATACONFIGURATION
#define INCLUDED_AG_DATACONFIGURATION



// Library headers.

// PCRaster library headers.
#include "ag_DataGuide.h"
#include "dal_DataSpace.h"
#include "AguilaXSD.h"

// Module headers.

namespace dal {
  /// class Dal;
  class DataSpaceQueryResult;
}
namespace ag {
  class VisGroup;
}



namespace ag {

struct DataItemInformation
{
  dal::DataSpace   space;
  pcrxml::AguilaData configuration;
  DataGuide        guide;

  DataItemInformation(
         dal::DataSpace const& _space,
         pcrxml::AguilaData const& _configuration,
         DataGuide _guide)
    : space(_space),
      configuration(_configuration),
      guide(_guide) {}
};

//! Only in support of the Viewer class
class DataConfiguration
{

  friend class DataConfigurationTest;

private:

  /// dal::Dal&        d_dal;

  dal::DataSpace   d_searchSpace;

  VisGroup*        d_group;

  //! Map to store dataset alias - dataset name combinations.
  typedef std::map<std::string, std::set<std::string> > NameMap;
  NameMap          d_nameMap;

  typedef std::multimap<std::string, DataItemInformation> DataMap2;
  DataMap2         d_dataMap2;

  size_t           d_nrScenarios;

  typedef std::map<std::string, pcrxml::AguilaData> DataMap;
  DataMap          d_dataMap;

  typedef std::map<std::string, DataGuide> GuideMap;
  GuideMap         d_guideMap;

  //! Assignment operator. NOT IMPLEMENTED.
  DataConfiguration& operator=         (DataConfiguration const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DataConfiguration   (DataConfiguration const& rhs);

  //! Default constructor. NOT IMPLEMENTED.
                   DataConfiguration   ();

  static dal::DataSpace dataSpace      (pcrxml::DataSpace const& xml);

  // dal::DataSpace   dataSpaceFor        (std::string const& name) const;

  void             add                 (std::string const& name,
                                        dal::DataSpace space,
                                        pcrxml::AguilaData const& configuration);

  void             add                 (std::string const& name,
                                        dal::DataSpace space,
                                        dal::DataSpaceQueryResult const& result,
                                        pcrxml::AguilaData const& configuration);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //                  DataConfiguration   (dal::Dal&     dal,
  //                                       dal::DataSpace const& searchSpace,
  //                                       VisGroup* group);

                   DataConfiguration   (/// dal::Dal& dal,
                                        VisGroup* group,
                                        pcrxml::VisualisationGroup const& g);

  /* virtual */    ~DataConfiguration  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  // dal::DataSpace const& searchSpace    () const;

  // pcrxml::DrawProperties drawProperties(std::string const& name) const;

  std::vector<std::vector<DataGuide> > guidesOfView2(
                                        pcrxml::AguilaView const& view) const;

  // std::vector<DataGuide>            guidesOfView(
  //                                       pcrxml::AguilaView const& view) const;

  // std::vector<DataGuide>            tableGuides(
  //                                       std::string const& name) const;

  // std::vector<std::string>           tableSelections(
  //                                       std::string const& name) const;

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
