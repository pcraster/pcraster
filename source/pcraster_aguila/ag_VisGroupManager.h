#ifndef INCLUDED_AG_VISGROUPMANAGER
#define INCLUDED_AG_VISGROUPMANAGER



#include <memory>
#include <vector>
#include <QtXml>
#include <QObject>
#include "ag_Configurable.h"
#include "ag_DataObject.h"
#include "ag_VisGroup.h"



namespace dal {
  class DataSpace;
}
namespace pcrxml {
  class VisualisationGroup;
}
namespace qt {
  class AppWindowProperties;
}
namespace ag {
  class CumDistributionFunctionWindow;
  class CursorWindow;
  class DataGuide;
  class DataObject;
  // class DataPropertiesDialog;
  class Map2D;
  class Map2DView;
  class Map2DWindow;
  class Map3DWindow;
  class MultiMap2DWindow;
  class TimePlotWindow;
  class VisGroupManagerPrivate;
  class IVisualisation;
  class VisualisationWindow;
}



namespace ag {



/*!
  \class VisGroupManager
  \brief short_description
*/
class PCR_AG_DECL VisGroupManager: public QObject // ,
                       // public ag::Configurable
{

private:

  Q_OBJECT

  std::auto_ptr<VisGroupManagerPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  VisGroupManager &operator=           (const VisGroupManager &);

  //! Copy constructor. NOT IMPLEMENTED.
                   VisGroupManager     (const VisGroupManager &);

  void             clean               ();

  void             add                 (VisGroup *g);

  VisGroup *       findGroup           (IVisualisation *v);

  VisGroup *       findGroup           (ag::DataObject& dataObject);

  VisGroup*        newGroup            (const pcrxml::VisualisationGroup& vgXml);

  // bool             controlCenterIsAlive() const;

public:

  typedef std::vector<VisGroup *>::const_iterator const_iterator;
  typedef std::vector<VisGroup *>::iterator iterator;
  typedef std::vector<VisGroup *>::const_reverse_iterator const_reverse_iterator;
  typedef std::vector<VisGroup *>::reverse_iterator reverse_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VisGroupManager     (const qt::AppWindowProperties& props);

  /* virtual */    ~VisGroupManager    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  VisGroup *       newGroup            ();

  VisGroup *       newGroup            (const ag::DataObject &o);

  void             erase               (VisGroup *g);

  // void             open                (VisGroup *g);

/*
  void             close               (ag::VisGroup* g,
                                        ag::VisualisationWindow* v);
                                        */

/*
  void             copy                (ag::VisGroup* g,
                                        ag::IVisualisation* v);
                                        */

  Map2DView*       addMap2DView        (ag::VisGroup* group,
                                        QWidget* parent = 0);

  Map2D*           addMap2D            (ag::VisGroup* group,
                                        QWidget* parent = 0);

  void             configure           (const QDomElement& n);

  // void             loadSettings        (const com::PathName& pn);

  void             sync                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           nrGroups            () const;

  size_t           nrVisualisations    () const;

  const_iterator   begin               () const;

  iterator         begin               ();

  const_iterator   end                 () const;

  iterator         end                 ();

  const_reverse_iterator rbegin        () const;

  reverse_iterator rbegin              ();

  const_reverse_iterator rend          () const;

  reverse_iterator rend                ();

  bool             exists              (const VisGroup* group) const;

  template<class T>
  VisGroup*        findCompatibleGroup (T const& dataset,
                                        dal::DataSpace const& space);

  VisGroup*        findCompatibleGroup (std::string const& name,
                                        dal::DataSpace const& space);

  void             show                ();

  // QDomElement      configuration       (const std::string& fName) const;

  // void             saveSettings        (const com::PathName& pn) const;

  VisGroup*        group               (IVisualisation const* visualisation);

public Q_SLOTS:

  Map2DWindow*     addMap2DWindow      (ag::VisGroup* g);

  Map2DWindow*     addMap2DWindow      (ag::VisualisationWindow* v);

  MultiMap2DWindow* addMultiMap2DWindow(ag::VisGroup* group,
                                              size_t nrRows,
                                              size_t nrCols);

  Map3DWindow*     addMap3DWindow      (ag::VisGroup* g);

  Map3DWindow*     addMap3DWindow      (ag::VisualisationWindow* v);

  Map3DWindow*     addMap3DWindow      (ag::VisGroup* g,
                                        ag::VisualisationWindow* v);

  TimePlotWindow*  addTimePlotWindow   (ag::VisGroup* visGroup);

  TimePlotWindow*  addTimePlotWindow   (ag::VisualisationWindow* visualisation);

  TimePlotWindow*  addTimePlotWindow   (ag::VisGroup* visGroup,
                                        ag::VisualisationWindow* visualisation);

  CumDistributionFunctionWindow* addProbabilityGraphWindow(
                                        VisGroup* group);

  // ag::DataPropertiesDialog* addDataPropertiesDialog(
  //                                       ag::DataObject& dataObject,
  //                                       const ag::DataGuide& dataGuide);

  void             newMap2DWindow      (ag::VisualisationWindow *v);

  void             newMap3DWindow      (ag::VisualisationWindow *v);

  void             newTimePlotWindow   (ag::VisualisationWindow *visualisation);

  CursorWindow*    addCursorWindow     (ag::VisGroup* group);

  void             close               ();

  // void             showControlCenter   ();

  // void             updateControlCenter ();

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

template<class T>
inline ag::VisGroup* ag::VisGroupManager::findCompatibleGroup(
         T const& dataset,
         dal::DataSpace const& space)
{
  VisGroup* group = 0;

  // Test group is reverse order.
  for(std::vector<VisGroup*>::reverse_iterator it = rbegin(); it != rend();
         ++it) {
    if((*it)->dataObject().compatibleData(dataset, space)) {
      group = *it;
      break;
    }
  }

  // Possibly 0.
  return group;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
