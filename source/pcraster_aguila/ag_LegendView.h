#ifndef INCLUDED_AG_LEGENDVIEW
#define INCLUDED_AG_LEGENDVIEW



// Library headers.
#include <vector>
#include <boost/tuple/tuple.hpp>

// PCRaster library headers.

// Module headers.
#include "ag_TableVisualisation.h"
#include "ag_Types.h"
#include "ag_Visualisation.h"



class QAction;
namespace ag {
  // LegendView declarations.
  class Legend;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class LegendView: public TableVisualisation
{

  friend class LegendViewTest;

private:

  Q_OBJECT

  typedef boost::tuple<std::vector<DataGuide>, Legend*> LegendTuple;
  typedef std::vector<LegendTuple> LegendTuples;

  ViewerType       _viewerType;

  //! Collection with legends in view.
  std::vector<Legend*> _legends;

  LegendTuples     _legendTuples;

  QAction*         _generalPropertiesAction;

  QAction*         _drawPropertiesAction;

  QAction*         _saveGraphAction;

  QAction*         _mapAction;

  QAction*         _timeSeriesAction;

  QAction*         _cumulativePropabilityPlotAction;

  QModelIndex _contextMenuIndex;

  std::vector<DataGuide> _contextMenuGuides;

  LegendTuples     legendTuples        (DataGuide const& guide);

  std::vector<Legend*> legends         (DataGuide const& guide);

  std::vector<DataGuide> const& dataGuides(
                                        Legend const* legend) const;

  std::vector<DataGuide> const& dataGuides(
                                        QModelIndex const& index) const;

  Legend*          contextMenuLegend   () const;

  void             createActions       ();

  void             recreateLegend      (std::vector<DataGuide> const& guides);

  void             handleDoubleClickedCell(
                                        int row,
                                        int col);

private Q_SLOTS:

  void             handleRequestedCustomContextMenu(
                                        QPoint const& pos);

  void             editGeneralProperties();

  void             editDrawProperties  ();

  void             saveGraphData       ();

  void             showMap             ();

  void             showTimeSeries      ();

  void             showCumulativeProbabilityPlot();

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LegendView          (DataObject* object,
                                        ViewerType type,
                                        QWidget* parent=0);

  /* virtual */    ~LegendView         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             addAttribute        (const DataGuide& dataGuide);

  void             rescan              ();

  void             process             ();

  void             visualise           ();

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
