#ifndef INCLUDED_AG_PLOTVIEW
#define INCLUDED_AG_PLOTVIEW



// Library headers.
#include "qwt_plot.h"

// PCRaster library headers.

// Module headers.
#include "ag_PlotVisualisation.h"



namespace ag {
  // PlotView declarations.
  class DataGuide;
  class DataObject;
}



namespace ag {



//! Time series.
/*!
*/
class PlotView: public PlotVisualisation
{

private:

  // void             selected            (QPointF const& point);

  void             appended            (QPointF const& point);

  void             moved               (QPointF const& point);

  bool             recreatePlotRequired() const;

  bool             replotRequired      () const;

  void             setXAxisTitle       ();

  void             setYAxisTitle       ();

  void             setXAxisScale       ();

  void             setYAxisScale       ();

  void             configureXAxis      ();

  void             configureYAxis      ();

  void             drawPlots           ();

protected:

  void             rescan              ();

  void             process             ();

  void             visualise           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PlotView            (DataObject* object,
                                        QWidget* parent = 0,
                                        const char* name = 0);

  /* virtual */    ~PlotView           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& dataGuide);

  void             createPlot          ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  // const QPixmap*   pixmap() const;

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
