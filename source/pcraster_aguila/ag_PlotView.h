#ifndef INCLUDED_AG_PLOTVIEW
#define INCLUDED_AG_PLOTVIEW



// Library headers.

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

  void             appended            (QPointF const& point) override;

  void             moved               (QPointF const& point) override;

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

  void             rescan              () override;

  void             process             () override;

  void             visualise           () override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PlotView            (DataObject* object,
                                        QWidget* parent = nullptr,
                                        const char* name = nullptr);

  /* virtual */    ~PlotView           () override;

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
