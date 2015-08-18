#ifndef INCLUDED_AG_CUMDISTRIBUTIONFUNCTIONVIEW
#define INCLUDED_AG_CUMDISTRIBUTIONFUNCTIONVIEW



// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_PlotVisualisation.h"



namespace ag {
  // CumDistributionFunctionView declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class CumDistributionFunctionView: public PlotVisualisation
{

  friend class CumDistributionFunctionViewTest;

private:

  Q_OBJECT

  void             appended            (QPointF const& point);

  void             moved               (QPointF const& point);

  QSize            minimumSizeHint     () const;

  void             createPlot          ();

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

                   CumDistributionFunctionView(DataObject* object,
                                        QWidget* parent=0,
                                        const char* name=0);

  /* virtual */    ~CumDistributionFunctionView();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& dataGuide);

  void             toggleMarker        ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
