#ifndef INCLUDED_AG_CUMDISTRIBUTIONFUNCTIONVIEW
#define INCLUDED_AG_CUMDISTRIBUTIONFUNCTIONVIEW

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

  void             appended            (QPointF const& point) override;

  void             moved               (QPointF const& point) override;

  QSize            minimumSizeHint     () const override;

  void             createPlot          ();

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

                   CumDistributionFunctionView(DataObject* object,
                                        QWidget* parent=nullptr,
                                        const char* name=nullptr);

  /* virtual */    ~CumDistributionFunctionView() override;

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
