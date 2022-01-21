#ifndef INCLUDED_AG_CUMDISTRIBUTIONFUNCTIONWINDOW
#define INCLUDED_AG_CUMDISTRIBUTIONFUNCTIONWINDOW



// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_VisualisationWindow.h"



namespace ag {
  // CumDistributionFunctionWindow declarations.
  class CumDistributionFunction;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class CumDistributionFunctionWindow: public VisualisationWindow
{

  friend class CumDistributionFunctionWindowTest;

private:

  Q_OBJECT

  CumDistributionFunction* d_cumDistributionFunction;

  //! Assignment operator. NOT IMPLEMENTED.
  CumDistributionFunctionWindow& operator=(CumDistributionFunctionWindow const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CumDistributionFunctionWindow(CumDistributionFunctionWindow const& rhs);

  void             createInterface     ();

  // dal::DataSpace const& profileDataSpace() const;

private Q_SLOTS:

  void             toggleMarker        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CumDistributionFunctionWindow(
                                        qt::AppWindowProperties const& props,
                                        DataObject* object);

  /* virtual */    ~CumDistributionFunctionWindow() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& guide) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             dataVisualised      () const override;

  std::string      windowName          () const override;

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
