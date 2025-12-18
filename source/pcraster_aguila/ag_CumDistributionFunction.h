#ifndef INCLUDED_AG_CUMDISTRIBUTIONFUNCTION
#define INCLUDED_AG_CUMDISTRIBUTIONFUNCTION

#include "ag_Visualisation.h"



class QSplitter;
namespace ag {
  // CumDistributionFunction declarations.
  class CumDistributionFunctionView;
  class DataGuide;
  class DataObject;
  class LegendView;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class CumDistributionFunction: public Visualisation<>
{

  friend class CumDistributionFunctionTest;

private:

  QSplitter*       d_splitter{};

  CumDistributionFunctionView* d_plotView{nullptr};

  LegendView*      d_legendView{nullptr};

  //! Assignment operator. NOT IMPLEMENTED.
  CumDistributionFunction& operator=   (CumDistributionFunction const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CumDistributionFunction(CumDistributionFunction const& rhs);

  void             createInterface     (DataObject* object);

  QSize            sizeHint            () const override;

  void             rescan              () override;

  void             process             () override;

  void             visualise           () override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CumDistributionFunction(DataObject* object,
                                           QWidget* parent);

  /* virtual */    ~CumDistributionFunction() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& guide);

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
