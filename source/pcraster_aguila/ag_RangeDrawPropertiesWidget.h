#ifndef INCLUDED_AG_RANGEDRAWPROPERTIESWIDGET
#define INCLUDED_AG_RANGEDRAWPROPERTIESWIDGET

#include "ag_DrawPropertiesWidget.h"
#include "ag_RangeDrawProps.h"
#include "ag_Types.h"

#include <memory>


namespace ag {
  // RangeDrawPropertiesWidget declarations.
  class RangeDrawPropertiesWidgetPrivate;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class RangeDrawPropertiesWidget: public DrawPropertiesWidget
{

private:

  Q_OBJECT

  std::unique_ptr<RangeDrawPropertiesWidgetPrivate> _data;

  bool             _probabilitiesLoaded;

  bool             _classifierPushed{false};

  //! Assignment operator. NOT IMPLEMENTED.
  RangeDrawPropertiesWidget& operator= (const RangeDrawPropertiesWidget&);

  //! Copy constructor. NOT IMPLEMENTED.
                   RangeDrawPropertiesWidget(const RangeDrawPropertiesWidget&);

  void             createInterface     ();

  void             createRangeDrawPropertiesInterface();

  size_t           nrClasses           () const;

  double           maxCutoff           () const;

  double           minCutoff           () const;

  void             setNrClasses        (size_t nrClasses);

  void             setMaxCutoff        (double cutoff);

  void             setMinCutoff        (double cutoff);

  RangeDrawProps::Mode classificationMode() const;

  RangeDrawProps::ProbabilityScale probabilityScale() const;

  RangeDrawProps::Algorithm classificationAlgorithm() const;

  DrawerType       drawerType          () const;

  bool             probabilitiesLoaded () const;

  double           confidenceLevel     ();

private Q_SLOTS:

  void             resetMaxCutoff      ();

  void             resetMinCutoff      ();

  void             handleClassificationAlgorithmSelection(
                                        int index);

protected:

  void     rescan              () override;

  void     apply               () override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RangeDrawPropertiesWidget(DataObject& dataObject,
                                       const DataGuide& dataGuide,
                                       QWidget* parent);

  /* virtual */    ~RangeDrawPropertiesWidget() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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
