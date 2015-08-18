#include "ag_RangeDrawPropertiesWidget.h"

// Library headers.
#include <QCheckBox>
#include <QComboBox>
#include <QGroupBox>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QSpinBox>
#include <QToolTip>
#include <QValidator>

// PCRaster library headers.
#include "com_userdefinedclassifier.h"
#include "qt_Const.h"
#include "qt_AppWindow.h"

// Module headers.
#include "ag_DataObject.h"
#include "ag_DataProperties.h"



/*!
  \file
  This file contains the implementation of the RangeDrawPropertiesWidget class.
*/



//------------------------------------------------------------------------------

namespace ag {

class RangeDrawPropertiesWidgetPrivate
{

public:

  QCheckBox*       _exactBorders;
  QCheckBox*       _exceedanceProbabilities;
  QSpinBox*        _nrClasses;
  QLineEdit*       _maxCutoff;
  QPushButton*     _resetMaxCutoff;
  QLineEdit*       _minCutoff;
  QPushButton*     _resetMinCutoff;
  QComboBox*       _classAlg;
  QLineEdit*       _confidenceLevel;
  QComboBox*       _drawerType;

  RangeDrawPropertiesWidgetPrivate()
    : _exactBorders(0), _confidenceLevel(0)
  {
  }

  ~RangeDrawPropertiesWidgetPrivate()
  {
  }

};



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RANGEDRAWPROPERTIESWIDGET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RANGEDRAWPROPERTIESWIDGET MEMBERS
//------------------------------------------------------------------------------

RangeDrawPropertiesWidget::RangeDrawPropertiesWidget(
         DataObject& object, const DataGuide& guide,
         QWidget* parent)

  : DrawPropertiesWidget(object, guide, parent),
    _data(new RangeDrawPropertiesWidgetPrivate),
    _classifierPushed(false)

{
  assert(dataGuide().type() == geo::STACK ||
         dataGuide().type() == geo::FEATURE ||
         dataGuide().type() == geo::TIMESERIES);
  assert(dataGuide().valueScale() == VS_SCALAR ||
         dataGuide().valueScale() == VS_DIRECTION);

  _probabilitiesLoaded =
         dataObject().dataSpace(dataGuide()).hasCumProbabilities() &&
         dataObject().hasSelectedValue();

  createInterface();
}



RangeDrawPropertiesWidget::~RangeDrawPropertiesWidget()
{
}



void RangeDrawPropertiesWidget::createInterface()
{
  createPaletteInterface();
  createRangeDrawPropertiesInterface();
}



void RangeDrawPropertiesWidget::createRangeDrawPropertiesInterface()
{
  RangeDrawProps const& drawProperties =
         dataObject().properties().rangeDrawProperties(dataGuide());

  QBoxLayout* box = 0;

  _data->_exactBorders = new QCheckBox("Exact legend borders");
  _data->_exactBorders->setChecked(drawProperties.mode() ==
         com::Classifier::EXACT);
  _data->_exactBorders->setEnabled(dataGuide().valueScale() == VS_SCALAR);
  _data->_exactBorders->setToolTip(
         "Select if you want to use the exact legend border values, instead of the rounded ones");

  box = new QHBoxLayout();
  box->setSpacing(0);
  box->addWidget(_data->_exactBorders);
  box->addStretch(1);
  groupBoxLayout()->addLayout(box);

  _data->_nrClasses = new QSpinBox();
  _data->_nrClasses->setRange(1, 250);
  _data->_nrClasses->setValue(static_cast<size_t>(
         drawProperties.nrClasses()));

  box = new QHBoxLayout();
  box->setSpacing(0);
  box->addWidget(new QLabel("Number of colours:"));
  box->addSpacing(qt::SPACING);
  box->addStretch(1);
  box->addWidget(_data->_nrClasses);
  groupBoxLayout()->addLayout(box);

  QDoubleValidator* doubleValidator = new QDoubleValidator(groupBox());

  _data->_maxCutoff = new QLineEdit();
  _data->_maxCutoff->setValidator(doubleValidator);
  if(drawProperties.cutoffsAreValid()) {
    _data->_maxCutoff->setText(QString::number(drawProperties.maxCutoff()));
  }
  _data->_maxCutoff->setEnabled(dataGuide().valueScale() == VS_SCALAR);
  _data->_resetMaxCutoff = new QPushButton();
  _data->_resetMaxCutoff->setText("Reset");
  _data->_resetMaxCutoff->setEnabled(dataGuide().valueScale() == VS_SCALAR);
  connect(_data->_resetMaxCutoff, SIGNAL(clicked()), this,
       SLOT(resetMaxCutoff()));

  box = new QHBoxLayout();
  box->setSpacing(0);
  box->addWidget(new QLabel("Maximum cutoff:"));
  box->addSpacing(qt::SPACING);
  box->addStretch(1);
  box->addWidget(_data->_maxCutoff);
  box->addSpacing(qt::SPACING);
  box->addStretch(1);
  box->addWidget(_data->_resetMaxCutoff);
  groupBoxLayout()->addLayout(box);

  _data->_minCutoff = new QLineEdit();
  _data->_minCutoff->setValidator(doubleValidator);
  if(drawProperties.cutoffsAreValid()) {
    _data->_minCutoff->setText(QString::number(drawProperties.minCutoff()));
  }
  _data->_minCutoff->setEnabled(dataGuide().valueScale() == VS_SCALAR);
  _data->_resetMinCutoff = new QPushButton();
  _data->_resetMinCutoff->setText("Reset");
  _data->_resetMinCutoff->setEnabled(dataGuide().valueScale() == VS_SCALAR);
  connect(_data->_resetMinCutoff, SIGNAL(clicked()), this,
       SLOT(resetMinCutoff()));

  box = new QHBoxLayout();
  box->setSpacing(0);
  box->addWidget(new QLabel("Minimum cutoff:"));
  box->addSpacing(qt::SPACING);
  box->addStretch(1);
  box->addWidget(_data->_minCutoff);
  box->addSpacing(qt::SPACING);
  box->addStretch(1);
  box->addWidget(_data->_resetMinCutoff);
  groupBoxLayout()->addLayout(box);

  QLabel* label = new QLabel("Colour assignment:");
  _data->_classAlg = new QComboBox();
  _data->_classAlg->insertItem(com::Classifier::LIN, "Linear");
  _data->_classAlg->insertItem(com::Classifier::LOG, "True logarithmic");
  _data->_classAlg->insertItem(com::Classifier::TLOG, "Shifted logarithmic");

  if(probabilitiesLoaded()) {
    // For the current data object quantiles are drawn.

    // Enable the selection of the user defined classifier which can be
    // configured by entering a confidence level.
    _data->_classAlg->insertItem(com::Classifier::USERDEFINED,
         "Confidence interval");
  }

  _data->_classAlg->setCurrentIndex(drawProperties.algorithm());
  connect(_data->_classAlg, SIGNAL(activated(int)), this,
         SLOT(handleClassificationAlgorithmSelection(int)));

  box = new QHBoxLayout();
  box->setSpacing(0);
  box->addWidget(label);
  box->addSpacing(qt::SPACING);
  box->addStretch(1);
  box->addWidget(_data->_classAlg);
  groupBoxLayout()->addLayout(box);

  _data->_confidenceLevel = new QLineEdit();
  _data->_confidenceLevel->setEnabled(false);
  _data->_confidenceLevel->setValidator(new QDoubleValidator(
       0.0, 1.0, 3, groupBox()));
  _data->_confidenceLevel->setText("0.95");

  box = new QHBoxLayout();
  box->setSpacing(0);
  box->addWidget(new QLabel("Confidence level (1 - alpha):"));
  box->addSpacing(qt::SPACING);
  box->addStretch(1);
  box->addWidget(_data->_confidenceLevel);
  groupBoxLayout()->addLayout(box);
  handleClassificationAlgorithmSelection(_data->_classAlg->currentIndex());

  _data->_exceedanceProbabilities = new QCheckBox("Exceedance probabilities");
  _data->_exceedanceProbabilities->setChecked(
         drawProperties.probabilityScale() !=
              RangeDrawProps::CumulativeProbabilities);
  _data->_exceedanceProbabilities->setEnabled(
         dataObject().dataSpace(dataGuide()).hasCumProbabilities());
         // probabilitiesLoaded());
  _data->_exceedanceProbabilities->setToolTip(
         "Select if you want to see exceedance probabilities instead of cumulative probabilities");
  box = new QHBoxLayout();
  box->setSpacing(0);
  box->addWidget(_data->_exceedanceProbabilities);
  box->addStretch(1);
  groupBoxLayout()->addLayout(box);

  label = new QLabel("Draw mode:");
  _data->_drawerType = new QComboBox();
  _data->_drawerType->insertItem(COLOURFILL, "Fill");
  _data->_drawerType->insertItem(CONTOUR, "Contour");
  _data->_drawerType->setCurrentIndex(drawProperties.drawerType());
  _data->_drawerType->setEnabled(dataGuide().valueScale() == VS_SCALAR);

  box = new QHBoxLayout();
  box->setSpacing(0);
  box->addWidget(label);
  box->addSpacing(qt::SPACING);
  box->addStretch(1);
  box->addWidget(_data->_drawerType);
  groupBoxLayout()->addLayout(box);

  setEnabled(drawProperties.cutoffsAreValid());
}



size_t RangeDrawPropertiesWidget::nrClasses() const
{
  return static_cast<size_t>(_data->_nrClasses->value());
}



void RangeDrawPropertiesWidget::setNrClasses(size_t nrClasses)
{
  _data->_nrClasses->setValue(static_cast<int>(nrClasses));
}



void RangeDrawPropertiesWidget::setMaxCutoff(double cutoff)
{
  // if(dataGuide().type() == geo::STACK &&
  //        dataGuide().valueScale() == VS_SCALAR) {
  // if(dataGuide().valueScale() == VS_SCALAR) {
    _data->_maxCutoff->setText(QString::number(cutoff));
  // }
}



void RangeDrawPropertiesWidget::setMinCutoff(double cutoff)
{
  // if(dataGuide().valueScale() == VS_SCALAR) {
  // if(dataGuide().type() == geo::STACK &&
  //        dataGuide().valueScale() == VS_SCALAR) {
    _data->_minCutoff->setText(QString::number(cutoff));
  // }
}



double RangeDrawPropertiesWidget::maxCutoff() const
{
  // if(dataGuide().type() == geo::STACK &&
  //        dataGuide().valueScale() == VS_SCALAR) {
  if(dataGuide().valueScale() == VS_SCALAR) {
    return _data->_maxCutoff->text().toDouble();
  }
  else {
    return dataObject().properties().rangeDrawProperties(dataGuide()).maxCutoff();
  }
}



double RangeDrawPropertiesWidget::minCutoff() const
{
  // if(dataGuide().type() == geo::STACK &&
  //        dataGuide().valueScale() == VS_SCALAR) {
  if(dataGuide().valueScale() == VS_SCALAR) {
    return _data->_minCutoff->text().toDouble();
  }
  else {
    return dataObject().properties().rangeDrawProperties(dataGuide()).minCutoff();
  }
}



void RangeDrawPropertiesWidget::resetMaxCutoff()
{
  RangeDrawProps& properties =
         dataObject().properties().rangeDrawProperties(dataGuide());
  setMaxCutoff(properties.max());
}



void RangeDrawPropertiesWidget::resetMinCutoff()
{
  RangeDrawProps& properties =
         dataObject().properties().rangeDrawProperties(dataGuide());
  setMinCutoff(properties.min());
}



RangeDrawProps::Mode RangeDrawPropertiesWidget::classificationMode() const
{
  if(dataGuide().valueScale() == VS_SCALAR) {
    return _data->_exactBorders->isChecked()
         ? com::Classifier::EXACT
         : com::Classifier::AUTO;
  }
  else {
    return dataObject().properties().rangeDrawProperties(dataGuide()).mode();
  }
}



RangeDrawProps::ProbabilityScale RangeDrawPropertiesWidget::probabilityScale() const
{
  if(dataGuide().valueScale() == VS_SCALAR) {
    return _data->_exceedanceProbabilities->isChecked()
         ? RangeDrawProps::ExceedanceProbabilities
         : RangeDrawProps::CumulativeProbabilities;
  }
  else {
    return dataObject().properties().rangeDrawProperties(dataGuide()).probabilityScale();
  }
}



RangeDrawProps::Algorithm RangeDrawPropertiesWidget::classificationAlgorithm() const
{
  return static_cast<RangeDrawProps::Algorithm>(
         _data->_classAlg->currentIndex());
}



DrawerType RangeDrawPropertiesWidget::drawerType() const
{
  // if(dataGuide().type() == geo::STACK &&
  //        dataGuide().valueScale() == VS_SCALAR) {
  if(dataGuide().valueScale() == VS_SCALAR) {
    return static_cast<DrawerType>(_data->_drawerType->currentIndex());
  }
  else {
    return dataObject().properties().rangeDrawProperties(dataGuide()).drawerType();
  }
}



void RangeDrawPropertiesWidget::rescan()
{
  DrawPropertiesWidget::rescan();
}



void RangeDrawPropertiesWidget::apply()
{
  DrawPropertiesWidget::apply();

  RangeDrawProps& properties =
           dataObject().properties().rangeDrawProperties(dataGuide());

  if(properties.cutoffsAreValid()) {
    if(classificationAlgorithm() != com::Classifier::USERDEFINED) {
      if(_classifierPushed) {
        // A user defined classifier has been pushed. Remove it to get at the
        // original classifier again.
        dataObject().popClassifiers(dataGuide(), false);
        _classifierPushed = false;

        // Revert dialog settings to before the push of the user defined
        // classifier.
        RangeDrawProps& properties(
              dataObject().properties().rangeDrawProperties(dataGuide()));
        setNrClasses(properties.nrClasses());
        setMaxCutoff(properties.maxCutoff());
        setMinCutoff(properties.minCutoff());
      }

      dataObject().setClassificationMode(dataGuide(), classificationMode(),
         false);
      dataObject().setNrClasses(dataGuide(), nrClasses(), false);

      if(maxCutoff() < minCutoff()) {
        qt::AppWindow::showInfo("Aguila",
              "Max cutoff is smaller than min cutoff.\n" \
              "Resetting both cutoffs.");
        resetMaxCutoff();
        resetMinCutoff();
      }

      if(classificationAlgorithm() == com::Classifier::LOG &&
              dal::smallerOrComparable(minCutoff(), 0.0)) {
        qt::AppWindow::showInfo("Aguila",
              "For the logarithmic classification algorithm\n" \
              "the minimum cutoff value must be larger than 0.\n" \
              "Switching to the shifted logarithmic algorithm.");
        _data->_classAlg->setCurrentIndex(com::Classifier::TLOG);
      }

      // Set algorithm and cutoffs in one go. Because the function will
      // classify(), both the algorithm and the cutoffs need to be in sync.
      dataObject().setClassificationProperties(dataGuide(),
         classificationAlgorithm(), minCutoff(), maxCutoff(), false);

      assert(!_classifierPushed);
    }
    else {
      // Configure classifier based on the confidence interval given.
      com::Classifier classifier(0.0, 1.0);
      com::UserDefinedClassifier<REAL8>* userDefinedClassifier =
         classifier.installUserDefined();
      std::vector<double> borders;
      double alpha = 1.0 - confidenceLevel();
      assert(dal::greaterOrComparable(alpha, 0.0));
      borders.push_back(0.0);
      borders.push_back(0.5 * alpha);
      borders.push_back(1.0 - 0.5 * alpha);
      borders.push_back(1.0);
      userDefinedClassifier->setBorders(borders);

      // User defined classifiers are being pushed on the existing classifier.
      // If the user selects one of the others again, we want their settings
      // not to get lost. Push the classifier and keep track of the fact
      // that it can be popped again to get at the original classifier.

      if(_classifierPushed) {
        // Already pushed, replace it.
        dataObject().replaceClassifier(dataGuide(), classifier, false);
      }
      else {
        // Not yet pushed, add it.
        dataObject().pushClassifier(dataGuide(), classifier, false);
        _classifierPushed = true;
      }

      dataObject().setClassificationAlgorithm(dataGuide(),
         classificationAlgorithm(), false);

      assert(_classifierPushed);
    }

    dataObject().setProbabilityScale(dataGuide(), probabilityScale(), false);
    dataObject().setDrawerType(dataGuide(), drawerType(), false);
    dataObject().notify();

    // When the data is auto classified, these might have just changed.
    setNrClasses(dataObject().nrClasses(dataGuide()));
    setMaxCutoff(dataObject().maxCutoff(dataGuide()));
    setMinCutoff(dataObject().minCutoff(dataGuide()));
  }
}



void RangeDrawPropertiesWidget::handleClassificationAlgorithmSelection(
         int index)
{
  _data->_exactBorders->setEnabled(
         static_cast<com::Classifier::Algorithm>(index) !=
         com::Classifier::USERDEFINED);
  _data->_nrClasses->setEnabled(
         static_cast<com::Classifier::Algorithm>(index) !=
         com::Classifier::USERDEFINED);
  _data->_maxCutoff->setEnabled(
         static_cast<com::Classifier::Algorithm>(index) !=
         com::Classifier::USERDEFINED);
  _data->_resetMaxCutoff->setEnabled(
         static_cast<com::Classifier::Algorithm>(index) !=
         com::Classifier::USERDEFINED);
  _data->_minCutoff->setEnabled(
         static_cast<com::Classifier::Algorithm>(index) !=
         com::Classifier::USERDEFINED);
  _data->_resetMinCutoff->setEnabled(
         static_cast<com::Classifier::Algorithm>(index) !=
         com::Classifier::USERDEFINED);
  _data->_confidenceLevel->setEnabled(probabilitiesLoaded() &&
         static_cast<com::Classifier::Algorithm>(index) ==
         com::Classifier::USERDEFINED);
}



bool RangeDrawPropertiesWidget::probabilitiesLoaded() const
{
  return _probabilitiesLoaded;
}



double RangeDrawPropertiesWidget::confidenceLevel()
{
  assert(_data->_confidenceLevel);

  if(!_data->_confidenceLevel->hasAcceptableInput()) {
    // Side effect.
    _data->_confidenceLevel->setText("0.95");
    assert(_data->_confidenceLevel->hasAcceptableInput());
  }

  return _data->_confidenceLevel->text().toDouble();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

