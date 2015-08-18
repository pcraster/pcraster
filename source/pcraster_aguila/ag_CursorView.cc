#include "ag_CursorView.h"

// Library headers.
#include <QGroupBox>
#include <QLabel>
#include <QLayout>

// PCRaster library headers.
#include "dal_DataSpace.h"
#include "dal_DataSpaceAddressMapper.h"
#include "qt_Const.h"

// Module headers.
#include "ag_DataObject.h"
#include "ag_DataSourceTable.h"
#include "ag_DimensionCoordinateEdit.h"
#include "ag_Util.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the CursorView class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CURSORVIEW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CURSORVIEW MEMBERS
//------------------------------------------------------------------------------

namespace ag {

CursorView::CursorView(
         DataObject* object,
         QWidget* parent)

  : Visualisation<>(object, "Cursor View", parent),
    d_datasetTable(0)

{
  // std::vector<DataGuide> guides(object->dataGuides());

  // for(size_t i = 0; i < guides.size(); ++i) {
  //   visualisationEngine().addAttribute(*object, guides[i]);
  // }

  createInterface();
  updateCoordinates();
}



CursorView::~CursorView()
{
}



void CursorView::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void CursorView::process()
{
}



void CursorView::visualise()
{
  if(    visualisationEngine().change() & VisEngine::CURSOR ||
         visualisationEngine().change() & VisEngine::VALUE_SELECTION) {
    // Implement recreateInterface when needed (data added or removed).
    // Update assumes that the interface elements are still valid.
    updateCoordinates();
  }

  visualisationEngine().finishedScanning(dataObject());
}



void CursorView::updateEditLabel(
         dal::Dimension const* dimension,
         dal::DataSpaceAddress const& address)
{
  dal::DataSpace const& space(dataObject().dataSpace());

  size_t tupleIndex = this->tupleIndex(dimension);
  QLabel* value = valueLabel(tupleIndex);

  if(dimension->meaning() == dal::Space) {
    value->setText(QString::fromUtf8(dal::coordinateToString(
         space, address, space.indexOf(dimension)).c_str()));
  }
  else {
    value->setText(QString(dataObject().globalToWorldMapper().toString(
         address, space.indexOf(dimension)).c_str()));
  }
}



dal::DataSpaceAddress CursorView::addressWithUpdatedCoordinate(
         dal::Dimension const* dimension,
         size_t index) const
{
  assert(dimension->meaning() != dal::Scenarios &&
         dimension->meaning() != dal::Space);

  dal::DataSpace const& space(dataObject().dataSpace());
  dal::DataSpaceAddress address(dataObject().dataSpaceAddress());
  size_t dimensionIndex = space.indexOf(dimension);

  switch(dimension->meaning()) {
    // FEATURE remove
    /// case dal::Scenarios: {
    ///   address.setCoordinate<std::string>(dimensionIndex,
    ///           dimension->coordinate<std::string>(index));
    ///   break;
    /// }
    case dal::CumulativeProbabilities: {
      address.setCoordinate<float>(dimensionIndex,
              dimension->coordinate<float>(index));
      break;
    }
    case dal::Samples:
    case dal::Time: {
    /// FEATURE remove
    /// case dal::Space: {
      address.setCoordinate<size_t>(dimensionIndex,
              dimension->coordinate<size_t>(index));
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return address;
}



void CursorView::updateCoordinate(
         dal::Dimension const* dimension,
         size_t index)
{
  assert(dimension->meaning() != dal::Scenarios &&
         dimension->meaning() != dal::Space);

  dataObject().setDataSpaceAddress(
         addressWithUpdatedCoordinate(dimension, index));
}



size_t CursorView::tupleIndex(
         dal::Dimension const* dimension) const
{
  size_t result = d_dimensionTuples.size();

  if(dimension->meaning() == dal::Space) {
    assert(result > 0);
    // We know space is the last in the grid.
    result -= 1;
  }
  else {
    for(size_t i = 0; i < d_dimensionTuples.size(); ++i) {
      DimensionCoordinateEdit const* edit = boost::get<1>(d_dimensionTuples[i]);
      if(edit->dimension() == dimension) {
        result = i;
        break;
      }
    }
  }

  assert(result < d_dimensionTuples.size());

  return result;
}



DimensionCoordinateEdit* CursorView::dimensionCoordinateEdit(
         size_t index)
{
  return boost::get<1>(d_dimensionTuples[index]);
}



QLabel* CursorView::valueLabel(
         size_t index)
{
  return boost::get<2>(d_dimensionTuples[index]);
}



void CursorView::createInterface()
{
  dal::DataSpace const& space(dataObject().dataSpace());
  dal::DataSpaceAddress const& address(dataObject().dataSpaceAddress());

  // Box and layout for dimension edit stuff.
  QGroupBox* dimensionEditGroupBox = new QGroupBox("Dimensions", this);
  QGridLayout* gridLayout = new QGridLayout(dimensionEditGroupBox);
  gridLayout->setColumnStretch(1, 1);
  gridLayout->setColumnMinimumWidth(2, 50);
  gridLayout->setVerticalSpacing(0);

  QLabel* title, *value;
  DimensionCoordinateEdit* edit;

  for(size_t i = 0; i < space.size(); ++i) {
    dal::Dimension const& dimension(space.dimension(i));

    if(dimension.meaning() != dal::Scenarios /* &&
         dimension.meaning() != dal::Space */ ) {
      title = new QLabel(QString(
         dal::dimensionToString(dimension).c_str()), dimensionEditGroupBox);
      gridLayout->addWidget(title, i, 0);

      if(dimension.meaning() == dal::Space) {
        edit = 0;
        value = new QLabel(QString(
              dal::coordinateToString(space, address, i).c_str()), this);
        gridLayout->addWidget(value, i, 1);
      }
      else {
        edit = new DimensionCoordinateEdit(&dimension, dimensionEditGroupBox);
        connect(edit, SIGNAL(coordinateSet(const dal::Dimension*, size_t)),
             this, SLOT(updateCoordinate(const dal::Dimension*, size_t)));
        gridLayout->addWidget(edit, i, 1);

        value = new QLabel(QString(
              // dal::coordinateToString(space, address, i).c_str()),
              dataObject().globalToWorldMapper().toString(address, i).c_str()),
              dimensionEditGroupBox);


        gridLayout->addWidget(value, i, 2);
      }

      d_dimensionTuples.push_back(boost::make_tuple(title, edit, value));
    }
  }

  assert(!d_datasetTable);
  d_datasetTable = new DataSourceTable(&dataObject(), this);

  // Top level vertical layout.
  QBoxLayout* topLayout = new QVBoxLayout(this);
  topLayout->addWidget(dimensionEditGroupBox);
  topLayout->addWidget(d_datasetTable);
}



void CursorView::updateCoordinates()
{
  dal::DataSpace const& space(dataObject().dataSpace());
  dal::DataSpaceAddress const& address(dataObject().dataSpaceAddress());

  size_t coordinateIndex = 0; // Shut up compiler.
  size_t tupleIndex;

  for(size_t i = 0; i < space.size(); ++i) {
    dal::Dimension const& dimension(space.dimension(i));

    if(dimension.meaning() == dal::Scenarios /* ||
         dimension.meaning() == dal::Space */) {
      continue;
    }

    tupleIndex = this->tupleIndex(&dimension);

    if(dimension.meaning() != dal::Space) {
      DimensionCoordinateEdit* edit = dimensionCoordinateEdit(tupleIndex);

      if(!address.isValid(i)) {
        edit->unsetCoordinate();
      }
      /// else if(dimension.meaning() == dal::Space && dimension.discretisation() == dal::BorderedDiscretisation) {
      ///   // FEATURE get rid of this if.
      ///   edit->unsetCoordinate();
      /// }
      else {
        switch(dimension.meaning()) {
          // FEATURE remove
          /// case dal::Scenarios: {
          ///   coordinateIndex = dimension.indexOf(
          ///       address.coordinate<std::string>(i));
          ///   break;
          /// }
          case dal::CumulativeProbabilities: {
            coordinateIndex = dimension.indexOf(address.coordinate<float>(i));
            break;
          }
          case dal::Samples:
          case dal::Time: {
            coordinateIndex = dimension.indexOf(address.coordinate<size_t>(i));
            break;
          }
          /// case dal::Space: {
          ///   switch(dimension.discretisation()) {
          ///     case dal::RegularDiscretisation: {
          ///       coordinateIndex = dimension.indexOf(address.coordinate<size_t>(i));
          ///       break;
          ///     }
          ///     /// case dal::BorderedDiscretisation: {
          ///     ///   /// FEATURE
          ///     ///   coordinateIndex = 0;
          ///     ///   break;
          ///     /// }
          ///     default: {
          ///       assert(false);
          ///       break;
          ///     }
          ///   }

          ///   break;
          /// }
          default: {
            assert(false);
            break;
          }
        }

        edit->setCoordinate(coordinateIndex);
      }
    }

    updateEditLabel(&dimension, address);
  }
}



QSize CursorView::sizeHint() const
{
  return QSize(500, 200);
}



void CursorView::addAttribute(
         DataGuide const& guide)
{
  d_datasetTable->addAttribute(guide);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag


