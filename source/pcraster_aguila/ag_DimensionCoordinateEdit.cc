#include "ag_DimensionCoordinateEdit.h"

// Library headers.
#include <QComboBox>
#include <QLayout>
#include <QSlider>

// PCRaster library headers.
#include "dal_Dimension.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the DimensionCoordinateEdit class.
*/



//------------------------------------------------------------------------------

/*
namespace ag {

class DimensionCoordinateEditPrivate
{
public:

  DimensionCoordinateEditPrivate()
  {
  }

  ~DimensionCoordinateEditPrivate()
  {
  }

};

} // namespace ag
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DIMENSIONCOORDINATEEDIT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DIMENSIONCOORDINATEEDIT MEMBERS
//------------------------------------------------------------------------------

namespace ag {

DimensionCoordinateEdit::DimensionCoordinateEdit(
         dal::Dimension const* dimension,
         QWidget* parent)

  : QWidget(parent),
    d_dimension(dimension),
    d_editWidget(0)

{
  assert(d_dimension);
  assert(d_dimension->meaning() != dal::Space);

  createInterface();
}



DimensionCoordinateEdit::~DimensionCoordinateEdit()
{
}



void DimensionCoordinateEdit::createInterface()
{
  assert(!d_editWidget);

  switch(d_dimension->discretisation()) {
    case dal::ExactDiscretisation: {
      QComboBox* comboBox = new QComboBox(this);

      for(size_t i = 0; i < d_dimension->nrCoordinates(); ++i) {
        comboBox->addItem(
              QString(dal::coordinateToString(*d_dimension, i).c_str()));
      }

      connect(comboBox, SIGNAL(activated(int)),
              this, SLOT(valueChanged(int)));
      d_editWidget = comboBox;
      break;
    }
    case dal::RegularDiscretisation: {
      QSlider* slider = new QSlider(Qt::Horizontal, this);
      slider->setMinimum(0);
      slider->setMaximum(d_dimension->nrCoordinates() - 1);
      slider->setPageStep(1);
      slider->setValue(0);
      slider->setTracking(false);
      connect(slider, SIGNAL(valueChanged(int)), this, SLOT(valueChanged(int)));
      d_editWidget = slider;
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  QHBoxLayout* layout = new QHBoxLayout(this);
  layout->addWidget(d_editWidget);

  assert(d_editWidget);
}



void DimensionCoordinateEdit::valueChanged(
         int index)
{
  assert(index >= 0);
  assert(index < static_cast<int>(d_dimension->nrCoordinates()));

  Q_EMIT coordinateSet(d_dimension, size_t(index));
}



void DimensionCoordinateEdit::setCoordinate(
         size_t index)
{
  assert(index < d_dimension->nrCoordinates());

  switch(d_dimension->discretisation()) {
    case dal::ExactDiscretisation: {
      QComboBox* comboBox = dynamic_cast<QComboBox*>(d_editWidget);
      assert(comboBox);
      comboBox->setCurrentIndex(int(index));
      break;
    }
    case dal::RegularDiscretisation: {
      QSlider* slider = dynamic_cast<QSlider*>(d_editWidget);
      assert(slider);
      slider->setValue(int(index));
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



void DimensionCoordinateEdit::unsetCoordinate()
{
  switch(d_dimension->discretisation()) {
    case dal::ExactDiscretisation: {
      // QComboBox* comboBox = dynamic_cast<QComboBox*>(d_editWidget);
      // assert(comboBox);
      // comboBox->setCurrentItem(int(index));
      break;
    }
    case dal::RegularDiscretisation: {
      // QSlider* slider = dynamic_cast<QSlider*>(d_editWidget);
      // assert(slider);
      // slider->setValue(int(index));
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



dal::Dimension const* DimensionCoordinateEdit::dimension() const
{
  return d_dimension;
}



} // namespace ag

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



