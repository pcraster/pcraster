#include "ag_DrawPropertiesWidget.h"

// Library headers.
#include <QGroupBox>
#include <QLabel>
#include <QLayout>
#include <QMouseEvent>
#include <QToolTip>

// PCRaster library headers.
#include "qt_Const.h"
#include "qtd_SelectPalette.h"
#include "qtw_PaletteBar.h"

// Module headers.
#include "ag_DataGuide.h"
#include "ag_DataObject.h"
#include "ag_DataProperties.h"



/*!
  \file
  This file contains the implementation of the DrawPropertiesWidget class.
*/



//------------------------------------------------------------------------------

namespace ag {

class DrawPropertiesWidgetPrivate
{
public:

  qtw::PaletteBar* d_paletteBar;
  qtd::SelectPalette* d_paletteDialog;

  DrawPropertiesWidgetPrivate()
    : d_paletteBar(0), d_paletteDialog(0)
  {
  }

  ~DrawPropertiesWidgetPrivate()
  {
  }

};

} // namespace ag



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DRAWPROPERTIESWIDGET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DRAWPROPERTIESWIDGET MEMBERS
//------------------------------------------------------------------------------

ag::DrawPropertiesWidget::DrawPropertiesWidget(
         DataObject& dataObject, const DataGuide& dataGuide,
         QWidget* parent)

  : PropertiesWidget("Draw properties", dataObject, dataGuide, parent),
    d_data(new DrawPropertiesWidgetPrivate())

{
}



ag::DrawPropertiesWidget::~DrawPropertiesWidget()
{
}



void ag::DrawPropertiesWidget::createPaletteInterface()
{
  QBoxLayout* layout = new QHBoxLayout();
  QLabel* label = new QLabel("Palette:");
  d_data->d_paletteBar = new qtw::PaletteBar();
  d_data->d_paletteBar->setFixedSize(150, 20);
  d_data->d_paletteBar->setOutline(true);
  d_data->d_paletteBar->setToolTip(
         "Click on palette for list of possible palettes");
  layout->addWidget(label);
  layout->addWidget(d_data->d_paletteBar);
  groupBoxLayout()->addLayout(layout);

  // QBoxLayout* box = new QHBoxLayout(layoutContainer);
  // box->setMargin(0);
  // box->addWidget(label);
  // box->addStretch(1);
  // box->addWidget(d_data->d_paletteBar);

  d_data->d_paletteDialog = new qtd::SelectPalette(this);

  connect(d_data->d_paletteBar,
         SIGNAL(mousePressed(qtw::PaletteBar *, QMouseEvent *)), this,
         SLOT(paletteBarClicked(qtw::PaletteBar *, QMouseEvent *)));

  configurePaletteInterface();
}



void ag::DrawPropertiesWidget::configurePaletteInterface()
{
  // Determine and use current palette.
  d_data->d_paletteBar->setPalette(
         dataObject().properties().palette(dataGuide()));

  // Set possible palettes to choose from.
  addPalettes(dataObject().properties().palettes(dataGuide().valueScale()));
}



void ag::DrawPropertiesWidget::addPalettes(
         const std::vector<const com::RawPalette*>& palettes)
{
  for(std::vector<const com::RawPalette*>::const_iterator it =
         palettes.begin(); it != palettes.end(); ++it) {
    d_data->d_paletteDialog->addPalette(*it);
  }
}



void ag::DrawPropertiesWidget::paletteBarClicked(
         qtw::PaletteBar * /* paletteBar */, QMouseEvent *event)
{
  if(event->button() == Qt::LeftButton) {
    if(d_data->d_paletteDialog->exec()) {
      if(d_data->d_paletteDialog->selected() != 0 &&
         d_data->d_paletteBar->palette() !=
         d_data->d_paletteDialog->selected()) {
        d_data->d_paletteBar->setPalette((d_data->d_paletteDialog->selected()));
        d_data->d_paletteBar->repaint();
      }
    }
  }
}



void ag::DrawPropertiesWidget::rescan()
{
  if(d_data->d_paletteBar->palette() !=
         dataObject().properties().palette(dataGuide())) {

    d_data->d_paletteBar->setPalette(
         dataObject().properties().palette(dataGuide()));
  }
}



void ag::DrawPropertiesWidget::apply()
{
  dataObject().setPalette(dataGuide(), d_data->d_paletteBar->palette(), false);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



