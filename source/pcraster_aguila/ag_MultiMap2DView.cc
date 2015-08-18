#include "ag_MultiMap2DView.h"

// Library headers.
#include <QLayout>
#include <QLineEdit>

// PCRaster library headers.

// Module headers.
#include "ag_DataObject.h"
#include "ag_Map2DView.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the MultiMap2DView class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MULTIMAP2DVIEW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MULTIMAP2DVIEW MEMBERS
//------------------------------------------------------------------------------

MultiMap2DView::MultiMap2DView(
         DataObject* dataObject,
         size_t nrRows,
         size_t nrCols,
         QWidget* parent)

  : Visualisation<>(dataObject, "Multi 2D Map View", parent),
    d_nrRows(nrRows), d_nrCols(nrCols)

{
  createInterface();
}



/* NOT IMPLEMENTED
//! Copy constructor.
MultiMap2DView::MultiMap2DView(MultiMap2DView const& rhs)

  : Base(rhs)

{
}
*/



MultiMap2DView::~MultiMap2DView()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
MultiMap2DView& MultiMap2DView::operator=(MultiMap2DView const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



void MultiMap2DView::createInterface()
{
  QGridLayout* layout = new QGridLayout(this);
  layout->setContentsMargins(0, 0, 0, 0);

  for(size_t row = 0; row < 2 * d_nrRows; row += 2) {
    for(size_t col = 0; col < d_nrCols; ++col) {
      QLineEdit* label = new QLineEdit(this);

      QPalette palette;
      palette.setColor(label->backgroundRole(),
         this->palette().color(this->backgroundRole()));
      label->setPalette(palette);

      label->setFrame(false);
      label->setAlignment(Qt::AlignHCenter);
      label->setSizePolicy(QSizePolicy(QSizePolicy::Expanding,
         QSizePolicy::Fixed));

      d_mapViews.push_back(
         boost::make_tuple(label, new Map2DView(&dataObject(), this)));

      layout->addWidget(d_mapViews.back().get<0>(), row, col);
      layout->addWidget(d_mapViews.back().get<1>(), row + 1, col);
    }
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void MultiMap2DView::setLabel(size_t row, size_t col)
{
  QLineEdit* label = d_mapViews[row * d_nrCols + col].get<0>();
  Map2DView const* view = d_mapViews[row * d_nrCols + col].get<1>();

  if(!label->isModified()) {

    std::string name = "No data loaded";
    std::vector<DataGuide> dataGuides = view->visualisationEngine().dataGuides();

    if(!dataGuides.empty()) {
      // name = d_dataObject->name(dataGuides[0]);
      name = dataObject().description(dataGuides[0]);
    }

    label->setText(QString(name.c_str()));
  }
}



void MultiMap2DView::addAttribute(DataGuide const& guide)
{
  for(size_t i = 0; i < d_mapViews.size(); ++i) {
    d_mapViews[i].get<1>()->addAttribute(guide);
  }
}



void MultiMap2DView::addAttribute(size_t row, size_t col,
         DataGuide const& guide)
{
  d_mapViews[row * d_nrCols + col].get<1>()->addAttribute(guide);
  setLabel(row, col);
}



size_t MultiMap2DView::nrCols() const
{
  return d_nrCols;
}



void MultiMap2DView::zoomAll()
{
  if(d_nrRows && d_nrCols) {
    d_mapViews[0].get<1>()->zoomAll();
  }
}



void MultiMap2DView::resetMapView()
{
  if(d_nrRows && d_nrCols) {
    d_mapViews[0].get<1>()->resetMapView();
  }
}



void MultiMap2DView::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void MultiMap2DView::process()
{
  if(visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    if(!dataObject().backgroundColour().isValid()) {
      setPalette(QPalette());

      for(size_t i = 0; i < d_mapViews.size(); ++i) {
        d_mapViews[i].get<0>()->setPalette(QPalette());
      }
    }
    else {
      QPalette palette;
      palette.setColor(backgroundRole(), dataObject().backgroundColour());
      setPalette(palette);

      for(size_t i = 0; i < d_mapViews.size(); ++i) {
        QLineEdit* label = d_mapViews[i].get<0>();
        QPalette palette;
        palette.setColor(label->backgroundRole(),
           dataObject().backgroundColour());
        label->setPalette(palette);
      }
    }
  }
}



void MultiMap2DView::visualise()
{
  // Done scanning, update stuff if needed.
  if(visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    for(size_t i = 0; i < d_mapViews.size(); ++i) {
      d_mapViews[i].get<0>()->update();
    }
  }

  visualisationEngine().finishedScanning(dataObject());
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag
