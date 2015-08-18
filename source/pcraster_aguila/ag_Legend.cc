#include "ag_Legend.h"
#include <QLabel>
#include <QLayout>
#include <QToolTip>
#include "ag_DataObject.h"
#include "ag_DataProperties.h"
#include "ag_ClassLegendBody.h"
#include "ag_GeometryLegendBody.h"
#include "ag_RangeLegendBody.h"



/*!
  \file
   This file contains the implementation of the Legend class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     object Data object \a guide resides in.
  \param     guide Guide to legend data.
  \param     parent Parent widget.
*/
ag::Legend::Legend(
         DataObject const& object,
         DataGuide const& guide,
         ViewerType type,
         QWidget* parent)

  : QWidget(parent),
    qt::EventFilterSubject(this)

{
  assert(object.isValid(guide));
  assert(guide.type() == geo::STACK ||
         guide.type() == geo::FEATURE ||
         guide.type() == geo::VECTOR ||
         guide.type() == geo::TIMESERIES);

  // setSelected(object.isSelected(guide));

  d_title = new QLabel(this);
  std::string title = object.title(guide);
  d_title->setText(title.c_str());
  d_title->adjustSize();
  d_title->setToolTip(QString(object.description(guide).c_str()));

  LegendBody* body = 0;

  switch(guide.type()) {
    case geo::STACK: {
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
          body = new ClassLegendBody(object, guide, type, this);
          break;
        }
        case VS_NOMINAL: {
          body = new ClassLegendBody(object, guide, type, this);
          break;
        }
        case VS_ORDINAL: {
          body = new ClassLegendBody(object, guide, type, this);
          break;
        }
        case VS_SCALAR: {
          body = new RangeLegendBody(object, guide, type, this);
          break;
        }
        case VS_DIRECTION: {
          body = new RangeLegendBody(object, guide, type, this);
          break;
        }
        case VS_LDD: {
          // body = new LddLegendBody(object, guide, type, this);
          body = new ClassLegendBody(object, guide, type, this);
          break;
        }
        default: {
          assert(false);
          break;
        }
      }
      break;
    }
    case geo::FEATURE: {
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
          body = new ClassLegendBody(object, guide, type, this);
          break;
        }
        case VS_NOMINAL: {
          body = new ClassLegendBody(object, guide, type, this);
          break;
        }
        case VS_ORDINAL: {
          body = new ClassLegendBody(object, guide, type, this);
          break;
        }
        case VS_SCALAR: {
          body = new RangeLegendBody(object, guide, type, this);
          break;
        }
        case VS_UNDEFINED: {
          body = new GeometryLegendBody(object, guide, type, this);
          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      break;
    }
    case geo::VECTOR: {
      assert(guide.valueScale() == VS_SCALAR);
      body = new RangeLegendBody(object, guide, type, this);
      break;
    }
    case geo::TIMESERIES: {
      assert(guide.valueScale() == VS_SCALAR);
      // body = new LinePlotLegendBody(object, guide, type, this);
      body = new RangeLegendBody(object, guide, type, this);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  assert(body);

  d_body = body;
  resetLayout();
  adjustSize();

  setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
}



//! Destructor.
/*!
*/
ag::Legend::~Legend()
{
}



//! Re-arranges all available legend elements (title, items).
/*!
*/
void ag::Legend::resetLayout()
{
  delete layout();

  QBoxLayout* layout = new QVBoxLayout(this);

  if(d_title) {
    layout->addWidget(d_title);
  }

  if(d_body) {
    layout->addWidget(d_body);
  }
}



//! Returns the title of the legend as a label.
/*!
  \return    Title label.
*/
QLabel const* ag::Legend::title() const
{
  assert(d_title);

  return d_title;
}



//! Returns the body of the legend.
/*!
  \return    Body.
*/
QWidget const* ag::Legend::body() const
{
  assert(d_body);

  return d_body;
}



void ag::Legend::redirectChildEventsTo(
         QObject* filter)
{
  if(d_title) {
    d_title->installEventFilter(filter);
  }

  if(d_body) {
    d_body->installEventFilter(filter);
  }
}



void ag::Legend::removeChildEventFilter(
         QObject* filter)
{
  if(d_title) {
    d_title->removeEventFilter(filter);
  }

  if(d_body) {
    d_body->removeEventFilter(filter);
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------
