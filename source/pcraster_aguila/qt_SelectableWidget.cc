#include "qt_SelectableWidget.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the SelectableWidget class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SELECTABLEWIDGET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SELECTABLEWIDGET MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     parent Parent widget.
  \param     name Widgets name.

  Default the widget is unselected.
*/
qt::SelectableWidget::SelectableWidget(QWidget* parent)

  : QFrame(parent),
    d_buffer(1)

{
  setLineWidth(d_buffer);

  // The default frame style is for unselected.
  setFrameStyle(QFrame::NoFrame);
}



//! Destructor.
/*!
*/
qt::SelectableWidget::~SelectableWidget()
{
}



//! Sets the state of the widget to selected.
/*!
  \sa        unSelect(), setSelected(bool), isSelected()

  Nothing happens if this widget is already selected. If the widget is not
  already selected the selected() signal is emitted.
*/
void qt::SelectableWidget::select()
{
  if(!isSelected()) {
    Q_EMIT selected();
    setFrameStyle(QFrame::Panel | QFrame::Sunken);
    //PORT
    // repaint(false);
    repaint();
  }
}



//! Sets the state of the widget to unselected.
/*!
  \sa        select(), setSelected(bool), isSelected()

  Nothing happens if this widget is already selected. If the widget is not
  already un selected the unSelected() signal is emitted.
*/
void qt::SelectableWidget::unSelect()
{
  if(isSelected()) {
    Q_EMIT unSelected();
    setFrameStyle(QFrame::NoFrame);
    //PORT
    // repaint(false);
    repaint();
  }
}



//! Sets the selected mode to \a select.
/*!
  \param     select Selection mode.
  \sa        select(), unSelect()

  This function calls select() or unSelect().
*/
void qt::SelectableWidget::setSelected(bool select)
{
  if(select) {
    SelectableWidget::select();
  }
  else {
    unSelect();
  }
}



//! Returns true if the widget is selected.
/*!
  \return    true or false.
  \sa        select(), unSelect()
*/
bool qt::SelectableWidget::isSelected() const
{
  return frameStyle() != QFrame::NoFrame;
}



//! Returns the buffer needed for visualizing the selection.
/*!
  \return    buffer.
*/
size_t qt::SelectableWidget::buffer() const
{
  return d_buffer;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



