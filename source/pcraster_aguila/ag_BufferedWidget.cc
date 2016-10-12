#include "ag_BufferedWidget.h"

// Library headers.
#include <cassert>
#include <cmath>
#include <iostream>
#include <QPainter>
#include <QPaintEvent>

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the BufferedWidget class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BUFFEREDWIDGET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BUFFEREDWIDGET MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     alignment Alignment.
  \param     parent Parent widget.
  \param     flags Widget flags.

  The default alignment is Center. The widget is dirty by default.
*/
BufferedWidget::BufferedWidget(
         Alignment alignment,
         QWidget* parent,
         Qt::WindowFlags flags)

  : QWidget(parent, flags),
    _alignment(alignment),
    _dirty(true)

{
  // The child is responsible for drawing all dirty areas.
  setAttribute(Qt::WA_NoSystemBackground);
  setAttribute(Qt::WA_OpaquePaintEvent);
}



//! Destructor.
/*!
*/
BufferedWidget::~BufferedWidget()
{
}



BufferedWidget::Alignment BufferedWidget::alignment() const
{
  return _alignment;
}



//! Returns the buffer to draw on.
/*!
  \return    Buffer.

  Use this buffer in an overloaded updateBuffer(QRectF const&).
*/
QPixmap& BufferedWidget::buffer()
{
  return _buffer;
}



QPixmap const& BufferedWidget::buffer() const
{
  return _buffer;
}



//! Returns the anchor position which can be used to align the contents.
/*!
  \return    Anchor position.

  If alignment is Center, the anchor position is in the center of the widget.
*/
QPointF const& BufferedWidget::anchor() const
{
  return _anchor;
}



//! Sets the widget buffer state to dirty.
/*!
  Call this function if the data which the widget's buffer relies on
  have changed. The paintEvent() will call updateBuffer() if the buffer
  is dirty. That way you can update the buffer just before it is used
  to fill the widget.
*/
void BufferedWidget::setDirty()
{
  _dirty = true;
}



//! Sets the widget buffer state to clean.
/*!
  Call this function if you know the buffer is ok. After calling this function
  no calls to updateBuffer() will be made anymore. Call this function as often
  as you can, but not too often!
*/
void BufferedWidget::setClean()
{
  _dirty = false;
  _dirtyLeftArea   = QRectF();
  _dirtyTopArea    = QRectF();
  _dirtyRightArea  = QRectF();
  _dirtyBottomArea = QRectF();
}



//! Updates the buffer's contents.
/*!
  \param     area Dirty area.

  Implement this function in your class to fill the buffer with the widgets
  contents. This function is called when the buffer is dirty and the widget
  needs to be repainted.

  The default does nothing.
*/
#ifdef DEBUG_DEVELOP
void BufferedWidget::updateBuffer(
         QRectF const& area)
#else
void BufferedWidget::updateBuffer(
         QRectF const& /* area */)
#endif
{
#ifdef DEBUG_DEVELOP
  QPainter painter(&buffer());

  static int red = 55;
  static int green = 155;
  static int blue = 255;

  QRectF dirtyArea(area.left(), area.top(), area.width() - 1,
         area.height() - 1);

  // Draw the scene.
  painter.setPen(Qt::white);
  red += 5; green += 20; blue += 15;
  painter.setBrush(QColor(red % 255, green % 255, blue % 255));
  painter.drawRect(dirtyArea);

  // Draw the anchor point.
  painter.setPen(Qt::black);
  painter.setBrush(Qt::red);
  painter.drawEllipse(anchor().x() - 5, anchor().y() - 5, 10, 10);
#endif
}



/*!
  \overload
*/
void BufferedWidget::paintEvent(
         QPaintEvent* event)
{
  // First update the dirty regions of the buffer. Than use the updated buffer
  // to repaint event->rect().

  if(_dirty) {
    // The whole buffer is dirty. Update the whole buffer. Resetting all 'dirty
    // settings'.
    updateBuffer(QRectF(0.0, 0.0, size().width(), size().height()));
    _dirty = false;
  }
  else {
    // Parts of the buffer are dirty. Update and reset only those parts.
    if(_dirtyLeftArea.isValid()) {
      updateBuffer(_dirtyLeftArea);
      _dirtyLeftArea = QRectF();
    }

    if(_dirtyTopArea.isValid()) {
      updateBuffer(_dirtyTopArea);
      _dirtyTopArea = QRectF();
    }

    if(_dirtyRightArea.isValid()) {
      updateBuffer(_dirtyRightArea);
      _dirtyRightArea = QRectF();
    }

    if(_dirtyBottomArea.isValid()) {
      updateBuffer(_dirtyBottomArea);
      _dirtyBottomArea = QRectF();
    }
  }

  QPainter painter(this);
  painter.drawPixmap(event->rect().topLeft(), _buffer, event->rect());
}



//! Initializes the widget's settings.
/*!
  \return    .
  \exception .
  \warning   .
  \sa        .

  Some specializations of this class can be initialized only when the widget
  has a valid size. This function is called when the state of the widget
  changes from an invalid sized widget to a widget with a certain size.
  Implementations can use size(), width() and height() to initialize
  themselves, if necessary.

  The default does nothing.
*/
void BufferedWidget::initializeWidget()
{
}



/*!
  \overload
*/
void BufferedWidget::resizeEvent(
         QResizeEvent* event)
{
  // Testing testing. Is the buffer in sync with the widget?
  assert(
       (event->oldSize().isEmpty() && _buffer.size().isNull()) ||
       (event->oldSize().width() == _buffer.size().width() &&
       event->oldSize().height() == _buffer.size().height()));
  assert(event->size() == size());

  QSize const oldSize = event->oldSize();
  QSize const newSize = event->size();

  // If the buffer grew, then we have something new to update.
  int deltaWidth = newSize.width() - oldSize.width();
  int deltaHeight = newSize.height() - oldSize.height();

  // Upper left corner of part of old buffer that needs to be copied into the
  // new, resized, buffer.
  double xRecycledBuffer = 0.0; // Shut up compiler.
  double yRecycledBuffer = 0.0; // Shut up compiler.

  if(_alignment == TopLeft) {

    // Anchor point is in the upper left corner and stays in that corner.
    assert(_anchor.x() == 0.0 && _anchor.y() == 0.0);

    if(deltaWidth > 0) {
      QRectF deltaDirtyRightArea(oldSize.width(), 0.0,
                   deltaWidth, newSize.height());

      if(!_dirtyRightArea.isValid()) {
        _dirtyRightArea = deltaDirtyRightArea;
      }
      else {
        _dirtyRightArea |= deltaDirtyRightArea;
      }
    }

    if(deltaHeight > 0) {
      QRectF deltaDirtyBottomArea(0.0, oldSize.height(),
                   oldSize.width(), deltaHeight);

      if(!_dirtyBottomArea.isValid()) {
        _dirtyBottomArea = deltaDirtyBottomArea;
      }
      else {
        _dirtyBottomArea |= deltaDirtyBottomArea;
      }
    }

    xRecycledBuffer = 0.0;
    yRecycledBuffer = 0.0;
  }
  else if(_alignment == Center) {
    // Anchor point is in the center and changes.
    // We create a new pixmap and copy stuff from the original one. Then we
    // get rid of the old one.

    // assert(_anchor.x() == oldSize.width() / 2.0 &&
    //          _anchor.y() == oldSize.height() / 2.0);

    // Determine delta width at the left and right side. Make sure that
    // the the anchor point stays in the center.
    double deltaWidthLeft = newSize.width() / 2.0 - oldSize.width() / 2.0;
    double deltaWidthRight = deltaWidth - deltaWidthLeft;
    double deltaHeightTop = newSize.height() / 2.0 - oldSize.height() / 2.0;
    double deltaHeightBottom = deltaHeight - deltaHeightTop;

    assert(deltaWidthLeft + deltaWidthRight == deltaWidth);
    assert(deltaWidthLeft + deltaWidthRight == deltaWidth);

    if(deltaWidth > 0) {

      //                    dw / 2      dw - dw / 2
      //   +-----+         +-----------------+
      //   |     |         |  x  |     |  x  |
      //   |     |         |     |     |     |
      //   |     |   ->    |     |     |     |
      //   |     |         |     |     |     |
      //   |     |         |     |     |     |
      //   +-----+         +-----------------+
      //

      // Left.
      QRectF deltaDirtyLeftArea(0.0, 0.0, deltaWidthLeft, newSize.height());

      if(!_dirtyLeftArea.isValid()) {
        _dirtyLeftArea = deltaDirtyLeftArea;
      }
      else {
        _dirtyLeftArea.translate(deltaWidthLeft, 0.0);
        _dirtyLeftArea |= deltaDirtyLeftArea;
      }

      // Right.
      QRectF deltaDirtyRightArea(deltaWidthLeft + oldSize.width(),
                 0.0, deltaWidthRight, newSize.height());

      if(!_dirtyRightArea.isValid()) {
        _dirtyRightArea = deltaDirtyRightArea;
      }
      else {
        _dirtyRightArea.translate(-deltaWidthRight, 0.0);
        _dirtyRightArea |= deltaDirtyRightArea;
      }
    }

    if(deltaHeight > 0) {

      // Top.
      QRectF deltaDirtyTopArea(std::max(deltaWidthLeft, 0.0), 0.0,
         oldSize.width(), deltaHeightTop);

      if(!_dirtyTopArea.isValid()) {
        _dirtyTopArea = deltaDirtyTopArea;
      }
      else {
        _dirtyTopArea.translate(0.0, deltaHeightTop);
        _dirtyTopArea |= deltaDirtyTopArea;
      }

      // Bottom.
      QRectF deltaDirtyBottomArea(std::max(deltaWidthLeft, 0.0),
         deltaHeightTop + oldSize.height(), oldSize.width(), deltaHeightBottom);

      if(!_dirtyBottomArea.isValid()) {
        _dirtyBottomArea = deltaDirtyBottomArea;
      }
      else {
        _dirtyBottomArea.translate(0.0, -deltaHeightBottom);
        _dirtyBottomArea |= deltaDirtyBottomArea;
      }
    }

    _anchor += QPointF(deltaWidthLeft, deltaHeightTop);

    // assert(_anchor.x() == newSize.width() / 2.0 &&
    //      _anchor.y() == newSize.height() / 2.0);

    xRecycledBuffer = deltaWidthLeft;
    yRecycledBuffer = deltaHeightTop;
  }

  // Blit the current contents of the buffer to the new one.
  // Since painting is clipped this works even if the widget is resized to a
  // smaller size.
  QPixmap newBuffer(newSize);

  if(!newBuffer.size().isNull()) {
    // Centralized anchor:
    // Take numerical dispersion into account. Resizing the window with small
    // steps (one pixel) will result in 0.5 pixel on both sides. In case this
    // is always rounded up, the vis will move to fast. Use anchor point to
    // decide whether the offset should be rounded up or down.
    qreal intPart, fractionalPart;

    fractionalPart = std::modf(_anchor.x(), &intPart);

    // xRecycledBuffer = qRound(fractionalPart) == 0
    xRecycledBuffer = fractionalPart < qreal(0.5)
      ? std::floor(xRecycledBuffer)
      : std::ceil(xRecycledBuffer);

    fractionalPart = std::modf(_anchor.y(), &intPart);

    // yRecycledBuffer = qRound(fractionalPart) == 0
    yRecycledBuffer = fractionalPart < qreal(0.5)
      ? std::floor(yRecycledBuffer)
      : std::ceil(yRecycledBuffer);

    QPainter painter(&newBuffer);
    painter.drawPixmap(QPointF(xRecycledBuffer, yRecycledBuffer),
         _buffer, QRectF(0.0, 0.0, oldSize.width(), oldSize.height()));
  }

  _buffer = newBuffer;

  if(!oldSize.isValid()) {
    initializeWidget();
  }
}



// Only deals with buffer and dirty regions. Other admin should be
// handled by the caller (eg offset).
//!
/*!
  \tparam    .
  \param     dx Number of pixels to move.
  \param     dy Number of pixels to move.
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void BufferedWidget::moveContents(
         int dx,
         int dy)
{
  // QPainter painter(&_buffer);
  QPixmap newBuffer(_buffer);
  QPainter painter(&newBuffer);

  if(dx > 0) {
    // Blit current contents.
    // QPainter painter(&newBuffer);
    painter.drawPixmap(dx, 0, _buffer, 0, 0, width() - dx, height());

    // Mark the dirty area.
    // Left.
    QRectF deltaDirtyLeftArea(0, 0, dx, height());

    if(!_dirtyLeftArea.isValid()) {
      _dirtyLeftArea = deltaDirtyLeftArea;
    }
    else {
      _dirtyLeftArea |= deltaDirtyLeftArea;
    }
  }
  else if(dx < 0) {
    // Blit current contents.
    // QPainter painter(&newBuffer);
    painter.drawPixmap(0, 0, _buffer, -dx, 0, width() - -dx, height());

    // Mark the dirty area.
    // Right.
    QRectF deltaDirtyRightArea(width() - -dx, 0, -dx, height());

    if(!_dirtyRightArea.isValid()) {
      _dirtyRightArea = deltaDirtyRightArea;
    }
    else {
      _dirtyRightArea |= deltaDirtyRightArea;
    }
  }

  if(dy > 0) {
    // Blit current contents.
    // QPainter painter(&newBuffer);
    painter.drawPixmap(0, dy, _buffer, 0, 0, width(), height() - dy);

    // Mark the dirty area.
    // Top
    QRectF deltaDirtyTopArea(0, 0, width(), dy);

    if(!_dirtyTopArea.isValid()) {
      _dirtyTopArea = deltaDirtyTopArea;
    }
    else {
      _dirtyTopArea |= deltaDirtyTopArea;
    }
  }
  else if(dy < 0) {
    // Blit current contents.
    // QPainter painter(&newBuffer);
    painter.drawPixmap(0, 0, _buffer, 0, -dy, width(), height() - -dy);

    // Mark the dirty area.
    // Bottom.
    QRectF deltaDirtyBottomArea(0, height() - -dy, width(), -dy);

    if(!_dirtyBottomArea.isValid()) {
      _dirtyBottomArea = deltaDirtyBottomArea;
    }
    else {
      _dirtyBottomArea |= deltaDirtyBottomArea;
    }
  }

  painter.end();
  _buffer = newBuffer;

  assert(_buffer.size() == size());
}



//!
/*!
  \tparam    .
  \param     dx Amount of pixels to move.
  \param     dy Amount of pixels to move.
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void BufferedWidget::moveBy(
         double dx,
         double dy)
{
  // moveContents(dx, dy);
  moveContents(qRound(dx), 0);
  moveContents(0.0, qRound(dy));
}



void BufferedWidget::moveBy(
         QPointF const& offset)
{
  moveBy(offset.x(), offset.y());
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


} // namespace ag
