#ifndef INCLUDED_AG_BUFFEREDWIDGET
#define INCLUDED_AG_BUFFEREDWIDGET



// Library headers.
#include <QPixmap>
#include <QPoint>
#include <QRect>
#include <QWidget>

// PCRaster library headers.

// Module headers.



namespace ag {
  // BufferedWidget declarations.
}



namespace ag {

//! This class implements a drawing buffer to avoid flickering while drawing.
/*!
  When you draw something big into a widget without double buffering, you will
  see flickering. To avoid this, you can make use of a second buffer (apart
  from the widget itself). First you draw into the buffer, who's contents
  are then copied into the widget.

  Overload updateBuffer() and call setDirty() and setClean() whenever
  appropriate. The rest comes naturally.

  Starting with version 4 Qt supports double buffering out of the box, but it
  is less useful than this class. For one thing, you cannot have centered
  contents. Qt's double buffering is useful when a widget is hidden by (part
  of) a window and becomes visible again. But when you resize the widget, it
  still calls repaint() with the whole widget area as dirty rectangle. You
  can simulate BufferedWidget with TopLeft anchor but not with Center anchor.
*/
class BufferedWidget: public QWidget
{

public:

  //! Determines which part of the widget the contents are pinned to.
  enum Alignment {
    //! When resizing, the contents should be pinned to the top left corner.
    TopLeft,

    //! When resizing, the contents should be pinned to the center.
    Center
  };

private:

  Q_OBJECT

  //! Widget's buffer.
  QPixmap          _buffer;

  Alignment        _alignment;

  //! Position of the anchor used to align the contents.
  QPointF          _anchor;

  //! The buffer is dirty. If dirty is true, than the whole widget is dirty.
  bool             _dirty;

  //! Left area which is dirty.
  QRectF           _dirtyLeftArea;

  //! Top area which is dirty.
  QRectF           _dirtyTopArea;

  //! Right area which is dirty.
  QRectF           _dirtyRightArea;

  //! Bottom area which is dirty.
  QRectF           _dirtyBottomArea;

  void             moveContents        (int dx,
                                        int dy);

  void             resizeEvent         (QResizeEvent* event);

protected:

                   BufferedWidget      (Alignment alignment,
                                        QWidget* parent=0,
                                        Qt::WindowFlags flags=Qt::Widget);

  Alignment        alignment           () const;

  QPixmap&         buffer              ();

  QPointF const&   anchor              () const;

  void             setDirty            ();

  void             setClean            ();

  virtual void     paintEvent          (QPaintEvent* event);

  virtual void     updateBuffer        (QRectF const& area);

  virtual void     initializeWidget    ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~BufferedWidget     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             moveBy              (double dx,
                                        double dy);

  void             moveBy              (QPointF const& offset);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  QPixmap const&   buffer              () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
