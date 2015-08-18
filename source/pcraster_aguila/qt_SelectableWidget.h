#ifndef INCLUDED_QT_SELECTABLEWIDGET
#define INCLUDED_QT_SELECTABLEWIDGET



// Library headers.
#include <QFrame>

// PCRaster library headers.

// Module headers.



namespace qt {
  // SelectableWidget declarations.
}



namespace qt {



//! The SelectableWidget class is for widget which are selectable.
/*!
  A selectable widget is a widget which visualy shows if it is selected.
  By looking at the widget you know it is selected or not.

  This class implements this by inheriting the QFrame class and drawing a
  frame around selected widgets. If your widgets has children, you should make
  sure that there's space for the frame around the children. Call buffer() for
  the amount of space the frame will take. Use this too for unselected widgets.
*/
class SelectableWidget: public QFrame
{

private:

  Q_OBJECT

  //! Buffer for selection visualization.
  size_t           d_buffer;

  //! Assignment operator. NOT IMPLEMENTED.
  SelectableWidget& operator=          (const SelectableWidget&);

  //! Copy constructor. NOT IMPLEMENTED.
                   SelectableWidget    (const SelectableWidget&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SelectableWidget    (QWidget* parent);

  virtual          ~SelectableWidget   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             select              ();

  void             unSelect            ();

  void             setSelected         (bool select);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isSelected          () const;

  size_t           buffer              () const;

Q_SIGNALS:

  void             selected            ();

  void             unSelected          ();

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



} // namespace qt

#endif
