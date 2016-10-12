#ifndef INCLUDED_QT_DIALOG
#define INCLUDED_QT_DIALOG



#include <QDialog>



class QWidget;
namespace qt {
  class DialogPrivate;
}



namespace qt {

/*!
  \class Dialog
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
class Dialog: public QDialog
{

private:

  // Qt-specific.
  Q_OBJECT

  DialogPrivate*   d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  Dialog&          operator=           (const Dialog &);

  //! Copy constructor. NOT IMPLEMENTED.
                   Dialog           (const Dialog &);

  //! Frees dynamically allocated memory.
  void             clean               ();

  //! Resets the layout to manage the current active parts of the dialog box.
  void             resetLayout         ();

protected:

protected Q_SLOTS:

  virtual void     acceptSettings      ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Dialog              (QWidget *   p = 0,
                                        bool        m = true,
                                        Qt::WindowFlags  f = Qt::Widget);

  //! Destructor.
  virtual          ~Dialog          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Sets the central widget of the dialog to \a w.
  void             setCentralWidget    (QWidget *w);

  //void             adjustSize          ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the central widget if set, else returns 0.
  QWidget *        centralWidget       () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace qt

#endif
