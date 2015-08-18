#ifndef INCLUDED_QTD_SELECTPALETTEDIALOG
#define INCLUDED_QTD_SELECTPALETTEDIALOG



#ifndef INCLUDED_QT_DIALOG
#include "qt_Dialog.h"
#endif



class QMouseEvent;
namespace com {
  class RawPalette;
}
namespace qtd {
  class SelectPalettePrivate;
}
namespace qtw {
  class PaletteBar;
}



namespace qtd {

typedef qt::Dialog qtDialog;

/*!
  \class SelectPalette
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
class SelectPalette: public qtDialog
{

private:

  Q_OBJECT

  SelectPalettePrivate *d_cw;

  //! Assignment operator. NOT IMPLEMENTED.
  SelectPalette &  operator=           (const SelectPalette &);

  //! Copy constructor. NOT IMPLEMENTED.
                   SelectPalette       (const SelectPalette &);

  //! Frees dynamically allocated memory.
  void             clean               ();

private Q_SLOTS:

  void             selectPaletteBar    (qtw::PaletteBar *pb,
                                        QMouseEvent *e);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   SelectPalette       (QWidget *p = 0,
                                        const char *n = 0);

  //! Destructor.
  /* virtual */    ~SelectPalette      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Adds a palette to the collection of selectable palettes.
  void             addPalette          (const com::RawPalette *p);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the currently selected palette or 0 if none is selected.
  const com::RawPalette *selected      () const;

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
