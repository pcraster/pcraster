#ifndef INCLUDED_QTW_PALETTEBAR
#define INCLUDED_QTW_PALETTEBAR



#include <vector>
#include <QColor>
#include <QSize>
#include <QWidget>



class QMouseEvent;
class QPaintEvent;
namespace com {
  class RawPalette;
}



namespace qtw {



/*!
  \class PaletteBar
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
class PaletteBar: public QWidget
{

private:

  Q_OBJECT

  //! Palette to show.
  const com::RawPalette *d_palette;

  //! Colours from palette.
  std::vector<QColor> d_colours;

  //! Draw outline or not.
  bool             d_outline;

  //! Assignment operator. NOT IMPLEMENTED.
  PaletteBar &     operator=           (const PaletteBar &);

  //! Copy constructor. NOT IMPLEMENTED.
                   PaletteBar          (const PaletteBar &);

  //! Frees dynamically allocated memory.
  void             clean               ();

protected:

  void             paintEvent          (QPaintEvent *e);

  void             mousePressEvent     (QMouseEvent *e);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   PaletteBar          (QWidget* p=0);

  //! Constructor.
                   PaletteBar          (const com::RawPalette *pal,
                                        QWidget *p=0);

  //! Destructor.
  /* virtual */    ~PaletteBar         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Sets the palette to draw to \a p.
  void             setPalette          (const com::RawPalette *p);

  //! Draw outline or not.
  void             setOutline          (bool s);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns a size hint.
  QSize            sizeHint            () const;

  //! Returns the palette.
  const com::RawPalette *palette       () const;

  //! Returns true if the outline will be drawn.
  bool             outline             () const;

Q_SIGNALS:

  void             mousePressed        (qtw::PaletteBar *pb,
                                        QMouseEvent *e);

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



} // namespace qtw

#endif
