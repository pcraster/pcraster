#ifndef INCLUDED_QT_CONST
#define INCLUDED_QT_CONST





#include <QColor>
#include <QPalette>



/*!
  \class qt_Const
  \brief The qt_Const class contains library wide constants.

  The qt_Const class is designed for library wide constants. No object of
  this type can be made. The class only contains static constant values.
*/
struct qt_Const
{

  static const QColor      BACKGROUNDCOLOUR;
  static const QColor      FOREGROUNDCOLOUR;
  static const int         MAXCONTENTSIZE;

};



namespace qt
{

  // //! Space between widgets.
  static const int SPACING = 5;

  // //! Border sizes in widgets.
  static const int MARGIN = 8;

  //! Width of most buttons (ok, cancel, )
  static const int BUTTONWIDTH = 80;

  //! Height of most buttons (ok, cancel, )
  static const int BUTTONHEIGHT = 28;

  //! Light on dark palette: dark background and light foreground.
  static const QPalette LIGHTONDARKPALETTE(
                   QColor(127, 127, 127), QColor(0, 0, 0));

  //! Dark on light palette: light background and dark foreground.
  static const QPalette DARKONLIGHTPALETTE(
                   QColor(127, 127, 127), QColor(255, 255, 255));

/*
  //! PCRaster logo for in About boxes.
  extern const char** PCRASTERLOGO;
*/

}

#endif
