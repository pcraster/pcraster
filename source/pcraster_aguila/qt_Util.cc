#include "qt_Util.h"
#include <cassert>
#include <QColor>
#include <QFileDialog>
#include <QRect>
#include <QWidget>
#include "dal_Formats.h"
#include "com_colour.h"
#include "com_fileformatinfo.h"



/*!
  This function converts a com_Colour object to a QColor object and returns the
  result.
*/
QColor qt::toQColor(const com_Colour &colour)
{
  return QColor(colour.getRed(), colour.getGreen(), colour.getBlue());
}



/*!
  \param   w1 Widget to center to.
  \return  w2 Widget to center.

  This function does nothing if \a w1 and/or \a w2 is not a toplevel widget.
  This function does nothing is the geometry of \a w1 and/or \a w2 is invalid.
*/
void qt::center(const QWidget &w1, QWidget &w2)
{
  if(!w1.isTopLevel() || !w2.isTopLevel()) return;

  // Get size and position of the frames of both widgets.
  QRect g1 = w1.geometry();
  QRect g2 = w2.geometry();

  if(g1.isValid() && g2.isValid())
  {
    int x = std::max<int>(w1.x(), w1.x() + (g1.width()  - g2.width())  / 2);
    int y = std::max<int>(w1.y(), w1.y() + (g1.height() - g2.height()) / 2);
    w2.setGeometry(x, y, w2.width(), w2.height());
  }
}



//! Shows an open file dialog tailored at \a fileFormat.
/*!
  \sa        getOpenFileName(const std::vector<com::FileFormatInfo>&,
             QWidget*, const char*)
*/
std::string qt::getOpenFileName(const com::FileFormatInfo& fileFormat,
                    QWidget* parent, const char* name)
{
  std::vector<com::FileFormatInfo> fileFormats;
  fileFormats.push_back(fileFormat);

  return getOpenFileName(fileFormats, parent, name /* , caption */);
}



std::string qt::getOpenFileName(
         dal::Formats const& formats,
         QWidget* parent,
         char const* name)
{
  std::vector<com::FileFormatInfo> fileFormats;

  for(size_t i = 0; i < formats.size(); ++i) {
    dal::Format const& format(formats[i]);

    if(format.isFileBased()) {
      fileFormats.push_back(com::FileFormatInfo(
         format.description(), format.extensions()));
    }
  }

  return getOpenFileName(fileFormats, parent, name);
}



//! Shows an open file dialog tailored at \a fileFormats.
/*!
  \param     fileFormats Most interesting file formats.
  \param     parent Dialog's parent.
  \param     name Name.
  \return    Filename or empty string.
  \warning   \a fileFormats must not be empty. Every file format in
             \a fileFormats must have at least one extension.

  The file format information in \a fileFormats is used to create filters.
  The last filter added is the "All files (*)" filter.
*/
std::string qt::getOpenFileName(
                   const std::vector<com::FileFormatInfo>& fileFormats,
                   QWidget* parent, const char* /* name */)
{
  // File filter.
  std::string filter;

  // For all file formats.
  for(std::vector<com::FileFormatInfo>::const_iterator it = fileFormats.begin();
                   it != fileFormats.end(); ++it) {

    std::vector<std::string> const& extensions = (*it).extensions();

    if(!extensions.empty()) {
      // Use description.
      assert(!(*it).description().empty());
      filter += (*it).description() + " (";

      // Add extension(s).
      assert(!extensions.empty());

      for(std::vector<std::string>::const_iterator ext_it = extensions.begin();
                     ext_it != extensions.end(); ++ext_it) {

        filter += "*." + *ext_it;

        // Add space BETWEEN the extension filters.
        if(*ext_it != extensions.back()) {
          filter += " ";
        }
      }

      filter += ")";

      // Seperator between the filters.
      filter += ";;";
    }
  }

  // Add 'all files' to filter.
  filter += ("All files (*)");

  QString fn = QFileDialog::getOpenFileName(parent, QString(), QString(),
                   QString(filter.c_str()));

  return fn.isEmpty() ? std::string() : std::string(fn.toUtf8().constData());
}


