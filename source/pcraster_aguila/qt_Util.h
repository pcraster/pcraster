#ifndef INCLUDED_QT_UTIL
#define INCLUDED_QT_UTIL



#include <string>
#include <vector>



class QColor;
class QWidget;
class com_Colour;
namespace dal {
  class Formats;
}
namespace com {
  class FileFormatInfo;
}



namespace qt {



  QColor           toQColor            (const com_Colour &colour);

  //! Centers child widget \c to parent widget \p.
  void             center              (const QWidget &p,
                                        QWidget &c);

  std::string      getOpenFileName     (const com::FileFormatInfo& fileFormat,
                                        QWidget* parent,
                                        const char* name);

  std::string      getOpenFileName     (dal::Formats const& formats,
                                        QWidget* parent,
                                        char const* name);

  std::string      getOpenFileName
                        (const std::vector<com::FileFormatInfo>& fileFormats,
                         QWidget* parent,
                         const char* name);

} // namespace qt

#endif
