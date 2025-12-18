#ifndef INCLUDED_AG_UTIL
#define INCLUDED_AG_UTIL

#include "ag_DataGuide.h"

#include <QPixmap>

#include <string>



class QWidget;



namespace ag {

std::string        getOpenDataFileName (QWidget* p = nullptr);

QPixmap            pixmap              (DataGuide const& guide);

} // namespace ag

#endif



