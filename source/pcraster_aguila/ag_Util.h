#ifndef INCLUDED_AG_UTIL
#define INCLUDED_AG_UTIL



#include <string>
#include <QPixmap>
#include "ag_DataGuide.h"



class QWidget;



namespace ag {

std::string        getOpenDataFileName (QWidget* p = 0);

QPixmap            pixmap              (DataGuide const& guide);

} // namespace ag

#endif



