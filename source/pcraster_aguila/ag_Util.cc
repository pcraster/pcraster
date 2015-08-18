#include "ag_Util.h"
#include <cassert>
#include <QFileDialog>



std::string ag::getOpenDataFileName(QWidget *p)
{
  QString fn = QFileDialog::getOpenFileName(p, QString::null, QString::null,
                   "All data files (*.csf *.map)");

  return fn.isEmpty() ? std::string() : std::string(fn.toUtf8().constData());
}
#include "icons/booleanstack.xpm"
#include "icons/directionalstack.xpm"
// #include "icons/data.xpm"
// #include "icons/group.xpm"
#include "icons/lddstack.xpm"
#include "icons/nominalstack.xpm"
#include "icons/ordinalstack.xpm"
#include "icons/scalarstack.xpm"
#include "icons/timeseries.xpm"
#include "icons/vector.xpm"

QPixmap ag::pixmap(
         DataGuide const& guide)
{
  QPixmap result;

  switch(guide.type()) {
    case geo::STACK: {
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
          result = QPixmap((const char **)booleanstack_xpm);
          break;
        }
        case VS_NOMINAL: {
          result = QPixmap((const char **)nominalstack_xpm);
          break;
        }
        case VS_ORDINAL: {
          result = QPixmap((const char **)ordinalstack_xpm);
          break;
        }
        case VS_SCALAR: {
          result = QPixmap((const char **)scalarstack_xpm);
          break;
        }
        case VS_DIRECTION: {
          result = QPixmap((const char **)directionalstack_xpm);
          break;
        }
        case VS_LDD: {
          result = QPixmap((const char **)lddstack_xpm);
          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      break;
    }
    case geo::FEATURE: {
      if(guide.valueScale() == VS_UNDEFINED) {
        // No attribute, only geometry.
        /// FEATURE create image for geometry only feature data.
        result = QPixmap((const char **)booleanstack_xpm);
      }
      else {
        switch(guide.valueScale()) {
          /// FEATURE create images for feature data.
          case VS_BOOLEAN: {
            result = QPixmap((const char **)booleanstack_xpm);
            break;
          }
          case VS_NOMINAL: {
            result = QPixmap((const char **)nominalstack_xpm);
            break;
          }
          case VS_ORDINAL: {
            result = QPixmap((const char **)ordinalstack_xpm);
            break;
          }
          case VS_SCALAR: {
            result = QPixmap((const char **)scalarstack_xpm);
            break;
          }
          default: {
            assert(false);
            break;
          }
        }
      }

      break;
    }
    case geo::VECTOR: {
      assert(guide.valueScale() == VS_SCALAR);
      result = QPixmap((const char **)vector_xpm);
      break;
    }
    case geo::TIMESERIES: {
      assert(guide.valueScale() == VS_SCALAR);
      result = QPixmap((const char **)timeseries_xpm);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}


