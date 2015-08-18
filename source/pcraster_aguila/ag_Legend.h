#ifndef INCLUDED_AG_LEGEND
#define INCLUDED_AG_LEGEND



#include <QWidget>
#include "ag_Types.h"
#include "qt_EventFilterSubject.h"



class QLabel;
namespace ag {
  class DataGuide;
  class DataObject;
}



namespace ag {



//! A Legend object is a QWidget with a legend drawn into it.
/*!
  Legends can be made for maps or graphs or whatever. The legend style is
  determined by the value scale of the data. For example, the legend for a
  classification of scalar data looks different from the legend of a number
  of boolean classes.
*/
class Legend: public QWidget,
              public qt::EventFilterSubject
{

private:

  //! Legend title.
  QLabel*          d_title;

  //! Legend body.
  QWidget*         d_body;

  //! Assignment operator. NOT IMPLEMENTED.
  Legend&          operator=           (const Legend&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Legend              (const Legend&);

  void             resetLayout         ();

  void             redirectChildEventsTo(QObject* filter);

  void             removeChildEventFilter(QObject* filter);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                  Legend               (DataObject const& object,
                                        DataGuide const& guide,
                                        ViewerType type,
                                        QWidget* parent = 0);

  virtual         ~Legend              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  QLabel const*    title               () const;

  QWidget const*   body                () const;

  // QSize            sizeHint            () const { return QSize(200, 200); }

};

} // namespace ag

#endif
