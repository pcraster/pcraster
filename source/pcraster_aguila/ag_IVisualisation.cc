#include "ag_IVisualisation.h"
#include <stdexcept>
#include <sstream>
#include "dev_Algorithm.h"
#include "dev_ToString.h"
#include "dal_Utils.h"
#include "com_exception.h"
#include "geo_DataType.h"
#include "ag_DataObject.h"
#include "ag_VisEngine.h"



//------------------------------------------------------------------------------

namespace ag {

namespace detail {

struct DataTypePrinter
{
  std::string operator()(geo::DataType dataType) {
    return geo::dataTypeToStr(dataType);
  }
};

struct ValueScalePrinter
{
  std::string operator()(CSF_VS valueScale) {
    return dal::valueScaleToString(valueScale);
  }
};

}

class IVisualisationPrivate
{
public:

  std::string      d_visualisationName;

  DataObject*      d_dataObject;

  VisEngine        d_engine;

  // dal::DataSpace   d_profileDataSpace;

  // bool             d_isFullScreen;

  // QToolButton*     d_fullScreenTB;

  // Suported data types.
  std::vector<geo::DataType> d_supportedDataTypes;

  // Supported value scales.
  std::vector<CSF_VS> d_supportedValueScales;

  // Supported file formats.
  std::vector<com::FileFormatInfo> d_fileFormats;

  IVisualisationPrivate()
    : d_dataObject(0)
    /*
      d_isFullScreen(false),
      d_fullScreenTB(0) */
  {
  }

  ~IVisualisationPrivate()
  {
  }
};

}



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

size_t ag::IVisualisation::d_nrCreated = 0;

size_t ag::IVisualisation::nrCreated()
{
  return d_nrCreated;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
ag::IVisualisation::IVisualisation(ag::DataObject* object,
         const std::string& visualisationName)

  : VisObserver(),
    d_data(new IVisualisationPrivate())

{
  assert(object);

  d_data->d_visualisationName = visualisationName;
  d_data->d_dataObject = object;
  d_data->d_dataObject->attach(this);

  ++d_nrCreated;
}



ag::IVisualisation::~IVisualisation()
{
  // First, enable others to respond to a destuct of this object.
  // Q_EMIT closed(this);
  d_data->d_dataObject->detach(this);
  delete d_data;
}



/*
bool ag::IVisualisation::eventFilter(QObject* object, QEvent *event)
{
  // See if mouse events are handled by the visualisation.
  if(event->type() == QEvent::MouseButtonPress) {
    QMouseEvent *me = static_cast<QMouseEvent *>(event);
    if(mousePressHandled(object, me)) {
      return true;
    }
  }
  else if(event->type() == QEvent::MouseButtonRelease) {
    QMouseEvent *me = static_cast<QMouseEvent *>(event);
    if(mouseReleaseHandled(object, me)) {
      return true;
    }
  }
  else if(event->type() == QEvent::MouseButtonDblClick)
  {
    QMouseEvent *me = static_cast<QMouseEvent *>(event);
    if(mouseDoubleClickHandled(object, me)) {
      return true;
    }
  }
  else if(event->type() == QEvent::MouseMove)
  {
    QMouseEvent *me = static_cast<QMouseEvent *>(event);
    if(mouseMoveHandled(object, me)) {
      return true;
    }
  }

  // Then maybe the main window knows how to handle this event.
  return QMainWindow::eventFilter(object, event);
}
*/



/*
void ag::IVisualisation::closeEvent(QCloseEvent *e)
{
  // Selecting the close menu item and pressing the close button of the system
  // menu will result in passing this event handler.
  e->accept();
}
*/



void ag::IVisualisation::setSupportedDataTypes(
                   const std::vector<geo::DataType>& dataTypes)
{
  assert(!dataTypes.empty());
  d_data->d_supportedDataTypes = dataTypes;
}



void ag::IVisualisation::setSupportedValueScales(
                   const std::vector<CSF_VS>& valueScales)
{
  assert(!valueScales.empty());
  d_data->d_supportedValueScales = valueScales;
}



void ag::IVisualisation::setSaveAsFileFormats(
                   const std::vector<com::FileFormatInfo>& fileFormats)
{
  assert(!fileFormats.empty());
  d_data->d_fileFormats = fileFormats;
}



/*
void ag::IVisualisation::showAboutDataObject()
{
#ifdef DEBUG_DEVELOP
  assert(d_dataObject);
  std::ostringstream s;
  s << *d_dataObject;
  (void)QMessageBox::information(this, "Data object contents", s.str());
#endif
}
*/



/*!
  \warning   Only call this function if the internal state of the visualisation
             (the cursor) has been changed (eg. when the user clicks on the
             map). Calling this function too often might result in a lot of
             (timely) updates.

  This function updates the internal state of the visualisation and calls
  notify() to inform observers of the change.
*/
/*
yepyep
void ag::IVisualisation::cursorChanged()
{
  // Update internal state so that all observers are able to determine what
  // has changed.
  // d_cursor = ...

  // Let's notify all observers. But only if the internal state really changed!
  notify();
}
*/



/*!
  \return    Current cursor.
*/
// const ag::CursorPos &ag::IVisualisation::cursor() const
// {
//   return d_cursor;
// }



/*
ag::IVisualisation *ag::IVisualisation::copy(ag::DataObject * o) const
{
  throw std::logic_error(std::string("copy visualisation is a no-op"));
}
*/



ag::DataObject &ag::IVisualisation::dataObject() const
{
  return *d_data->d_dataObject;
}



/*
bool ag::IVisualisation::mousePressHandled(QObject * o,
                   QMouseEvent * e)
{
  return false;
}



bool ag::IVisualisation::mouseReleaseHandled(QObject *  o ,
                   QMouseEvent *  e )
{
  return false;
}



bool ag::IVisualisation::mouseDoubleClickHandled(QObject *  o ,
                   QMouseEvent *  e )
{
  return false;
}



bool ag::IVisualisation::mouseMoveHandled(QObject *  o ,
                   QMouseEvent *  e )
{
  return false;
}
*/



/*
void ag::IVisualisation::toggleAnimation()
{
  Q_EMIT animate(false);
}
*/



/*
void ag::IVisualisation::animationStarted()
{
  assert(d_data->d_animateAction);

  d_data->d_animateAction->setIconSet(d_data->d_stopAnimIcon);
}



void ag::IVisualisation::animationStopped()
{
  assert(d_data->d_animateAction);

  d_data->d_animateAction->setIconSet(d_data->d_startAnimIcon);
}
*/



/*
void ag::IVisualisation::showAnimationDialog()
{
  Q_EMIT animate(true);
}
*/



/*
void ag::IVisualisation::showDataPropertiesDialog(
                   const ag::DataGuide& dataGuide)
{
  // yepyep: group moet een member var van visualisation zijn.
  // yepyep: de meeste emits kunnen er dan uit?
  // nee: emit signal, visgroup heeft een static dialoog.
  // nee: emit signal, vismanager -> vgl met control center
  // met een dataguide en een dataobject moeten alle eigenschappen van de
  // data uit te vinden zijn -> dataguide klasse niet uitbreiden maar het
  // dataobject props laten leveren.

  // group->showDataPropertiesDialog(dataGuide);
}
*/



void ag::IVisualisation::saveAsPNG(
         boost::filesystem::path const& /* path */)
{
  // If we end up here, a subclass didn't call setSaveAsFileFormats() or did it
  // with the wrong arguments.
  assert(false);
}



void ag::IVisualisation::saveAsEPS(
         boost::filesystem::path const& /* path */)
{
  // If we end up here, a subclass didn't call setSaveAsFileFormats() or did it
  // with the wrong arguments.
  assert(false);
}



void ag::IVisualisation::testDataGuide(
         geo::DataGuide const& dataGuide) const
{
  testDataType(dataGuide.type());
  testValueScale(dataGuide.valueScale());
}



void ag::IVisualisation::testDataType(
         geo::DataType dataType) const
{
  if(!dev::hasElement(d_data->d_supportedDataTypes, dataType)) {
    std::ostringstream stream1;
    stream1 << "Data type " << geo::dataTypeToStr(dataType)
                   << ": Not a valid data type for "
                   << d_data->d_visualisationName
                   << " visualisations";
    com::Exception exception(stream1.str());

    std::ostringstream stream2;
    stream2 << "Valid data types are: "
                   << dev::toString<geo::DataType, detail::DataTypePrinter>(
                   d_data->d_supportedDataTypes.begin(),
                   d_data->d_supportedDataTypes.end(),
                   detail::DataTypePrinter(), std::string(", "));
    exception.append(stream2.str());

    throw exception;
  }
}

void ag::IVisualisation::testValueScale(
         CSF_VS valueScale) const
{
  if(!dev::hasElement(d_data->d_supportedValueScales, valueScale)) {
    std::ostringstream stream1;
    stream1 << "Value scale " << dal::valueScaleToString(valueScale)
                   << ": Not a valid value scale for "
                   << d_data->d_visualisationName
                   << " visualisations";
    com::Exception exception(stream1.str());

    std::ostringstream stream2;
    stream2 << "Valid value scales are: "
                   << dev::toString<CSF_VS, detail::ValueScalePrinter>(
                   d_data->d_supportedValueScales.begin(),
                   d_data->d_supportedValueScales.end(),
                   detail::ValueScalePrinter(), std::string(", "));
    exception.append(stream2.str());

    throw exception;
  }
}


const std::vector<com::FileFormatInfo>& ag::IVisualisation::fileFormats() const
{
  return d_data->d_fileFormats;
}



const std::string& ag::IVisualisation::visualisationName() const
{
  return d_data->d_visualisationName;
}



namespace ag {

VisEngine& IVisualisation::visualisationEngine()
{
  return d_data->d_engine;
}



VisEngine const& IVisualisation::visualisationEngine () const
{
  return d_data->d_engine;
}



//! Returns data space of data visualised.
/*!
  \param     .
  \return    .
  \exception .
  \warning   Returned data space is not the data space of the whole
             visualisation group but of the visualisation only. This might
             be the same space or a subset of the group space.
  \sa        .
*/
dal::DataSpace IVisualisation::visualisationDataSpace() const
{
  return dataObject().dataSpace(visualisationEngine().dataGuides());
}



dal::DataSpace IVisualisation::dataSpace() const
{
  return dataObject().dataSpace();
}



//! Returns the currently set data space address.
/*!
  \return    Data space address.
*/
dal::DataSpaceAddress const& IVisualisation::dataSpaceAddress() const
{
  return dataObject().dataSpaceAddress();
}



// void IVisualisation::setProfileDataSpace(
//          dal::DataSpace const& space)
// {
//   d_data->d_profileDataSpace = space;
// }



// dal::DataSpace const& IVisualisation::profileDataSpace() const
// {
//   return d_data->d_profileDataSpace;
// }

} // namespace ag



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


