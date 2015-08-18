#ifndef INCLUDED_QT_EVENTFILTERSUBJECT
#define INCLUDED_QT_EVENTFILTERSUBJECT



// Library headers.

// PCRaster library headers.

// Module headers.



class QObject;
namespace qt {
  // EventFilterSubject declarations.
}



namespace qt {



//! The EventFilterSubject class is for objects which are filtered by others.
/*!
  The events of objects can be filtered by other objects (the event filter
  objects). This means that events generated for this object are first seen
  by the filter object who can do whatever it wants with it.

  A filter object can retrieve the events of a filter subject by calling
  redirectEventsTo(QObject*). This function will call
  redirectChildEventsTo(QObject*) which can be overridden. This way
  you can redirect the events of the children to the filter also. When
  events should no longer be filtered you should call
  removeEventFilter(QObject*), which will call removeChildEventFilter(QObject*).

  \sa  QObject::installEventFilter(const QObject*), QObject::eventFilter()
*/
class EventFilterSubject
{

private:

  // Actual, layered filter subject.
  QObject* d_filterSubject;

  //! Object which filters the events of this subject.
  QObject* d_filter;

  //! Assignment operator. NOT IMPLEMENTED.
  EventFilterSubject& operator=        (const EventFilterSubject&);

  //! Copy constructor. NOT IMPLEMENTED.
                   EventFilterSubject  (const EventFilterSubject&);

protected:

  QObject*         eventFilterSubject  () const;

  const QObject*   eventFilter         () const;

  virtual void     redirectChildEventsTo(QObject* filter);

  virtual void     removeChildEventFilter(QObject* filter);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   EventFilterSubject  (QObject* filterSubject);

  virtual          ~EventFilterSubject ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             redirectEventsTo    (QObject* filter);

  void             removeEventFilter   ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
