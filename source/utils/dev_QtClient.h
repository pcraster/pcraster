#ifndef INCLUDED_DEV_QTCLIENT
#define INCLUDED_DEV_QTCLIENT

// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.

// Module headers.



namespace dev {
  // QtClient declarations.
}



namespace dev {

//! Instantiates a Qt application object.
/*!
  \warning   You should only instantiate one QtClient object.
  \sa        .

  You can instantiate this class with the Qt application classes:
  - QCoreApplication
  - QApplication

  See their respective docs for more info.
*/
template<class Application>
class QtClient: private boost::noncopyable
{

  friend class QtClientTest;

private:

  static bool      _initialized;

  //! The Qt application object.
  Application*     _application;

protected:

                   QtClient            (int& argc,
                                        char** argv);

  virtual          ~QtClient           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isInitialized       () const;

  Application&     application         ();

};

template<class Application>
bool QtClient<Application>::_initialized = false;



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class Application>
QtClient<Application>::QtClient(
         int& argc,
         char** argv)

  : _application(0)

{
  assert(!_initialized); // Don't create more than one instance!!!
  _application = new Application(argc, argv);
  _initialized = true;
}



template<class Application>
QtClient<Application>::~QtClient()
{
  assert(_initialized);
  assert(_application);

  delete _application;

  _application = 0;
  _initialized = false;
}



template<class Application>
bool QtClient<Application>::isInitialized() const
{
  assert((_application != 0) == _initialized);
  return _application != 0;
}



template<class Application>
Application& QtClient<Application>::application()
{
  assert(isInitialized());

  return *_application;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dev

#endif
