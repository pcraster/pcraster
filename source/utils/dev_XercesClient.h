#ifndef INCLUDED_DEV_XERCESCLIENT
#define INCLUDED_DEV_XERCESCLIENT

// External headers.
#ifndef INCLUDED_XERCESC_UTIL_TRANSSERVICE
#include <xercesc/util/TransService.hpp>
#define INCLUDED_XERCESC_UTIL_TRANSSERVICE
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.

// Module headers.



namespace dev {
  // XercesClient declarations.
}



namespace dev {

//! Utility class for clients that use the Xerces library.
/*!
  Creating a XercesClient object will initialize the Xerces library. Once
  the object goes out of scope the matching terminate will be called.

  You can instantiate more than one XercesClient object. It is safe to use
  this class when the Xerces library is already initialized.
*/
class XercesClient: private boost::noncopyable
{

  friend class XercesClientTest;

private:

  //! Number of times Xerces is initialized without being terminated.
  static unsigned short _count;

  static xercesc::XMLTranscoder* _utf8Transcoder;

  //! Whether initialization of the Xerces library succeeded.
  bool             _initialized;

protected:

                   XercesClient        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~XercesClient       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  static xercesc::XMLTranscoder& utf8Transcoder();

  bool             isInitialized       () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dev

#endif
