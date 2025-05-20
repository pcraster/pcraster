#ifndef INCLUDED_DEV_XERCESCLIENT
#define INCLUDED_DEV_XERCESCLIENT

// External headers.
#ifndef INCLUDED_XERCESC_UTIL_TRANSSERVICE
#include <xercesc/util/TransService.hpp>
#define INCLUDED_XERCESC_UTIL_TRANSSERVICE
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
class XercesClient
{

  friend class XercesClientTest;

private:

  //! Number of times Xerces is initialized without being terminated.
  static unsigned short _count;

  static xercesc::XMLTranscoder* _utf8Transcoder;

  //! Whether initialization of the Xerces library succeeded.
  bool             _initialized{false};

protected:

                   XercesClient        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   XercesClient        (const XercesClient&) = delete;

  XercesClient&    operator=           (const XercesClient&) = delete;

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
