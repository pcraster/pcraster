#include "dev_XercesClient.h"

#include <xercesc/util/PlatformUtils.hpp>



/*!
  \file
  This file contains the implementation of the XercesClient class.
*/



namespace dev {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC XERCESCLIENT MEMBERS
//------------------------------------------------------------------------------

unsigned short XercesClient::_count = 0;

xercesc::XMLTranscoder* XercesClient::_utf8Transcoder = nullptr;



xercesc::XMLTranscoder& XercesClient::utf8Transcoder()
{
  assert(_utf8Transcoder);

  return *_utf8Transcoder;
}



//------------------------------------------------------------------------------
// DEFINITION OF XERCESCLIENT MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  Calls xercesc::XMLPlatformUtils::Initialize().
*/
XercesClient::XercesClient()



{
  try {
    xercesc::XMLPlatformUtils::Initialize();

    if(!_utf8Transcoder) {
      assert(_count == 0);

      xercesc::XMLTransService::Codes failReason;

      _utf8Transcoder =
         xercesc::XMLPlatformUtils::fgTransService->makeNewTranscoderFor(
              xercesc::XMLRecognizer::UTF_8, failReason, 1024);

      assert(_utf8Transcoder);
    }

    _initialized = true;
    ++_count;
  }
  catch(xercesc::XMLException const& /* exception */) {
    // Nothing to do. The user should check isInitialized().
  }
}



//! Destructor.
/*!
  Calls xercesc::XMLPlatformUtils::Terminate().
*/
XercesClient::~XercesClient()
{
  if(_initialized) {
    assert(_count > 0);

    if(_count == 1) {
      // Busy destroying the last client.
      delete _utf8Transcoder;
      _utf8Transcoder = nullptr;
    }

    xercesc::XMLPlatformUtils::Terminate();
    --_count;
  }

  _initialized = _count > 0;
}



//! Returns whether initialization of the Xerces library succeeded.
/*!
  \return    true or false
  \warning   When this function returns false, you cannot use the Xerces API.
*/
bool XercesClient::isInitialized() const
{
  return _initialized;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dev

