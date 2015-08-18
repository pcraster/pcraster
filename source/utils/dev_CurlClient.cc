#ifndef INCLUDED_DEV_CURLCLIENT
#include "dev_CurlClient.h"
#define INCLUDED_DEV_CURLCLIENT
#endif

// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_CURL_CURL
#include <curl/curl.h>
#define INCLUDED_CURL_CURL
#endif

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the CurlClient class.
*/



namespace dev {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CURLCLIENT MEMBERS
//------------------------------------------------------------------------------

unsigned short CurlClient::d_count = 0;



//------------------------------------------------------------------------------
// DEFINITION OF CURLCLIENT MEMBERS
//------------------------------------------------------------------------------

CurlClient::CurlClient()

  : d_initialized(false)

{
  CURLcode result = curl_global_init(CURL_GLOBAL_ALL);

  if(result == 0) {
    d_initialized = true;
    ++d_count;
  }
}



CurlClient::~CurlClient()
{
  if(d_initialized) {
    assert(d_count > 0);
    curl_global_cleanup();
    --d_count;
  }
}



bool CurlClient::isInitialized() const
{
  return d_initialized;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dev

