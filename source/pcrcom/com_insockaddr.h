#ifndef INCLUDED_COM_INSOCKADDR
#define INCLUDED_COM_INSOCKADDR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifdef BORLANDC
  #ifndef INCLUDED_WINSOCK2
  #include <winsock2.h>
  #define INCLUDED_WINSOCK2
  #endif
#else
  #ifndef INCLUDED_IN
  #include <netinet/in.h>
  #define INCLUDED_IN
  #endif
#endif



struct in_addr;



//namespace pack {



/*!
  \class com_InSockAddr
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
class com_InSockAddr
{

private:

  //! The structure for Internet Socket Address (address family AF_INET).
  struct sockaddr_in d_addr;

  //! Assignment operator. NOT IMPLEMENTED.
  com_InSockAddr & operator=           (const com_InSockAddr &addr);

  //! Copy constructor. NOT IMPLEMENTED.
                   com_InSockAddr      (const com_InSockAddr &addr);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   com_InSockAddr      (const std::string &host,
                                        const std::string &service);

  //! Constructor.
                   com_InSockAddr      (const std::string &host,
                                        short int          port);

  //! Destructor.
                   ~com_InSockAddr     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the IP address family.
  short            getFamily           () const;

  //! Returns the service port.
  unsigned short   getPort             () const;

  //! Returns the host number (internet address).
  const struct in_addr &getHostNumber  () const;

  //! Returns the address.
  const struct sockaddr_in &getAddress () const;

  //! Returns the IP address as a string.
  std::string      getIPAddress        () const;

  //! Returns the IP address of the address \a address as a string.
  static std::string getIPAddress      (const struct in_addr &address);

  //! Returns the hostname of the local host.
  static std::string getHostName       ();

};



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------



//} // namespace pack

#endif
