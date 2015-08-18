#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_INSOCKADDR
#include "com_insockaddr.h"
#define INCLUDED_COM_INSOCKADDR
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_CERRNO
#include <cerrno>
#define INCLUDED_CERRNO
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifdef BORLANDC
  #ifndef INCLUDED_IO
  #include <io.h>
  #define INCLUDED_IO
  #endif

  #ifndef INCLUDED_WINSOCK2
  #include <winsock2.h>
  #define INCLUDED_WINSOCK2
  #endif
#else
  #ifndef INCLUDED_NETINET_IN
  #include <netinet/in.h>
  #define INCLUDED_NETINET_IN
  #endif

  #ifndef INCLUDED_ARPA_INET
  #include <arpa/inet.h>
  #define INCLUDED_ARPA_INET
  #endif

  #ifndef INCLUDED_NETDB
  #include <netdb.h>
  #define INCLUDED_NETDB
  #endif

  #ifndef INCLUDED_SYS_SOCKET
  #include <sys/socket.h>
  #define INCLUDED_SYS_SOCKET
  #endif

  #ifndef INCLUDED_UNISTD
  #include <unistd.h>
  #define INCLUDED_UNISTD
  #endif
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif



// Host should be an url, not an ip-adress (not yet).
// This constructor fills the IP address type/family, port number and IP
// address of the remote host.port where we want to connect to find a service.
com_InSockAddr::com_InSockAddr(const std::string &host,
                               const std::string &service)
{
  struct servent *sp;
  struct hostent *hp;

  // Translate a service name into the port number.
  if((sp = getservbyname(service.c_str(), "tcp")) == NULL)
  {
    std::string msg = com::createMessage(
        "unable to provide service information for service %s and tcp protocol",
        service.c_str());
    throw com::Exception(msg);
  }

  // Translate host name into an IP number.
  if((hp = gethostbyname(host.c_str())) == NULL)
  {
    std::string msg = com::createMessage(
                              "unable to translate host name %s into IP number",
                              host.c_str());
    throw com::Exception(msg);
  }

  ::memset((char *)&d_addr, 0, sizeof(d_addr));

  // Fill IP address type. This defines the interpretation of the data.
  d_addr.sin_family = AF_INET;

  // Fill host number.
  ::memcpy((char *)&d_addr.sin_addr, hp->h_addr, hp->h_length);

  // Fill service port.
  d_addr.sin_port = sp->s_port;
}



// This constructor fills the IP address type/family, port number and IP
// address of the remote host.port where we want to connect to find a service.
com_InSockAddr::com_InSockAddr(const std::string &host, short int port)
{
  struct hostent *hp;

  // Translate host name into an IP number.
  if((hp = gethostbyname(host.c_str())) == NULL)
  {
    std::string msg = com::createMessage(
                              "unable to translate host name %s into IP number",
                              host.c_str());
    throw com::Exception(msg);
  }

  ::memset((char *)&d_addr, 0, sizeof(d_addr));

  // Fill IP address type. This defines the interpretation of the data.
  d_addr.sin_family = AF_INET;

  // Fill host number.
  ::memcpy((char *)&d_addr.sin_addr, hp->h_addr, hp->h_length);

  // Fill service port.
  d_addr.sin_port = htons(port);
}



com_InSockAddr::~com_InSockAddr()
{
}



short com_InSockAddr::getFamily() const
{
  // sin_family must be in Host Byte Order.
  return d_addr.sin_family;
}



unsigned short com_InSockAddr::getPort() const
{
  // sin_port must be in Network Byte Order.
  return d_addr.sin_port;
}



const struct in_addr &com_InSockAddr::getHostNumber() const
{
  // sin_addr must be in Network Byte Order.
  return d_addr.sin_addr;
}



const struct sockaddr_in &com_InSockAddr::getAddress() const
{
  return d_addr;
}



std::string com_InSockAddr::getIPAddress() const
{
  return inet_ntoa(getHostNumber());
}



std::string com_InSockAddr::getIPAddress(const struct in_addr &address)
{
  return inet_ntoa(address);
}



std::string com_InSockAddr::getHostName()
{
  char buffer[1024];

#ifdef BORLANDC
  if(::gethostname(buffer, 1024) != 0)
  {
    throw com::Exception("unable to retrieve hostname");
  }
#else
  if(::gethostname(buffer, 1024) < 0)
  {
    throw com::Exception(::strerror(errno));
  }
#endif

  // yepyep: following line won't compile?!
  //std::cout << "Display is serving on: " << buffer << endl;

  return static_cast<std::string>(buffer);
}

