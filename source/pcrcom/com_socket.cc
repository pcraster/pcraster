#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_SOCKET
#include "com_socket.h"
#define INCLUDED_COM_SOCKET
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_ERRNO
#include <cerrno>
#define INCLUDED_ERRNO
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
  #ifndef INCLUDED_FCNTL
  #include <fcntl.h>
  #define INCLUDED_FCNTL
  #endif

  #ifndef INCLUDED_NETINET_IN
  #include <netinet/in.h>
  #define INCLUDED_NETINET_IN
  #endif

  #ifndef INCLUDED_NETDB
  #include <netdb.h>
  #define INCLUDED_NETDB
  #endif

  #ifndef INCLUDED_SYS_SOCKET
  #include <sys/socket.h>
  #define INCLUDED_SYS_SOCKET
  #endif

  #ifndef INCLUDED_SYS_TYPES
  #include <sys/types.h>
  #define INCLUDED_SYS_TYPES
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

#ifndef INCLUDED_COM_INSOCKADDR
#include "com_insockaddr.h"
#define INCLUDED_COM_INSOCKADDR
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif



/*!
  \param proto Name of the protocol to be used with the socket. Protocol "tcp"
               will be convenient in most cases.

  After a com_Socket object is created, one can use it for different purposes.
  One example is to use the socket as a server. A socket will listen for
  incoming calls if it is bound to a port (bind()), and listens to it
  (listen()). If a clients connects to the port the server socket has to
  accept() the connection before communication can take place.
*/
com_Socket::com_Socket(const std::string &proto)

#ifdef BORLANDC
  : d_fd(INVALID_SOCKET)
#else
  : d_fd(-1)
#endif

{
  struct protoent *p;

  if((p = ::getprotobyname(proto.c_str())) == NULL)
  {
    std::string msg = com::createMessage("unknown prototype: %s",
                                                proto.c_str());
    throw com::Exception(msg);
  }

#ifdef BORLANDC
  if((d_fd = ::socket(AF_INET, SOCK_STREAM, p->p_proto)) == INVALID_SOCKET)
  {
    throw com::Exception("unable to create socket");
  }
#else
  if((d_fd = ::socket(AF_INET, SOCK_STREAM, p->p_proto)) < 0)
  {
    throw com::Exception(::strerror(errno));
  }
#endif

#ifdef BORLANDC
  unsigned long param = 1;
  if(::ioctlsocket(d_fd, FIONBIO, &param) != 0)
  {
    throw com::Exception("error while changing socket to non-blocking");
  }
#else
  if(::fcntl(d_fd, F_SETFL, O_NONBLOCK) < 0)
  {
    throw com::Exception(::strerror(errno));
  }
#endif
}



com_Socket::~com_Socket()
{
#ifdef BORLANDC
  if(d_fd != INVALID_SOCKET) close(d_fd);
#else
  if(d_fd >= 0) close(d_fd);
#endif
}



/*!
  \param   addr Binds the socket to the address \a addr.

  A socket is created without a name. Until a name is bound to a socket,
  processes have no way to reference it and, consequently, no messages may be
  received on it.
*/
void com_Socket::bind(const com_InSockAddr &addr)
{
#ifdef BORLANDC
  PRECOND(d_fd != INVALID_SOCKET);
#else
  PRECOND(d_fd >= 0);
#endif

#ifdef BORLANDC
  if(::bind(d_fd, (struct sockaddr *)&addr.getAddress(),
          sizeof(addr.getAddress())) == SOCKET_ERROR)
  {
    throw com::Exception("unable to bind socket to internet address");
  }
#else
  if(::bind(d_fd, (struct sockaddr *)&addr.getAddress(),
          sizeof(addr.getAddress())) < 0)
  {
    throw com::Exception(::strerror(errno));
  }
#endif
}



/*!
  \param   n The maximum number of outstanding connections which may be queued
             awaiting acceptance by the server process; this number may be
             limited by the system.
  \warning If the socket isn't bound to a certain socket with bind() it will
           start listening to a random port which probably isn't very useful.

  Should a connection be requested while the queue is full, the connection will
  not be refused, but rather the individual messages which comprise the request
  will be ignored.
*/
void com_Socket::listen(size_t n)
{
  PRECOND(d_fd >= 0);

#ifdef BORLANDC
  if(::listen(d_fd, n) == SOCKET_ERROR)
  {
    throw com::Exception("unable to listen to socket connection");
  }
#else
  if(::listen(d_fd, n) < 0)
  {
    throw com::Exception(::strerror(errno));
  }
#endif
}



/*!
  \return  The file descriptor associated with the socket.
*/
int com_Socket::getFileDescr() const
{
  return d_fd;
}



/*!
  \param   m If \a m is 0, further receives will be disallowed. If \a m is 1,
             further sends will be disallowed. If \a m is 2, further sends and
             receives will be disallowed.
  \sa      close()
*/
void com_Socket::shutdown(int m)
{
  // yepyep; functie aanpassen voor ms shutdown (andere opties).
  if(::shutdown(d_fd, m) < 0)
  {
    throw com::Exception(::strerror(errno));
  }
}



/*!
  \sa      shutdown()

  If data is associated with a socket which promises reliable delivery (stream
  socket) when a close() takes place, the system will continue to attempt to
  transfer the data. However, after a fairly long period of time, if the data
  is still undelivered, it will be discarded. Should the user have no use for
  any pending data, it may perform a shutdown() on the socket prior to closing
  it.
*/
void com_Socket::close()
{
  close(d_fd);

  // Mark file as closed.
#ifdef BORLANDC
  d_fd = INVALID_SOCKET;
#else
  d_fd = -1;
#endif
}



/*!
  \param   socket The socket to accept a connection from.
  \return  A non-negative integer that is a descriptor for the accepted socket.
*/
int com_Socket::accept(int socket)
{
  int                fd;
  struct sockaddr_in theirAddress;
#ifdef BORLANDC
  int                size = static_cast<int>(sizeof(theirAddress));
#else
#ifdef ALPHA_OSF
  int                size = static_cast<int>(sizeof(theirAddress));
#else
  size_t             size = sizeof(theirAddress);
#endif
#endif

#ifdef BORLANDC
  if((fd = ::accept(socket, (struct sockaddr *)&theirAddress, &size))
                                                              == INVALID_SOCKET)
  {
    throw com::Exception("unable to accept connection");
  }
#else
  if((fd = ::accept(socket, (struct sockaddr *)&theirAddress, &size)) < 0)
  {
    throw com::Exception(::strerror(errno));
  }
#endif

  return fd;
}



/*!
  \param   socket The socket to close.
*/
void com_Socket::close(int socket)
{
#ifdef BORLANDC
  if(::closesocket(socket) == SOCKET_ERROR)
  {
    throw com::Exception("unable to close socket");
  }
#else
  if(::close(socket) < 0)
  {
    throw com::Exception(::strerror(errno));
  }
#endif
}

