#ifndef INCLUDED_COM_SOCKET
#define INCLUDED_COM_SOCKET



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif



class com_InSockAddr;



//namespace pack {



/*!
  \class com_Socket
  \brief The com_Socket class encapsulates the 4.4BSD interprocess
         communication facilities.

  Sockets are the endpoints of communication to which a name may be bound. A
  socket has a type and one or more associated processes. Sockets exist within
  communication domains.

  The com_Socket class models sockets with certain properties:

  The type of com_Socket sockets is the stream socket. This type of socket
  provides for the bidirectional, reliable, sequenced, and unduplicated flow
  of data without record boundaries.
 
  Com_Socket objects live in the internet domain (AF_INET).

  To setup a qt-application as a server the folowing code can be used:
  \code
    com_InsockAddr   address("localhost", 8015); // Create internet address.
    com_Socket       socket("tcp");              // Create socket.
    QSocketNotifier *sn;

    socket.bind(address);                      // Bind socket to address.
    socket.listen(5);                          // Listen to up to 5 connections.

    // Create the notifier for reading.
    sn = new QSocketNotifier(socket.getFileDescr(),
                             QSocketNotifier::Read, this);

    // Connect the reading slot to the activated signal.
    QObject::connect(sn, SIGNAL(activated(int)), this, SLOT(dataReceived(int)));
  \endcode
*/
//       1         2         3         4         5         6         7         8
class com_Socket
{

private:

  //! File descriptor associated with the socket.
  int              d_fd;

  //! Assignment operator. NOT IMPLEMENTED.
  com_Socket &     operator=           (const com_Socket &socket);

  //! Copy constructor. NOT IMPLEMENTED.
                   com_Socket          (const com_Socket &socket);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   com_Socket          (const std::string &proto = "tcp");

  //! Destructor.
                   ~com_Socket         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Binds the socket to an address to listen to.
  void             bind                (const com_InSockAddr &addr);

  //! Sets max number of connection requests that will be queued.
  void             listen              (size_t n);

  //! Shut down all or part of a full-duplex connection.
  void             shutdown            (int m);

  //! Closes the socket.
  void             close               ();

  //! Closes socket \a socket.
  static void      close               (int socket);

  //! Accepts a connection with socket \a socket.
  static int       accept              (int socket);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the descriptor associated with the socket.
  int              getFileDescr        () const;

};



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------



//} // namespace pack

#endif
