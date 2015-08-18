#ifndef INCLUDED_COM_DEBUGSTREAM
#define INCLUDED_COM_DEBUGSTREAM



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif


namespace com {
  class DebugStream;
}

extern com::DebugStream dbs;

namespace com {


/*!
  \class DebugStream
  \brief The DebugStream class is a stream class for writing debuggging
         messages to.

  The DebugStream class is designed to be used during development for
  writing debugging messages. It inherits functionality from std::ofstream.

  An object of type DebugStream is declared in this header and has to be
  defined somewhere in the users code (eg. in the module which contains the
  main-function):

  \code
  #include "com_debugstream.h"

  com::DebugStream dbs("debug.out");

  int main(int argc, char **argv)
  {
    dbs << "starting run" << endl;
    ...
    ...

    dbs << "ending run" << endl;

    return 0;
  }
  \endcode

  Modules who want to use the functionality of DebugStream have to include
  this header and can use the global variable named dbs for writing debugging
  messages to.

  KDJ: The App class defines one now.
*/
class DebugStream: public std::ofstream
{

private:

  //! Offset.
  size_t           d_offset;

  //! Copy constructor. NOT IMPLEMENTED.
                   DebugStream         (const DebugStream &s);

  //! Assignment operator. NOT IMPLEMENTED.
  DebugStream &    operator=           (const DebugStream &s);


  std::ofstream &dbsStream() {
   // cast needed for MSC
   return static_cast<std::ofstream &>(*this);
  }

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /*! Constructor.
  */
                   DebugStream         ()
  : std::ofstream(),
    d_offset(0)
  {
    POSTCOND(!is_open());
  }

  /*! Constructor.
    Constructor takes a filename of the file to write messages to.
  */
                   DebugStream         (const std::string& fileName)
  : std::ofstream(fileName.c_str()),
    d_offset(0)
  {
    POSTCOND(is_open());
  }

  /*!
    Destructor.
  */
                   ~DebugStream        () {
    if(is_open()) {
      close();
    }
  }

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! \sa IN_MEMBER_FUNCTION() in stddefx.h
  void             inMemberFunction    (const std::string& functionOrSrcFile,
                                        size_t lineNo = 0)
  {
    if (is_open()) {
      dbsStream() << std::string(d_offset, ' ') << functionOrSrcFile;
      if (lineNo) { // functionOrSrcFile is (src) __FILE__
        dbsStream() << ":" << lineNo;
      }
      dbsStream() << " {" << std::endl;
      d_offset += 2;
    }
  }

  //! \sa OUT_MEMBER_FUNCTION() in stddefx.h
  void             outMemberFunction   ()
  {
    if (is_open()) {
      d_offset -= 2;
      dbsStream() << std::string(d_offset, ' ') << '}' << std::endl;
    }
  }

  void             inBlock             (const std::string& description,
                                        size_t lineNo = 0)
  {
    if (is_open()) {
      dbsStream() << std::string(d_offset, ' ') << description;
      if (lineNo) { // functionOrSrcFile is (src) __FILE__
        dbsStream() << ":" << lineNo;
      }
      dbsStream() << " {" << std::endl;
      d_offset += 2;
    }
  }

  void             outBlock            ()
  {
    if (is_open()) {
      d_offset -= 2;
      dbsStream() << std::string(d_offset, ' ') << '}' << std::endl;
    }
  }

  void             message             (std::string const& msg)
  {
    if (is_open()) {
      dbsStream() << std::string(d_offset, ' ') << msg << std::endl;
    }
  }

};



} // namespace com



#endif

