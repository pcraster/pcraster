#ifndef INCLUDED_COM_STREAMWRITER
#define INCLUDED_COM_STREAMWRITER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.

// Module headers.



namespace com {
  // StreamWriter declarations.
}



namespace com {



//! Base class for stream writer classes.
/*!
  This class layers a reference to an std::ostream.
*/
class StreamWriter
{

  friend class StreamWriterTest;

private:

  std::ostream&   d_stream;

  //! Number of characters written on the stream.
  size_t          d_nrCharactersWritten;

  //! Assignment operator. NOT IMPLEMENTED.
  StreamWriter&    operator=           (StreamWriter const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   StreamWriter        (StreamWriter const& rhs);

protected:

  size_t           nrCharactersWritten () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   StreamWriter        (std::ostream& stream);

  /* virtual */    ~StreamWriter       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  StreamWriter&    operator<<          (char character);

  StreamWriter&    operator<<          (const std::string& characters);

  void             flush               ();

  void             clear               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  std::ostream&    stream              ();

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



} // namespace com

#endif
