#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_STREAMWRITER
#include "com_streamwriter.h"
#define INCLUDED_COM_STREAMWRITER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the StreamUser class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class StreamWriterPrivate
{
public:

  StreamWriterPrivate()
  {
  }

  ~StreamWriterPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC STREAMUSER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STREAMUSER MEMBERS
//------------------------------------------------------------------------------

com::StreamWriter::StreamWriter(std::ostream& stream)

  : d_stream(stream), d_nrCharactersWritten(0)

{
}



com::StreamWriter::~StreamWriter()
{
}



std::ostream& com::StreamWriter::stream()
{
  return d_stream;
}



com::StreamWriter& com::StreamWriter::operator<<(char character)
{
  d_stream << character;
  ++d_nrCharactersWritten;
  return *this;
}



com::StreamWriter& com::StreamWriter::operator<<(const std::string& characters)
{
  d_stream << characters;
  d_nrCharactersWritten += characters.size();
  return *this;
}



void com::StreamWriter::flush()
{
  d_stream << std::flush;
}



void com::StreamWriter::clear()
{
  for(size_t i = 0; i < d_nrCharactersWritten; ++i) {
    d_stream << '\b';
  }

  d_nrCharactersWritten = 0;
}



size_t com::StreamWriter::nrCharactersWritten() const
{
  return d_nrCharactersWritten;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



