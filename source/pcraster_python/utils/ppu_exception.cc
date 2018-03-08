#include "ppu_exception.h"

#include "com_strlib.h"
#include <sstream>


pcraster::python::PyUtilsException::PyUtilsException(
     const std::string& message):
  d_message(message)
{
  com::removeFrontEndSpace(d_message);
  d_message+='\n';
  append(d_message.c_str());
}

pcraster::python::PyUtilsException::~PyUtilsException()
{
}

const std::string& pcraster::python::PyUtilsException::message() const
{
  return d_message;
}
