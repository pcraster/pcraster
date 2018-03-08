#include "calc_exception.h"

#include "com_strlib.h"
#include <sstream>


calc::Exception::Exception(
     const std::string& message):
  d_message(message)
{
  com::removeFrontEndSpace(d_message);
  d_message+='\n';
  append(d_message.c_str());
}

calc::Exception::~Exception()
{
}

const std::string& calc::Exception::message() const
{
  return d_message;
}
