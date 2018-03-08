#pragma once

#include "com_exception.h"
#include "pcraster_python_utils_export.h"

#include <string>


namespace pcraster {
namespace python {


class PCRASTER_PYTHON_UTILS_EXPORT PyUtilsException : public com::Exception
{

private:

  std::string      d_message;

public:

  PyUtilsException                     ()=delete;

  PyUtilsException                     (const std::string& message);

  /* virtual */  ~PyUtilsException     ();

  const std::string& message           () const;
};


} // namespace
} // namespace
