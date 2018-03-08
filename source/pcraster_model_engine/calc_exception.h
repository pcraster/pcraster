#pragma once

#include "pcrdll.h"
#include "com_exception.h"

#include <string>



namespace calc {


class PCR_DLL_CLASS Exception : public com::Exception
{

private:

  std::string      d_message;

public:

  Exception                            ()=delete;

  Exception                            (const std::string& message);

  /* virtual */  ~Exception            ();

  const std::string& message           () const;
};


} // namespace
