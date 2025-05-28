#ifndef INCLUDED_CALC_EXCEPTION
#define INCLUDED_CALC_EXCEPTION

#include "pcraster_model_engine_export.h"
#include "com_exception.h"

#include <string>



namespace calc {


class PCR_ME_EXPORT Exception : public com::Exception
{

private:

  std::string      d_message;

public:

  Exception                            ()=delete;

  Exception                            (const std::string& message);

  /* virtual */  ~Exception            () override;

  const std::string& message           () const;
};


} // namespace

#endif