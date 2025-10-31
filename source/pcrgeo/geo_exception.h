#ifndef INCLUDED_GEO_EXCEPTION
#define INCLUDED_GEO_EXCEPTION

#include "com_exception.h"



namespace geo {

class NotA_PCRasterMap : public com::FileFormatError
{
public:
       NotA_PCRasterMap(const std::string& fileName);
  ~NotA_PCRasterMap() override;
};

}

#endif

