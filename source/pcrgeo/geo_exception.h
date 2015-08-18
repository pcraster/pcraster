#ifndef INCLUDED_GEO_EXCEPTION
#define INCLUDED_GEO_EXCEPTION


#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

namespace geo {

class NotA_PCRasterMap : public com::FileFormatError
{
public:
       NotA_PCRasterMap(const std::string& fileName);
  virtual ~NotA_PCRasterMap();
};

}

#endif

