#ifndef INCLUDED_COMMON
#define INCLUDED_COMMON

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifdef DEBUG_DEVELOP_LIN
#include <sys/time.h>
#endif

// PCRaster library headers.
#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

// Module headers.


class PCRModflow;

class Common{
 protected:
 private:
  PCRModflow *d_mf;
 public:
  Common(PCRModflow *mf);
  ~Common();
  void error(const std::string &msg, const std::string &methodName);
  bool writeToFile(const std::string &filename, const std::string &msg);
  void writeMatrix(std::stringstream &aStream, const std::string &aString, std::vector<int> &l2BlockLayer, const discr::BlockData<REAL4> &bdata, size_t layer);
  void writeMatrix2(std::stringstream &aStream, std::vector<int> &l2BlockLayer, const discr::BlockData<REAL4> &bdata, size_t layer);
  template<typename T>
    void writeMatrix(std::stringstream &aStream, const std::string &aString, const discr::BlockData<T> &bdata, size_t layer);
  template<typename T>
    void setDiscrBlockData(const discr::BlockData<T> &source, discr::BlockData<T> &result);
};

#endif // INCLUDED_COMMON
