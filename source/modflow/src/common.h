#ifndef INCLUDED_MODFLOW_COMMON
#define INCLUDED_MODFLOW_COMMON

#include "stddefx.h"
#include "discr_blockdata.h"

#include <string>
#include <sstream>
#include <vector>



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

#endif // INCLUDED_MODFLOW_COMMON
