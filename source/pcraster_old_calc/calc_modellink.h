#ifndef INCLUDED_CALC_MODELLINK
#define INCLUDED_CALC_MODELLINK

/*!
 * \file
 *  interface used for external modellinks
 */

#include "pcrcalcd.h"
#include <string>
#include <vector>

#include "csftypes.h"

namespace geo {
 class RasterSpace;
}

namespace calc {

class ModelLinkException {
  std::string d_msg;
public:
  ModelLinkException(const std::string& msg):
    d_msg(msg) {};
  const std::string& what() const { return d_msg; }; 
};

typedef struct ModelLinkArgSpec {
    //! all accepted vs's
    PCR_VS vs;
    //! spatial type
    PCR_ST st;
    //! implementing MAP_'cr'
    PCR_CR cr;
    //! opaque map handle
    void *value;
} ModelLinkArgSpec;

class ModelLinkMethodSignature {
public:
  bool        d_strArgGiven;
  std::string d_strArg;
  std::vector<ModelLinkArgSpec> d_input;
  std::vector<ModelLinkArgSpec> d_result;
  ModelLinkMethodSignature(
    const std::string& strArg,
    size_t nrInArg):
      d_strArgGiven(strArg != ""),
      d_strArg(strArg),
      d_input(nrInArg)
      {
      }
};


class ModelLink {
public:

  ModelLink() {};

  virtual ~ModelLink() {};

  //! return 0 if not found
  static const ModelLink* findModelLink(
      const std::string& name);

  //! name of model (e.g. ModelName)
  virtual const std::string& name() const=0;

  //! called when X = modelName(...) is parsed
  /*! in: strArg*,input:(vs,st)
   *    --> rewrite input:(vs,st,cr)
   */
  virtual void initCheck(ModelLinkMethodSignature& s) const=0;

  //! called when X = Modelname(...) is executed
  /*!
   *  when this is called everything is known such as the
   *   raster space dimension and the number of timesteps of
   *   the (caller) model. FTTB pass RasterSpace, later more
   *   info.
   */
  virtual void initExecute(
              const geo::RasterSpace& d_rasterSpace,
              ModelLinkMethodSignature& s)=0;


  //! check args and pass back the resulting arguments
  /*!
      called when .. = modelName::methodName(...) is parsed
      \returns true if methodName exists, false if not
      <br>
      other errors are thrown by a ModelLinkException
      in: strArg*,input:(vs,st)
                --> rewrite input:(vs,st,cr)
                --> result (vs,st,cr) note cr can have value PCR_ST_BOTH
   */
  virtual bool methodCheck(
    const std::string& methodName,
    ModelLinkMethodSignature& s) const =0;
  /*!
      called when .. = modelName::methodName(...) is executed
   */
  virtual void methodExecute(
    const std::string& methodName,
    ModelLinkMethodSignature& s)=0;
};


#define PCR_NR_EXT_MODELLINKS 64

typedef ModelLink* (*PCR_EXTERNAL_MODELLINK_CREATOR)();

typedef struct PCR_EXTERNAL_MODELLINK_SYNOPSIS {
  char name[128];
  PCR_EXTERNAL_MODELLINK_CREATOR creator;
} PCR_EXTERNAL_MODELLINK_SYNOPSIS;

typedef struct PCR_EXTERNAL_MODELLINK_LIST {
  int apiVersionNr;
  int nrModelLinks;
  PCR_EXTERNAL_MODELLINK_SYNOPSIS modelLinkSynopsisList[PCR_NR_EXT_MODELLINKS];
} PCR_EXTERNAL_MODELLINK_LIST;

}
#endif
