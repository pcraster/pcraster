/*!
\note
Do not edit, generated from devbin/calcLibWrap
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif
#ifndef INCLUDED_CALC_OBJECTLINKMETA
#include "calc_objectlinkmeta.h"
#define INCLUDED_CALC_OBJECTLINKMETA
#endif
#ifndef INCLUDED_CALC_OBJECTLINKPROXY
#include "calc_objectlinkproxy.h"
#define INCLUDED_CALC_OBJECTLINKPROXY
#endif
#ifndef INCLUDED_MLDD_MLDD
#include "mldd_mldd.h"
#define INCLUDED_MLDD_MLDD
#endif
namespace calc {

            template <>
            bool ObjectLinkProxy<mldd::Mldd>::dispatch(
                   const std::string&  methodName,
                   const std::vector<Field *>& fields)
            {
             

       if(methodName == "setDem") {
         const REAL4 *  a0;

         tieProxyArgument(a0,fields.at(0));

         d_obj->setDem(a0
            );
            return true;
            }

       if(methodName == "getDem") {
         REAL4 * a0;

         tieProxyArgument(a0,fields.at(0));

         d_obj->getDem(a0
            );
            return true;
            }

       if(methodName == "addStream") {
         const UINT1 *  a0;

         tieProxyArgument(a0,fields.at(0));

         d_obj->addStream(a0
            );
            return true;
            }

       if(methodName == "setStream") {
         std::vector<const UINT1 * > a0;

         tieProxyArgument(a0,fields.at(0));
tieProxyArgument(a0,fields.at(1));
tieProxyArgument(a0,fields.at(2));
tieProxyArgument(a0,fields.at(3));
tieProxyArgument(a0,fields.at(4));
tieProxyArgument(a0,fields.at(5));
tieProxyArgument(a0,fields.at(6));
tieProxyArgument(a0,fields.at(7));

         d_obj->setStream(a0
            );
            return true;
            }

       if(methodName == "removeStream") {
         std::vector<const UINT1 * > a0;

         tieProxyArgument(a0,fields.at(0));
tieProxyArgument(a0,fields.at(1));
tieProxyArgument(a0,fields.at(2));
tieProxyArgument(a0,fields.at(3));
tieProxyArgument(a0,fields.at(4));
tieProxyArgument(a0,fields.at(5));
tieProxyArgument(a0,fields.at(6));
tieProxyArgument(a0,fields.at(7));

         d_obj->removeStream(a0
            );
            return true;
            }

       if(methodName == "getStream") {
         std::vector<UINT1 *> a0;

         tieProxyArgument(a0,fields.at(0));
tieProxyArgument(a0,fields.at(1));
tieProxyArgument(a0,fields.at(2));
tieProxyArgument(a0,fields.at(3));
tieProxyArgument(a0,fields.at(4));
tieProxyArgument(a0,fields.at(5));
tieProxyArgument(a0,fields.at(6));
tieProxyArgument(a0,fields.at(7));

         d_obj->getStream(a0
            );
            return true;
            }

       if(methodName == "getWeight") {
         std::vector<REAL4 *> a0;

         tieProxyArgument(a0,fields.at(0));
tieProxyArgument(a0,fields.at(1));
tieProxyArgument(a0,fields.at(2));
tieProxyArgument(a0,fields.at(3));
tieProxyArgument(a0,fields.at(4));
tieProxyArgument(a0,fields.at(5));
tieProxyArgument(a0,fields.at(6));
tieProxyArgument(a0,fields.at(7));

         d_obj->getWeight(a0
            );
            return true;
            }

       if(methodName == "upstream") {
         REAL4 * a0;
const REAL4 *  a1;

         tieProxyArgument(a0,fields.at(0));
tieProxyArgument(a1,fields.at(1));

         d_obj->upstream(a0,a1
            );
            return true;
            }

       if(methodName == "accuflux") {
         REAL4 * a0;
const REAL4 *  a1;

         tieProxyArgument(a0,fields.at(0));
tieProxyArgument(a1,fields.at(1));

         d_obj->accuflux(a0,a1
            );
            return true;
            }

       if(methodName == "diffuse") {
         REAL4 * a0;
const REAL4 *  a1;
const REAL4 *  a2;
const REAL4 *  a3;
std::vector<const REAL4 * > a4;
INT4 a12;

         tieProxyArgument(a0,fields.at(0));
tieProxyArgument(a1,fields.at(1));
tieProxyArgument(a2,fields.at(2));
tieProxyArgument(a3,fields.at(3));
tieProxyArgument(a4,fields.at(4));
tieProxyArgument(a4,fields.at(5));
tieProxyArgument(a4,fields.at(6));
tieProxyArgument(a4,fields.at(7));
tieProxyArgument(a4,fields.at(8));
tieProxyArgument(a4,fields.at(9));
tieProxyArgument(a4,fields.at(10));
tieProxyArgument(a4,fields.at(11));
tieProxyArgument(a12,fields.at(12));

         d_obj->diffuse(a0,a1,a2,a3,a4,a12
            );
            return true;
            }

              return false;
            }

            extern "C" PCR_DLL_C ObjectLinkMeta getMeta() {
              ObjectLinkMeta olm("mldd",
              ObjectLinkProxy<mldd::Mldd>::create);
            
olm.add("setDem");
olm.pushBack("setDem",0,VS_S,ST_SPATIAL);
olm.add("getDem");
olm.pushBack("getDem",1,VS_S,ST_SPATIAL);
olm.add("addStream");
olm.pushBack("addStream",0,VS_L,ST_SPATIAL);
olm.add("setStream");
olm.pushBack("setStream",0,VS_L,ST_SPATIAL);
olm.pushBack("setStream",0,VS_L,ST_SPATIAL);
olm.pushBack("setStream",0,VS_L,ST_SPATIAL);
olm.pushBack("setStream",0,VS_L,ST_SPATIAL);
olm.pushBack("setStream",0,VS_L,ST_SPATIAL);
olm.pushBack("setStream",0,VS_L,ST_SPATIAL);
olm.pushBack("setStream",0,VS_L,ST_SPATIAL);
olm.pushBack("setStream",0,VS_L,ST_SPATIAL);
olm.add("removeStream");
olm.pushBack("removeStream",0,VS_B,ST_SPATIAL);
olm.pushBack("removeStream",0,VS_B,ST_SPATIAL);
olm.pushBack("removeStream",0,VS_B,ST_SPATIAL);
olm.pushBack("removeStream",0,VS_B,ST_SPATIAL);
olm.pushBack("removeStream",0,VS_B,ST_SPATIAL);
olm.pushBack("removeStream",0,VS_B,ST_SPATIAL);
olm.pushBack("removeStream",0,VS_B,ST_SPATIAL);
olm.pushBack("removeStream",0,VS_B,ST_SPATIAL);
olm.add("getStream");
olm.pushBack("getStream",1,VS_L,ST_SPATIAL);
olm.pushBack("getStream",1,VS_L,ST_SPATIAL);
olm.pushBack("getStream",1,VS_L,ST_SPATIAL);
olm.pushBack("getStream",1,VS_L,ST_SPATIAL);
olm.pushBack("getStream",1,VS_L,ST_SPATIAL);
olm.pushBack("getStream",1,VS_L,ST_SPATIAL);
olm.pushBack("getStream",1,VS_L,ST_SPATIAL);
olm.pushBack("getStream",1,VS_L,ST_SPATIAL);
olm.add("getWeight");
olm.pushBack("getWeight",1,VS_S,ST_SPATIAL);
olm.pushBack("getWeight",1,VS_S,ST_SPATIAL);
olm.pushBack("getWeight",1,VS_S,ST_SPATIAL);
olm.pushBack("getWeight",1,VS_S,ST_SPATIAL);
olm.pushBack("getWeight",1,VS_S,ST_SPATIAL);
olm.pushBack("getWeight",1,VS_S,ST_SPATIAL);
olm.pushBack("getWeight",1,VS_S,ST_SPATIAL);
olm.pushBack("getWeight",1,VS_S,ST_SPATIAL);
olm.add("upstream");
olm.pushBack("upstream",1,VS_S,ST_SPATIAL);
olm.pushBack("upstream",0,VS_S,ST_SPATIAL);
olm.add("accuflux");
olm.pushBack("accuflux",1,VS_S,ST_SPATIAL);
olm.pushBack("accuflux",0,VS_S,ST_SPATIAL);
olm.add("diffuse");
olm.pushBack("diffuse",1,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_S,ST_SPATIAL);
olm.pushBack("diffuse",0,VS_N,ST_NONSPATIAL);

              return olm;
            }
} // eo namespace calc
