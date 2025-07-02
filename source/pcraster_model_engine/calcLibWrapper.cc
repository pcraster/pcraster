/*!
\note
Do not edit, generated from devbin/calcLibWrap
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#include "pcraster_model_engine_export.h"

#ifndef INCLUDED_CALC_OBJECTLINKMETA
#include "calc_objectlinkmeta.h"
#define INCLUDED_CALC_OBJECTLINKMETA
#endif
#ifndef INCLUDED_CALC_OBJECTLINKPROXY
#include "calc_objectlinkproxy.h"
#define INCLUDED_CALC_OBJECTLINKPROXY
#endif
#ifndef INCLUDED_CALC_CALCLIBDEMOOBJECTLINK
#include "calc_calclibdemoobjectlink.h"
#define INCLUDED_CALC_CALCLIBDEMOOBJECTLINK
#endif
namespace calc {

            template <>
            bool ObjectLinkProxy<calc::CalcLibDemoObjectLink>::dispatch(
                   const std::string&  methodName,
                   const std::vector<Field *>& fields)
            {


       if(methodName == "setDem") {
         const REAL4 *  a0 = nullptr;

         tieProxyArgument(a0,fields.at(0));

         d_obj->setDem(a0
            );
            return true;
            }

       if(methodName == "testOrder") {
         REAL4 * a0 = nullptr;
std::vector<const REAL4 * > a1;
UINT1 a3 = 0;

         tieProxyArgument(a0,fields.at(0));
tieProxyArgument(a1,fields.at(1));
tieProxyArgument(a1,fields.at(2));
tieProxyArgument(a3,fields.at(3));

         d_obj->testOrder(a0,a1,a3
            );
            return true;
            }

       if(methodName == "testOrder2") {
         std::vector<REAL4 *> a0;
std::vector<const REAL4 * > a2;
UINT1 a4 = 0;

         tieProxyArgument(a0,fields.at(0));
tieProxyArgument(a0,fields.at(1));
tieProxyArgument(a2,fields.at(2));
tieProxyArgument(a2,fields.at(3));
tieProxyArgument(a4,fields.at(4));

         d_obj->testOrder2(a0,a2,a4
            );
            return true;
            }

       if(methodName == "getDem") {
         REAL4 * a0 = nullptr;

         tieProxyArgument(a0,fields.at(0));

         d_obj->getDem(a0
            );
            return true;
            }

       if(methodName == "noArguments") {
         ;

         ;

         d_obj->noArguments(
            );
            return true;
            }

              return false;
            }

            extern "C" PCR_ME_EXPORT ObjectLinkMeta getMeta() {
              ObjectLinkMeta olm("CalcLibDemoObjectLink",
              ObjectLinkProxy<calc::CalcLibDemoObjectLink>::create);

olm.add("setDem");
olm.pushBack("setDem",false,VS_S,ST_SPATIAL);
olm.add("testOrder");
olm.pushBack("testOrder",true,VS_S,ST_SPATIAL);
olm.pushBack("testOrder",false,VS_S,ST_SPATIAL);
olm.pushBack("testOrder",false,VS_S,ST_SPATIAL);
olm.pushBack("testOrder",false,VS_L,ST_NONSPATIAL);
olm.add("testOrder2");
olm.pushBack("testOrder2",true,VS_S,ST_SPATIAL);
olm.pushBack("testOrder2",true,VS_S,ST_SPATIAL);
olm.pushBack("testOrder2",false,VS_S,ST_SPATIAL);
olm.pushBack("testOrder2",false,VS_S,ST_SPATIAL);
olm.pushBack("testOrder2",false,VS_L,ST_NONSPATIAL);
olm.add("getDem");
olm.pushBack("getDem",true,VS_S,ST_SPATIAL);
olm.add("noArguments");

              return olm;
            }
} // eo namespace calc
