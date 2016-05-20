#ifndef INCLUDED_CALC_RUNTIMEENGINE
#define INCLUDED_CALC_RUNTIMEENGINE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

// Module headers.



namespace geo {
  // RunTimeEngine declarations.
  class RasterSpace;
}



namespace calc {

class DataValue;
class RunTimeEnv;
class DataStorageId;
class Field;
class Operator;
class ObjectLink;

//! Engine to run single operations (or entire scripts in the future)
/*!
 * All push methods without the transfer prefix will push a pointer of
 * an object to the stack that is garantueed not to be modified.
 * The callee must garantuee that the object is alive during its
 * lifetime.
 * The following example shows what not to do (bad) and what is
 * accepted (good)
     \code
     void bad() {
       RunTimeEngine *rte;
       ....
       {
         DataStorageId t("someTable.txt");
         rte->pushDataStorageId(&t);
         // t will go out of scope
       }
       // now exec with t on rte stack
       // ERROR: CRASH
       rte->checkAndExec(o,3);
     }

     void good() {
       RunTimeEngine *rte;
       ....
       {
         DataStorageId t("someTable.txt");
         rte->pushDataStorageId(&t);
          // now exec with t on rte stack
         rte->checkAndExec(o,1); // OK
         // t will go out of scope
       }
     }
     \endcode
  All releasePop method are garantueed to return a new allocated object
  that must be deleted by the callee. Hence push a value and then direct
  popping will not return the push value:
  \code
   rte->pushField(f);
   Field *result=rte->transferPopField();
   assert(f!=result);
   delete result;
  \endcode
  Note that even though the arguments of the push methods are const,
  some internal flags are modified when the objects are used by
  RunTimeEngine. These flags are of no relevance to the class user.
*/
class PCR_DLL_CLASS RunTimeEngine
{


private:

  RunTimeEnv   *d_rte;

  //! Assignment operator. NOT IMPLEMENTED.
  RunTimeEngine&           operator=           (RunTimeEngine const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   RunTimeEngine               (RunTimeEngine const& rhs);

  void             pushUnmanaged               (const DataValue     *d);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RunTimeEngine               (const geo::RasterSpace& rs);

  /* virtual */    ~RunTimeEngine              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             pushField           (const Field         *f);
  void             pushObjectLink      (const ObjectLink    *d);
  void             pushDataStorageId   (const DataStorageId *id);

  void             transferPushField   (Field *f);

  ObjectLink*      releasePopObjectLink();
  Field*           releasePopField     ();

  void             setNrTimeSteps      (size_t nrTimeSteps);
  void             setCurrentTimeStep  (size_t currentTimeStep);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             checkAndExec        (const Operator& op,
                                        size_t nrPushedInputs);

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
