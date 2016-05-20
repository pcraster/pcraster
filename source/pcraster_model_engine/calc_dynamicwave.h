#ifndef INCLUDED_CALC_DYNAMICWAVE
#define INCLUDED_CALC_DYNAMICWAVE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_IOPIMPL
#include "calc_iopimpl.h"
#define INCLUDED_CALC_IOPIMPL
#endif



namespace calc {
  // DynamicWave declarations.
}



namespace calc {

struct DynamicWaveTable {
   enum   LookupColumns { profileId=0,H=1,A=2,P=3 };
};

//! Dynamic wave implementation
class DynamicWave: public IOpImpl
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  DynamicWave&           operator=           (DynamicWave const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DynamicWave               (DynamicWave const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DynamicWave               ();

  /* virtual */    ~DynamicWave              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void         exec              (RunTimeEnv* rte,
                                  const Operator& op,
                                  size_t nrActualArgs) const;

};

//! Kinematic  wave implementation (kinwaveflux,kinwavestate)
struct KinematicWave: public IOpImpl
{
  void         exec              (RunTimeEnv* rte,
                                  const Operator& op,
                                  size_t nrActualArgs) const;

};

class LookupPotential : public IOpImpl {
public:
  void         exec              (RunTimeEnv* rte,
                                  const Operator& op,
                                  size_t nrActualArgs) const;
};

class LookupState : public IOpImpl {
public:
  void         exec              (RunTimeEnv* rte,
                                  const Operator& op,
                                  size_t nrActualArgs) const;
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

extern  DynamicWave     dynamicWave;
#define Do_dynwave     &dynamicWave
extern  KinematicWave   kinematicWave;
#define Do_kinwave     &kinematicWave

extern  LookupState        builtIn_lookupstate;
extern  LookupPotential    builtIn_lookuppotential;

class Kinematic: public IOpImpl {
public:
  void             exec                 (RunTimeEnv* rte,
                                         const Operator& op,
                                         size_t nrArgs) const;
};
extern Kinematic builtIn_kinematic;

class Muskingum: public IOpImpl {
public:
  void             exec                 (RunTimeEnv* rte,
                                         const Operator& op,
                                         size_t nrArgs) const;
};

extern Muskingum builtIn_muskingumtimestep;
extern Muskingum builtIn_muskingum;


} // namespace calc

#endif
