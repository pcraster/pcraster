#ifndef INCLUDED_CALC_OPIMPL
#define INCLUDED_CALC_OPIMPL


#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// Module headers.
#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif
#ifndef INCLUDED_CALC_IOPIMPL
#include "calc_iopimpl.h"
#define INCLUDED_CALC_IOPIMPL
#endif
#ifndef INCLUDED_CALC_POINTIMPL
#include "calc_pointimpl.h"
#define INCLUDED_CALC_POINTIMPL
#endif
#ifndef INCLUDED_CALC_IFOPOINTARRAY
#include "calc_ifopointarray.h"
#define INCLUDED_CALC_IFOPOINTARRAY
#endif


namespace calc {
  // IOpImpl declarations.
}


namespace calc {


//! Operation Implementation
class SameUn: public PointImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  SameUn&           operator=           (const SameUn& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   SameUn               (const SameUn& rhs);

  const ISameUn*    d_fieldOp;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SameUn               (const ISameUn* fieldOp);

  virtual         ~SameUn               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
  virtual void genPointCode      (PointCodeGenerator* g) const;
};

//! Operation Implementation
class Trig: public IOpImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Trig&           operator=           (const Trig& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Trig               (const Trig& rhs);

  //! directional input, scalar needs conversion first
  const ISameUn*    d_fieldOp;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Trig               (const ISameUn* fieldOp);

  virtual         ~Trig               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
  virtual void genPointCode      (PointCodeGenerator* g) const;
};

//! Operation Implementation
class GenSpatial: public IOpImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  GenSpatial&           operator=           (const GenSpatial& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   GenSpatial               (const GenSpatial& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GenSpatial              ();

  virtual         ~GenSpatial              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
  virtual void genPointCode      (PointCodeGenerator* g) const;
};

//! Operation Implementation
class GenNonSpatial: public IOpImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  GenNonSpatial&           operator=        (const GenNonSpatial& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   GenNonSpatial            (const GenNonSpatial& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GenNonSpatial           ();

  virtual         ~GenNonSpatial           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
  virtual void genPointCode      (PointCodeGenerator* g) const;
};

//! Operation Implementation
class SameBin: public PointImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  SameBin&           operator=           (const SameBin& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   SameBin               (const SameBin& rhs);

  std::vector<const ISameBin*>    d_fieldOp;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SameBin               (const ISameBin* op1,
                                          const ISameBin* op2=0,
                                          const ISameBin* op3=0);

  virtual         ~SameBin               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
  void genPointCode      (PointCodeGenerator* g) const;
};

/*! two input arguments, both of the same type
 *  and a new created result type
 */
class DiffBin: public PointImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  DiffBin&           operator=           (const DiffBin& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DiffBin               (const DiffBin& rhs);

  std::vector<const IDiffBin*>    d_fieldOp;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DiffBin               (const IDiffBin* op1,
                                          const IDiffBin* op2,
                                          const IDiffBin* op3=0);

  virtual         ~DiffBin               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
  void genPointCode      (PointCodeGenerator* g) const;
};

//! if then else construct only
class IfThenElse: public PointImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  IfThenElse&           operator=           (const IfThenElse& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   IfThenElse               (const IfThenElse& rhs);

  std::vector<const IIfThenElse*>    d_fieldOp;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IfThenElse            (const IIfThenElse* op1,
                                          const IIfThenElse* op2,
                                          const IIfThenElse* op3);

  virtual         ~IfThenElse               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
  void genPointCode      (PointCodeGenerator* g) const;

};

/*! Unary operation with the following features:
 *  - cr() of result and argument are different.
 *  - spatial type of result is derived and equal to argument spatial type
 */
class DiffUn: public PointImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  DiffUn&           operator=           (const DiffUn& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DiffUn               (const DiffUn& rhs);

  std::vector<const IDiffUn*>    d_fieldOp;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DiffUn               (const IDiffUn* op1,
                                         const IDiffUn* op2=0,
                                         const IDiffUn* op3=0);

  virtual         ~DiffUn               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void exec     (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
  void genPointCode     (PointCodeGenerator* g) const;
};

/*! spatial conversion is a special case
 */
class SpatialImpl : public DiffUn {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  SpatialImpl&           operator=           (const SpatialImpl& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   SpatialImpl               (const SpatialImpl& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SpatialImpl               (const IDiffUn* op1,
                                              const IDiffUn* op2,
                                              const IDiffUn* op3);

  virtual         ~SpatialImpl               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};

//! Operation Implementation
class Conversion: public IOpImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Conversion&           operator=        (const Conversion& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Conversion            (const Conversion& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Conversion           ();

  virtual         ~Conversion           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
  virtual void genPointCode      (PointCodeGenerator* g) const;
};

class AreaTotal: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};
class AreaAverage: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};
class AreaMaximum: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};
class AreaMinimum: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};
class Order: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};
class AreaOrder: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};

class ArgOrderAddAreaLimited: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};
class ArgOrderWithIdAddAreaLimited: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};
class ArgOrderAreaLimited: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};
class ArgOrderWithIdAreaLimited: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};
class ArgOrder: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};
class ArgOrderWithId: public IOpImpl {
 void exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
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

extern AreaTotal                 builtIn_areatotal;
extern AreaAverage               builtIn_areaaverage;
extern AreaMinimum               builtIn_areaminimum;
extern AreaMaximum               builtIn_areamaximum;
extern Order                     builtIn_order;
extern AreaOrder                 builtIn_areaorder;
extern ArgOrderAreaLimited       builtIn_argorderarealimited;
extern ArgOrderWithIdAreaLimited builtIn_argorderwithidarealimited;
extern ArgOrderAddAreaLimited       builtIn_argorderaddarealimited;
extern ArgOrderWithIdAddAreaLimited builtIn_argorderwithidaddarealimited;
extern ArgOrder                  builtIn_argorder;
extern ArgOrderWithId            builtIn_argorderwithid;

} // namespace calc

#endif
