#ifndef INCLUDED_CALC_OPERATOR
#define INCLUDED_CALC_OPERATOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
# include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_MAJOR_OP
# include "major_op.h"
#define INCLUDED_MAJOR_OP
#endif
#ifndef INCLUDED_CALC_DOMAINILL
#include "calc_domainill.h"
#define INCLUDED_CALC_DOMAINILL
#endif
#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif
#ifndef INCLUDED_CALC_OBJECTLINKRUNTIME
#include "calc_objectlinkruntime.h"
#define INCLUDED_CALC_OBJECTLINKRUNTIME
#endif

namespace calc {
  // Operator declarations.
  class IOpImpl;
  class RunTimeEnv;
}



namespace calc {

/*!
 * \brief
 *    execution type
 *
 *  A is in { UINT1, INT4, REAL4 }
 *
 * s,n,?,d = spatial,nonspatial,both, derived
 *
 * \todo
 *   this enums are parallel with Exec attribute of operation.dtd
 *   who are parallel with IOpImpl subclasses, let them all use the
 *   same name!
 */
typedef enum ExecType {
        EXEC_TYPE_MISC,   /*!< MISNAMED, more a EXEC_MIXED, mis-used for test_until */
        /* class PointOC: */
        EXEC_TYPE_SAME_UN,   /*!< Ad = op A?, */
        EXEC_TYPE_SAME_BIN,  /*!< Ad = A? op A? */
        EXEC_TYPE_DIFF_UN,   /*!< Ad = op A?, */
        EXEC_TYPE_DIFF_BIN,  /*!< Ad = A? op A? */
        EXEC_TYPE_IFTHENELSE,/*!< Ad = mif(t then j else k ) */
        EXEC_TYPE_IFTHEN,    /*!< Ad = mif(t then j else k ) */
        EXEC_TYPE_GEN_NS,    /*!< generate something non spatial */
        EXEC_TYPE_GEN_SP,    /*!< generate something spatial */
        /* not class PointOC: */
        EXEC_TYPE_GLOBAL,    /* args can be spatial or non-spatial
                         * and of all cell representations
                         */
        EXEC_TYPE_DASS,      /* same as GLOBAL but two results */
        EXEC_TYPE_TOUT,
        EXEC_TYPE_EXTERN,    /* dynamic linked functions */
        /* exec_types that are remapped to one of the
         * above in the makecode module
         */
        EXEC_TYPE_MRF,      /* HACK HACK */
        EXEC_TYPE_DOUBLE,   /* function that returns 1 of the result of a DASS type */
        EXEC_TYPE_POLY,     /* there are different impl. for uint1,int4 and scalar  */
        EXEC_TYPE_TRIG,     /* there are different impl. for scalar and directional input  */
        EXEC_TYPE_CONV,     /* there are different impl. stored in the conv. matrix */
        EXEC_TYPE_ASS,      /* syntax only not an executable construction, +=, *=, etc. */
        EXEC_TYPE_T_IN,     /* timeinputfunctions */
        EXEC_TYPE_INDEX,    /* indextablefunctions */
        EXEC_TYPE_Direct    /* builtIn_op defined */
} ExecType;

/*! appearance of an operator/function
 */
typedef enum SYNTAX {
    SYNTAX_FUNC=0, // Function: like func(...)
    SYNTAX_MRF=3,  // Multiple Return Function
    SYNTAX_OP=1,   // Operator: like ... op ...
    SYNTAX_NONE=2  // Internal: grammar construction
} SYNTAX;

class Operator
{

  //! name of operator or function
  const  std::string d_name;
  //! implementation name of operator or function, can be identical
  /*!
   *  this holds the method name for ObjectLink methods without
   *  the class name prefix
   */
  const  std::string d_implName;

  std::vector<DataType> d_inputs;
  //! the number of inputs that can be repeated counted from right to left
  size_t                d_inputTailRepeat;
  //! what types the operator returns
  std::vector<DataType> d_results;

  //! major number of operation
  MAJOR_CODE            d_major;
  SYNTAX                d_syntax;

  //! can we include this operation in a PointCodeBlock?
  bool                  d_pointOperator;

  //! type of domain checking
  DomainIll             d_domainIll;


  const ExecType          d_execType;
  const IOpImpl          *d_impl;
  const ObjectLinkFactoryPtr d_objectLinkFactory;

  bool                  d_commutative;



private:

                   Operator               ();

  //! Assignment operator. NOT IMPLEMENTED.
  Operator&           operator=           (Operator const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Operator               (Operator const& rhs);

  friend class Operations;
  void             pushBackInput          (VS vs, ST st, bool repeat);
  void             pushBackResult         (VS vs, ST st);
  void             setPointOn             (DomainIll domainType);

                   Operator               (
                      const char *name,
                      const char *implName,
                      MAJOR_CODE major,SYNTAX syntax,
                      ExecType execType, IOpImpl *impl,
                      bool commutative,
                      size_t inputTailRepeat);


  // end friend interface

  VS                 argVs               (size_t a)  const;
  ST                 argSt               (size_t a)  const;

public:

  typedef std::vector<DataType> ArgTypes;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   Operator               (
                      const std::string&           fullyQualifiedName,
                      const std::string&           name,
                      const std::vector<OP_ARGS>&  result,
                      const std::vector<OP_ARGS>&  input,
                      const ObjectLinkFactoryPtr&  impl);
                   Operator               (
                      const std::string&           name,
                      const std::vector<DataType>& result,
                      const std::vector<DataType>& input);


  virtual         ~Operator              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual bool       commutative         () const;

  const std::string& name                () const;
  const std::string& implName            () const;

  // result methods:
  VS                 vs                  () const;
  ST                 st                  () const;
  size_t             nrResults           () const;
  DataType           resultType          (size_t r=0) const;

  DataType           computeResultType   (const ArgTypes& args,size_t r) const;

  // input methods:
  size_t             firstFieldInput     () const;

  int                firstPolyInput      () const;
  bool               isPolyInput         (size_t i) const;
  size_t             polyEqualityInputBegin() const;

  DataType           argType             (size_t argNr) const;
  std::string        strInput            (int argNr) const;
  static size_t      maxInput            ();
  std::string        checkNrInputs       (size_t actualNrInputs) const;


  std::string        syntax              () const;
  MAJOR_CODE         opCode              () const;
  ExecType           execType            () const;

  bool               isDynamicSectionOperation() const;

  void               exec                (RunTimeEnv* rte, size_t nrActualInputs)const;
  void               checkAndExec        (RunTimeEnv* rte, size_t nrActualInputs)const;

  DomainIll          domainIll            () const;


  //! TODO if point functions but domain is limited (div, etc.sqrt<0)
  bool               hasDomainCheck       () const;

  bool               pointOperator        () const;

  size_t             actualInput          (size_t argNr) const;
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

//! declared here, defined in calc_findsymbol.cc
MAJOR_CODE otherOneOfMRF(MAJOR_CODE op);
//! declared here, defined in calc_findsymbol.cc
const Operator& oneOf2Mrf(MAJOR_CODE op);

//! declared here, defined in calc_findsymbol.cc
bool oneOfMrfIsStackTop(MAJOR_CODE oneOfMRF);


} // namespace calc

#endif
