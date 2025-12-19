#ifndef INCLUDED_CALC_OPERATIONS
#define INCLUDED_CALC_OPERATIONS

#include "stddefx.h"
#include "major_op.h"
#include "calc_calclib.h"

#include <vector>
#include <map>
#include <string>


namespace calc {
  // Operations declarations.
}



namespace calc {

class CalcLib;
class Operator;
class MRF;

//! Table with operations stored
class Operations
{

private:
  typedef const Operator *O;

  //! Assignment operator. NOT IMPLEMENTED.
  Operations&           operator=           (const Operations& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Operations               (const Operations& rhs);

  void clean();
  void add(const Operator *o);
  void add(MAJOR_CODE mrf, MAJOR_CODE oneOf1, MAJOR_CODE oneOf2);

  struct MRFRelatives {
    bool          d_stackTop;
    MAJOR_CODE    d_mrf;
    MAJOR_CODE    d_otherOneOfMRF;
  };
  //! example OP_SPREAD,{OP_SPREAD_MRF,OP_SPREADZONE}
  typedef std::map<MAJOR_CODE, MRFRelatives> MRFRelations;
  MRFRelations                               d_mrfRelations;

  typedef std::map<std::string,O>          NameOp;
  //! d_nameOp owns the Operator objects
  /*!
   *  stores both built-ins and externals
   */
  NameOp                                   d_nameOp;

  typedef std::map<MAJOR_CODE ,O>          CodeOp;
  //! only  stores both built-ins
  CodeOp                                   d_codeOp;
  typedef std::map<std::string,MAJOR_CODE> Funcs;
  Funcs                                    d_funcs;

  typedef std::map<std::string,CalcLib *>  LibMap;
  LibMap                                   d_libs;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Operations               ();

  /* virtual */    ~Operations              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             init();

  void             loadLib                  (const std::string& libName);
  void             load                     (const CalcLib::GetMeta& gm);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  MAJOR_CODE      function  (const std::string& name) const;
  const Operator* operator[](const std::string& name) const;
  const Operator* operator[](const MAJOR_CODE   major)const;
  size_t          size      ()                        const;


  MAJOR_CODE      otherOneOfMRF (MAJOR_CODE oneOfMRF) const;
  MAJOR_CODE      oneOf2Mrf     (MAJOR_CODE oneOfMRF) const;
  bool        oneOfMrfIsStackTop(MAJOR_CODE oneOfMRF) const;
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

extern Operations globalOperations;


} // namespace calc

#endif
