#ifndef INCLUDED_CALC_ASTNUMBER
#define INCLUDED_CALC_ASTNUMBER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTID
#include "calc_astid.h"
#define INCLUDED_CALC_ASTID
#endif

namespace calc {
  // ASTNumber declarations.
}



namespace calc {

class Id;
class Field;


//! an AST node holding a number
class ASTNumber : public ASTId {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ASTNumber&           operator=           (const ASTNumber& rhs);


                   ASTNumber               ();

  //! the value as string (exactly as parsed!)
  const   std::string d_strRepr;

  double              d_value;

  void init();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ASTNumber              (const std::string& name);
                   ASTNumber              (const Id&          id);
                   ASTNumber              (const Id& castFunctionName,
                                           VS        castDestination,
                                           const Id& v);

                   ASTNumber               (const ASTNumber& rhs);



     virtual       ~ASTNumber              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             accept                  (ASTVisitor& v);
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  double           value                   () const;
  VS                  vs                   () const;

  std::string      strRepr()  const;
  std::string      qName                   () const;
  ASTNumber*       createClone             () const;
  Field*           createNonSpatial        () const;

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
