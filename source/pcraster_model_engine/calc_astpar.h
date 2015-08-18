#ifndef INCLUDED_CALC_ASTPAR
#define INCLUDED_CALC_ASTPAR

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
#ifndef INCLUDED_CALC_IDLIST
#include "calc_idlist.h"
#define INCLUDED_CALC_IDLIST
#endif


namespace calc {
  // ASTPar declarations.
}



namespace calc {



//! reference to Parameter and it's optional indices within the AST
/*! Note that not everything is part of the AST, non-AST identifiers
 * should use Id
 */
class ASTPar: public ASTId
{

private:

  //! is this node a last use of par's value, before code end or redefinition
  bool             d_lastUse;

  //! it's indices
  IdList           d_index;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  //! Assignment operator.
  ASTPar&           operator=           (const ASTPar&);

  //! Copy constructor.
                   ASTPar               (const ASTPar&);

                   ASTPar               ();

                   ASTPar               (const std::string& name);

                   ASTPar               (const Id&          id);

  /* virtual */    ~ASTPar              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  virtual void     accept               (ASTVisitor& v);

  void             pushBackIndex        (const Id& i);
  void             setLastUse          (bool lastUse);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             symError             (const std::string& msg) const;
  ASTPar          *createClone          () const;
  virtual void     runtimeError         (size_t timeStep,
                                         const std::string& msg) const;

  bool             lastUse             () const;
  const IdList&    index               () const;

};




//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool operator< (const ASTPar& lhs, const ASTPar& rhs);
bool operator==(const ASTPar& lhs, const ASTPar& rhs);
inline bool operator!=(const ASTPar& lhs, const ASTPar& rhs) {
  return !(lhs==rhs);
}

//! in support of ParSet
class ASTParPtrLessName {
  public:
    bool operator()(const ASTPar* lhs, const ASTPar* rhs) const {
      return lhs->name() < rhs->name();
    }
};
//! in support of ParSet
class ASTParPtrEqName {
  public:
    bool operator()(const ASTPar* lhs, const ASTPar* rhs) const {
      return lhs->name() == rhs->name();
    }
};

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
