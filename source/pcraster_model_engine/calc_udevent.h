#ifndef INCLUDED_CALC_UDEVENT
#define INCLUDED_CALC_UDEVENT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // UDEvent declarations.
}



namespace calc {


//! Use Def Event as used in UseDefAnalyzer
class UDEvent {
public:
  enum Type {
         Def=0, /*<! (re-)define the value of the parameter, reference */
         Use=1, /*<! use the value of the parameter, reference */
         Jump=2,/*<! parameter is keepLive at this JumpNode */
         Enter=3/*<! parameter is keepLive at this BlockEntrance */
       };
private:

  Type d_type;

  /*!  if true: a node (but not this node!) reached from here in the
   *   CFG requires a keepLive definition, false if not
   */
  bool  d_keepLive;

  void init(Type type) {
    d_type          =type;
    d_keepLive      =false;
  }

  union {
    //! Use or Def: the parameter used or defined
    ASTPar     *d_par;
    //! Jump or Enter
    BasicBlock *d_block;
  };

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  UDEvent(Type type, ASTPar *par):
    d_par(par)
    {
     init(type);
     DEVELOP_PRECOND(par);
     DEVELOP_PRECOND(type==Def||type==Use);
    }
  UDEvent(Type type, BasicBlock *block):
    d_block(block)
    {
     init(type);
     DEVELOP_PRECOND(block);
     DEVELOP_PRECOND(type==Jump||type==Enter);
    }
  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  /*!
   * \param keepLive true if some node reachable from here
   *                 in the CFG needs par to be keepLive, false otherwise
   * \returns backward keepLive value
   */
  bool mergeKeepLive(bool keepLive) {
    // 1) update here: if any reachable node needs keepLive,
    //    it must be keepLive here
    if (keepLive)
      d_keepLive=keepLive;

    // 2) compute new backward value
    if (use())
      return true;
    if (def())
      return false;
    return d_keepLive;
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  BasicBlock*      block               () const {
      DEVELOP_PRECOND(jump()||enter());
      return d_block;
  }

  inline ASTPar* par() const {
    DEVELOP_PRECOND(def()||use());
    return d_par;
  }

  inline bool keepLive() const {
    return d_keepLive;
  }

  inline const std::string& name() const {
    return par()->name();
  }
  inline Type type() const {
    return d_type;
  }
  inline bool jump() const {
    return d_type==Jump;
  }
  inline bool enter() const {
    return d_type==Enter;
  }
  inline bool def() const {
    return d_type==Def;
  }
  inline bool use() const {
    return d_type==Use;
  }
  inline bool reference() const {
    return use()||def();
  }

  void print() const {
   const char *names[4]= { "Def", "Use", "Jump", "Enter"};
   std::cerr << names[(int)type()]
             << " d_keepLive(" << d_keepLive << ")";
   if (def()||use())
     std::cerr << " d_par->shortPosText(" << d_par->shortPosText() << ")";
   else
     std::cerr << " d_block(" << d_block << ")";
   std::cerr << std::endl;
  }

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
