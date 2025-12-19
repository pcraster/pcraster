#ifndef INCLUDED_CALC_POINTCODEBLOCK
#define INCLUDED_CALC_POINTCODEBLOCK

#include "stddefx.h"
#include "calc_astnode.h"
#include "calc_astnodelist.h"
#include "calc_parset.h"


namespace calc {
  // PointCodeBlock declarations.
}



namespace calc {

class RunTimeEnv;
class Code;

/*!
 * \brief
 * A PointCodeBlock is a series of uninterrupted statements as found in
 * PointCodeBlockReplacer.
 */
class PointCodeBlock : public ASTNode
{
private:

  //! part of d_partOf in transferred to here and replaced by this
  /*!
   * all ParSet's below do contains ptrs to ASTPar node's owned by
   * d_pointCode. These ASTPar node's are then reused and reset for
   * their attributes such as lastUse() (and maybe report() in the future)
   */
  Code*                d_pointCode{nullptr};

  typedef ASTNodeList::iterator       PointCodeIterator;

#ifdef DEBUG_DEVELOP
  /*! \brief all pars used either lhs or rhs in d_pointCode
   *  debug/performance analyze only
   */
  ParSet      d_pars;
#endif
  // nr of statements replaced. (UNIT TEST)
  size_t      d_size{};
  // range text of statements replaced. (UNIT TEST)
  std::string d_rangeText;

  ParSet   d_input;
  ParSet   d_output;
  ParSet   d_local;

  //! debug/performance analyze only
  size_t   d_nrOps;

  const void*      d_dllFunctionAddress{nullptr};


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  PointCodeBlock&           operator=           (PointCodeBlock const& rhs);

                   PointCodeBlock               (PointCodeBlock const& rhs);

                   PointCodeBlock               (ASTNodeList* partOf,
                                                 PointCodeIterator begin,
                                                 PointCodeIterator end,
                                                 const ParSet& pars,
                                                 size_t     nrOps);

  /* virtual */    ~PointCodeBlock              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void    accept                   (ASTVisitor& v) override;
  void            exec                     (RunTimeEnv& rte);
  void            setDllFunctionAddress    (const void* dllFunctionAddress);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  PointCodeBlock*   createClone    () const override;

  void                      genCode        (std::ostream& s)const;
  const void*      dllFunctionAddress      () const;
  std::string      dllFunctionName         () const;

  ASTNode*         replacedCode            () const;


  const ParSet&             input          () const;
  const ParSet&             output         () const;
        ParSet              transfer       () const;
  const ParSet&             local          () const;


  void                      print          (std::ostream& s) const;
  std::string               rangeText      () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------





//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

std::ostream &operator<<(std::ostream& s, const calc::PointCodeBlock& p);

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

} // namespace calc

#endif
