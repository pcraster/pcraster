#ifndef INCLUDED_CALC_ASTNODECONTAINER
#define INCLUDED_CALC_ASTNODECONTAINER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif



namespace calc {
  // ASTNodeContainer declarations.
}



namespace calc {



//! container of nodes
/*!
   aggregate common code, specilazation required are or based on std::deque to
   allow random access or std::list to allow deletion of element ranges without
   invalidating other iterators.
*/
template<
  typename STLContainer>
class ASTNodeContainer: public ASTNode
{

public:
  typedef STLContainer AC;
  typedef typename AC::iterator            iterator;
  typedef typename AC::const_iterator      const_iterator;
protected:
  AC                         d_container;

  void deepClone(const AC& src);

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  ASTNodeContainer&           operator=           (const ASTNodeContainer&);

                   ASTNodeContainer               (const ASTNodeContainer&);


                   ASTNodeContainer               ();


  /* virtual */    ~ASTNodeContainer              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  virtual void         accept                (ASTVisitor& v);
  void                 visitAll              (ASTVisitor& v);
  void                 transferPushBack      (ASTNode *n);
  void                 transferPushFront     (ASTNode *n);
  /*! clear contents, calling delete on all nodes
   */
  void                 clear                 ();


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t               size                  () const;
  const_iterator       begin                 () const;
  const_iterator       end                   () const;

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
