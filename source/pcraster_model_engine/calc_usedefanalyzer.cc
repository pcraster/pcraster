#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_USEDEFANALYZER
#include "calc_usedefanalyzer.h"
#define INCLUDED_CALC_USEDEFANALYZER
#endif

// Library headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.
#ifndef INCLUDED_STACK
#include <stack>
#define INCLUDED_STACK
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// Module headers.
#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_JUMPNODE
#include "calc_jumpnode.h"
#define INCLUDED_CALC_JUMPNODE
#endif
#ifndef INCLUDED_CALC_BLOCKENTRANCE
#include "calc_blockentrance.h"
#define INCLUDED_CALC_BLOCKENTRANCE
#endif
#ifndef INCLUDED_CALC_BASICBLOCK
#include "calc_basicblock.h"
#define INCLUDED_CALC_BASICBLOCK
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_POINTCODEBLOCK
#include "calc_pointcodeblock.h"
#define INCLUDED_CALC_POINTCODEBLOCK
#endif
#ifndef INCLUDED_CALC_UDEVENT
#include "calc_udevent.h"
#define INCLUDED_CALC_UDEVENT
#endif
#ifndef INCLUDED_CALC_IOTYPE
#include "calc_iotype.h"
#define INCLUDED_CALC_IOTYPE
#endif
/*!
  \file
  This file contains the implementation of the UseDefAnalyzer class.
*/

//#define TRACE_LOG(x)    x
#define TRACE_LOG(x)


//------------------------------------------------------------------------------

namespace calc {

struct RecordFirst : boost::noncopyable
{
  typedef std::vector<UDEvent>::const_iterator P;
  // use end as not yet initialized marker
  const P d_end;
  // first in entire script
  P ever;
  // first in initial
  P initial;
  // first in dynamic
  P dynamic;
  RecordFirst(P end):
    d_end(end),
    ever(d_end),
    initial(d_end),
    dynamic(d_end)
  {}
  void update(P e, bool inDynamic) {
    if (ever == d_end)
      ever=e;
    P& u(inDynamic ? dynamic:initial);
    if (u == d_end)
      u=e;
  }
};


//! chain of events of a variable.
/*!
 *  After construction (series of add() calls), Enter and Jump 
 *  events are
 *  paired and nested, for the same BasicBlock each
 *  Enter event can find a first Jump event as forward
 *  in the chain for the same BasicBlock.
 *
 *  The Enter and Jump events of a BasicBlock in which the variable
 *  is not referenced (Use or Def) are purged.
 */
class EventChain : public std::vector<UDEvent>
{
 public:
  //! return iterator to first reference past the possible dummy
  const_iterator beginRealRef(const ASTPar *dummy) const {
    const_iterator e=begin();
    while (e!=end()) {
     if (e->reference() && e->par() != dummy)
        break;
     ++e;
    }
    return e;
  }
  void add(const UDEvent& e) {
      if (e.jump() && back().enter()) {
        // remove the enter and do not add the jump
        DEVELOP_PRECOND(e.block() == back().block());
        pop_back();
      } else
        push_back(e);
  }

  reverse_iterator jumpOfEnter(reverse_iterator e) const {
    PRECOND(e->enter());
    BasicBlock *b=e->block();
    for(  ; ; --e) {
      if (e->jump() && e->block()==b) {
        return e;
      }
    }
    PRECOND(FALSE);
    return e;
  }

  void  setKeepLive() {

   bool jumpBackHasChanged;
   do {
    jumpBackHasChanged=false;

    // initial, the path that start at end is not live:
    bool keepLive= false;

    for(reverse_iterator e=rbegin(); e!=rend(); e++) {
       keepLive = e->mergeKeepLive(keepLive);
       if (e->enter() && e->block()->hasBackBranch() && keepLive) {
         // propagate this keepLive back
         // to the last event
         // before (+1 of reverse_iterator) Jump
         reverse_iterator b=jumpOfEnter(e)+1;
         if (!b->keepLive()) {
           // back propagation did change something
           // that need a next iteration over the chain
           b->mergeKeepLive(keepLive);
           jumpBackHasChanged=true;
         }
       }
    }
   } while(jumpBackHasChanged);
  }

  //! return the first Use or 0 if a Def preceeds the first Use
  ASTPar* firstUse(const ASTPar *dummy) const {
    for(const_iterator e=beginRealRef(dummy); e!=end(); ++e)
      switch(e->type()) {
        case UDEvent::Use: return e->par();
        case UDEvent::Def: return 0;
        default:;
      }
    POSTCOND(false); // should have found a Use or Def
    return 0;
  }

  /*! returns the ASTPar node being the first Def in this chain
   *  and that is kept live for use outside this chain (e.g. last chain node has
   *  lastUse set to false()). Or 0 if no such node can be found.
   *
   *  Note newLiveDef only used to get output set, hence what ASTPar node
   *  is returned does not matter
   */
  ASTPar* newLiveDef(const ASTPar *dummy) const {
    const_iterator def=beginRealRef(dummy);
    for(; def!=end(); ++def)
      if(def->def())
        break;
    if (def != end())
     for(const_reverse_iterator e=rbegin(); e!=rend(); ++e)
      if (e->reference())
        return (e->par()->lastUse()) ? 0 : def->par();
    return 0;
  }

  void  print() const {
    std::cerr << "----------" << std::endl;
    for(const_iterator e=begin(); e!=end(); ++e)
       e->print();
    std::cerr << "----------" << std::endl;
  }

  void  setLastUse(const std::string& name) const {
    TRACE_LOG(std::cerr << "setLastUse for " << name << std::endl);
    for(const_iterator e=begin(); e!=end(); ++e) {
      TRACE_LOG(e->print());
      switch(e->type()) {
        case UDEvent::Use:
        case UDEvent::Def:
          //  een Def node heeft altijd een d_par die toehoort aan een
          //  ASTAss::d_pars.
          //  als die in die ASTAss niet geschreven (!= reportPar), zou een
          //  warning kunnen geven, not-used.
          //  FTTB, do-not-assign rhs value, but delete.
          //  Later  propagate (notUsed) door naar rhs-expr!
          //   -> dead-code elimination
          e->par()->setLastUse(!e->keepLive());
          break;
        case UDEvent::Jump:
          // a par only "jumps out of" block if it is reference'd in the block
          //  hence there is a node before e (e-1)
          PRECOND(e != begin());
          // voeg de ASTPar aan de "killSet when taking
          //  "forward" (i.t.t. BackBranch) branch van JumpNode
          // no longer needed in forward branch?
          if (!e->keepLive()) {
            // but not marked as lastUse in
            // e-1 (=last node within the basicBlock this Jump belongs to)?
            if ((e-1)->keepLive())
              e->block()->addDeleteOnForward(name);
          }
        case UDEvent::Enter: /* nothing */;
      } // eoswitch
     } // eofor
  }

  IOType ioType(void* dynamicSectionPtr) const {
    typedef const_iterator P;
    RecordFirst use(end()),def(end());

    bool inDynamicSection(false);
    for(P e=begin(); e!=end(); ++e)
      switch(e->type()) {
        case UDEvent::Use:
           use.update(e, inDynamicSection);
           break;
        case UDEvent::Def:
           def.update(e, inDynamicSection);
           break;
        case UDEvent::Jump:
           if (e->block() == dynamicSectionPtr)
              inDynamicSection = false;
           break;
        case UDEvent::Enter:
           if (e->block() == dynamicSectionPtr)
              inDynamicSection = true;
           break;
      }

    IOType t;

    PRECOND(use.ever!=def.ever);

    t.setInput(pcrxml::ModelInputType::None);
    if (use.ever < def.ever) {
      // need input value
      if (use.dynamic == end() ||
          def.dynamic != end())
        t.setInput(pcrxml::ModelInputType::Initial);
      else 
        t.setInput(pcrxml::ModelInputType::Dynamic);
    }
    t.setOutput(pcrxml::ModelOutputType::Fixed);
    if (def.ever != end()) {
      // some definitions will change output
      if (def.dynamic != end())
       t.setOutput(pcrxml::ModelOutputType::Dynamic);
      else
       t.setOutput(pcrxml::ModelOutputType::Initial);
    }
    return t;
  }

};

//! an EventChain for each symbol
class UseDefRecorder : std::map<std::string, EventChain>
{
  /*!
   * Use-def analysis algorithm always assume a Def is first,
   * as in language where variable must be defined first.
   * That's what addPrologue does. The ioType algorithm is however much
   * easier to code if we follow the pcrcalc convention that a Use also defines
   * the variable.
   */
   bool                     d_prefixFirstUseByDef;

   //! tricky: do not return chains having the address of d_dummy
   ASTPar                   d_dummy;

   //! current nesting of blocks, will only have Enter events
   EventChain               d_currentBlockNesting;

   //! only need address,0 if never encountered
   void*                    d_dynamicSection;

  //! simulates the runtime stack
  /*!
   * contains 0 for not ASTPar nodes or
   * ptrs to ASTPar, implemented as vector
   *  since we evaluate ranges of the stack in pop()
   */
  std::stack<ASTPar *>  d_stack;

  //! update existing or add new one
  EventChain& update(const std::string& name)
  { return operator[](name); }

 public:
   UseDefRecorder(bool prefixFirstUseByDef):
    d_prefixFirstUseByDef(prefixFirstUseByDef),
    d_dummy("dummy"),
    d_dynamicSection(0)
   {
   }
   ~UseDefRecorder() {
    // POSTCOND(d_stack.empty());
    // POSTCOND(d_currentBlockNesting.empty());
   }
   void setDynamicSection(void *dynamicSection)
   {
     d_dynamicSection=dynamicSection;
   }

   void push(ASTPar *p) {
     d_stack.push(p);
   }

   void pop(size_t n) {
    // UseDef for par used more than once in same expr
    // needs this order of stack evaluation
    typedef std::vector<ASTPar *>::iterator I;
    for(size_t i=0; i < n; ++i) {
      PRECOND(d_stack.size());
      ASTPar *p=d_stack.top();
      d_stack.pop();
      if (p)
        add(UDEvent(UDEvent::Use,p));
    }
   }

   //! add prologue if e is first Event
   void addPrologue(
       const UDEvent& e) {

     // if we already have an EventChain
     // then no need to add prologue
     if (count(e.name()))
       return;

     /*
      there is always an enclosing block.
      if this fails, the UseDefAnalyzer may be accidently 
      initialized by a non BasicBlock-Node see precondition of
      UseDefAnalyzer ctor.
      */
     DEVELOP_PRECOND(!d_currentBlockNesting.empty());

     EventChain prologue(d_currentBlockNesting);

     // preceed with all enclosing Enter's since all Jump's
     // will also be added
     if (d_prefixFirstUseByDef && e.use()) {
       // insert dummy above Enter'ing the most outer block
       // that has a loop back
       EventChain::iterator i=prologue.begin();
       for( ;i != prologue.end(); ++i)
         if (i->block()->hasBackBranch()) {
           prologue.insert(i,UDEvent(UDEvent::Def,&d_dummy));
           break;
         }
     }
     insert(std::make_pair(e.name(),prologue));
   }

   void add(const UDEvent& e) {
     switch (e.type()) {
       case UDEvent::Use:
           addPrologue(e);
           update(e.name()).push_back(e);
           break;
       case UDEvent::Def:
           addPrologue(e);
           update(e.name()).push_back(e);
           break;
       case UDEvent::Enter:
          d_currentBlockNesting.add(e);
         // every par already recorded will enter a block
         for(iterator i=begin(); i!=end();++i)
            i->second.add(e);
         break;
       case UDEvent::Jump:
          d_currentBlockNesting.add(e);
         // every par already recorded will jump out of a block
         for(iterator i=begin(); i!=end();++i)
            i->second.add(e);
      }
   }

   void setKeepLive(bool keepLiveAtEnd) {
      for(iterator i=begin(); i!=end();++i) {
        TRACE_LOG(std::cerr << "liveAnalysis for " << i->first << std::endl);
        if (keepLiveAtEnd)
          i->second.add(UDEvent(UDEvent::Use,&d_dummy));
        i->second.setKeepLive();
      }
   }

   void setLastUse() const {
     for(const_iterator i=begin(); i!=end();++i)
        i->second.setLastUse(i->first);
   }

   ParSet inputSet() const {
    ParSet s;
    for(const_iterator i=begin(); i!=end();++i) {
       ASTPar *p=i->second.firstUse(&d_dummy);
       if (p)
         s.insert(p);
    }
    return s;
   }

   ParSet newLiveDefSet() const {
    ParSet s;
    for(const_iterator i=begin(); i!=end();++i) {
      ASTPar *p=i->second.newLiveDef(&d_dummy);
      if (p)
        s.insert(p);
    }
    return s;
   }
   std::map<std::string,IOType> ioTypes() const {
    std::map<std::string,IOType> m;
    for(const_iterator i=begin(); i!=end();++i) {
      m.insert(std::make_pair(
        i->first, i->second.ioType(d_dynamicSection)));
    }
    return m;
   }

};

} // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF STATIC USEDEFANALYZER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF USEDEFANALYZER MEMBERS
//------------------------------------------------------------------------------

/*!
 * \pre
 *   first node of cfg is BlockEntrance, e.g. the AST has a BasicBlock as top level.
 */
calc::UseDefAnalyzer::UseDefAnalyzer(CFGNode *cfg,
    bool keepLiveAtEnd,
    bool prefixFirstUseByDef):
  CFGVisitor(cfg),
  d_keepLiveAtEnd(keepLiveAtEnd)
{
  if(!dynamic_cast<BlockEntrance*>(current()))
     PRECOND(dynamic_cast<BlockEntrance*>(current()));
  d_rec = new UseDefRecorder(prefixFirstUseByDef);
}

calc::UseDefAnalyzer::~UseDefAnalyzer()
{
  delete d_rec;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::UseDefAnalyzer& calc::UseDefAnalyzer::operator=(const UseDefAnalyzer& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::UseDefAnalyzer::UseDefAnalyzer(const UseDefAnalyzer& rhs):
  Base(rhs)
{
}
*/

void calc::UseDefAnalyzer::setLastUse()
{
  d_rec->setKeepLive(d_keepLiveAtEnd);
  d_rec->setLastUse();
}

calc::ParSet calc::UseDefAnalyzer::inputSet() const
{
  return d_rec->inputSet();
}

std::map<std::string,calc::IOType>
 calc::UseDefAnalyzer::ioTypes() const
{
  return d_rec->ioTypes();
}

calc::ParSet calc::UseDefAnalyzer::newLiveDefSet() const
{
  return d_rec->newLiveDefSet();
}

//! right hand side visit
void calc::UseDefAnalyzer::visitPar (ASTPar *p)
{
   d_rec->push(p);
}


void calc::UseDefAnalyzer::visitNumber (ASTNumber *)
{
   d_rec->push(0);
}

void calc::UseDefAnalyzer::visitNonAssExpr(NonAssExpr   *)
{
   d_rec->pop(1);
}


void calc::UseDefAnalyzer::doExpr(BaseExpr *e)
{
  d_rec->pop(e->nrArgs());
  for(size_t i=0; i< e->nrReturns(); ++i)
    d_rec->push(0);
}

void calc::UseDefAnalyzer::visitExpr(BaseExpr* e)
{
  doExpr(e);
}

void calc::UseDefAnalyzer::visitAss(ASTAss *a)
{
  d_rec->pop(a->nrPars());

  // left hand side visit
  for(size_t i=0; i < a->nrPars(); ++i)
   d_rec->add(UDEvent(UDEvent::Def,a->par(i)));
}

void calc::UseDefAnalyzer::visitPointCodeBlock(PointCodeBlock *b)
{
  typedef ParSet::const_iterator I;

  for(I i=b->input().begin(); i!=b->input().end(); ++i)
   d_rec->add(UDEvent(UDEvent::Use,*i));
  for(I i=b->output().begin(); i!=b->output().end(); ++i)
   d_rec->add(UDEvent(UDEvent::Def,*i));

}

void calc::UseDefAnalyzer::visitJumpNode(JumpNode *j)
{
  d_rec->add(UDEvent(UDEvent::Jump,j->block()));
}

void calc::UseDefAnalyzer::visitBlockEntrance(BlockEntrance *e)
{
  d_rec->add(UDEvent(UDEvent::Enter,e->block()));
}

void calc::UseDefAnalyzer::enterDynamicSection(DynamicSection* d)
{
  d_rec->setDynamicSection(d);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//!  for all ASTPar nodes in \a cfg reset the ASTPar::lastUse() attribute
/*!
 *  \param cfg the CFG that will have its ASTPar nodes updated
 *  \param keepLiveAtEnd the last computed value
 *         is kept live at the end of the cfg, intermediate computed
 *         values may be deleted.
 *         This allows for the execution of a cfg that will have all
 *         data in the DataTable at end, in contrast to the normal
 *         case where a computed value is discarded at the moment it is
 *         no longer used in the cfg.
 */
void calc::setLastUse(CFGNode *cfg,bool keepLiveAtEnd) {
  UseDefAnalyzer a(cfg,keepLiveAtEnd,true);
  a.visit();
  a.setLastUse();
}

/*! return the parameters that are present in cfg and have a first UDEvent::Use
    before UDEvent::Def
 */
calc::ParSet calc::inputSet(CFGNode   *cfg) {
  UseDefAnalyzer a(cfg,false,true);
  a.visit();
  return a.inputSet();
}

std::map<std::string,calc::IOType>
 calc::ioTypes(CFGNode   *cfg) {
  UseDefAnalyzer a(cfg,false,false);
  a.visit();
  return a.ioTypes();
}

//! return the output parameters
/*!
 * \param subCfg is a (subset of a) cfg with ASTPar::lastUse attributes 
 *        already set.
 *
 * Output is here relative, subCfg is a subset of a larger cfg,
 * in that larger cfg lastUse attributes are set. The output of
 * this \a subCfg is all parameters getting
 * a value (UDEvent::Def) <i>in</i> subCfg and the value is used in the 
 * larger cfg after execution of this cfg
 */
calc::ParSet calc::newLiveDefSet(CFGNode   *subCfg) {
  UseDefAnalyzer a(subCfg,false,true);
  a.visit();
  return a.newLiveDefSet();
}
