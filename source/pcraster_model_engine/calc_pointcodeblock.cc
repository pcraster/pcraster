#include "stddefx.h"
#include "calc_pointcodeblock.h"
#include "calc_cfgcreator.h"
#include "calc_field.h" // only createDestCloneIfReadOnly
#include "calc_astnodelist.h"
#include "calc_astpar.h"
#include "calc_usedefanalyzer.h"
#include "calc_generatepointcodebody.h"
#include "calc_runtimeenv.h"
#include "calc_parpcb.h"
#include "calc_code.h"

#include <sstream>
#include <iostream>
#include <vector>

/*!
  \file
  This file contains the implementation of the PointCodeBlock class.
*/




//------------------------------------------------------------------------------
// DEFINITION OF STATIC POINTCODEBLOCK MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF POINTCODEBLOCK MEMBERS
//------------------------------------------------------------------------------

/*! \par partOf the ASTNodeList in which the ctor range [begin,end)
 *              is replaced
 */
calc::PointCodeBlock::PointCodeBlock(
    ASTNodeList* partOf,
    PointCodeIterator   begin,
    PointCodeIterator   end,
    const ParSet& pars,
    size_t     nrOps):
   ASTNode(**begin),
   d_pointCode(nullptr),
#ifdef DEBUG_DEVELOP
   d_pars(pars),
#endif
   d_nrOps(nrOps)
{

  auto *l=new ASTNodeList();
  for(auto i=begin;i!=end; ++i)
      l->transferPushBack(*i);
  d_pointCode = new Code(l);

  ScopedCFG cfg(d_pointCode);

  d_input  = inputSet(cfg.cfg);
  d_output = newLiveDefSet(cfg.cfg);
  d_local  = setDifference(pars, setUnion(d_input, d_output));


// UNIT TEST ONLY
  d_size = std::distance(begin,end);

  std::ostringstream s;
  DEVELOP_PRECOND(*begin);
  ASTNodeList::const_iterator back(end);
  --back;
  DEVELOP_PRECOND(*back);
  s << "BLOCK [" << (*begin)->shortPosText()
                 <<      ","    << (*back)->shortPosText() << "]";

  d_rangeText = s.str();
// END UNIT TEST

  partOf->replace(this,begin,end);

}

//! Copy constructor.
calc::PointCodeBlock::PointCodeBlock(PointCodeBlock const& rhs):
    ASTNode(rhs),
    d_pointCode(new Code(rhs.d_pointCode)),
#ifdef DEBUG_DEVELOP
    d_pars(rhs.d_pars),
#endif
    d_input(rhs.d_input),
    d_output(rhs.d_output),
    d_local(rhs.d_local),
    d_nrOps(rhs.d_nrOps),
    d_dllFunctionAddress(nullptr)
{
}

calc::PointCodeBlock::~PointCodeBlock()
{
  delete d_pointCode;
}

//! Assignment operator.
calc::PointCodeBlock& calc::PointCodeBlock::operator=(PointCodeBlock const& rhs)
{
  if (this != &rhs) {
    d_pointCode=new Code(rhs.d_pointCode);
#ifdef DEBUG_DEVELOP
    d_pars     =rhs.d_pars;
#endif
    d_input    =rhs.d_input;
    d_output   =rhs.d_output;
    d_local    =rhs.d_local;
    d_nrOps    =rhs.d_nrOps;
  }
  return *this;
}

//! get value of d_input
const calc::ParSet& calc::PointCodeBlock::input() const
{
  return d_input;
}

calc::PointCodeBlock* calc::PointCodeBlock::createClone()const
{
  return new PointCodeBlock(*this);
}

//! get value of d_output
const calc::ParSet& calc::PointCodeBlock::output() const
{
  return d_output;
}

//! set of Pars that are transferred to dll (union of input and output)
calc::ParSet calc::PointCodeBlock::transfer() const
{
  return setUnion(d_input, d_output);
}

//! get value of d_local
const calc::ParSet& calc::PointCodeBlock::local() const
{
  return d_local;
}

//! set value of d_dllFunctionAddress
void calc::PointCodeBlock::setDllFunctionAddress(const void* dllFunctionAddress)
{
  d_dllFunctionAddress=dllFunctionAddress;
}

//! get value of d_dllFunctionAddress
const void* calc::PointCodeBlock::dllFunctionAddress() const
{
  return d_dllFunctionAddress;
}

std::string calc::PointCodeBlock::dllFunctionName() const
{
  std::ostringstream s;
  s << "pointCode" << this;
  return s.str();
}

//! generate <i>void pointCode0xThisPtr(...) { ... }</i> on s
/*!
 *  const d_pointCode is Casted away!
 */
void calc::PointCodeBlock::genCode(std::ostream& s) const
{
  s << '\n' << "extern \"C\" void " 
    << dllFunctionName() << "(CellPtr* v,size_t n) {"
    << '\n';

  ParSet vContents=transfer();

#ifdef DEBUG
  s << "/* " << std::endl
    << *this;

  // index into array v
  //  enum { A=0,B=1,C=2, etc, };
  s << "enum {" << std::endl;
  size_t index=0;
  for(auto vContent : vContents) {
   s << vContent->name() << "=" << index++ << "," << std::endl;
  }
  s << "};" << std::endl;

  s << "*/ " << std::endl;
#endif

  generatePointCodeBody(s,d_pointCode,vContents);

  s << '\n' << "}" << '\n';
}

void calc::PointCodeBlock::accept(ASTVisitor& v)
{
  v.visitPointCodeBlock(this);
}

namespace calc {
  // scoped vector
  class ParPCBVector: public std::vector<ParPCB *> {
  public:
    ~ParPCBVector() {
      for(auto & i : *this)
        delete i;
    }
  };
}

void calc::PointCodeBlock::exec(RunTimeEnv& rte)
{
  ParPCBVector vector;

  // set defines alfabetic order mapped to vector index v
  ParSet set(transfer());


  for(auto p : set) {
    // add new, incr size
    vector.push_back(new ParPCB());
    ParPCB& pcb(*(vector.back()));

    // can be set to 0
    pcb.setInput(d_input.find(p));
    // can be set to 0
    pcb.setOutput(d_output.find(p));

    if (pcb.input()) {
     // get input
     // use stack as input mechanism, since it will automatic
     // read input maps when needed (StackedValue)
     rte.pushValue(pcb.input());
     pcb.setField(rte.popField());

     // read/write ?
     if (pcb.output())
       pcb.setField(createDestCloneIfReadOnly(pcb.field()));
    } else {
      // no input, must be output only
      PRECOND(pcb.output());
      PRECOND(!pcb.field());
      pcb.setField(rte.createResultField(pcb.output()->returnDataType()));
    }
  }

  execPCB(vector,d_dllFunctionAddress);

  // reverse order due to stack assign
  for(auto i=vector.rbegin(); i!=vector.rend(); ++i)
    if ((*i)->output())
      rte.pushField((*i)->releaseField());

  // assign outputs from stack
  rte.assignStackTop(output().toSortedVector());
}


std::string calc::PointCodeBlock::rangeText() const
{
  return d_rangeText;
}

void calc::PointCodeBlock::print(std::ostream& s) const
{
  s << rangeText() << '\n';
#ifdef DEBUG_DEVELOP
  s << " d_pars"     << d_pars << std::endl;
  s << " Update"     << setIntersection(d_input,d_output) << std::endl;
#endif
  s << " d_input"    << d_input    << '\n';
  s << " d_output"   << d_output   << '\n';
  s << " d_local"    << d_local    << '\n';
  s << " d_nrOps "   << d_nrOps    << '\n';
}

calc::ASTNode* calc::PointCodeBlock::replacedCode() const
{
  return d_pointCode;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


std::ostream& calc::operator<<(
    std::ostream& s,
    const calc::PointCodeBlock& p)
{
  p.print(s);
  return s;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
