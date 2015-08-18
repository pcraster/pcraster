#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_POSITION
#include "calc_position.h"
#define INCLUDED_CALC_POSITION
#endif
#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h"
#define INCLUDED_CALC_POSITIONNAME
#endif


/*!
  \file
  This file contains the implementation of the ASTNode class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ASTNodePrivate
{
public:

  ASTNodePrivate()
  {
  }

  ~ASTNodePrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTNODE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTNODE MEMBERS
//------------------------------------------------------------------------------

calc::ASTNode::ASTNode():
  d_pos(new PositionName())
{
}

calc::ASTNode::ASTNode(const Position* pos):
  d_pos(0)
{
  setPosition(pos);
}

calc::ASTNode::ASTNode(const ASTNode& n):
  d_pos(0),
  d_returnDataType(n.d_returnDataType)
{
  setPosition(n.d_pos);
}

calc::ASTNode&  calc::ASTNode::operator=(const ASTNode& n)
{
  if (&n != this) {
    setPosition(n.d_pos);
    d_returnDataType=n.d_returnDataType;
  }
  return *this;
}

calc::ASTNode::~ASTNode()
{
  delete d_pos;
}

void calc::ASTNode::setPosition(const Position* p)
{
  delete d_pos;
  PRECOND(p);
  d_pos=p->createClone();
}

//! as Pos::posError() with timestep info added
void calc::ASTNode::runtimeError(
  size_t   timeStep,
  const std::string& msg) const
{
  posError(runtimeErrorFmt(timeStep,msg));
}

//! textual description of its position
std::string calc::ASTNode::shortPosText() const
{
  return position()->shortText();
}

const calc::Position *calc::ASTNode::position() const
{
  DEVELOP_PRECOND(d_pos);
  return d_pos;
}

void calc::ASTNode::posError(const std::string& msg) const
{
  d_pos->throwError(msg);
}

void calc::ASTNode::posError(const std::ostringstream& msg) const
{
  posError(msg.str());
}

//! returns the nr of results/types when executed
/*!
 * typically 1 for normal expressions, numbers (ASTNumber) and parameters and
 * runtime determined for ObjectLink stuff.
 */
size_t calc::ASTNode::nrReturns() const
{
  return d_returnDataType.size();
}

//! set the nr of (data) types returned and init them all to default DataType ctor
void calc::ASTNode::setNrReturns(size_t n)
{
  d_returnDataType.clear();
  d_returnDataType.resize(n);
}

calc::DataType& calc::ASTNode::returnDataType(size_t i)
{
  PRECOND(i < d_returnDataType.size());
  return d_returnDataType[i];
}

const calc::DataType& calc::ASTNode::returnDataType(size_t i) const
{
  PRECOND(i < d_returnDataType.size());
  return d_returnDataType[i];
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//! return RUNTIME, time identification and msg
std::string calc::runtimeErrorFmt(
  size_t   timeStep,
  const std::string& msg)
{
  std::ostringstream m;
  m << "\n" << "RUNTIME";
  if (timeStep >= 1)
    m << " (at timestep " << timeStep << ")";
  m << " " << msg;
  return m.str();
}
