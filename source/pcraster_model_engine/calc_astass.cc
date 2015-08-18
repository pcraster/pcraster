#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_CLONE
#include "com_clone.h"
#define INCLUDED_COM_CLONE
#endif

// Module headers.
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif

/*!
  \file
  This file contains the implementation of the ASTAss class.
*/



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTASS MEMBERS
//------------------------------------------------------------------------------

calc::ASTAss::ASTAss():
   d_rhs(0)
{
}

//! ctor with full deepClone
calc::ASTAss::ASTAss(const ASTPar&  par,
                     const ASTNode* rhs):
   d_rhs(0)
{
  addPar(par);
  setRhs(rhs);
}

calc::ASTAss::~ASTAss()
{
  clean();
}

void calc::ASTAss::clean()
{
  com::clearClone(d_pars);
  delete d_rhs;
  d_rhs=0;
}

//! Assignment operator.
calc::ASTAss& calc::ASTAss::operator=(const ASTAss& rhs)
{
  if (this != &rhs) {
    clean();
    setRhs(rhs.rhs());
    com::copyClone(rhs.d_pars,d_pars);
  }
  return *this;
}

//! Copy constructor.
calc::ASTAss::ASTAss(const ASTAss& rhs):
  ASTNode(rhs),
  d_rhs(rhs.rhs()->createClone())
{
  com::copyClone(rhs.d_pars,d_pars);
}

void calc::ASTAss::accept(ASTVisitor& v)
{
  v.visitAss(this);
}

const std::vector<calc::ASTPar *>& calc::ASTAss::pars() const
{
  return d_pars;
}

//! return parameter \a parIndex
calc::ASTPar* calc::ASTAss::par(size_t parIndex) const
{
  PRECOND(parIndex < d_pars.size());
  return d_pars[parIndex];
}

//! return parameters
size_t calc::ASTAss::nrPars() const
{
  return d_pars.size();
}

calc::ASTNode * calc::ASTAss::rhs() const
{
  return d_rhs;
}

//! add copy of \a p to parameters
/*!
 * if d_pars empty on forehand then set position
 */
void calc::ASTAss::addPar(const ASTPar& p)
{
  if (d_pars.empty())
    setPosition(p.position()); // assignment starts with 1st par
  d_pars.push_back(p.createClone());
}

/*!
 * \brief only needed to parse the relic m1,m2 = fid1,fid2(....)
 * see calcparser.g (/relic)
 */
void calc::ASTAss::swap01() {
  PRECOND(d_pars.size()==2);
  std::swap(d_pars[0],d_pars[1]);
}

//! transfer rhs
void calc::ASTAss::transferRhs(ASTNode* rhs)
{
  if (d_rhs)
      delete d_rhs;
  d_rhs=rhs;
}

void calc::ASTAss::setRhs(const ASTNode* rhs)
{
  transferRhs(rhs->createClone());
}

calc::ASTAss* calc::ASTAss::createClone() const
{
  return new ASTAss(*this);
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



