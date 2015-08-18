#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTNODECONTAINER
#include "calc_astnodecontainer.h"
#define INCLUDED_CALC_ASTNODECONTAINER
#endif

#ifndef INCLUDED_CALC_ASTNODEVECTOR
#include "calc_astnodevector.h"
#define INCLUDED_CALC_ASTNODEVECTOR
#endif

#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif
// Library headers.
#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
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
  This file contains the implementation of the ASTNodeContainer class.
*/



//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTNODECONTAINER MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF ASTNODECONTAINER MEMBERS
//------------------------------------------------------------------------------

#define TEMPLATE_DECL template<typename S>

TEMPLATE_DECL
calc::ASTNodeContainer<S>::ASTNodeContainer()
{
}



//! Copy constructor.
TEMPLATE_DECL
calc::ASTNodeContainer<S>::ASTNodeContainer(ASTNodeContainer const& rhs)
  :ASTNode(rhs)
{
  deepClone(rhs.d_container);
}


TEMPLATE_DECL
calc::ASTNodeContainer<S>::~ASTNodeContainer()
{
  com::forWhole(d_container,com::Delete<ASTNode>());
}

TEMPLATE_DECL
void calc::ASTNodeContainer<S>::clear()
{
  com::forWhole(d_container,com::Delete<ASTNode>());
  d_container.clear();
}

TEMPLATE_DECL
void calc::ASTNodeContainer<S>::deepClone(const AC& src)
{
    std::transform(src.begin(),src.end(), std::back_inserter(d_container),
        boost::mem_fn(&ASTNode::createClone));
}


//! Assignment operator.
TEMPLATE_DECL
 calc::ASTNodeContainer<S>&
  calc::ASTNodeContainer<S>::operator=(ASTNodeContainer<S> const& rhs)
{
  if (this != &rhs) {
    clear();
    deepClone(rhs.d_container);
  }
  return *this;
}

TEMPLATE_DECL
void calc::ASTNodeContainer<S>::accept(ASTVisitor& v)
{
  visitAll(v);
}

TEMPLATE_DECL
void calc::ASTNodeContainer<S>::visitAll(ASTVisitor& v)
{
  com::forWhole(d_container,
      boost::bind(&ASTNode::accept,_1,boost::ref(v)));
}

TEMPLATE_DECL
void calc::ASTNodeContainer<S>::transferPushBack(ASTNode *n)
{
  d_container.push_back(n);
}

TEMPLATE_DECL
void calc::ASTNodeContainer<S>::transferPushFront(ASTNode *n)
{
  d_container.push_front(n);
}

TEMPLATE_DECL
size_t calc::ASTNodeContainer<S>::size() const
{
  return d_container.size();
}

TEMPLATE_DECL
typename calc::ASTNodeContainer<S>::const_iterator
 calc::ASTNodeContainer<S>::begin() const
{
  return d_container.begin();
}

TEMPLATE_DECL
typename calc::ASTNodeContainer<S>::const_iterator
 calc::ASTNodeContainer<S>::end() const
{
  return d_container.end();
}

typedef std::deque<calc::ASTNode *> Deque;
typedef std::list<calc::ASTNode *>  List;
template class calc::ASTNodeContainer<Deque>;
template class calc::ASTNodeContainer<List>;

calc::ASTNodeVector::ASTNodeVector()
{
}

calc::ASTNodeVector::~ASTNodeVector()
{
}

//! Assignment operator.
calc::ASTNodeVector& calc::ASTNodeVector::operator=(ASTNodeVector const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

calc::ASTNodeVector::ASTNodeVector(const ASTNodeVector& rhs):
  ASTNodeContainer<Deque>(rhs)
{
}


calc::ASTNodeVector* calc::ASTNodeVector::createClone() const
{
  return new ASTNodeVector(*this);
}


//! release all contents into return argument
/*!
 * void calc::ASTExpr::transferFunctionArgs(ASTNodeVector *al)
 * is the only one that needs this
 */
std::deque<calc::ASTNode *> calc::ASTNodeVector::release()
{
  AC l=d_container;
  d_container.clear();
  return l;
}

/*
size_t calc::ASTNodeVector::size() const
{
  return d_container.size();
}
*/

calc::ASTNode* calc::ASTNodeVector::operator[](size_t i) const
{
  return at(i);
}

calc::ASTNode* calc::ASTNodeVector::at(size_t i) const
{
  PRECOND(i < d_container.size());
  return d_container[i];
}

calc::ASTNodeList::ASTNodeList()
{
}



//! Copy constructor.
calc::ASTNodeList::ASTNodeList(ASTNodeList const& rhs):
  ASTNodeContainer<List>(rhs)
{
}



calc::ASTNodeList::~ASTNodeList()
{
}



//! Assignment operator.
calc::ASTNodeList& calc::ASTNodeList::operator=(ASTNodeList const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

calc::ASTNodeList::iterator calc::ASTNodeList::begin() {
   return d_container.begin();
}
calc::ASTNodeList::iterator calc::ASTNodeList::end() {
   return d_container.end();
}

void calc::ASTNodeList::accept(ASTVisitor& v)
{
  v.visitNodeList(this);
}

calc::ASTNodeList::const_iterator calc::ASTNodeList::begin() const
{
  return ASTNodeContainer<List>::begin();
}
calc::ASTNodeList::const_iterator calc::ASTNodeList::end() const
{
  return ASTNodeContainer<List>::end();
}


//! replace [begin,end) with by
/*!
 * Nodes are not deleted, replace only in support of PointCodeBlock
 */
void calc::ASTNodeList::replace(ASTNode *by, iterator begin, iterator end)
{
  iterator i=d_container.erase(begin,end);
  d_container.insert(i,by);
}


calc::ASTNodeList* calc::ASTNodeList::createClone() const
{
  return new ASTNodeList(*this);
}
