#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEOM_PRDIMTREE
#include "geom_prdimtree.h"
#define INCLUDED_GEOM_PRDIMTREE
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif

// Module headers.
#ifndef INCLUDED_GEOM_PROXIMITYSEARCH
#include "geom_proximitysearch.h"
#define INCLUDED_GEOM_PROXIMITYSEARCH
#endif



/*!
  \file
  This file contains the implementation of the PRDimTree class.
*/

namespace geom {
template<class Point,class Elem, class KA>
  struct PRDimElemCloser: public geo::Closer<Point> {
   PRDimElemCloser(const Point& closerTo):
     geo::Closer<Point>(closerTo) {};
    bool operator() (const Elem &p1,const Elem& p2) const
     { return geo::Closer<Point>::operator()(KA::key(p1),KA::key(p2)); }
  };
template<class Point,class Elem, class KA>
  struct PRDimElemEqual: public std::unary_function<Elem, bool> {
     const Point& d_eqTo;
   PRDimElemEqual(const Point& eqTo):d_eqTo(eqTo){};
    bool operator() (const Elem& e) const
     { return geo::operator==(d_eqTo,KA::key(e));  }
  };
}




//------------------------------------------------------------------------------
// DEFINITION OF STATIC PRDIMTREE MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF PRDIMTREE MEMBERS
//------------------------------------------------------------------------------

/*!
 * \todo
 *  see  insert how to create new top nodes, this elliminates
 *  \a center and \a halfWidth. But a hint if the data is merely
 *  used on a grid on an grid corner X,Y and cell size is nice, so
 *  the regions allign nicely to the grid.
 * \todo
 *   FTTB fixed initial rectangle
 * \todo
 *   Coordinate allignment=quad corner as multiple of X or 1/X of
 *   that allignment, Zoek uit is dat 1 km voor MD-data?
 */
template <typename T>
geom::PRDimTree<T>::PRDimTree(
    const typename T::Point&      centre,
          typename T::Coordinate  halfWidth,
          size_t                  bucketSize):
     d_bucketSize(bucketSize)
{
  typename T::Region region(centre,halfWidth);
  d_root = new PRInternalNode<T>(region,this);
}



template <typename T>
geom::PRDimTree<T>::~PRDimTree()
{
  delete d_root;
}

/*!
   I did have the assumption that points outside the Square defined by centre
   and halfWidth, can be inserted and will live in the nearest region. Could be
   but inserting may split nodes indefitenely

   \todo
     If not contained create new root and let old root be one of its quadrants, then reinsert
   \returns true if inserted, false if not because of outside
 */
template <typename T>
bool geom::PRDimTree<T>::insert(const Element& e)
{
  if (!d_root->region().contains(KeyAccess::key(e)))
    return false;

  d_root->insert(e);
  return true;
}

//! count number of points equal to p
template <typename T>
size_t geom::PRDimTree<T>::count(const typename T::Point& p) const
{
  return d_root->count(p);
}

//! get value of bucketSize
template <typename T>
size_t geom::PRDimTree<T>::bucketSize() const
{
  return d_bucketSize;
}

template <typename T>
size_t geom::PRLeafNode<T>::count(const typename T::Point& p) const
{
  const PRDimElemEqual<Point,Element,KeyAccess> eqTo(p);
  // std::count(d_bucket.begin(),d_bucket.end(),eqTo);
  size_t count=0;
 for (BucketIter i=d_bucket.begin(); i!=d_bucket.end(); ++i)
    count +=eqTo(*i);
  return count;
}

//! perform a proximity search
/*!
 * \param d radius, points with distance <= d are returned
 *
 * \todo
 *   start with PRDimNode that p in it and try to satisfy ps.maxNr()
 *   within there, if p.distance met maxNr < distance to Node border
 *   than ready
 */
template <typename T>
void geom::PRDimTree<T>::proximitySearch(
  std::vector<Element>& result,
  const Point& p, const ProximitySearch& ps) const
{
  result.clear();
  d_root->proximitySearch(result,p,ps);

  if (ps.minNr() > result.size()) {
    result.clear();
    POSTCOND(result.empty());
  }

  if (ps.maxNr() < result.size()) {
    PRDimElemCloser<Point,Element,KeyAccess> closeTo(p);
    std::nth_element(result.begin(),
        result.begin()+ps.maxNr()-1,result.end(),closeTo);
    result.resize(ps.maxNr());
  }
}

template <typename T>
geom::PRDimTreeStats geom::PRDimTree<T>::stats() const
{
  return d_root->stats();
}

template <typename T>
const typename T::Point& geom::PRDimTree<T>::centre() const
{
  return d_root->centre();
}


//! always create with 4 leafs
template <typename T>
geom::PRInternalNode<T>::PRInternalNode(
    const Region& region,
    const PRDimTree<T> *partOf):
  PRDimNode<T>(region,partOf),
  d_son(1<<T::dim) // this number of sons
{
  for(size_t i=0; i<d_son.size(); i++)
    d_son[i]=new PRLeafNode<T>(region.quadSquareAt(i),partOf);
}

template <typename T>
geom::PRInternalNode<T>::~PRInternalNode()
{
  com::forWhole(d_son,com::Delete<PRDimNode<T> >());
}

template <typename T>
geom::PRDimNode<T> *geom::PRInternalNode<T>::insert(const typename T::Element& p)
{
  size_t i=key(p).indexDirection(this->centre());
  PRDimNode<T> *insertedIn = d_son[i]->insert(p);
  if (insertedIn != d_son[i]) {
     // insertion caused split
     delete d_son[i];
     d_son[i]=insertedIn;
  }
  return this;
}

template <typename T>
size_t geom::PRInternalNode<T>::count(const typename T::Point& p) const
{
  size_t i=p.indexDirection(this->centre());
  return d_son[i]->count(p);
}

template <typename T>
void geom::PRInternalNode<T>::proximitySearch(
  std::vector<Element>& result,
  const Point& p, const ProximitySearch& ps) const
{
  Region rs(p,ps.radius());
  for(size_t i=0; i < d_son.size(); ++i) {
    if (d_son[i]->region().intersects(rs))
      d_son[i]->proximitySearch(result,p,ps);
  }
}

template <typename T>
geom::PRDimTreeStats geom::PRInternalNode<T>::stats() const
{
  PRDimTreeStats s;
  s.nrInternalNodes++;
  for(size_t i=0; i < d_son.size(); ++i)
    s += d_son[i]->stats();
  return s;
}

/*!
 * \todo
 *   make bucket with run-length encoded pointers.
 *   a=allignment, s=sizeof(struct), bla bla
 */
template <typename T>
geom::PRLeafNode<T>::PRLeafNode(
    const typename T::Region& region,
    const PRDimTree<T> *partOf):
  PRDimNode<T>(region,partOf),
  d_allEqual(true)
{
}

template <typename T>
geom::PRLeafNode<T>::~PRLeafNode()
{
}

/*!
 * \todo
 *   the case where all points are identical. Policy? do not insert identical
 *   point OR allow the bucket to grow larger than the fixed bucket size.
 *   The latter is the current implementation.
 *   If nothing is done then insertion will crash on splitting infinite.
 * \todo
 *   forbid splitting if halfWidth goes to 1/inf. special case for integer
 *   and float.
 */
template <typename T>
geom::PRDimNode<T> *geom::PRLeafNode<T>::insert(const typename T::Element& p)
{
#ifdef DEBUG_DEVELOP
  /* rounding errors requires this, use contains carefull
   * in the code, indexDirection is better since it is not a
   * containment
   */
  Region withTolerance(this->region());
  withTolerance.setHalfWidth(this->region().halfWidth() *1.0001);
  DEVELOP_PRECOND(withTolerance.contains(key(p)));
#endif

  // use the capacity as the bucketSize
  // but only when inserting: empy Leaf must not occupy space
  // if(d_bucket.empty())
  //   d_bucket.reserve(d_partOf->bucketSize());
  // but RLPtrVector does not have reserve

  // can insert
  if (this->d_partOf->bucketSize() > d_bucket.size()) {
    d_bucket.push_back(p);
    d_allEqual = d_allEqual && key(p) == key(*d_bucket.begin());
    return this;
  }
  // overflow but all equal, just add
  if (d_allEqual && key(p) == key(*d_bucket.begin())) {
    d_bucket.push_back(p);
    return this;
  }

  // overflow
  // insert existing elements in new PRInternalNode
  PRInternalNode<T> *n=new PRInternalNode<T>(this->region(), this->d_partOf);
  for (BucketIter i=d_bucket.begin(); i!=d_bucket.end(); ++i)
    n->insert(*i);

  // insert new node
  n->insert(p);
  return n;
}

template <typename T>
void geom::PRLeafNode<T>::proximitySearch(
  std::vector<Element>& result,
  const Point& p, const ProximitySearch& ps) const
{
  if(ps.circularRadius()) {
    double d2=ps.radius();
    d2*=d2;
    for (BucketIter i=d_bucket.begin(); i!=d_bucket.end(); ++i)
     if (key(*i).squaredDistance(p) <= d2)
      result.push_back(*i);
  } else {
    PRECOND(ps.squaredRadius());
    Region sq(p,ps.radius());
    for (BucketIter i=d_bucket.begin(); i!=d_bucket.end(); ++i)
      if (sq.contains(key(*i)))
        result.push_back(*i);
  }
}

template <typename T>
geom::PRDimTreeStats geom::PRLeafNode<T>::stats() const
{
  PRDimTreeStats s;
  // skipped empty leaf nodes
  if (d_bucket.size()) {
    s.nrLeafNodes=1;
    s.nrElements=d_bucket.size();
    s.memSize   =d_bucket.capacity();
    /* ptr distance count PSEUDO CODE
       for(i=0; i < d_bucket.size();)  {
         size_t j=0;
         while (d_bucket[i].ptr()+j == d_bucket[i+j].ptr())
           j++;
         count[j]++;
         i+=j;
       }
     */
  }
  return s;
}

