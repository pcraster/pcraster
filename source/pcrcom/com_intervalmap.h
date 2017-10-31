#ifndef INCLUDED_COM_INTERVALMAP
#define INCLUDED_COM_INTERVALMAP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif


namespace com {
  // IntervalMap declarations.
}



namespace com {

namespace intervalMap {
 template<typename R>
  class PartitionFO : public std::unary_function<R, bool> {
    const com::Interval<R> *d_i;
    public:
      PartitionFO(const com::Interval<R> *i):
        d_i(i) {};
      bool operator()(R v) const {
        return d_i->valid(v);
      }
  };
  class IntervalPtrLess {
   public:
   template<typename R>
    bool operator() (
        const Interval<R> *i1,
        const Interval<R> *i2) const
    {
      return i1->less(*i2);
    }
  };
  template<class IntervalMapT>
    bool insertIntervals(
        IntervalMapT &m,
        const std::vector<const com::Interval< 
                 typename IntervalMapT::IT> * >& v) {
      for(size_t i=0; i< v.size(); ++i)
        if (!m.insertInterval(*v[i]))
          return false;
      return true;
    }
}

//! a map of class Interval
/*!
 * a number of distinct/non overlapping intervals can be inserted using
 * insertInterval() and then recalled by findValue().
 *
 * Since the intervals are not overlapping the intervals are also fully
 * ordered.
 */
template<class T,
         typename R=double>
 class IntervalMap:
  public std::map<const Interval<R> *,T,intervalMap::IntervalPtrLess>
{

public:
  typedef T                                       ValueFO;
  //! interval type
  typedef R                                       IT;
  typedef Interval<R>                             IV;
  typedef typename IntervalMap<T,R>::iterator       iterator;
  typedef typename IntervalMap<T,R>::const_iterator const_iterator;
private:
  typedef std::map<const IV *,T,intervalMap::IntervalPtrLess> Base;

  T      d_outside;
  size_t d_nrVisits;

  void clean() {
    for(iterator i=this->begin();i!=this->end();++i)
     delete i->first;
  }

  void copy(const IntervalMap<T>& rhs) {
    d_nrVisits=rhs.d_nrVisits;
    for(const_iterator i=rhs.begin();i!=rhs.end();++i)
      insert(std::make_pair(i->first->createClone(),i->second));
  }
  struct NotInInterval:public std::unary_function<IT, bool> 
  {
    const IntervalMap<T,R> &d_m;
    NotInInterval(const IntervalMap<T,R>& m):
      d_m(m) {};
    public:
      bool operator()(IT v) const {
         EqualTo<IT> key(v);
         const_iterator p(d_m.find(&key));
         return p == d_m.end();
      }
  };
 public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  IntervalMap():
     d_outside(),
     d_nrVisits(0)
     {}

  virtual ~IntervalMap() {
    clean();
  }

  //! Assignment operator.
  IntervalMap<T>& operator=(const IntervalMap<T>& rhs) {
    if (this != &rhs) {
      clear();
      copy(rhs);
    }
    return *this;
  }

  //! Copy constructor.
  IntervalMap(const IntervalMap<T>& rhs):
    Base(),
    d_outside(),
    d_nrVisits(0)
  {
      copy(rhs);
  }

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! insert a copy of interval into map, default ctor T
  /*!
   * An interval that already overlaps with an inserted interval
   * is rejected.
   * \returns whether i is inserted.
   */
  bool insertInterval(
         const IV& i)
  {
    IV *c=i.createClone();
    std::pair<iterator,bool> p=
     this->insert(std::make_pair(c,T()));
    if (!p.second)
      delete c;
    return p.second;
  }
  //! insert a series of intervals at once
  /*!
   * \return true if all inserted, false otherwise
   */
  bool insertIntervals(const std::vector<const IV *>& v) {
    return intervalMap::insertIntervals(*this,v);
  }

  //! find value T of interval that includes \a k
  /*! always returns a valid reference to T, if no interval is found
   *  outside() is returned.
   */
  T& findValue(IT k) {
    EqualTo<IT> key(k);
    iterator p = this->find(&key);
    if (p==this->end())
      return d_outside;
    // std::cout << "found at " << *(p->first) << "\n";
    DEVELOP_POSTCOND(p->first->valid(k));
    return p->second;
  }

  //! key/value are equal
  void visit(IT kv) {
    visit(kv,kv);
  }

  //! key/value are not identical
  void visit(IT k, IT v) {
    d_nrVisits++;
    findValue(k)(v);
  }

  void clear() {
    d_nrVisits=0;
    clean();
    Base::clear();
  }

  T& outside() {
    return d_outside;
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool distinct() const {
    return true;
  }


  const T& findValue(IT k) const {
    EqualTo<IT> key(k);
    const_iterator p = find(&key);
    if (p==this->end())
      return d_outside;
    DEVELOP_POSTCOND(p->first->valid(k));
    return p->second;
  }
  const T& outside() const {
    return d_outside;
  }
  size_t nrVisits() const {
    return d_nrVisits;
  }

  /*!
     \brief partition range [beginR,endR) on belonging to interval \a i
   */
  template<typename Iter,
           typename GetValueOp>
  Iter partition(Iter beginR, Iter endR, const_iterator i, GetValueOp op) const {
     intervalMap::PartitionFO<IT> fo(i->first);
     return std::partition(beginR,endR,boost::bind(fo,boost::bind(op,_1)));
  }

  /*!
     \brief partition range [beginR,endR) on not belonging to any interval
   */
  template<typename Iter,
           typename GetValueOp>
  Iter partitionOutside(Iter beginR, Iter endR, GetValueOp op) const {
     NotInInterval nii(*this);
     return std::partition(beginR,endR,
           boost::bind(nii,boost::bind(op,_1)));
  }

  void print(std::ostream& s) const {
    for(const_iterator i=this->begin();i!=this->end();++i)
      s << *(i->first) << " " << i->second << "\n";
  }
};

//! a multimap of class Interval
/*!
 * Implementatie als std::multimap werkte niet, intervallen zijn niet
 * eenduidig te sorteren als intervallen overlappen, echte oplossing is
 * denk ik (CW) te vinden in quadtree bijbel
 * Deze "MultiMap" is gewoon een lineaire search.
 */
template<class T,
         typename R=double>
 class IntervalMultiMap:
 public std::vector<std::pair<const Interval<R> *,T > >
{
public:
  typedef R                                       IT;
  typedef Interval<R>                             IV;
  typedef typename IntervalMultiMap<T,R>::iterator       iterator;
  typedef typename IntervalMultiMap<T,R>::const_iterator const_iterator;
  typedef std::pair<const IV *,T&>                     Pair;
private:
  typedef std::vector<std::pair<const IV *,T > > Base;

  T      d_outside;
  size_t d_nrVisits;

  void clean() {
    for(iterator i=this->begin();i!=this->end();++i)
     delete i->first;
  }
  void copy(const IntervalMultiMap<T>& rhs) {
    d_nrVisits=rhs.d_nrVisits;
    for(const_iterator i=rhs.begin();i!=rhs.end();++i)
     push_back(std::make_pair(i->first->createClone(),i->second));
  }

  struct NotInInterval:
      public std::unary_function<IT, bool>
  {
    const IntervalMultiMap<T,R>* d_m;
    NotInInterval(const IntervalMultiMap<T,R>& m):
      d_m(&m) {};
    public:
      bool operator()(IT v) const {
       for(typename Base::const_iterator i=d_m->begin();i!=d_m->end();++i)
          if (i->first->valid(v))
            return false;
       return true;
      }
  };

 public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  IntervalMultiMap():
     d_outside(),
     d_nrVisits(0)
     {}

  virtual ~IntervalMultiMap() {
    clean();
  }

  //! Assignment operator.
  IntervalMultiMap<T>& operator=(const IntervalMultiMap<T>& rhs) {
    if (this != &rhs) {
      clear();
      copy(rhs);
    }
    return *this;
  }

  //! Copy constructor.
  IntervalMultiMap(const IntervalMultiMap<T>& rhs):
   Base()
  {
      copy(rhs);
  }


  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! insert a copy of interval into multimap, default ctor T
  bool insertInterval(
         const IV& i)
  {
    IV *c=i.createClone();
    this->push_back(std::make_pair(c,T()));
    return true; // always success in a multimap
  }
  //! insert a series of intervals at once
  /*!
   * \return true if all inserted, false otherwise
   */
  bool insertIntervals(const std::vector<const IV *>& v) {
    return intervalMap::insertIntervals(*this,v);
  }

  //! T is a function object, call () for all T's maching \a v
  /*! if none are matching the d_outside is called
   */
  void visit(IT k,IT v) {
      d_nrVisits++;
      bool intervalFound=false;
      for(iterator i=this->begin();i!=this->end();++i) {
          if (i->first->valid(k)) {
            intervalFound=true;
            i->second(v);
          }
        }
      if (!intervalFound)
        d_outside(v);
  }

  //! T is a function object, call visit() for all T's maching \a v
  /*! if none are matching the d_outside is called
   */
  void visit2(IT k,IT v) {
      d_nrVisits++;
      bool intervalFound=false;
      for(iterator i=this->begin();i!=this->end();++i) {
          if (i->first->valid(k)) {
            intervalFound=true;
            i->second.visit(v);
          }
        }
      if (!intervalFound)
        d_outside.visit(v);
  }

  //! key/value are identical
  void visit(IT kv) {
    visit(kv,kv);
  }

  void clear() {
    clean();
    Base::clear();
  }

  T& outside() {
    return d_outside;
  }

  /*!
     \brief partition range [beginR,endR) on belonging to interval \a i
   */
  template<typename Iter,
           typename GetValueOp>
  Iter partition(Iter beginR, Iter endR, const_iterator i, GetValueOp op) const {
     intervalMap::PartitionFO<IT> fo(i->first);
     return std::partition(beginR,endR,boost::bind(fo,boost::bind(op,_1)));
  }

  /*!
     \brief partition range [beginR,endR) on not belonging to any interval
   */
  template<typename Iter,
           typename GetValueOp>
  Iter partitionOutside(Iter beginR, Iter endR, GetValueOp op) const {
     NotInInterval nii(*this);
     return std::partition(beginR,endR,
           boost::bind(nii,boost::bind(op,_1)));
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool distinct() const {
    return false;
  }

  const T& outside() const {
    return d_outside;
  }
  size_t nrVisits() const {
    return d_nrVisits;
  }
  void print(std::ostream& s) const {
    for(const_iterator i=this->begin();i!=this->end();++i)
      s << *(i->first) << " " << i->second << "\n";
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

template <typename R>
 bool noOverlap(const std::vector<const Interval<R> *>& v);


} // namespace com

#endif
