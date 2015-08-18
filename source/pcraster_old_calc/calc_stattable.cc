
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_STATTABLE
#include "calc_stattable.h"
#define INCLUDED_CALC_STATTABLE
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_INTERVALMAP
#include "com_intervalmap.h"
#define INCLUDED_COM_INTERVALMAP
#endif
#ifndef INCLUDED_COM_CLONE
#include "com_clone.h"
#define INCLUDED_COM_CLONE
#endif
#ifndef INCLUDED_TAB_CLASSCOUNTMAP
#include "tab_classcountmap.h"
#define INCLUDED_TAB_CLASSCOUNTMAP
#endif
#ifndef INCLUDED_TAB_CLASSCLASSCOUNTMAP
#include "tab_classclasscountmap.h"
#define INCLUDED_TAB_CLASSCLASSCOUNTMAP
#endif
#ifndef INCLUDED_TAB_CLASSINTERVALMAP
#include "tab_classintervalmap.h"
#define INCLUDED_TAB_CLASSINTERVALMAP
#endif
#ifndef INCLUDED_COM_MVGENERIC
#include "com_mvgeneric.h"
#define INCLUDED_COM_MVGENERIC
#endif
#ifndef INCLUDED_COM_STATISTICS
#include "com_statistics.h"
#define INCLUDED_COM_STATISTICS
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_API
#include "api.h"
#define INCLUDED_API
#endif
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif
// Module headers.
#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
# include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif



/*!
  \file
  This file contains the implementation of the StatTable class.
*/



//------------------------------------------------------------------------------

calc::StatTable::InputMap::InputMap():
  d_expr(0)
{
}

calc::StatTable::InputMap::~InputMap() {
  com::clearClone(d_intervals);
  delete d_expr;
}

void calc::StatTable::InputMap::swap(InputMap& rhs)
{
  // implements here swap instead of = and copy ctor
  std::swap(d_expr,     rhs.d_expr);
  std::swap(d_intervals,rhs.d_intervals);
  std::swap(d_name,     rhs.d_name);
}

//    InputMap& operator=(const InputMap& rhs);

bool calc::StatTable::InputMap::defined() const {
  return d_expr != 0;
}

bool calc::StatTable::InputMap::hasIntervals() const {
  return !d_intervals.empty();
}

CSF_CR calc::StatTable::InputMap::cr() const {
  PRECOND(defined());
  return biggestCellRepr(d_expr->vs());
}

void calc::StatTable::InputMap::verbosePrint(
    std::ofstream& out) const
{
  if (!hasIntervals()) {
    out << "\tniet opgegeven\n";
  } else {
    out << "\n";
    for (size_t i=0; i!=d_intervals.size();++i) {
      out << *d_intervals[i] << "\n";
    }
    out << "\n";
  }
}

namespace calc {
  namespace detail {
    //! Class Scalar Cross Pair
    struct CS_CP {
      union {
       INT4    d_subjectI;
        REAL4  d_subjectR;
      };
      REAL4    d_cross;

      CS_CP(INT4  subject,REAL4 cross):
        d_subjectI(subject),d_cross(cross)
        {}
      CS_CP(REAL4 subject,REAL4 cross):
        d_subjectR(subject),d_cross(cross)
        {}
      REAL4  subjectR() const {
        return d_subjectR;
      };
      bool subjectRLess(const CS_CP& rhs) const {
        return d_subjectR < rhs.d_subjectR;
      };
      bool subjectILess(const CS_CP& rhs) const {
        return d_subjectI < rhs.d_subjectI;
      }
      INT4   subjectI() const {
        return d_subjectI;
      }
      bool crossLess(const CS_CP& rhs) const {
        return d_cross < rhs.d_cross;
      }
      REAL4  cross() const {
        return d_cross;
      }
     private:
      //! very confusing with 3 value members to compare!
      bool operator<(const CS_CP& rhs) const;
        // was -> return subjectILess(rhs);

    };
/*
    //! Scalar Scalar Cross Pair
    struct SS_CP : public CS_CP {
      SS_CP(REAL4 subject,REAL4 cross):
        CS_CP(subject,cross)
        {};
     private:
      //! very confusing with 3 value members to compare!
      bool operator<(const SS_CP& rhs) const;
        // was -> return subjectILess(rhs);
    };
//  typedef std::vector<SS_CP> SS_V;
*/

  typedef std::vector<CS_CP> CS_V;

  struct CSPolicy {
    typedef CS_CP CP;
    typedef CS_V  V;
    static inline REAL4   partitionValue(const CP& v) {
      return v.cross();
    }
  };
  struct Kruis3Policy {
    typedef CS_CP CP;
    typedef CS_V  V;
    static inline REAL4   partitionValue(const CP& v) {
      return v.subjectR();
    }
  };
  struct GGPolicy {
    typedef CS_CP CP;
    typedef CS_V  V;
    static inline REAL4   partitionValueFirst(const CP& v) {
      return v.subjectR();
    }
    static inline REAL4   partitionValue(const CP& v) {
      return v.cross();
    }
  };

  struct ScalarStats : public com::AverageSdMinMax<REAL4> {
      REAL4 d_med;
      ScalarStats():
        d_med(0) {};

     template<typename KeyT>
     void printLine(
         const KeyT&   key,
         double        area,
         std::ostream& out) const {
           out << key;
           printLineCommon(area,out);
         }
     private:
        void printLineCommon(
            double        area,
            std::ostream& out) const {
           out << "\t" << area*nr();
           if (nr())
            out << "\t" << sum()
               << "\t" << minimum()
               << "\t" << maximum()
               << "\t" << average()
               << "\t" << sd()
               << "\t" << d_med;
           out << "\n";
         };
  };
  static ScalarStats scalarStats(
    const REAL4* begin , const REAL4*end)
  {
    std::vector<REAL4> r;
    r.reserve(std::distance(begin,end));
    detail::ScalarStats s;
    for(const REAL4 *i=begin; i != end; ++i) {
      if (!pcr::isMV(*i)) {
        r.push_back(*i);
        s(*i);
      }
    }
    s.d_med= *(com::median(r.begin(),r.end()));
    return s;
  }
} // namespace detail


static double median(
    detail::CS_V::iterator begin,
    detail::CS_V::iterator end)
{
  return com::median(begin,end,
   std::mem_fun_ref(&detail::CS_CP::crossLess))->cross();
}

template< class P,
          class M>
static void  crossPercentiles(
    M& m,
    typename P::V::iterator begin,
    typename P::V::iterator end)
{
  typedef typename P::V::iterator I;
  I endP;
  if (m.outside().nr()) {
    endP = m.partitionOutside(begin,end,P::partitionValue);
    m.outside().d_med=median(begin,endP);
    // what is outside is not in any interval
    begin=endP;
  }

  for (typename M::iterator i=m.begin();i!=m.end();++i) {
   switch(i->second.nr()) {
     case 0: break; // else d_med keeps 0
     case 1:
     case 2: i->second.d_med = i->second.maximum(); break;
     default:
       endP = m.partition(begin,end,i,P::partitionValue);
       i->second.d_med= median(begin,endP);
       // POSTCOND(std::distance(begin,endP)==(int)(i->second.nr()));
/* ONE INDEX OFF seems OK
       if(std::distance(begin,endP)!=(int)(i->second.nr())) {
        PRINT_VAR(std::distance(begin,endP));
        PRINT_VAR(i->second.nr());
       }
*/
   }
   // if intervals are distinct we can ignore the part just used
   if (m.distinct())
    begin+=i->second.nr();
  }
}

}

//------------------------------------------------------------------------------
// DEFINITION OF STATIC STATTABLE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STATTABLE MEMBERS
//------------------------------------------------------------------------------


calc::StatTable::StatTable(
  FieldExpr *subject):
  Statement(*subject),
  d_resultTable("statTable.txt"),
  d_verbose(false)
{
  d_subject.d_expr=subject;
  d_subject.d_name="subjectName";
    d_cross.d_name="crossName";
}

calc::StatTable::~StatTable()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::StatTable& calc::StatTable::operator=(const StatTable& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::StatTable::StatTable(const StatTable& rhs):
  Base(rhs)
{
}
*/
bool calc::StatTable::buildTypes()
{
  d_subject->buildTypesRecursive(VS_FIELD);
  if (d_cross.defined()) {
   d_cross->buildTypesRecursive(VS_FIELD);

   // Reverse if needed
   bool doSwap=false;
   if (d_subject.cr()==CR_REAL4) {
     if (d_cross.cr()!=CR_REAL4)
       doSwap=true; // r1,r2
     else
       if ( (!d_subject.hasIntervals()) && d_cross.hasIntervals())
       doSwap=true;//r3
   }
   if (doSwap)
    d_cross.swap(d_subject);
  }
  return false;
}

void calc::StatTable::prepareExecution()
{
  // in stacked order!
  if (d_cross.defined())
   d_cross->prepareExecution();

  d_subject->prepareExecution();
}


void calc::StatTable::open(std::ofstream& out) const
{
  com::open(out,d_resultTable);

  if (d_verbose) {
    out << "Naam bestand: " << d_resultTable << "\n";
    out << "Klassen/Grenzen Onderwerp:";
    d_subject.verbosePrint(out);
    if (d_cross.defined()) {
      out << "Klassen/Grenzen Indeling:";
      d_cross.verbosePrint(out);
    }
    out << "BEGIN\tWERKELIJKE\tTABEL:\n";
  }
}

void calc::StatTable::run()
{
  calc::FieldStack stack;

  // put cross below on stack
  if (d_cross.defined())
    d_cross->execute(stack);
  d_subject->execute(stack);

  // select on subject type
  switch(biggestCellRepr(d_subject->vs())) {
    case CR_INT4:
             classSubject<INT4>(stack);
             break;
    case CR_UINT1:
             classSubject<UINT1>(stack);
             break;
    case CR_REAL4:
             scalarSubject(stack);
             break;
    default:
         POSTCOND(FALSE);
  }
}


double calc::StatTable::area(
          size_t cellCount)
{
  return cellCount*Area();
}

void  calc::StatTable::setCross(FieldExpr *cross)
{
  d_cross.d_expr=cross;
}

//! set value of resultTable
void calc::StatTable::setResultTable(const com::PathName& resultTable)
{
  d_resultTable=resultTable;
}

template<typename T>
void calc::StatTable::classSubject(
    FieldStack& stack) const
{
  FieldHandle d(stack.popReadOnly());
  const T* s=(const T *)d->srcValue();

  if (!d_cross.defined())
    classTable(s,s+d->nrValues());
  else {
    switch(biggestCellRepr(d_cross->vs())) {
      case CR_INT4:  classCrossTable<T,INT4>(s,stack); break;
      case CR_UINT1: classCrossTable<T,UINT1>(s,stack); break;
      case CR_REAL4:
              if (!d_cross.hasIntervals())
                 classScalarTable(s,stack);
              else {
                if (com::noOverlap(d_cross.d_intervals)) {
                  typedef typename
                    com::IntervalMap< detail::ScalarStats, float > IM;
                     classIntervalTable<IM>(s,stack);
                } else {
                  typedef typename
                    com::IntervalMultiMap< detail::ScalarStats, float > IM;
                     classIntervalTable<IM>(s,stack);
                }
              }
              break;
      default:
         POSTCOND(FALSE);
    }
  }
}

template<typename T>
  void calc::StatTable::classTable(
      const T* begin, const T*end) const
{
  typedef tab::ClassCountMap<INT4> M;
  M m;
  addSubjectClasses(m);
  m=com::forEachNonMV(begin,end,m);

  std::ofstream out;
  open(out);
  out << d_subject.d_name << "\t" << "opp" << "\n";
  for(M::const_iterator i=m.begin(); i!=m.end();++i)
      out << i->first << "\t" << area(i->second) << "\n";
}

template<typename SubjectType, typename CrossType>
  void calc::StatTable::classCrossTable(
      const SubjectType* subject,
      FieldStack& stack) const
{
  typedef tab::ClassClassCountMap<INT4> M;
  M m;
  this->addCrossClasses(m);

  FieldHandle d(stack.popReadOnly());
  const CrossType* cross=(const CrossType *)d->srcValue();
  for(size_t i=0; i < d->nrValues(); ++i)
    if (!pcr::isMV(subject[i]) && !pcr::isMV(cross[i]))
      m(subject[i],cross[i]);

  std::ofstream out;
  open(out);

  crossHeader(out);

  // header line 2
  out << d_subject.d_name;
  typedef typename std::set<INT4> S;
  typedef typename S::const_iterator SI;
  S col=m.colClasses();
  S row=m.rowClasses();

  for(SI c=col.begin(); c!=col.end(); ++c)
   out << "\t" << *c;
  out << "\n";

  for(SI r=row.begin(); r!=row.end(); ++r) {
    out << *r;
    for(SI c=col.begin(); c!=col.end(); ++c)
     out << "\t" << area(m.getCount(*r,*c));
    out << "\n";
  }
}


void calc::StatTable::crossHeader(std::ostream& out) const
{
  if (d_cross.defined())
   out << "\t" << d_cross.d_name << "\n";
  else
   out << "\t" << d_subject.d_name << "\n";
}

void calc::StatTable::scalarCrossHeader(
    std::ostream& out,
    int  nrRowDescriptors) const
{
  crossHeader(out);
  out << d_subject.d_name;
  // print with intervening space
  //  do not allow empty between tab seperators
  if (nrRowDescriptors)
    out << "\t";
  for(int i=1;i<nrRowDescriptors;++i)
    out << " \t";
  out << "opp" << "\t" <<
         "som" << "\t" <<
         "min" << "\t" <<
         "max" << "\t" <<
         "gem" << "\t" <<
         "sd"  << "\t" <<
         "med" << "\n" ;
}

/*!
 * \todo
 *   Interval<float> can-not represent every integer precise
 */
template<typename CountMap>
 void calc::StatTable::addSubjectClasses(CountMap& m) const
{
  const Intervals& cl(d_subject.d_intervals);
  for(Intervals::const_iterator i=cl.begin(); i!=cl.end();++i) {
    const com::IntervalF& iv(**i);
    // normal case: PRECOND(iv.min()==iv.max());
    //  but we simple cast min to integer

    // MT() init 0, default Ctor
    typedef typename CountMap::mapped_type MT; 
    m.insert(std::make_pair(static_cast<int>(iv.min()),MT()));
  }
}

template<typename CountMap>
 void calc::StatTable::addCrossClasses(CountMap& m) const
{
  const Intervals& s(d_subject.d_intervals);
  const Intervals& c(d_cross.d_intervals);
  if (c.empty())
   for(size_t i=0; i <s.size();++i)
     m.addClass(static_cast<int>(s[i]->min()));
  else
   for(Intervals::const_iterator si=s.begin(); si!=s.end();++si)
    for(Intervals::const_iterator ci=c.begin(); ci!=c.end();++ci) {
     const com::IntervalF& siv(**si);
     PRECOND(siv.min()==siv.max());
     const com::IntervalF& civ(**ci);
     PRECOND(civ.min()==civ.max());
     m.addClass(static_cast<int>(siv.min()),
               static_cast<int>(civ.min()));
  }
}

//! d_cross is REAL4 and has intervals a.k.a. type KG
template< class    IntervalMapT,
          typename SubjectType>
  void calc::StatTable::classIntervalTable(
      const SubjectType* subject,
      FieldStack& stack) const
{
  PRECOND(d_cross.hasIntervals());

  FieldHandle d(stack.popReadOnly());
  const REAL4* cross=(const REAL4 *)d->srcValue();

  typedef detail::CS_CP   CP;
  detail::CS_V r;
  r.reserve(d->nrValues());

  typedef tab::ClassIntervalMap< IntervalMapT > M;
  M m(d_cross.d_intervals);
  addSubjectClasses(m);

  for(size_t i=0;i< d->nrValues(); i++)
   if (!pcr::isMV(subject[i]) && !pcr::isMV(cross[i])) {
      m.visit(subject[i],cross[i]);
      r.push_back(CP(subject[i],cross[i]));
    }

  // sort on subject for percentile computation
  std::sort(r.begin(),r.end(),std::mem_fun_ref(&CP::subjectILess));

  // compute percentiles per subject
  typedef detail::CS_V::iterator I;
  detail::CS_V::iterator start=r.begin();
  for(typename M::iterator i=m.begin(); i!= m.end(); ++i) {
    I end=start+i->second.nrVisits();
    crossPercentiles<detail::CSPolicy>(i->second,start,end);
    start=end;
  }

  std::ofstream out;
  open(out);
  scalarCrossHeader(out,2);
  for(typename M::const_iterator k=m.begin(); k!=m.end();++k) {
   for(typename IntervalMapT::const_iterator i=k->second.begin(); i!=k->second.end();++i) {
    out << k->first << "\t";
    i->second.printLine(*(i->first),area(1),out);
   }
   if (k->second.outside().nr()) {
    out << k->first << "\t";
    k->second.outside().printLine("anders",area(1),out);
   }
  }
}

//! type Kruis-3
template<class IntervalMapT>
 void calc::StatTable::scalarScalarTable(
      const REAL4* subject,
      FieldStack& stack,
      size_t      nrValues) const
{
  typedef detail::CS_CP   CP;
  typedef std::vector<CP> R;
  R r;
  r.reserve(nrValues);

  typedef IntervalMapT M;
  M m;
  m.insertIntervals(d_subject.d_intervals);
  POSTCOND(m.size()==d_subject.d_intervals.size());

  const REAL4* cross;
  std::auto_ptr<FieldHandle> d;

  if (!d_cross.defined())
    cross=subject;
  else {
    d.reset(new FieldHandle(stack.popReadOnly()));
    cross=(const REAL4 *)(*d)->srcValue();
  }

  for(size_t i=0;i< nrValues; i++) {
   if (!pcr::isMV(subject[i]) && !pcr::isMV(cross[i])) {
      m.visit(subject[i],cross[i]);
      r.push_back(CP(subject[i],cross[i]));
    }
  }

  crossPercentiles<detail::Kruis3Policy>(m,r.begin(),r.end());
  std::ofstream out;
  open(out);
  scalarCrossHeader(out);
  for(typename M::const_iterator i=m.begin(); i!=m.end();++i)
    i->second.printLine(*(i->first),area(1),out);
  if (m.outside().nr())
    m.outside().printLine("anders",area(1),out);
}

//! type GG
 void calc::StatTable::GGTable(
      const REAL4* subject,
      FieldStack& stack) const
{
  PRECOND(d_cross.cr()==CR_REAL4);

  FieldHandle d(stack.popReadOnly());
  const REAL4* cross=(const REAL4 *)d->srcValue();


  typedef detail::CS_CP   CP;
  typedef std::vector<CP> R;
  R r;
  r.reserve(d->nrValues());

  typedef com::IntervalMultiMap< detail::ScalarStats, float> MapKey;
  typedef com::IntervalMultiMap< MapKey, float>            M;

  M m;
  m.insertIntervals(d_subject.d_intervals);
  POSTCOND(m.size()==d_subject.d_intervals.size());
  for(M::iterator i=m.begin(); i!=m.end();++i)
    i->second.insertIntervals(d_cross.d_intervals);

  for(size_t i=0;i< d->nrValues(); i++)
   if (!pcr::isMV(subject[i]) && !pcr::isMV(cross[i])) {
      m.visit2(subject[i],cross[i]);
      r.push_back(CP(subject[i],cross[i]));
    }

  // what is outside is not in any interval
  R::iterator endOutside=
    m.partitionOutside(r.begin(),r.end(),
                       detail::GGPolicy::partitionValueFirst);
    crossPercentiles<detail::GGPolicy>(m.outside(),r.begin(),endOutside);

  for (M::iterator i=m.begin();i!=m.end();++i) {
   R::iterator endP=
    m.partition(endOutside,r.end(),i,
                       detail::GGPolicy::partitionValueFirst);
    crossPercentiles<detail::GGPolicy>(i->second,endOutside,endP);
  }

  std::ofstream out;
  open(out);
  scalarCrossHeader(out,2);
  for(M::const_iterator k=m.begin(); k!=m.end();++k) {
   for(MapKey::const_iterator i=k->second.begin(); i!=k->second.end();++i) {
    out << *k->first << "\t";
    i->second.printLine(*(i->first),area(1),out);
   }
   if (k->second.outside().nr()) {
    out << *k->first << "\t";
    k->second.outside().printLine("anders",area(1),out);
   }
  }

  const MapKey& mko(m.outside());
  if (mko.nrVisits()) {
   for(MapKey::const_iterator i=mko.begin(); i!=mko.end();++i) {
    out << "anders" << "\t";
    i->second.printLine(*(i->first),area(1),out);
   }
   if (mko.outside().nr()) {
    out << "anders" << "\t";
    mko.outside().printLine("anders",area(1),out);
   }
  }
}

template<typename T>
 void calc::StatTable::popSubject(
      FieldStack& stack) const
{
 if (d_maskSet) {
    calc::FieldHandle d(stack.popDest(d_subject->vs()));
    T * s=(T *)d->destValue();
    // mask out here
 } else {
   FieldHandle d(stack.popReadOnly());
   const T* s=(const T *)d->srcValue();
 }
}

//! d_cross is REAL4 and has no intervals
template<typename SubjectType>
  void calc::StatTable::classScalarTable(
      const SubjectType* subject,
      FieldStack& stack) const
{
  FieldHandle d(stack.popReadOnly());
  const REAL4* cross=(const REAL4 *)d->srcValue();

  typedef detail::CS_CP   CP;
  typedef std::vector<CP> R;
  R r;
  r.reserve(d->nrValues());

  typedef std::map<INT4,detail::ScalarStats> M;
  M m;
  addSubjectClasses(m);

  for(size_t i=0;i< d->nrValues(); i++)
   if (!pcr::isMV(subject[i]) && !pcr::isMV(cross[i])) {
      m[subject[i]](cross[i]);
      r.push_back(CP(subject[i],cross[i]));
    }

  // sort on subject for percentile computation
  std::sort(r.begin(),r.end(),std::mem_fun_ref(&CP::subjectILess));

  // compute percentile per subject
  R::iterator start=r.begin();
  for(M::const_iterator i=m.begin(); i!=m.end();++i) {
    R::iterator end=start+i->second.nr();
    if (start!=end)
      m[i->first].d_med= median(start,end);
    start=end;
  }

  std::ofstream out;
  open(out);
  scalarCrossHeader(out);
  for(typename M::const_iterator i=m.begin(); i!=m.end();++i)
    i->second.printLine(i->first,area(1),out);
}

void calc::StatTable::scalarTable(
  const REAL4* begin , const REAL4*end) const
{
  detail::ScalarStats s= detail::scalarStats(begin,end);

  std::ofstream out;
  open(out);
  out <<     "\t"<< d_subject.d_name << "\n";
  out << "opp\t" << area(s.nr())     << "\n";
  if (!s.nr()) // empty, skip
    return;
  out << "som" << "\t" << s.sum()     << "\n";
  out << "min" << "\t" << s.minimum() << "\n";
  out << "max" << "\t" << s.maximum() << "\n";
  out << "gem" << "\t" << s.average() << "\n";
  out << "sd"  << "\t" << s.sd()      << "\n";
  out << "med" << "\t" << s.d_med     << "\n";
}

void calc::StatTable::scalarTable(
  const REAL4* beginS, const REAL4 *endS,
  const REAL4* beginC, const REAL4 *endC) const
{
  detail::ScalarStats s= detail::scalarStats(beginS,endS);
  detail::ScalarStats c= detail::scalarStats(beginC,endC);

  std::ofstream out;
  open(out);
  out <<     "\t"<< d_subject.d_name<< "\t" << d_cross.d_name << "\n";
  out << "opp\t" << area(s.nr())    << "\t" << area(c.nr())   << "\n";
  if (!s.nr() && !c.nr()) // empty, skip
    return;
  // just put 0 if one of two is empty
  if (!s.nr()) s(0); // init with 0
  if (!c.nr()) c(0); // init with 0

  out << "som" << "\t" << s.sum()     << "\t" << c.sum()    << "\n";
  out << "min" << "\t" << s.minimum() << "\t" << c.minimum()<< "\n";
  out << "max" << "\t" << s.maximum() << "\t" << c.maximum()<< "\n";
  out << "gem" << "\t" << s.average() << "\t" << c.average()<< "\n";
  out << "sd"  << "\t" << s.sd()      << "\t" << c.sd()     << "\n";
  out << "med" << "\t" << s.d_med     << "\t" << c.d_med    << "\n";
}

void calc::StatTable::scalarSubject(
    FieldStack& stack) const
{
  FieldHandle d(stack.popReadOnly());
  const REAL4* r=(const REAL4 *)d->srcValue();

  if (!d_cross.defined() && !d_subject.hasIntervals()) {
     scalarTable(r, r+d->nrValues());
     return;
  }

  // scalarScalarTable with possible implicit cross = subject
  //                   if cross is not specified

  if (d_subject.hasIntervals()) {
     if (d_cross.hasIntervals()) {
        // TODO split up in 4 cases with overlap on subject/cross
        // typedef com::IntervalMap<detail::ScalarStats, float> IM;
        GGTable(r,stack); // GG
     } else
      if (com::noOverlap(d_subject.d_intervals)) {
        typedef com::IntervalMap<detail::ScalarStats, float> IM;
        scalarScalarTable<IM>(r,stack,d->nrValues()); // Kruis-3
      } else {
        typedef com::IntervalMultiMap<detail::ScalarStats, float> IMM;
         scalarScalarTable<IMM>(r,stack,d->nrValues()); // Kruis-3
      }
 } else {
     // scalarTable (met 2 kolommen)
     FieldHandle cd(stack.popReadOnly());
     const REAL4* c=(const REAL4 *)cd->srcValue();
     scalarTable(r, r+d->nrValues(),c,c+cd->nrValues());
  }
}

//! set value of subject.d_name
void calc::StatTable::setSubjectName(const std::string& subjectName)
{
  d_subject.d_name=subjectName;
}

//! set value of crossName
void calc::StatTable::setCrossName(const std::string& crossName)
{
  d_cross.d_name=crossName;
}

void calc::StatTable::setSubjectIntervals(const Intervals&   intervals)
{
  com::clearClone(d_subject.d_intervals);
  com::copyClone(intervals,d_subject.d_intervals);
}

/*!
 * \pre d_cross is defined, e.g. setCross() is called
 *
 * precondition is needed so d_cross.hasIntervals() also implies
 * d_cross.defined() and vice versa
 */
void calc::StatTable::setCrossIntervals(const Intervals&   intervals)
{
  PRECOND(d_cross.defined());
  com::clearClone(d_cross.d_intervals);
  com::copyClone(intervals,d_cross.d_intervals);
}

//! set value of verbose
void calc::StatTable::setVerbose(const bool verbose)
{
  d_verbose=verbose;
}

//! get value of verbose
bool calc::StatTable::verbose() const
{
  return d_verbose;
}

//! get value of subjectName
const std::string& calc::StatTable::subjectName() const
{
  return d_subject.d_name;
}

//! get value of crossName
const std::string& calc::StatTable::crossName() const
{
  return d_cross.d_name;
}

//! get value of resultTable
const com::PathName& calc::StatTable::resultTable() const
{
  return d_resultTable;
}



void calc::StatTable::print(calc::InfoScript& i)const
{
  d_subject->print(i);
  if (d_cross.defined())
    d_cross->print(i);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


