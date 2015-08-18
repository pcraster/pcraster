
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_STATTABLE
#include "calc_stattable.h"
#define INCLUDED_CALC_STATTABLE
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

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

#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif
#ifndef INCLUDED_CALC_EXECARGUMENTS
#include "calc_execarguments.h"
#define INCLUDED_CALC_EXECARGUMENTS
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEMSTRING
#include "calc_MemoryExchangeItemString.h"
#define INCLUDED_CALC_MEMORYEXCHANGEITEMSTRING
#endif

/*!
  \file
  This file contains the implementation of the StatTable class.
*/
namespace calc {
  bool  StatTable::d_verbose=false;

  class StatComputation {
  public:
    typedef  std::vector<const com::IntervalF *>  Intervals;
    typedef  float Real;
  private:
    struct InputMap {
      //! name to put in table;
      std::string            d_name;
      Field const*           d_field;
      Intervals const*       d_intervals;
      void verbosePrint(std::ostream& out) const;
    };

    std::ostream&    d_out;
    InputMap         d_subject,d_cross;

    //! verbose, print more (debug) data in table
    bool             d_verbose;



  private:

    //! Assignment operator. NOT IMPLEMENTED.
    StatComputation&           operator=           (const StatComputation& rhs);

    //! Copy constructor. NOT IMPLEMENTED.
                     StatComputation               (const StatComputation& rhs);

    void crossHeader                         (std::ostream& out) const;
    void scalarCrossHeader                   (std::ostream& out,
                                              int  nrRowDescriptors=1) const;

    template< class    IntervalMapT>
    void scalarScalarTable(
        const REAL4* subject,
        size_t      nrValues) const;
    void GGTable(
        const REAL4* subject) const;

    template<typename CountMap>
     void addSubjectClasses(CountMap& m) const;
    template<typename CountMap>
     void addCrossClasses(CountMap& m) const;
    template<typename SubjectType, typename CrossType>
     void classCrossTable(
        const SubjectType* subject) const;
    template<typename SubjectType>
     void classScalarTable(
        const SubjectType* subject) const;
    template< class    IntervalMapT,
              typename SubjectType>
     void classIntervalTable(
        const SubjectType* subject) const;

    template<typename T>
            void    classTable(const T* begin, const T*end) const;
    void scalarTable(const REAL4* begin, const REAL4*end) const;
    void scalarTable(const REAL4* beginS, const REAL4 *endS,
                     const REAL4* beginC, const REAL4 *endC) const;
    template<typename T>
            void    classSubject() const;

    void             scalarSubject() const;

    void             verboseHeader() const;

    static double    area(size_t cellCount);

  public:

    //----------------------------------------------------------------------------
    // CREATORS
    //----------------------------------------------------------------------------
                     StatComputation               (
                                              std::ostream& out,
                                              bool          verbose,
                                              Field const* subjectField,
                                              Intervals const* subjectTable,
                                              std::string const& subjectName,
                                              Field const* crossField,
                                              Intervals const*crossTable,
                                              std::string const& crossName);

    //----------------------------------------------------------------------------
    // MANIPULATORS
    //----------------------------------------------------------------------------


    //----------------------------------------------------------------------------
    // ACCESSORS
    //----------------------------------------------------------------------------

  };
}

//------------------------------------------------------------------------------

calc::StatTable::InputMap::InputMap():
  d_intervals(0),
  d_field(0)
{
}

void calc::StatComputation::InputMap::verbosePrint(
    std::ostream& d_out) const
{
  if (!d_intervals) {
    d_out << "\tniet opgegeven\n";
  } else {
    d_out << "\n";
    for (Intervals::const_iterator i=d_intervals->begin();
        i!=d_intervals->end();++i) {
      d_out << **i << "\n";
    }
    d_out << "\n";
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
         std::ostream& d_out) const {
           d_out << key;
           printLineCommon(area,d_out);
         }
     private:
        void printLineCommon(
            double        area,
            std::ostream& d_out) const {
           d_out << "\t" << area*nr();
           if (nr())
            d_out << "\t" << sum()
               << "\t" << minimum()
               << "\t" << maximum()
               << "\t" << average()
               << "\t" << sd()
               << "\t" << d_med;
           d_out << "\n";
         }
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
    m.outside().d_med=(REAL4)median(begin,endP);
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
       i->second.d_med= (REAL4)median(begin,endP);
       // ONE INDEX OFF seems OK
       // POSTCOND(std::distance(begin,endP)==(int)(i->second.nr()));
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
    std::string const& id,
    InputMap const& subject,
    InputMap const& cross):
  BaseExpr("statistics"),
  d_id(new ASTPar(id)),
  d_copyStringToMemoryOutputId(ASTSymbolInfo::noMemoryExchangeId()),
  d_subject(subject),
  d_cross(cross),
  d_op(0)
{
  const OP_ARGS fieldArgType = { VS_FIELD, ST_SPATIAL};
  const OP_ARGS tableArgType = { VS_TABLE, ST_NON};
  std::vector<OP_ARGS>  input;

  // table upfront
  if (d_subject.d_intervals) {
    PRECOND(d_subject.d_field);
    input.push_back(tableArgType);
    transferArg(d_subject.d_intervals);
  }
  if (d_cross.d_intervals) {
    PRECOND(d_cross.d_field);
    input.push_back(tableArgType);
    transferArg(d_cross.d_intervals);
  }
  if (d_subject.d_field) {
    input.push_back(fieldArgType);
    transferArg(d_subject.d_field);
  }
  if (d_cross.d_field) {
    input.push_back(fieldArgType);
    transferArg(d_cross.d_field);
  }
  d_op = new Operator("statistics","statistics",
                      std::vector<OP_ARGS>(), // no results
                      input,0);
  setNrReturns(0);
}


calc::StatTable::~StatTable()
{
  delete d_id;
  delete d_op;
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

void calc::StatComputation::verboseHeader() const
{

  if (d_verbose) {
    d_out << "Klassen/Grenzen Onderwerp:";
    d_subject.verbosePrint(d_out);
    if (d_cross.d_field) {
      d_out << "Klassen/Grenzen Indeling:";
      d_cross.verbosePrint(d_out);
    }
    d_out << "BEGIN\tWERKELIJKE\tTABEL:\n";
  }
}

//! set value of resultTable
void calc::StatTable::setIdBinding(const ASTSymbolInfo& sym)
{
  assert(sym.name() == d_id->name());
//  const std::string&     name                  () const;
// void calc::StatTable::setIdBinding(const com::PathName& resultTable)
  if (sym.memoryOutputId() == ASTSymbolInfo::noMemoryExchangeId()) {
    d_writeToFile= sym.externalName();
  } else {
    d_copyStringToMemoryOutputId = sym.memoryOutputId();
  }
}

void calc::StatTable::exec(
    RunTimeEnv* rte) const
{
  typedef StatComputation::Intervals I;
  struct FetchIntervals {
    static I const* create(RunTimeEnv* rte) {
      DataValue *dv=rte->popDataValue();
      POSTCOND(dv);
      LookupTable const* tab= dynamic_cast<LookupTable const*>(dv);
      POSTCOND(tab);
      POSTCOND(tab->nrCols()==1);
      I *iv= new I();
      LookupTable::Records const& r(tab->records());
      for(size_t i=0; i < r.size(); ++i)
        iv->push_back(&(r[i].col(0)));
      return iv;
    }
  };
  Field const * subjectField(0);
  Field const * crossField(0);
  std::auto_ptr<I const> subjectTable;
  std::auto_ptr<I const> crossTable;

  if(d_cross.d_field)
    crossField = rte->popField();
  PRECOND(d_subject.d_field);
    subjectField = rte->popField();
  if (d_cross.d_intervals)
    crossTable.reset(FetchIntervals::create(rte));
  if (d_subject.d_intervals)
    subjectTable.reset(FetchIntervals::create(rte));

  std::ostringstream out;
  StatComputation sc(out,
                     d_verbose,
                     subjectField,
                     subjectTable.get(),
                     d_subject.d_name,
                     crossField,
                     crossTable.get(),
                     d_cross.d_name);
  if (d_writeToFile.empty()) {
    // std::cout << out.str();
    rte->transferMemoryExchangeItemIntoDataTransferArray(
      new MemoryExchangeItemString(
           d_id->name(),
           d_copyStringToMemoryOutputId,
           out.str()));
  } else {
     std::ofstream outFile;
     com::open(outFile,d_writeToFile);
     outFile << out.str();
  }

  // TODO leaks on exception
  deleteFromPcrme(subjectField);
  deleteFromPcrme(crossField);
}

calc::StatComputation::StatComputation(
     std::ostream& out,
     bool          verbose,
     Field const* subjectField,
     Intervals const* subjectTable,
     std::string const& subjectName,
     Field const* crossField,
     Intervals const*crossTable,
     std::string const& crossName):
       d_out(out),
       d_verbose(verbose)
{
  d_subject.d_field=subjectField;
  d_subject.d_intervals=subjectTable;
  d_subject.d_name=subjectName;
  d_cross.d_field=crossField;
  d_cross.d_intervals=crossTable;
  d_cross.d_name=crossName;

  // Reverse if needed
  bool doSwap=false;
  if (d_cross.d_field) {
   if (d_subject.d_field->cr()==CR_REAL4) {
    if (d_cross.d_field->cr()!=CR_REAL4)
      doSwap=true; // r1,r2
    else
      if ( (!d_subject.d_intervals) && d_cross.d_intervals)
      doSwap=true;//r3
   }
  }
  if (doSwap) {
   std::swap(d_subject.d_field,    d_cross.d_field);
   std::swap(d_subject.d_intervals,d_cross.d_intervals);
   std::swap(d_subject.d_name,     d_cross.d_name);
  }

  verboseHeader();
  // select on subject type
  switch(biggestCellRepr(d_subject.d_field->vs())) {
    case CR_INT4:
             classSubject<INT4>();
             break;
    case CR_UINT1:
             classSubject<UINT1>();
             break;
    case CR_REAL4:
             scalarSubject();
             break;
    default:
         POSTCOND(FALSE);
  }
}

template<typename T>
void calc::StatComputation::classSubject() const
{
  const T* s=d_subject.d_field->src_t<T>();

  if (!d_cross.d_field)
    classTable(s,s+d_subject.d_field->nrValues());
  else {
    switch(biggestCellRepr(d_cross.d_field->vs())) {
      case CR_INT4:  classCrossTable<T,INT4>(s); break;
      case CR_UINT1: classCrossTable<T,UINT1>(s); break;
      case CR_REAL4:
              if (!d_cross.d_intervals)
                 classScalarTable(s);
              else {
                if (com::noOverlap(*d_cross.d_intervals)) {
                  typedef typename
                    com::IntervalMap< detail::ScalarStats, Real > IM;
                     classIntervalTable<IM>(s);
                } else {
                  typedef typename
                    com::IntervalMultiMap< detail::ScalarStats, Real > IM;
                     classIntervalTable<IM>(s);
                }
              }
              break;
      default:
         POSTCOND(FALSE);
    }
  }
}

template<typename T>
  void calc::StatComputation::classTable(
      const T* begin, const T*end) const
{
  typedef tab::ClassCountMap<INT4> M;
  M m;
  addSubjectClasses(m);
  m=com::forEachNonMV(begin,end,m);

  d_out << d_subject.d_name << "\t" << "area" << "\n";
  for(M::const_iterator i=m.begin(); i!=m.end();++i)
      d_out << i->first << "\t" << area(i->second) << "\n";
}

template<typename SubjectType, typename CrossType>
  void calc::StatComputation::classCrossTable(
      const SubjectType* subject) const
{
  typedef tab::ClassClassCountMap<INT4> M;
  M m;
  this->addCrossClasses(m);

  const CrossType* cross= d_cross.d_field->src_t<CrossType>();
  for(size_t i=0; i < d_cross.d_field->nrValues(); ++i)
    if (!pcr::isMV(subject[i]) && !pcr::isMV(cross[i]))
      m(subject[i],cross[i]);

  crossHeader(d_out);

  // header line 2
  d_out << d_subject.d_name;
  typedef typename std::set<INT4> S;
  typedef typename S::const_iterator SI;
  S col=m.colClasses();
  S row=m.rowClasses();

  for(SI c=col.begin(); c!=col.end(); ++c)
   d_out << "\t" << *c;
  d_out << "\n";

  for(SI r=row.begin(); r!=row.end(); ++r) {
    d_out << *r;
    for(SI c=col.begin(); c!=col.end(); ++c)
     d_out << "\t" << area(m.getCount(*r,*c));
    d_out << "\n";
  }
}


void calc::StatComputation::crossHeader(std::ostream& d_out) const
{
  if (d_cross.d_field)
   d_out << "\t" << d_cross.d_name << "\n";
  else
   d_out << "\t" << d_subject.d_name << "\n";
}

void calc::StatComputation::scalarCrossHeader(
    std::ostream& d_out,
    int  nrRowDescriptors) const
{
  crossHeader(d_out);
  d_out << d_subject.d_name;
  // print with intervening space
  //  do not allow empty between tab seperators
  if (nrRowDescriptors)
    d_out << "\t";
  for(int i=1;i<nrRowDescriptors;++i)
    d_out << " \t";
  d_out << "area" << "\t" <<
         "sum" << "\t" <<
         "minimum" << "\t" <<
         "maximum" << "\t" <<
         "average" << "\t" <<
         "standard deviation"  << "\t" <<
         "median" << "\n" ;
}

template<typename CountMap>
 void calc::StatComputation::addSubjectClasses(CountMap& m) const
{
  if (!d_subject.d_intervals)
    return;
  const Intervals& cl(*d_subject.d_intervals);
  for(Intervals::const_iterator i=cl.begin(); i!=cl.end();++i) {
    const com::Interval<Real>& iv(**i);
    // normal case: PRECOND(iv.minimum()==iv.maximum());
    //  but we simple cast minimum to integer

    // MT() init 0, default Ctor
    typedef typename CountMap::mapped_type MT; 
    m.insert(std::make_pair(static_cast<int>(iv.min()),MT()));
  }
}

template<typename CountMap>
 void calc::StatComputation::addCrossClasses(CountMap& m) const
{
  if (!d_subject.d_intervals)
    return;

  const Intervals& s(*d_subject.d_intervals);
  if (!d_cross.d_intervals)
   for(size_t i=0; i <s.size();++i)
     m.addClass(static_cast<int>(s[i]->min()));
  else {
   const Intervals& c(*d_cross.d_intervals);
   for(Intervals::const_iterator si=s.begin(); si!=s.end();++si)
    for(Intervals::const_iterator ci=c.begin(); ci!=c.end();++ci) {
     const com::Interval<Real>& siv(**si);
     PRECOND(siv.min()==siv.max());
     const com::Interval<Real>& civ(**ci);
     PRECOND(civ.min()==civ.max());
     m.addClass(static_cast<int>(siv.min()),
               static_cast<int>(civ.min()));
    }
  }
}

//! d_cross is REAL4 and has intervals a.k.a. type KG
template< class    IntervalMapT,
          typename SubjectType>
  void calc::StatComputation::classIntervalTable(
      const SubjectType* subject) const
{
  PRECOND(d_cross.d_intervals);

  const REAL4* cross=d_cross.d_field->src_f();

  typedef detail::CS_CP   CP;
  detail::CS_V r;
  r.reserve(d_cross.d_field->nrValues());

  typedef tab::ClassIntervalMap< IntervalMapT > M;
  M m(*d_cross.d_intervals);
  addSubjectClasses(m);

  for(size_t i=0;i< d_cross.d_field->nrValues(); i++)
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

  scalarCrossHeader(d_out,2);
  for(typename M::const_iterator k=m.begin(); k!=m.end();++k) {
   for(typename IntervalMapT::const_iterator i=k->second.begin(); i!=k->second.end();++i) {
    d_out << k->first << "\t";
    i->second.printLine(*(i->first),area(1),d_out);
   }
   if (k->second.outside().nr()) {
    d_out << k->first << "\t";
    k->second.outside().printLine("otherwise",area(1),d_out);
   }
  }
}

//! type Kruis-3
template<class IntervalMapT>
 void calc::StatComputation::scalarScalarTable(
      const REAL4* subject,
      size_t      nrValues) const
{
  typedef detail::CS_CP   CP;
  typedef std::vector<CP> R;
  R r;
  r.reserve(nrValues);

  typedef IntervalMapT M;
  M m;
  m.insertIntervals(*d_subject.d_intervals);
  POSTCOND(m.size()==d_subject.d_intervals->size());

  const REAL4* cross;

  if (!d_cross.d_field)
    cross=subject;
  else {
    cross=d_cross.d_field->src_f();
  }

  for(size_t i=0;i< nrValues; i++) {
   if (!pcr::isMV(subject[i]) && !pcr::isMV(cross[i])) {
      m.visit(subject[i],cross[i]);
      r.push_back(CP(subject[i],cross[i]));
    }
  }

  crossPercentiles<detail::Kruis3Policy>(m,r.begin(),r.end());
  scalarCrossHeader(d_out);
  for(typename M::const_iterator i=m.begin(); i!=m.end();++i)
    i->second.printLine(*(i->first),area(1),d_out);
  if (m.outside().nr())
    m.outside().printLine("otherwise",area(1),d_out);
}

//! type GG
 void calc::StatComputation::GGTable(
      const REAL4* subject) const
{
  const REAL4* cross=d_cross.d_field->src_f();


  typedef detail::CS_CP   CP;
  typedef std::vector<CP> R;
  R r;
  r.reserve(d_cross.d_field->nrValues());

  typedef com::IntervalMultiMap< detail::ScalarStats, Real> MapKey;
  typedef com::IntervalMultiMap< MapKey, Real>            M;

  M m;
  m.insertIntervals(*d_subject.d_intervals);
  POSTCOND(m.size()==d_subject.d_intervals->size());
  for(M::iterator i=m.begin(); i!=m.end();++i)
    i->second.insertIntervals(*d_cross.d_intervals);

  for(size_t i=0;i< d_cross.d_field->nrValues(); i++)
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

  scalarCrossHeader(d_out,2);
  for(M::const_iterator k=m.begin(); k!=m.end();++k) {
   for(MapKey::const_iterator i=k->second.begin(); i!=k->second.end();++i) {
    d_out << *k->first << "\t";
    i->second.printLine(*(i->first),area(1),d_out);
   }
   if (k->second.outside().nr()) {
    d_out << *k->first << "\t";
    k->second.outside().printLine("otherwise",area(1),d_out);
   }
  }

  const MapKey& mko(m.outside());
  if (mko.nrVisits()) {
   for(MapKey::const_iterator i=mko.begin(); i!=mko.end();++i) {
    d_out << "otherwise" << "\t";
    i->second.printLine(*(i->first),area(1),d_out);
   }
   if (mko.outside().nr()) {
    d_out << "otherwise" << "\t";
    mko.outside().printLine("otherwise",area(1),d_out);
   }
  }
}

//! d_cross is REAL4 and has no intervals
template<typename SubjectType>
  void calc::StatComputation::classScalarTable(
      const SubjectType* subject) const
{
  const REAL4* cross=d_cross.d_field->src_f();

  typedef detail::CS_CP   CP;
  typedef std::vector<CP> R;
  R r;
  r.reserve(d_cross.d_field->nrValues());

  typedef std::map<INT4,detail::ScalarStats> M;
  M m;
  addSubjectClasses(m);

  for(size_t i=0;i< d_cross.d_field->nrValues(); i++)
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
      m[i->first].d_med= (REAL4)median(start,end);
    start=end;
  }

  scalarCrossHeader(d_out);
  for(typename M::const_iterator i=m.begin(); i!=m.end();++i)
    i->second.printLine(i->first,area(1),d_out);
}

void calc::StatComputation::scalarTable(
  const REAL4* begin , const REAL4*end) const
{
  detail::ScalarStats s= detail::scalarStats(begin,end);

  d_out <<     "\t"<< d_subject.d_name << "\n";
  d_out << "area\t" << area(s.nr())     << "\n";
  if (!s.nr()) // empty, skip
    return;
  d_out << "sum" << "\t" << s.sum()     << "\n";
  d_out << "minimum" << "\t" << s.minimum() << "\n";
  d_out << "maximum" << "\t" << s.maximum() << "\n";
  d_out << "average" << "\t" << s.average() << "\n";
  d_out << "standard deviation"  << "\t" << s.sd()      << "\n";
  d_out << "median" << "\t" << s.d_med     << "\n";
}

void calc::StatComputation::scalarTable(
  const REAL4* beginS, const REAL4 *endS,
  const REAL4* beginC, const REAL4 *endC) const
{
  detail::ScalarStats s= detail::scalarStats(beginS,endS);
  detail::ScalarStats c= detail::scalarStats(beginC,endC);

  d_out <<     "\t"<< d_subject.d_name<< "\t" << d_cross.d_name << "\n";
  d_out << "area\t" << area(s.nr())    << "\t" << area(c.nr())   << "\n";
  if (!s.nr() && !c.nr()) // empty, skip
    return;
  // just put 0 if one of two is empty
  if (!s.nr()) s(0); // init with 0
  if (!c.nr()) c(0); // init with 0

  d_out << "sum" << "\t" << s.sum()     << "\t" << c.sum()    << "\n";
  d_out << "minimum" << "\t" << s.minimum() << "\t" << c.minimum()<< "\n";
  d_out << "maximum" << "\t" << s.maximum() << "\t" << c.maximum()<< "\n";
  d_out << "average" << "\t" << s.average() << "\t" << c.average()<< "\n";
  d_out << "standard deviation"  << "\t" << s.sd()      << "\t" << c.sd()     << "\n";
  d_out << "median" << "\t" << s.d_med     << "\t" << c.d_med    << "\n";
}

void calc::StatComputation::scalarSubject() const
{
  const REAL4* r=d_subject.d_field->src_f();

  if (!d_cross.d_field && !d_subject.d_intervals) {
     scalarTable(r, r+d_subject.d_field->nrValues());
     return;
  }

  // scalarScalarTable with possible implicit cross = subject
  //                   if cross is not specified

  if (d_subject.d_intervals) {
     if (d_cross.d_intervals) {
        // TODO split up in 4 cases with overlap on subject/cross
        // typedef com::IntervalMap<detail::ScalarStats, Real> IM;
        GGTable(r); // GG
     } else
      if (com::noOverlap(*d_subject.d_intervals)) {
        typedef com::IntervalMap<detail::ScalarStats, Real> IM;
        scalarScalarTable<IM>(r,d_subject.d_field->nrValues()); // Kruis-3
      } else {
        typedef com::IntervalMultiMap<detail::ScalarStats, Real> IMM;
         scalarScalarTable<IMM>(r,d_subject.d_field->nrValues()); // Kruis-3
      }
 } else {
     // scalarTable (met 2 kolommen)
     const REAL4* cross=d_cross.d_field->src_f();
     scalarTable(r, r+d_subject.d_field->nrValues(),cross,cross+d_cross.d_field->nrValues());
  }
}

double calc::StatComputation::area(
          size_t cellCount)
{
  return cellCount*Area();
}

void calc::StatTable::setVerbose(bool verbose)
{
  d_verbose=verbose;
}

const calc::Operator&  calc::StatTable::op() const
{
  return *d_op;
}

calc::ASTId* calc::StatTable::createClone() const
{
  PRECOND(false);
  return 0;
}

calc::ASTPar* calc::StatTable::id() const
{
  return d_id;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


