#ifndef INCLUDED_TAB_CLASSCLASSINTERVALMAP
#define INCLUDED_TAB_CLASSCLASSINTERVALMAP

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
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
// Module headers.


namespace tab {



template< class   IntervalMapT,
          typename Class =int
         >
class ClassIntervalMap : public std::map<Class,IntervalMapT>
{
 typedef std::map<Class,IntervalMapT>  Base;
 typedef typename Base::const_iterator I;
 typedef typename std::set<Class>      S;
 typedef typename S::iterator          SI;

 typedef std::vector<const com::Interval<typename IntervalMapT::IT> *> Intervals;

 Intervals d_intervals;

  //! Assignment operator. NOT IMPLEMENTED.
  ClassIntervalMap& operator=          (const ClassIntervalMap& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
  ClassIntervalMap                     (const ClassIntervalMap& rhs);

 public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  ClassIntervalMap(const Intervals& intervals)
  {
    com::copyClone(intervals,d_intervals);
  }

  virtual ~ClassIntervalMap()
  {
    com::deleteCloneContainer(d_intervals);
  }


  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! if class no yet in map then add with initial 0 count
  void addClass(Class row) {
    if (this->count(row)==0)
       (*this)[row].insertIntervals(d_intervals);
  }

  /*! \brief incr count for class combination \a r \a c,
   *         or set to 1 if not existent
   *  \todo
   *    let addClass return the position to apply visit on
   *    now 2 lookups then 1.
   */
  void visit(Class r,double c) {
    addClass(r);
    (*this)[r].visit(c);
  }
  //! as incr(r,c)
  void operator()(Class r,double c) {
    visit(r,c);
  }
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  /*
  S rowClasses() const {
    S s;
    for (I i=begin();i!=end();++i)
      s.insert(i->first);
    return s;
  }

  //! union set of all classes occurring in some columns
  S colClasses() const {
    S s;
    for (I i=begin();i!=end();++i) {
      S a(i->second.classes());
      s.insert(a.begin(),a.end());
    }
    return s;
  }

  //! return the count, 0 if not in map
  size_t getCount(Class r, Class c) const {
    I i=find(r);
    if (i!=end())
      return i->second.getCount(c);
    return 0;
  }
  */
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



} // namespace tab

#endif
