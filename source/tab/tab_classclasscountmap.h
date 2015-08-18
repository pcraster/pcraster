#ifndef INCLUDED_TAB_CLASSCLASSCOUNTMAP
#define INCLUDED_TAB_CLASSCLASSCOUNTMAP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_TAB_CLASSCOUNTMAP
#include "tab_classcountmap.h"
#define INCLUDED_TAB_CLASSCOUNTMAP
#endif

// Module headers.


namespace tab {



//! cross two classes, as a table with rows and columns
/*!
 * The underlying std::map concept does not have all entries
 * in the row columns form of the table, the getCount() will just
 * return 0 for non existent entries.
 */
template<typename Class=int>
class ClassClassCountMap : public std::map<Class,ClassCountMap<Class> >
{
 typedef std::map<Class,ClassCountMap<Class> > Base;
 typedef typename Base::const_iterator      I;
 typedef typename std::set<Class>           S;
 typedef typename S::iterator               SI;
 public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! if class no yet in map then add with initial 0 count
  void addClass(Class row) {
       this->insert(std::make_pair(row,typename Base::mapped_type()));
  }

  //! if class no yet in map then add with initial 0 count
  void addClass(Class row, Class col) {
       (*this)[row].addClass(col);
  }

  /*! \brief incr count for class combination \a r \a c,
   *         or set to 1 if not existent
   */
  void incr(Class r,Class c) {
      (*this)[r][c]++;
  }

  //! as incr(r,c)
  void operator()(Class r,Class c) {
    incr(r,c);
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  S rowClasses() const {
    S s;
    for (I i=this->begin();i!=this->end();++i)
      s.insert(i->first);
    return s;
  }

  //! union set of all classes occurring in some columns
  S colClasses() const {
    S s;
    for (I i=this->begin();i!=this->end();++i) {
      S a(i->second.classes());
      s.insert(a.begin(),a.end());
    }
    return s;
  }

  //! return the count, 0 if not in map
  size_t getCount(Class r, Class c) const {
    I i=this->find(r);
    if (i!=this->end())
      return i->second.getCount(c);
    return 0;
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



} // namespace tab

#endif
