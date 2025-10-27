#ifndef INCLUDED_TAB_CLASSCOUNTMAP
#define INCLUDED_TAB_CLASSCOUNTMAP

#include "stddefx.h"

#include <map>
#include <set>



namespace tab {
  // ClassCountMap declarations.
}



namespace tab {

//! count class occurence using std::map
template<typename Class=int>
class ClassCountMap : public std::map<Class,size_t>
{
   typedef typename std::set<Class>           S;
   typedef typename std::map<Class,size_t>    Base;
   typedef typename Base::const_iterator      I;
 public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
   //! return the classes in map
    S classes() const {
      S s;
      for (auto i=this->begin();i!=this->end();++i)
        s.insert(i->first);
      return s;
    }

    //! if class not yet in map then add with initial 0 count
    void addClass(Class c) {
      if (!this->count(c))
       (*this)[c]=0;
    }
    //! incr count for class \a c, or set to 1 if not existent
    void incr(Class c) {
      (*this)[c]++;
    }
    //! incr count for class \a c, or set to 1 if not existent
    void operator()(Class c) {
      incr(c);
    }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

    //! return the count, 0 if not in map
    size_t getCount(Class c) const {
      auto i=this->find(c);
      if (i!=this->end())
        return i->second;
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
