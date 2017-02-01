#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_RELATIONRECORD
#include "calc_relationrecord.h"
#define INCLUDED_CALC_RELATIONRECORD
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_TABLE
#include "table.h"    // LOOK_UP_KEY
#define INCLUDED_TABLE
#endif
#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif

#include <functional>

// Module headers.



/*!
  \file
  This file contains the implementation of the RelationRecord class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class RelationRecordPrivate
{
public:

  RelationRecordPrivate()
  {
  }

  ~RelationRecordPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RELATIONRECORD MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RELATIONRECORD MEMBERS
//------------------------------------------------------------------------------

calc::RelationRecord::RelationRecord()
{
}

//! constuct from old style
calc::RelationRecord::RelationRecord(const LOOK_UP_KEY *keys,size_t nrKeys)
{
  reserve(nrKeys);
  for(size_t k=0; k < nrKeys; k++) {
     const LOOK_UP_KEY *l=keys+k;
     switch (l->t) {
       case TEST_ONE    :
         push_back(new com::EqualTo<Float>((Float)l->l));
         break;
       case TEST_INF_INF:
         push_back(new com::AnythingInterval<Float>());     // infinity
         break;
       case TEST_GE_INF:
         push_back(new com::GreaterThanEqualTo<Float>((Float)l->l));// [l  ,inf>
         break;
       case TEST_GT_INF :
         push_back(new com::GreaterThan<Float>((Float)l->l));         // <l  ,inf>
         break;
       case TEST_INF_LE :
         push_back(new com::LessThanEqualTo<Float>((Float)l->h));     // <inf,h]
         break;
       case TEST_GE_LE  :
         push_back(new com::BetweenLimits<Float>(
                       com::GreaterThanEqualTo<Float>((Float)l->l),
                       com::LessThanEqualTo<Float>((Float)l->h)));              // [l  ,h]
         break;
       case TEST_GT_LE  :
         push_back(new com::BetweenLimits<Float>(
                        com::GreaterThan<Float>((Float)l->l),
                        com::LessThanEqualTo<Float>((Float)l->h)));              // <l  ,h]
         break;
       case TEST_INF_LT :
         push_back(new com::LessThan<Float>((Float)l->h));            // <inf,h>
         break;
       case TEST_GE_LT  :
         push_back(new com::BetweenLimits<Float>(
                        com::GreaterThanEqualTo<Float>((Float)l->l),
                        com::LessThan<Float>((Float)l->h)));                     // [l  ,h>
         break;
       case TEST_GT_LT  :
         push_back(new com::BetweenLimits<Float>(
                        com::GreaterThan<Float>((Float)l->l),
                        com::LessThan<Float>((Float)l->h)));                     // <l  ,h>
         break;
       default:           PRECOND(FALSE);
     }
   }
}


/* NOT IMPLEMENTED
//! Copy constructor.
calc::RelationRecord::RelationRecord(RelationRecord const& rhs)

  : Base(rhs)

{
}
*/



/* NOT IMPLEMENTED
//! Assignment operator.
calc::RelationRecord& calc::RelationRecord::operator=(RelationRecord const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! copy contents
void calc::RelationRecord::copy(const RelationRecord& rhs)
{
  DEVELOP_PRECOND(empty());
  std::transform(rhs.begin(),rhs.end(), std::back_inserter(*this),
                  std::mem_fun(&com::Interval<Float>::createClone));
}

//! delete all records of \a key and clear the vector
void calc::RelationRecord::clean()
{
  com::forWhole(*this,com::Delete<com::Interval<Float> >());
  clear();
}

calc::RelationRecord&
calc::RelationRecord::operator=(const RelationRecord& r)
{
  if (this != &r) {
   clean();
   copy(r);
  }
  return *this;
}

calc::RelationRecord::RelationRecord(const RelationRecord& r):
  IntervalVector()
{
   copy(r);
}

calc::RelationRecord::~RelationRecord()
{
  clean();
}

//! predicate if RelationRecord matches
/*! key.size() <= size(), the comparision is on the first key.size() intervals.
 */
bool calc::RelationRecord::match(const Key& key) const
{
  DEVELOP_PRECOND(key.size() <= size());
  for(size_t i=0; i < key.size(); i++)
    if (!col(i).valid(key[i]))
      return false;
  return true;
}

//! compare record column \a in strcmp fashion against \a key
/*!
 *
 * \returns 0 if \a key equals column \a c, -1 (&lt; 0) if less than \a key,
 *                                   1 (&gt; 0) if greater than \a key
 */
int calc::RelationRecord::compare(Float key, size_t c) const
{
  DEVELOP_PRECOND(c <= size());
  if (!col(c).valid(key)) {
     if (col(c).operator<(key))
          return -1;
     if (col(c).operator>(key))
          return  1;
  }
  return 0;
}

bool calc::RelationRecord::operator==(const RelationRecord& rhs) const
{
  for(size_t i=0; i < size(); ++i)
    if (col(i) != rhs.col(i))
      return false;
  return true;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

std::ostream& calc::operator<<(
        std::ostream& stream,
        const RelationRecord& r)
{
    for(size_t i=0; i < r.size(); ++i)
      stream << i << ":(" << *(r[i]) << "),";
    return stream;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



