#ifndef INCLUDED_CALC_RELATIONRECORD
#define INCLUDED_CALC_RELATIONRECORD



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif

// Module headers.



namespace calc {
  // RelationRecord declarations.
}

struct LOOK_UP_KEY;

namespace calc {



//! a relation record for relations such as lookup
class RelationRecord : public std::vector<com::Interval<float> * >
{

   void        copy             (const RelationRecord& rhs);
   void        clean            ();

public:
  typedef float                  Float;
  typedef com::Interval<Float>   Interval;
  //! the Base class
  typedef std::vector<Interval*> IntervalVector;
  //! Key matched up against IntervalVector
  typedef std::vector<Float>     Key;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  RelationRecord&           operator=          (RelationRecord const& rhs);

                    RelationRecord             (RelationRecord const& rhs);

                    RelationRecord             ();

                    RelationRecord             (const LOOK_UP_KEY *keys,size_t nrKeys);

  /* virtual */    ~RelationRecord             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool               match                      (const Key& key) const;
  int                compare                    (const Key& key) const;
  int                compare                    (Float  key, size_t c) const;
  bool               operator==                 (const RelationRecord& rhs) const;
  const Interval&    col                        (size_t i) const;
};

/*!
 * function object implementing &lt; (less) for a column (\a col)
 * of a RelationRecord.
 *
 * function accepts both RelationRecord or a float or double.
 */
class RelationRecordColLess {
   //! column to compare
   size_t d_col;
 public:
   RelationRecordColLess(size_t col):
     d_col(col)
   {}
   bool operator()(
     const RelationRecord& e1,
     const RelationRecord& e2) const {
     return e1.col(d_col) < e2.col(d_col);
   }
   bool operator()(
     const RelationRecord& e1,
     const RelationRecord::Float& e2) const {
     return e1.col(d_col) < e2;
   }
   bool operator()(
     const RelationRecord::Float& e1,
     const RelationRecord& e2) const {
     return e1 < e2.col(d_col).min();
   }
};


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Interval of column i
inline const RelationRecord::Interval& RelationRecord::col(size_t i) const
{
  return *(IntervalVector::operator[](i));
}


//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

std::ostream&   operator<<          (std::ostream& stream,
                                     const RelationRecord& r);




//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
