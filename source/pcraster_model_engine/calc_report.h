#ifndef INCLUDED_CALC_REPORT
#define INCLUDED_CALC_REPORT



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

// Module headers.
#ifndef INCLUDED_CALC_ASTID
#include "calc_astid.h"
#define INCLUDED_CALC_ASTID
#endif
#ifndef INCLUDED_CALC_ID
#include "calc_id.h"
#define INCLUDED_CALC_ID
#endif



namespace calc {
  // Report declarations.
  class Timer;
}



namespace calc {

//! define the moment
typedef struct ParsReportMoment {
  //! the single value, or the start of step (inclusive)
  /*!
   *  -1 marks endTime
   */
  int start;
  //! the step increase, 0 if not set by user
  /*! no step given by user (1 is default,so 0 -> 1)
    */
  int step;
  /* the end of the step (inclusive),0 marks a single value
   * stored in start
   *  -1 marks endTime
   */
  int end;
  //! verify correctness

  void check(void);
} ParsReportMoment;


//! a definition as it appears in the timer section or in situ
/*!
 * keeps a boolean list of when something should be reported.
 */
class Report: public ASTId {
private:
  //! initialized in update()
  std::vector<bool>             d_reportAt;
  typedef std::vector<ParsReportMoment> PL;
  PL d_list;

  size_t d_startInt, d_lastInt;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Assignment operator.
  Report&           operator=          (const Report& rhs);

  //! Copy constructor.
                   Report              (const Report& rhs);

                   Report              ();

                   Report(const Id& s, const PL& list);

  /* virtual */    ~Report             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              accept             (ASTVisitor& v);
  void              update         (const Timer& timer);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  Report*           createClone        () const;

  bool              atInt              (size_t t) const;
  size_t            startInt           () const;
  size_t            lastInt            () const;

#ifdef DEBUG_DEVELOP
  void              print              () const;
#endif

  static Report     reportDefault      ();

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



} // namespace calc

#endif
