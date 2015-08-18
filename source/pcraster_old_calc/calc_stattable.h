#ifndef INCLUDED_CALC_STATTABLE
#define INCLUDED_CALC_STATTABLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif
#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif

// Module headers.
#ifndef INCLUDED_CALC_STATEMENT
# include "calc_statement.h"
#define INCLUDED_CALC_STATEMENT
#endif


namespace calc {

class FieldExpr;
class FieldStack;


//! an expression that print to stdout
class StatTable : public Statement
{
  typedef  std::vector<const com::IntervalF *>  Intervals;
  struct InputMap {
    //! expr that generates input maps
    FieldExpr                          *d_expr;
    //! name to put in table;
    std::string                         d_name;
    //! set of intervals
    /*! in case of integer d_expr they are expected
     *  to be single (com::EqualTo) values.
     */
    Intervals  d_intervals;
    bool   defined()      const;
    bool   hasIntervals() const;
    CSF_CR cr()           const;
    FieldExpr * operator->() const {
      return d_expr;
    }
    void verbosePrint(std::ofstream& out) const;
    ~InputMap();
    InputMap();
    void swap( InputMap& rhs);
  private:
    InputMap(const InputMap& rhs);
    InputMap& operator=(const InputMap& rhs);
  };

  InputMap         d_subject,d_cross;

  //! write to this file
  com::PathName    d_resultTable;

  //! verbose, print more (debug) data in table
  bool             d_verbose;

  bool             d_maskSet;


private:

  //! Assignment operator. NOT IMPLEMENTED.
  StatTable&           operator=           (const StatTable& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   StatTable               (const StatTable& rhs);

  void crossHeader                         (std::ostream& out) const;
  void scalarCrossHeader                   (std::ostream& out,
                                            int  nrRowDescriptors=1) const;

  template< class    IntervalMapT>
  void scalarScalarTable(
      const REAL4* subject,
      FieldStack& stack,
      size_t      nrValues) const;
  void GGTable(
      const REAL4* subject,
      FieldStack& stack) const;

  template<typename T>
   void popSubject(FieldStack& stack) const;

  template<typename CountMap>
   void addSubjectClasses(CountMap& m) const;
  template<typename CountMap>
   void addCrossClasses(CountMap& m) const;
  template<typename SubjectType, typename CrossType>
   void classCrossTable(
      const SubjectType* subject,
      FieldStack& stack) const;
  template<typename SubjectType>
   void classScalarTable(
      const SubjectType* subject,
      FieldStack& stack) const;
  template< class    IntervalMapT,
            typename SubjectType>
   void classIntervalTable(
      const SubjectType* subject,
      FieldStack& stack) const;

  template<typename T>
          void    classTable(const T* begin, const T*end) const;
  void scalarTable(const REAL4* begin, const REAL4*end) const;
  void scalarTable(const REAL4* beginS, const REAL4 *endS,
                   const REAL4* beginC, const REAL4 *endC) const;
  template<typename T>
          void    classSubject(FieldStack& stack) const;

  void             scalarSubject(FieldStack& stack) const;

  void             open        (std::ofstream& out) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   StatTable               (FieldExpr *subject);

  /* virtual */    ~StatTable              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setSubjectName      (const std::string& subjectName);
  void             setCrossName        (const std::string& crossName);
  void             setSubjectIntervals (const Intervals&   intervals);
  void             setCrossIntervals   (const Intervals&   intervals);
  void             setResultTable      (const com::PathName& resultTable);

  void             setCross            (FieldExpr *cross);

  bool             buildTypes();
  void             prepareExecution();
  void             run();
  void             setVerbose          (const bool verbose);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             print(InfoScript& i)const;
  const com::PathName& resultTable         () const;

  const std::string& subjectName         () const;
  const std::string& crossName           () const;

  static double    area(size_t cellCount);
  bool             verbose             () const;

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
