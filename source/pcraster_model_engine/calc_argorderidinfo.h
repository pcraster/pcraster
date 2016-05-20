#ifndef INCLUDED_CALC_ARGORDERIDINFO
#define INCLUDED_CALC_ARGORDERIDINFO



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

// Module headers.



namespace calc {
  // ArgOrderIdInfo declarations.
}



namespace calc {

//! Info or state used in the argorder and addarealimited functions
class ArgOrderIdInfo
{

private:

  /*
  //! Assignment operator. DEFAULT
  ArgOrderIdInfo&           operator=           (ArgOrderIdInfo const& rhs);

  //! Copy constructor. DEFAULT
                   ArgOrderIdInfo               (ArgOrderIdInfo const& rhs);
  */
   //! chance argument, not owned (size nrCells)
   const REAL4 *d_chance;
   //! id to assign
  UINT4            d_id;

   //! area limit or in case of AddArea the area to add (counted as number of cells)
  size_t           d_areaLimit;
  size_t           d_orgAreaLimit;

   //! area assigned so far (counted as number of cells)
  size_t           d_areaAssigned;

  //! area taken in AddArea (counted as number of cells)
  size_t           d_areaTaken;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ArgOrderIdInfo               (const REAL4* chance,
                                                 UINT4        id);
                   ArgOrderIdInfo               (const REAL4* chance,
                                                 UINT4        id,
                                                 double       areaLimit);


  /* virtual */    ~ArgOrderIdInfo              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setId               (UINT4 id);
  void             incrementAreaAssigned     ();
  void             incrementAreaTaken        ();
  void             setAreaLimit        (double areaLimit);
  void             resetForSweep       ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  REAL4 const*     chance              () const;
  UINT4            id                  () const;
  size_t           areaLimit           () const;
  size_t           areaAssigned        () const;
  size_t           areaTaken           () const;

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
