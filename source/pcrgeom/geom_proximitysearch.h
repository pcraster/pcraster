#ifndef INCLUDED_GEOM_PROXIMITYSEARCH
#define INCLUDED_GEOM_PROXIMITYSEARCH



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace geom {
  // ProximitySearch declarations.
}



namespace geom {



//! Describes a proximity search criteria
/*! developed in support in support of \class PRDimTree
*/
class ProximitySearch
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ProximitySearch&           operator=           (const ProximitySearch& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ProximitySearch               (const ProximitySearch& rhs);

  //! search radius, incl. test <=
  /*
   * special values are -1: not specified, 0 search equal to
   */
  double  d_radius;

  //! square search, default circle
  bool    d_square;

  //! minimum number of points
  /*!
   * \todo
   *  Con: is this really a criteria, seems more like a reject after search
   *  criteria. Pro: when searching subsets, one can already quit search if
   *  a set is smaller than d_minNr
   */
  size_t  d_minNr;

  //! maximum number of points
  /*!
   * special values: 1 means closest point only
   */
  size_t  d_maxNr;

  void init();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ProximitySearch               (double radius);

  /* virtual */    ~ProximitySearch              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setSquared                    (bool isSquare);
  void             setMinNr                      (size_t minNr);
  void             setMaxNr                      (size_t maxNr);
  void             setRadius                     (double radius);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  size_t minNr()          const;
  size_t maxNr()          const;
  bool   maxNrIsBounded() const;
  bool   circularRadius() const;
  bool   squaredRadius()  const;
  double radius()         const;

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



} // namespace geom

#endif
