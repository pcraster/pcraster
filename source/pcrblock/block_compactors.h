#ifndef INCLUDED_BLOCK_COMPACTORS
#define INCLUDED_BLOCK_COMPACTORS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_BOOST_FUNCTION_FUNCTION2
#include <boost/function/function2.hpp>
#define INCLUDED_BOOST_FUNCTION_FUNCTION2
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.
#ifndef INCLUDED_BLOCK_TYPES
#include "block_types.h"
#define INCLUDED_BLOCK_TYPES
#endif



namespace block {
  // Compactors declarations.
}



namespace block {



//! Class for storing sediment id - compactor maps.
/*!
  This class can store the relationship between sediment id's and compactors.
  For each sediment id a compactor can be stored. These compactors can be used
  later to select the right compactor for each sediment id.

  The type of the compactors is a template argument of this class so it can
  be used for different compactor types.

  This class inherits from std::map::INT4, Compactor>.
*/
template<class Compactor>
class Compactors:
    public std::map<INT4, Compactor>
{

  friend class CompactorsTest;

public:

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Compactors&      operator=           (Compactors const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Compactors          (Compactors const& rhs);

  bool             hasCompactor        (INT4 sediment) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Compactors          ();

  /* virtual */    ~Compactors         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setCompactor        (INT4 sediment,
                                        Compactor const& compactor);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  Compactor const& compactor           (INT4 sediment) const;

  // REAL4            compact             (INT4 sediment,
  //                                       REAL4 originalThickness,
  //                                       REAL4 depth) const;

  // REAL4            compact             (INT4 sediment,
  //                                       REAL4 initialThickness,
  //                                       REAL4 cummulativeLoad,
  //                                       double duration) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
*/
template<class Compactor>
inline Compactors<Compactor>::Compactors()
  : std::map<INT4, Compactor>()
{
}

/* NOT IMPLEMENTED
//! Copy constructor.
template<class Compactor>
inline Compactors<Compactor>::Compactors(
         Compactors const& rhs)

  : Base(rhs)

{
}
*/

//! Destructor.
/*!
*/
template<class Compactor>
inline Compactors<Compactor>::~Compactors()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
template<class Compactor>
inline Compactors& Compactors<Compactor>::operator=(
         Compactors const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/

//! Sets the compactor for \a sediment to \a compactor.
/*!
  \param     sediment Sediment id to set compactor for.
  \param     compactor Compactor to set for sediment id.
  \warning   Any previous setting is lost.
*/
template<class Compactor>
inline void Compactors<Compactor>::setCompactor(
         INT4 sediment,
         Compactor const& compactor)
{
  this->operator[](sediment) = compactor;
}

//! Returns whether a compactor has been set for \a sediment.
/*!
  \param     sediment Sediment id to check.
  \return    true or false
*/
template<class Compactor>
inline bool Compactors<Compactor>::hasCompactor(
         INT4 sediment) const
{
  return this->find(sediment) != this->end();
}

//! Returns the compactor set for \a sediment.
/*!
  \param     sediment Sediment id to return compactor for.
  \return    Compactor.
  \warning   This function assumes that a compactor has been set for
             \a sediment.
*/
template<class Compactor>
inline Compactor const& Compactors<Compactor>::compactor(
         INT4 sediment) const
{
  DEVELOP_PRECOND(hasCompactor(sediment));

  return (*this->find(sediment)).second;
}

// template<class Compactor>
// inline REAL4 Compactors<Compactor>::compact(
//          INT4 sediment,
//          REAL4 originalThickness,
//          REAL4 depth) const
// {
//   DEVELOP_PRECOND(hasCompactor(sediment));
//
//   return compactor(sediment)(originalThickness, depth);
// }
//
// template<class Compactor>
// inline REAL4 Compactors<Compactor>::compact(
//          INT4 sediment,
//          REAL4 initialThickness,
//          REAL4 cummulativeLoad,
//          double duration) const
// {
//   DEVELOP_PRECOND(hasCompactor(sediment));
//
//   return compactor(sediment)(initialThickness, cummulativeLoad, duration);
// }



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace block

#endif
