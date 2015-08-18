#ifndef INCLUDED_GEO_CSFSTACK
#define INCLUDED_GEO_CSFSTACK

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifdef DEBUG_DEVELOP
  #ifndef INCLUDED_IOSTREAM
  #include <iostream>
  #define INCLUDED_IOSTREAM
  #endif
#endif

#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

#ifndef INCLUDED_GEO_CSFSTACKNAME
#include "geo_csfstackname.h"
#define INCLUDED_GEO_CSFSTACKNAME
#endif

#ifndef INCLUDED_GEO_RASTER
#include "geo_raster.h"
#define INCLUDED_GEO_RASTER
#endif

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

namespace com {
  template<class INT4>
    class Legend;
}

namespace geo {
#ifdef DEBUG_DEVELOP
  template<class T>
    class CSFStack;
  template<class T>
    std::ostream &operator<<(std::ostream &s, const CSFStack<T> &stack);
#endif
  class CSFMap;
}



namespace geo {

/*!
  \class CSFStack
  \brief The CSFStack class is for objects representing static or dynamic csf
         raster layers.

  This class encapsulates the hassle of dealing with more files in case of a
  dynamic csf dataset. You can use this class for objects representing both
  static and dynamic datasets.

  A static dataset consists of spatial data which is not variable in time. A
  dynamic dataset is output by a dynamic script and consists of spatial data
  for discrete timesteps.
*/
template<class T>
class CSFStack
{

private:

  //! Stackname of stack.
  CSFStackName     d_name;

  //! Currently loaded timestep.
  int              d_step;

  //! Minimum value in the whole stack.
  T                d_min;

  //! True if d_min is not a missing value.
  bool             d_minIsValid;

  //! Maximum value in the whole stack.
  T                d_max;

  //! True if d_max is not a missing value.
  bool             d_maxIsValid;

/*
  //! Map file pointer to the currently opened raster.
  CSFMap *         d_map;
*/

  //! Value scale of the raster.
  CSF_VS           d_valueScale;

  //! Use type of the raster.
  CSF_CR           d_useType;

  //! Pointer to currently loaded raster.
  Raster<T> *      d_raster;

  //! Legend, if available.
  std::auto_ptr<com::Legend<INT4> > d_legend;

  //! Assignment operator. NOT IMPLEMENTED.
  CSFStack &       operator=           (const CSFStack &);

  //! Frees dynamically allocated memory.
  void             clean               ();

  void             init                ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CSFStack            (std::string const& name,
                                        CSF_CR useType = CR_UNDEFINED);

  //! Constructor.
                   CSFStack            (const geo::CSFStackName &name,
                                        CSF_CR useType = CR_UNDEFINED);

/*
  //! Constructor.
                   CSFStack            (RasterSpace& space,
                                        CSF_CR useType = CR_UNDEFINED);
                                        */

  //! Copy constructor.
                   CSFStack            (const CSFStack &rhs);

  //! Destructor.
  /* virtual */    ~CSFStack           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Loads the data for timestep \a t in the stack.
  void             load                (size_t t = 0);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             loaded              (size_t t) const;

  bool             isAvailable         (size_t t) const;

  //! Returns the number of the first timestep in the stack.
  size_t           firstStep           () const;

  //! Returns the number of the last timestep in the stack.
  size_t           lastStep            () const;

  //! Returns the currently loaded raster.
  const Raster<T> &raster              () const;

  CSF_VS           valueScale          () const;

  //! Sets value pointed to by \a m to minimum of currently loaded raster.
  bool             min                 (T *m) const;

  //! Sets value pointed to by \a m to maximum of currently loaded raster.
  bool             max                 (T *m) const;

  //! Returns true if the stack only contains missing values.
  bool             allMV               () const;

  //! Returns the name of the stack.
  const CSFStackName &name             () const;

  const RasterSpace *rasterSpace       () const;

  bool             hasLegend           () const;

  const com::Legend<INT4>* legend      () const;

#ifdef DEBUG_DEVELOP
  friend std::ostream &operator<< <>   (std::ostream &s,
                                        const CSFStack &stack);
#endif

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

#ifdef DEBUG_DEVELOP
template<class T>
std::ostream &operator<<(std::ostream &s, const geo::CSFStack<T> &stack)
{
  s << stack.d_name
    << "  loaded timestep:  " << stack.d_step << '\n';

  s << std::flush;
  return s;
}
#endif



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------

// typedef CSFStack<BoolType>   BoolStack;
// typedef CSFStack<NomType>    NomStack;
// typedef CSFStack<OrdType>    OrdStack;
// typedef CSFStack<ScalType>   ScalStack;
// typedef CSFStack<DirectType> DirectStack;
// typedef CSFStack<LddType>    LddStack;



} // namespace geo

#endif
