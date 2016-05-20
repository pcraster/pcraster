#ifndef INCLUDED_CALC_RASTERGRAPH
#define INCLUDED_CALC_RASTERGRAPH

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
#ifndef INCLUDED_CALC_IFIELDRDCONVERSION
#include "calc_ifieldrdconversion.h"
#define INCLUDED_CALC_IFIELDRDCONVERSION
#endif

// Module headers.
#ifndef INCLUDED_CALC_VFIELD
#include "calc_vfield.h"
#define INCLUDED_CALC_VFIELD
#endif



namespace calc {
  // RasterGraph declarations.
}

namespace calc {

typedef IFieldRDConversion::FieldId  FieldId;
typedef IFieldRDConversion::RasterId RasterId;

//! base for graph implementations on raster configurations.
/*!
    Notes:
    -  all edges are directed
*/
class RasterGraph
{
public:

  typedef unsigned char                Byte;

protected:

  //! part of raster with mv's
  /*!
   * these are vertices with no values and no edges
   * \post
   *   d_mv.size() <= d_nrVertices
   * \todo store as RunLength
   */
  std::vector<FieldId> d_mv;

  size_t               d_nrVertices;

  IFieldRDConversion  *d_frc;

                   RasterGraph               (size_t nrVertices);

  //! Assignment operator. NOT IMPLEMENTED.
  RasterGraph&           operator=           (RasterGraph const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   RasterGraph               (RasterGraph const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~RasterGraph              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  size_t           nrVertices                () const;
  void             unsetMVField              (BitField& mvField) const;

  template<typename D,typename S>
  void             initField                 (D *f, const S& value) const;

  template<typename D, typename S>
  void             copyField                 (D *dest, const S* src) const;
  template<typename D, typename S>
  void             copyField                 (D *dest, const VField<S>& src) const;

  const IFieldRDConversion& iFieldRDConversion() const;

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
