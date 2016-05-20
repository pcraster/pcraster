#ifndef INCLUDED_CALC_VSPATIAL
#define INCLUDED_CALC_VSPATIAL



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // VSpatial declarations.
}



namespace calc {

/*!
 * \brief Interface of using different types (e.g. UINT1, INT4) by common larger
 * type at runtime
 */
template<typename OutType>
 class IVSpatial
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  IVSpatial&           operator=           (IVSpatial const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   IVSpatial               (IVSpatial const& rhs);


protected:
                    IVSpatial              () {} ;


public:
     virtual       ~IVSpatial              () {} ;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  //
  virtual bool    isMV                    (size_t i) const=0;
  /*!
   \warning can not return a MV correctly under all circumstances
            always use isMV() explictly
  */
  virtual OutType operator[]              (size_t i) const=0;

};

//! use different types (e.g. UINT1, INT4) by common larger type at runtime
/*!
 * types implemented are:
 * \verbatim
 *   VSpatial<INT4,UINT1>;
 *   VSpatial<INT4,INT4>;
 *   VSpatial<double,UINT1>;
 *   VSpatial<double,INT4>;
 *   VSpatial<double,REAL4>;
 * \endverbatim
 */
template<typename OutType,
         typename InType >
 class VSpatial : public IVSpatial<OutType>
{

  friend class VSpatialTest;

  const  InType *d_data;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  VSpatial&           operator=           (VSpatial const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   VSpatial               (VSpatial const& rhs);


protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    VSpatial              (InType const*data);

  /* virtual */    ~VSpatial              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  //
  bool             isMV                    (size_t i) const;
  OutType          operator[]              (size_t i) const;

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
