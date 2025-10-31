#ifndef INCLUDED_COM_SINGLEVALUEDRASTER
#define INCLUDED_COM_SINGLEVALUEDRASTER

#include "stddefx.h"
#include "com_iraster.h"



namespace com {
  // SingleValuedRaster declarations.
}



namespace com {



/*! a single value (aka non spatial), viewed as raster
 *   with identical values everywhere
 */
template<typename T>
class SingleValuedRaster: public IRaster<T>
{

private:

  //! Only raster value.
  T                d_value;

  //! Assignment operator. NOT IMPLEMENTED.
  SingleValuedRaster& operator=        (const SingleValuedRaster&);

  //! Copy constructor. NOT IMPLEMENTED.
                   SingleValuedRaster  (const SingleValuedRaster&);


  T&               operator[]         (size_t index) override;

  const T&         operator[]         (size_t index) const override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SingleValuedRaster  (size_t nrRows,
                                        size_t nrCols);

                   SingleValuedRaster  (size_t nrRows,
                                        size_t nrCols,
                                        const T& value);

  /* virtual */    ~SingleValuedRaster () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  SingleValuedRaster<T>& add           (const IRaster<T>& rhs) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isMultiValued      () const override;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class T>
inline SingleValuedRaster<T>::~SingleValuedRaster ()
{
}

template<class T>
inline SingleValuedRaster<T>::SingleValuedRaster(size_t nrRows, size_t nrCols):
  IRaster<T>(nrRows,nrCols),d_value()
{
}

template<class T>
inline SingleValuedRaster<T>::SingleValuedRaster(size_t nrRows, size_t nrCols, const T& value):
  IRaster<T>(nrRows,nrCols),d_value(value)
{
}

template<class T>
inline const T& SingleValuedRaster<T>::operator[](size_t /* index */) const
{
  return d_value;
}

template<class T>
inline T& SingleValuedRaster<T>::operator[](size_t /* index */)
{
  return d_value;
}

template<class T>
inline bool SingleValuedRaster<T>::isMultiValued () const
{
  return false;
}


//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
