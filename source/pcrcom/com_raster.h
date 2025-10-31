#ifndef INCLUDED_COM_RASTER
#define INCLUDED_COM_RASTER

#include "stddefx.h"
#include "com_iraster.h"

#include <algorithm>



namespace com {
  // Raster declarations.
}



namespace com {



/*!
 *
 */
template<typename T>
class Raster: public IRaster<T>
{

private:

  //! Only raster value.
  T                *d_values;

  //! Assignment operator. NOT IMPLEMENTED.
  Raster& operator=        (const Raster&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Raster  (const Raster&);


  T&               operator[]          (size_t index) override;

  const T&         operator[]          (size_t index) const override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Raster  (size_t nrRows,
                                        size_t nrCols);

                   Raster  (size_t nrRows,
                                        size_t nrCols,
                                        const T& value);

  /* virtual */    ~Raster () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Raster<T>&       add                 (const IRaster<T>& rhs) override;
  T*               asCArray           ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isMultiValued      () const override;
  const T*         asCArray           () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class T>
inline Raster<T>::~Raster ()
{
  delete[] d_values;
}

template<class T>
inline Raster<T>::Raster(size_t nrRows, size_t nrCols):
  IRaster<T>(nrRows,nrCols), d_values(new T[nrRows * nrCols])
{
}

template<class T>
inline Raster<T>::Raster(size_t nrRows, size_t nrCols, const T& value):
  IRaster<T>(nrRows,nrCols),d_values(new T[nrRows * nrCols])
{
  std::fill(d_values, d_values + IRaster<T>::nrCells(), static_cast<T>(value));
}

template<class T>
inline const T& Raster<T>::operator[](size_t index) const
{
  DEVELOP_PRECOND(index < IRaster<T>::nrCells());
  return d_values[index];
}

template<class T>
inline T& Raster<T>::operator[](size_t index)
{
  DEVELOP_PRECOND(index < IRaster<T>::nrCells());
  return d_values[index];
}

template<class T>
inline bool Raster<T>::isMultiValued () const
{
  return true;
}

template<class T>
inline const T*  Raster<T>::asCArray () const
{
  return d_values;
}

template<class T>
inline T*  Raster<T>::asCArray ()
{
  return d_values;
}


//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
