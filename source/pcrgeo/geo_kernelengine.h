#ifndef INCLUDED_GEO_KERNELENGINE
#define INCLUDED_GEO_KERNELENGINE

#include "stddefx.h"
#include "com_labeledprogresstracked.h"
#include "com_progressbar.h"
#include "geo_kernel.h"
#include "geo_kernelsourcebuffer.h"
#include "geo_simpleraster.h"



namespace geo {
  // KernelEngine declarations.
}



namespace geo {



//! The KernelEngine class operates a kernel on a raster.
/*!
  Running the engine operates a kernel on the cells of a source raster and
  fills a target raster.
*/
// T: source raster value type
// U: buffer raster value type
// V: target raster value type
template<class T, class U, class V>
class KernelEngine: public com::LabeledProgressTracked<com::ProgressBar>
{

private:

  //! Source buffer.
  KernelSourceBuffer<T, U>& d_sourceBuffer;

  //! Target raster.
  SimpleRaster<V>& d_targetRaster;

  //! Kernel.
  const Kernel<U, V>& d_kernel;

  //! Assignment operator. NOT IMPLEMENTED.
  KernelEngine&    operator=           (const KernelEngine&);

  //! Copy constructor. NOT IMPLEMENTED.
                   KernelEngine        (const KernelEngine&);

protected:

  KernelSourceBuffer<T, U>& sourceBuffer();

  SimpleRaster<V>& targetRaster        ();

  const Kernel<U, V>& kernel           () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   KernelEngine        (KernelSourceBuffer<T, U>& sourceBuffer,
                                        SimpleRaster<V>& targetRaster,
                                        Kernel<U, V>& kernel);

  virtual          ~KernelEngine       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  virtual void     run                 ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
