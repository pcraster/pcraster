#ifndef INCLUDED_GEO_KERNELENGINE
#define INCLUDED_GEO_KERNELENGINE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_LABELEDPROGRESSTRACKED
#include "com_labeledprogresstracked.h"
#define INCLUDED_COM_LABELEDPROGRESSTRACKED
#endif

#ifndef INCLUDED_COM_PROGRESSBAR
#include "com_progressbar.h"
#define INCLUDED_COM_PROGRESSBAR
#endif

// Module headers.
#ifndef INCLUDED_GEO_KERNEL
#include "geo_kernel.h"
#define INCLUDED_GEO_KERNEL
#endif

#ifndef INCLUDED_GEO_KERNELSOURCEBUFFER
#include "geo_kernelsourcebuffer.h"
#define INCLUDED_GEO_KERNELSOURCEBUFFER
#endif

#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif



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
