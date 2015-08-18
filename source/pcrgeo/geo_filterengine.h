#ifndef INCLUDED_GEO_FILTERENGINE
#define INCLUDED_GEO_FILTERENGINE



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
#ifndef INCLUDED_GEO_FILTER
#include "geo_filter.h"
#define INCLUDED_GEO_FILTER
#endif

#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif



namespace geo {
  // FilterEngine declarations.
}



namespace geo {



//! FilterEngine objects apply a Filter to a SimpleRaster.
/*!
  This class implements the logic of applying a Filter object to a SimpleRaster
  object taking corners and borders into account. These regions of a raster
  are special because they don't have values for all cells of the filter. The
  upper left corner for example doesn't have cells to the left and top of it.

  Optionally, the user of this class can decide to install a com::Progress
  object to give the end user some feed-back on progress of the calculations.
*/
template<class SrcType, class DstType>
class FilterEngine: public com::LabeledProgressTracked<com::ProgressBar>
{

private:

  //! Source raster.
  const SimpleRaster<SrcType>& d_source;

  //! Filter to apply to source raster.
  const Filter<SrcType, DstType>& d_filter;

  //! Destination raster.
  SimpleRaster<DstType>& d_destination;

  //! Assignment operator. NOT IMPLEMENTED.
  FilterEngine&    operator=           (const FilterEngine&);

  //! Copy constructor. NOT IMPLEMENTED.
                   FilterEngine        (const FilterEngine&);

  inline void      calcUL              ();

  inline void      calcUL              (SimpleRaster<UINT1> const& sample);

  inline void      calcUR              ();

  inline void      calcUR              (SimpleRaster<UINT1> const& sample);

  inline void      calcLR              ();

  inline void      calcLR              (SimpleRaster<UINT1> const& sample);

  inline void      calcLL              ();

  inline void      calcLL              (SimpleRaster<UINT1> const& sample);

  inline void      calcTop             ();

  inline void      calcTop             (SimpleRaster<UINT1> const& sample);

  inline void      calcBottom          ();

  inline void      calcBottom          (SimpleRaster<UINT1> const& sample);

  inline void      calcLeft            ();

  inline void      calcLeft            (SimpleRaster<UINT1> const& sample);

  inline void      calcRight           ();

  inline void      calcRight           (SimpleRaster<UINT1> const& sample);

  inline void      calcInterior        ();

  inline void      calcInterior        (SimpleRaster<UINT1> const& sample);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  inline           FilterEngine        (const SimpleRaster<SrcType>& source,
                                        const Filter<SrcType, DstType>& filter,
                                        SimpleRaster<DstType>& destination);

  inline           ~FilterEngine       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  inline void      calc                ();

  inline void      calc                (SimpleRaster<UINT1> const& sample);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   Don't destroy \a source, \a filter and \a destination while we
             have them.
  \sa        .
*/
template<class SrcType, class DstType>
FilterEngine<SrcType, DstType>::FilterEngine(
                   const SimpleRaster<SrcType>& source,
                   const Filter<SrcType, DstType>& filter,
                   SimpleRaster<DstType>& destination)

  : com::LabeledProgressTracked<com::ProgressBar>(),
    d_source(source), d_filter(filter), d_destination(destination)

{
}

template<class SrcType, class DstType>
FilterEngine<SrcType, DstType>::~FilterEngine()
{
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calc()
{
  // Progress stuff. Each time a row is processes, finishedStep() should
  // be called.
  com::ProgressTracked<com::LabeledProgressTracker<com::ProgressBar> >::init(
         d_source.nrRows());

  // Corners.
  calcUL();
  calcUR();
  calcLR();
  calcLL();

  // Sides.
  calcTop();
  calcBottom();
  calcLeft();
  calcRight();

  // Interior.
  calcInterior();
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calc(SimpleRaster<UINT1> const& sample)
{
  // Progress stuff. Each time a row is processes, finishedSteps() should
  // be called.
  com::ProgressTracked<com::LabeledProgressTracker<com::ProgressBar> >::init(
         d_source.nrRows());

  // Corners.
  calcUL(sample);
  calcUR(sample);
  calcLR(sample);
  calcLL(sample);

  // Sides.
  calcTop(sample);
  calcBottom(sample);
  calcLeft(sample);
  calcRight(sample);

  // Interior.
  calcInterior(sample);
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcUL()
{
  for(size_t row = 0; row < d_filter.radius(); ++row) {
    for(size_t col = 0; col < d_filter.radius(); ++col) {

      DEVELOP_POSTCOND(row <= d_source.nrRows());
      DEVELOP_POSTCOND(col <= d_source.nrCols());

      d_destination.cell(row, col) = d_filter.calcUL(d_source, row, col);
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcUL(SimpleRaster<UINT1> const& sample)
{
  for(size_t row = 0; row < d_filter.radius(); ++row) {
    for(size_t col = 0; col < d_filter.radius(); ++col) {

      DEVELOP_POSTCOND(row <= d_source.nrRows());
      DEVELOP_POSTCOND(col <= d_source.nrCols());

      if(!sample.isMV(row, col) && sample.cell(row, col) != 0) {
        d_destination.cell(row, col) = d_filter.calcUL(d_source, row, col);
      }
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcUR()
{
  size_t maxRow = std::min(d_filter.radius(), d_source.nrRows());
  size_t minCol = d_source.nrCols() > d_filter.radius()
         ? d_source.nrCols() - d_filter.radius() : 0;

  for(size_t row = 0; row < maxRow; ++row) {
    for(size_t col = minCol; col < d_source.nrCols(); ++col) {

      d_destination.cell(row, col) = d_filter.calcUR(d_source, row, col);
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcUR(SimpleRaster<UINT1> const& sample)
{
  for(size_t row = 0; row < d_filter.radius(); ++row) {
    for(size_t col = d_source.nrCols() - d_filter.radius();
         col < d_source.nrCols(); ++col) {

      if(!sample.isMV(row, col) && sample.cell(row, col) != 0) {
        d_destination.cell(row, col) = d_filter.calcUR(d_source, row, col);
      }
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcLR()
{
  for(size_t row = d_source.nrRows() - d_filter.radius();
         row < d_source.nrRows(); ++row) {
    for(size_t col = d_source.nrCols() - d_filter.radius();
         col < d_source.nrCols(); ++col) {

      d_destination.cell(row, col) = d_filter.calcLR(d_source, row, col);
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcLR(SimpleRaster<UINT1> const& sample)
{
  for(size_t row = d_source.nrRows() - d_filter.radius();
         row < d_source.nrRows(); ++row) {
    for(size_t col = d_source.nrCols() - d_filter.radius();
         col < d_source.nrCols(); ++col) {

      if(!sample.isMV(row, col) && sample.cell(row, col) != 0) {
        d_destination.cell(row, col) = d_filter.calcLR(d_source, row, col);
      }
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcLL()
{
  for(size_t row = d_source.nrRows() - d_filter.radius();
         row < d_source.nrRows(); ++row) {
    for(size_t col = 0; col < d_filter.radius(); ++col) {

      d_destination.cell(row, col) = d_filter.calcLL(d_source, row, col);
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcLL(SimpleRaster<UINT1> const& sample)
{
  for(size_t row = d_source.nrRows() - d_filter.radius();
         row < d_source.nrRows(); ++row) {
    for(size_t col = 0; col < d_filter.radius(); ++col) {

      if(!sample.isMV(row, col) && sample.cell(row, col) != 0) {
        d_destination.cell(row, col) = d_filter.calcLL(d_source, row, col);
      }
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcTop()
{
  for(size_t col = d_filter.radius();
         col < d_source.nrCols() - d_filter.radius(); ++col) {
    for(size_t row = 0; row < d_filter.radius(); ++row) {

      d_destination.cell(row, col) = d_filter.calcTop(d_source, row, col);
    }
  }

  finishedSteps(d_filter.radius());
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcTop(SimpleRaster<UINT1> const& sample)
{
  for(size_t col = d_filter.radius();
         col < d_source.nrCols() - d_filter.radius(); ++col) {
    for(size_t row = 0; row < d_filter.radius(); ++row) {

      if(!sample.isMV(row, col) && sample.cell(row, col) != 0) {
        d_destination.cell(row, col) = d_filter.calcTop(d_source, row, col);
      }
    }
  }

  finishedSteps(d_filter.radius());
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcBottom()
{
  for(size_t col = d_filter.radius();
         col < d_source.nrCols() - d_filter.radius(); ++col) {
    for(size_t row = d_source.nrRows() - d_filter.radius();
         row < d_source.nrRows(); ++row) {

      d_destination.cell(row, col) = d_filter.calcBottom(d_source, row, col);
    }
  }

  finishedSteps(d_filter.radius());
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcBottom(SimpleRaster<UINT1> const& sample)
{
  for(size_t col = d_filter.radius();
         col < d_source.nrCols() - d_filter.radius(); ++col) {
    for(size_t row = d_source.nrRows() - d_filter.radius();
         row < d_source.nrRows(); ++row) {

      if(!sample.isMV(row, col) && sample.cell(row, col) != 0) {
        d_destination.cell(row, col) = d_filter.calcBottom(d_source, row, col);
      }
    }
  }

  finishedSteps(d_filter.radius());
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcLeft()
{
  for(size_t row = d_filter.radius();
         row < d_source.nrRows() - d_filter.radius(); ++row) {
    for(size_t col = 0; col < d_filter.radius(); ++col) {

      d_destination.cell(row, col) = d_filter.calcLeft(d_source, row, col);
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcLeft(SimpleRaster<UINT1> const& sample)
{
  for(size_t row = d_filter.radius();
         row < d_source.nrRows() - d_filter.radius(); ++row) {
    for(size_t col = 0; col < d_filter.radius(); ++col) {

      if(!sample.isMV(row, col) && sample.cell(row, col) != 0) {
        d_destination.cell(row, col) = d_filter.calcLeft(d_source, row, col);
      }
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcRight()
{
  for(size_t row = d_filter.radius();
         row < d_source.nrRows() - d_filter.radius(); ++row) {
    for(size_t col = d_source.nrCols() - d_filter.radius();
         col < d_source.nrCols(); ++col) {

      d_destination.cell(row, col) = d_filter.calcRight(d_source, row, col);
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcRight(SimpleRaster<UINT1> const& sample)
{
  for(size_t row = d_filter.radius();
         row < d_source.nrRows() - d_filter.radius(); ++row) {
    for(size_t col = d_source.nrCols() - d_filter.radius();
         col < d_source.nrCols(); ++col) {

      if(!sample.isMV(row, col) && sample.cell(row, col) != 0) {
        d_destination.cell(row, col) = d_filter.calcRight(d_source, row, col);
      }
    }
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcInterior()
{
  // Skip radius cells on all sides.
  for(size_t row = d_filter.radius();
         row < d_source.nrRows() - d_filter.radius(); ++row) {
    for(size_t col = d_filter.radius();
         col < d_source.nrCols() - d_filter.radius(); ++col) {

      d_destination.cell(row, col) = d_filter.calcInterior(d_source, row, col);
    }

    finishedStep();
  }
}

template<class SrcType, class DstType>
void FilterEngine<SrcType, DstType>::calcInterior(SimpleRaster<UINT1> const& sample)
{
  // Skip radius cells on all sides.
  for(size_t row = d_filter.radius();
         row < d_source.nrRows() - d_filter.radius(); ++row) {
    for(size_t col = d_filter.radius();
         col < d_source.nrCols() - d_filter.radius(); ++col) {

      if(!sample.isMV(row, col) && sample.cell(row, col) != 0) {
        d_destination.cell(row, col) = d_filter.calcInterior(d_source, row, col);
      }
    }

    finishedStep();
  }
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
