#ifndef INCLUDED_CALC_SPATIAL
#define INCLUDED_CALC_SPATIAL

#include "pcraster_model_engine_export.h"

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

#ifdef DEBUG_DEVELOP
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
#endif


namespace discr {

template<typename ValueType>
  class RasterData;
} // namespace discr



namespace calc {

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning( disable:4251 )
#endif


//! holds data for a spatial value
class PCR_ME_EXPORT Spatial : public Field {
public:
#ifdef DEBUG_DEVELOP
  //! different sizes created
  static std::set<size_t> d_sizes;
#endif
private:
  static size_t           d_maxBPC;
  static size_t           d_currentBPC;

  void          countBPC(VS vs) const;

  const size_t d_nrValues;

  /*! linear array with data (nrCols*nrRows) values
   *  The union is here for debug purposes
   */
  union {
   REAL4   *d_vals{};
   UINT1   *d_val1;
   INT4    *d_val4;

   void *d_val;
  };

  //! ptr to buffer that can hold the data, 0 if not allocated
  void *valuePtr() const;

  void    allocate();
  size_t  valLen() const;

  //! not implemented
  Spatial& operator=           (const Spatial& rhs);

public:
  Spatial(VS vs, CRIndex cri, size_t nrValues);

  Spatial(const Spatial& rhs);

  /*!
    It would be nice to be able to pass ownership of the pointer passed in
    to the resulting object. This could then prevent the extra allocation and
    copy currently done in the constructor.

    The pointer must be delete[]-able.

    There will be an issue on Windows when the memory is allocated in another
    dll (eg Dal), so either:
    - Make this support only available on Linux.
    - Use a wrapped pointer that knows how to delete itself safely (involved).

    Implementation options:
    - Extra constructor argument, which tells the implementation how to treat
      the pointer passed in (TakeOwnership, DoNotTakeOwnership).
    - Extra constructor that supports this scenario.
  */
  template<typename CR>
   Spatial(VS vs, const CR* data, size_t n):
     Field(vs,crIndex<CR>()), d_nrValues(n)
   {
     allocate();
     beMemCpyDest(data);
   }

                   Spatial             (VS vs,
                                        discr::RasterData<UINT1> const& data);

                   Spatial             (VS vs,
                                        discr::RasterData<INT4> const& data);

                   Spatial             (VS vs,
                                        discr::RasterData<REAL4> const& data);

  ~Spatial() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void        setCell(const double& value, size_t i) override;
  void       *dest() override;
  static void resetBPC();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool                isSpatial() const override;
  bool                isMV()      const override;
  size_t              nrValues()  const override;

  const void*    src                  ()         const override;
  bool           getCell              (double& value, size_t i) const override;

  Spatial*       createClone          () const override;
  void           analyzeBoolean       (bool& noneAreTrue,bool& noneAreFalse) const override;
  Spatial*       findMVinMask         (const std::vector<bool>& areaMask) const override;

  static size_t  maxBPC();
  static size_t  currentBPC();

};

#ifdef _MSC_VER
#pragma warning (pop)
#endif

}

#endif
