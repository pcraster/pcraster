#ifndef INCLUDED_CALC_SPATIAL
#define INCLUDED_CALC_SPATIAL

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

namespace calc {

class IScript;
class GridMap;
class Compressor;

//! holds data for a spatial value
/*! mutable fields are meant for "late" reading of data
 *  through loadExternal()
 */
class Spatial : public Field {
private:
  static size_t d_maxBPC;
  //! the number of bytes max saved
  static size_t d_currentBPC;
  void          countBPC(VS vs) const;

  void makeDataAvailable() const;

     //! linear array with data (nrCols*nrRows) values
     /*! if d_val is 0 then loadExternal() must be called to
      *  try initialization.
      *  The union is here for debug purposes
      */
  union {
   mutable REAL4   *d_vals;
   mutable UINT1   *d_val1;
   mutable INT4    *d_val4;

   mutable void *d_val;
  };

  //! (potential) size of d_val array
  /*! even if d_val, d_nrValues is initialized to the number of values
   *  allocated/filled later
   */
  const size_t d_nrValues;

protected:
  //! called to load data from outside (file) prior to returning value
  /*! if nessecary this method is reimplemented
   */
  virtual void loadExternal() const;

  //! ptr to buffer that can hold the data, 0 if not allocated
  void *valuePtr() const;


  void    allocate() const;
  size_t  valLen() const;

public:
  Spatial(VS vs, size_t nrValues, bool doAllocation);
  Spatial(VS vs, size_t nrValues, void *valueBuffer);
  virtual ~Spatial();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void       *destValue();
  virtual void        setCell(const double& value, size_t i);

  void        loadGrid(GridMap& m,
                       const Compressor& c) const;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool           isMv()const;
  bool           isSpatial() const;
  const void *   srcValue() const;
  virtual Spatial *copy() const;
  virtual void   analyzeBoolean(bool& noneAreTrue,bool& noneAreFalse) const;
  bool           checkDebug(const IScript& s, bool& allZero,size_t& bpc) const;
  size_t         nrValues() const;

  virtual bool   getCell(double& value, size_t i) const;

  static size_t  maxBPC();
  static size_t  currentBPC();

};

}

#endif
