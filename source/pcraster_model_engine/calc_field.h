#ifndef INCLUDED_CALC_FIELD
#define INCLUDED_CALC_FIELD

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_CALC_CR
#include "calc_cr.h"
#define INCLUDED_CALC_CR
#endif

#ifndef INCLUDED_CALC_DATAVALUE
#include "calc_datavalue.h"
#define INCLUDED_CALC_DATAVALUE
#endif

namespace calc {


class DataType;
class Spatial;


class PCR_DLL_CLASS Field : public DataValue {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Field&           operator=           (const Field&);


  //! possible set of vs's
  VS               d_vs;
  //! cell representation
  CRIndex          d_cri;

protected:
                   Field               (VS vs, CRIndex cri=CRI_X);
                   Field               (const Field&);

public:
/*
  //! thrown by Handle&lt;Field&gt; FieldValue::value()
  struct NotInitialized {
  };
*/

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

   virtual         ~Field              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  virtual  Field*   load               ();
  virtual  void*    dest               ()=0;
  UINT1*            dest_1             ();
  REAL4*            dest_f             ();
  INT4*             dest_4             ();
  void              beMemCpyDest       (const void *src);

  void              resetVs            (VS newVs);

  //! set cell at linear index \a i to \a value, value can be MV
  /*!
   * only used by calc::LookupExpr::execute and PCRasterPython
   *  \throws DomainError  if a NonSpatial is set to MV
   */
  virtual void          setCell        (const double& value, size_t i)=0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual const void *  src            () const=0;
  template<typename T>
  const T            *  src_t          () const;
  const UINT1        *  src_1          () const;
  const REAL4        *  src_f          () const;
  const INT4         *  src_4          () const;
  void                  beMemCpySrc    (void *dest) const;

  virtual bool          isSpatial      () const=0;
  virtual bool          isMV           () const=0;
  virtual Field      *  createClone    () const=0;


  CRIndex               cri            () const;
  CSF_CR                cr             () const;
  OVS                   ovs            () const;
  VS                    vs             () const;
  DataType              type           () const;

  //! analyze a boolean field, PRECOND (vs() == VS_B)
  virtual void          analyzeBoolean (bool& noneAreTrue,bool& noneAreFalse) const=0;
  virtual Field*        findMVinMask   (const std::vector<bool>& areaMask) const;

  //! get value at linear index \a i as double,store in \a value
  /*!
   * \return
   *   false if value is MV, true otherwise
   */
  virtual bool          getCell        (double& value, size_t i) const=0;

  //! return nr of cells if spatial or 1 if nonspatial
  virtual size_t        nrValues       () const=0;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
inline const T*  Field::src_t() const
{
  return static_cast<const T *>(src());
}


//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

Field *createDestCloneIfReadOnly(Field *v);

} // namespace calc

std::ostream &operator<<(std::ostream& s, const calc::Field& f);

#endif
