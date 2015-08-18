#ifndef INCLUDED_CALC_FIELD
#define INCLUDED_CALC_FIELD

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_CALC_OBJCOUNT
#include "calc_objcount.h"
#define INCLUDED_CALC_OBJCOUNT
#endif

#ifndef INCLUDED_CALC_HANDLE
#include "calc_handle.h"
#define INCLUDED_CALC_HANDLE
#endif

namespace calc {



class Field : private ObjCount<Field> {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Field&           operator=           (const Field&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Field               (const Field&);

  VS               d_vs;
protected:
  Field(VS vs );

public:
  //! thrown by Handle&lt;Field&gt; FieldValue::value()
  struct NotInitialized {
  };

  //! thrown by NonSpatial::setCell()
  struct SetNonSpatialToMV {
  };


  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Field               ();

   virtual         ~Field              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  virtual       void *  destValue      () =0;
  void                  resetVs        (VS newVs);

  //! set cell at linear index \a i to \a value, value can be MV
  /*!
   *  \throws SetNonSpatialToMV
   */
  virtual void          setCell         (const double& value, size_t i)=0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual const void *  srcValue       () const=0;
  virtual bool          isSpatial      () const=0;
  virtual bool          isMv           () const=0;

  virtual Field *       copy           () const=0;

  VS                    vs             () const;

  //! analyze a boolean field, PRECOND (vs() == VS_B)
  virtual void          analyzeBoolean (bool& noneAreTrue,bool& noneAreFalse) const=0;

  //! get value at linear index \a i as double,store in \a value
  /*!
   * \return
   *   false if value is MV, true otherwise
   */
  virtual bool          getCell        (double& value, size_t i) const=0;

  //! return nr of cells if spatial or 1 if nonspatial
  virtual size_t        nrValues       () const=0;

};

typedef Handle<Field>  FieldHandle;


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
