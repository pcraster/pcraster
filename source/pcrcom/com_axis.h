#ifndef INCLUDED_COM_AXIS
#define INCLUDED_COM_AXIS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



//namespace pack {



/*!
  \class com_Axis
  \brief The com_Axis class is for describing a range in datavalues.

  The class is very simple and can be combined with a com_Classifier
  specialisation to make a classification.
*/
//       1         2         3         4         5         6         7         8
template<class ValueType>
class com_Axis
{

private:

  //! Minimum datavalue.
  ValueType        d_min;

  //! Minimum cutoff value.
  ValueType        d_minCutOff;

  //! Maximum datavalue.
  ValueType        d_max;

  //! Maximum cutoff value.
  ValueType        d_maxCutOff;

  //! Assignment operator. NOT IMPLEMENTED.
  com_Axis &       operator=           (const com_Axis &a);

  //! Copy constructor. NOT IMPLEMENTED.
                   com_Axis            (const com_Axis &a);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Default constructor.
                   com_Axis            ();

  //! Constructor.
                   com_Axis            (ValueType min,
                                        ValueType max);

  //! Destructor.
                   ~com_Axis           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Sets the minimum value and minimum cutoff value.
  void             setMin              (ValueType min,
                                        ValueType minCutOff);

  //! Sets the maximum value and maximum cutoff value.
  void             setMax              (ValueType max,
                                        ValueType maxCutOff);

  //! Sets the cutoff range to \a min - \a max.
  void             setCutOff           (ValueType min,
                                        ValueType max);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the minimum value.
  ValueType        getMin              () const;

  //! Returns the maximum value.
  ValueType        getMax              () const;

  //! Returns the minimum cutoff value.
  ValueType        getMinCutOff        () const;

  //! Returns the maximum cutoff value.
  ValueType        getMaxCutOff        () const;

  //! Returns the range in values.
  ValueType        getRange            () const;

  //! Returns the range in cutoff values.
  ValueType        getCutOffRange      () const;

};



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------



//} // namespace pack

#endif
