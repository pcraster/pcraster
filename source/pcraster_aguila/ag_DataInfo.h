#ifndef INCLUDED_APP_DATAINFO
#define INCLUDED_APP_DATAINFO



// Library headers.

// PCRaster library headers.
#include "dal_DataSpace.h"
#include "csftypes.h"

// Module headers.



namespace ag {
  // DataInfo declarations.
  class RangeDrawProps;
}



namespace ag {

//! The DataInfo class contains all data info needed by the DataManager.
/*!
  This class just combines some useful information about a data set.

  \todo Get rid of the draw properties.
*/
template<class T>
class DataInfo
{

  template<class U>
  friend bool operator==(DataInfo<U> const&, DataInfo<U> const&);

  template<class U>
  friend bool operator!=(DataInfo<U> const&, DataInfo<U> const&);

private:

  //! Pointer to the data.
  T*               _data;

  //! Value scale of the data.
  CSF_VS           _valueScale;

  //! Data space defined by data set.
  dal::DataSpace   _space;

  //! Draw properties in case data is continuous.
  RangeDrawProps*  _rangeDrawProperties;

  bool             dataIsContinuous    () const;

  bool             equals              (DataInfo const& info) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataInfo            (T* data);

                   DataInfo            (T* data,
                                        CSF_VS valueScale);

                   DataInfo            (T* data,
                                        CSF_VS valueScale,
                                        dal::DataSpace const& space);

                   DataInfo            (T* data,
                                        CSF_VS valueScale,
                                        dal::DataSpace const& space,
                                        RangeDrawProps* drawProperties);

  /* virtual */    ~DataInfo           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  T*               data                ();

  const T*         data                () const;

  CSF_VS           valueScale          () const;

  dal::DataSpace const& space          () const;

  const RangeDrawProps* rangeDrawProperties() const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

template<class T>
inline bool operator==(
         DataInfo<T> const& lhs,
         DataInfo<T> const& rhs)
{
  return lhs.equals(rhs);
}



template<class T>
inline bool operator!=(
         DataInfo<T> const& lhs,
         DataInfo<T> const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

#endif
