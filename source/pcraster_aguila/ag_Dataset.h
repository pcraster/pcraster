#ifndef INCLUDED_AG_DATASET
#define INCLUDED_AG_DATASET



// Library headers.
#include <string>
#include <boost/any.hpp>
#include <boost/noncopyable.hpp>

// PCRaster library headers.
#include "dal_DataSource.h"

// Module headers.



namespace dal {
  class DataSpace;
  class DataSpaceAddress;
  class DataSpaceAddressMapper;
}
namespace ag {
  // Dataset declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class Dataset: private boost::noncopyable
{

  friend class DatasetTest;

private:

  std::string      _name;

  dal::DataSource  _source;

  boost::any       _min;

  boost::any       _max;

  dal::DataSpaceAddressMapper* _localToWorldMapper;

  dal::DataSpaceAddressMapper* _globalToLocalMapper;

  //! Data space address of data read in local (data set) coordinates.
  dal::DataSpaceAddress _addressRead;

  boost::any       _selectedValue;

  void             initialiseLocalToWorldMapper(
                                        dal::DataSpace const& space);

  void             initialiseGlobalToLocalMapper(
                                        dal::DataSpace const& space);

protected:

  // void             setDataSpace        (dal::DataSpace const& space);

  dal::DataSpaceAddress localAddress   (dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  dal::DataSource&  dataSource         ();

  dal::DataSource const& dataSource    () const;

  virtual bool     isRead              (dal::DataSpaceAddress const& address) const=0;

  /// boost::any const& selectedValue      () const;

  dal::DataSpaceAddress const& addressRead() const;

  void             setAddressRead      (dal::DataSpaceAddress const& address);

  void             setExtremes         (boost::any const& min,
                                        boost::any const& max);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Dataset             (std::string const& name,
                                        dal::DataSpace const& space);

  virtual          ~Dataset            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setSelectedValue    (REAL4 value);

  void             unsetSelectedValue  ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  dal::DataSpace const& dataSpace      () const;

  std::string const& name              () const;

  dal::DataSpaceAddressMapper const& localToWorldMapper() const;

  dal::DataSpaceAddressMapper& localToWorldMapper();

  dal::DataSpaceAddressMapper const& globalToLocalMapper() const;

  dal::DataSpaceAddressMapper& globalToLocalMapper();

  bool             hasSelectedValue    () const;

  REAL4            selectedValue       () const;

  bool             isRead              (dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  bool             allMV               () const;

  template<typename T>
  bool             min                 (T& value) const;

  template<typename T>
  bool             max                 (T& value) const;

  template<typename T>
  T                min                 () const;

  template<typename T>
  T                max                 () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
bool Dataset::min(
         T& value) const
{
  if(!_min.empty()) {
    value = boost::any_cast<T>(_min);

    if(!pcr::isMV(value)) {
      return true;
    }
  }

  return false;
}



template<typename T>
bool Dataset::max(
         T& value) const
{
  if(!_max.empty()) {
    value = boost::any_cast<T>(_max);
    if(!pcr::isMV(value)) {
      return true;
    }
  }

  return false;
}

template<typename T>
T Dataset::min() const
{
  assert(!_min.empty());
  return boost::any_cast<T>(_min);
}

template<typename T>
T Dataset::max() const
{
  assert(!_max.empty());
  return boost::any_cast<T>(_max);
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

#endif
