#ifndef INCLUDED_AG_DATAMANAGER
#define INCLUDED_AG_DATAMANAGER



#include <utility>
#include <set>
#include <vector>
#include <boost/noncopyable.hpp>
#include "ag_DataGuide.h"
#include "ag_DataInfo.h"



namespace ag {

//! The DataManager class is for keeping track of loaded data.
/*!
  This is a management class; no data is created or deleted here.

  \todo Create base class DataManagerBase with non-template stuff
        (eg DataGuides).
  \todo Get rid of time steps stuff.
*/
template<class T>
class DataManager: private boost::noncopyable
{

public:

  typedef typename std::vector< DataInfo<T> >::iterator data_iter;
  typedef typename std::vector< DataInfo<T> >::const_iterator const_data_iter;
  typedef typename std::vector<DataGuide>::iterator guide_iter;
  typedef typename std::vector<DataGuide>::const_iterator const_guide_iter;

private:

  // //! Data type of data managed.
  geo::DataType    _dataType;

  //! Collection of data managed.
  std::vector< DataInfo<T> > _data;

  //! Collection of guides pointing to data managed.
  std::vector<DataGuide> _guides;

  bool             isConsistent        () const;

  bool             isConsistent        (DataGuide const& guide) const;

  guide_iter       findGuide           (DataGuide::Address const& address);

  data_iter        findData            (DataGuide::Address const& address);

  const_data_iter  findData            (DataGuide::Address const& address) const;

  bool             exists              (DataGuide const& guide) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataManager         (geo::DataType dataType);

  virtual          ~DataManager        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  DataGuide        add                 (DataInfo<T> const& data);

  void             remove              (DataGuide const& guide);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isValid             (DataGuide const& guide) const;

  bool             exists              (DataGuide::Address const& address) const;

  DataGuide const& dataGuide           (geo::DataGuide const& guide) const;

  T&               data                (DataGuide const& guide);

  T const&         data                (DataGuide const& guide) const;

  size_t           size                () const;

  bool             empty               () const;

  std::set<size_t> timeSteps           (DataGuide const& guide) const;

  std::set<size_t> timeSteps           () const;

  data_iter        data_begin          ();

  data_iter        data_end            ();

  const_data_iter  data_begin          () const;

  const_data_iter  data_end            () const;

  const_guide_iter guides_begin        () const;

  const_guide_iter guides_end          () const;

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



} // namespace ag

#endif
