#ifndef INCLUDED_AG_DATAOBJECTBASE
#define INCLUDED_AG_DATAOBJECTBASE



// Library headers.
#include <memory>
#include <boost/noncopyable.hpp>

// PCRaster library headers.

// Module headers.
#include "ag_DataGuide.h"
#include "ag_DataManager.h"



namespace dal {
  class DataSpace;
  class DataSpaceAddress;
}
namespace ag {
  // DataObjectBase declarations.
  template<class T>
    class DataInfo;
}



namespace ag {

//! The DataObjectBase class is for creating, managing and deleting data.
/*!
*/
template<class T>
class DataObjectBase: private boost::noncopyable
{

  friend class DataObjectBaseTest;

private:

  //! Type of name, space, dataInfo tuples.
  typedef boost::tuple<std::string, dal::DataSpace, DataInfo<T> > Tuple;

  //! Type of tuple iterator.
  typedef typename std::vector<Tuple>::iterator tuple_iter;
  //
  //! Type of const tuple iterator.
  typedef typename std::vector<Tuple>::const_iterator const_tuple_iter;

  // TODO why is this a static collection?
  //! Collection with tuples.
  static std::vector<Tuple> _tuples;

  //! Data manager used to manage the data.
  DataManager<T>   _manager;

  tuple_iter       find                (T const* data) const;

  tuple_iter       find                (std::string const& name,
                                        dal::DataSpace const& space) const;

  bool             exists              (std::string const& name,
                                        dal::DataSpace const& space) const;

  virtual DataInfo<T> openData         (std::string const& name,
                                        dal::DataSpace const& space) const=0;

protected:

                   DataObjectBase      (geo::DataType dataType);

public:

  typedef typename DataManager<T>::data_iter data_iter;
  typedef typename DataManager<T>::const_data_iter const_data_iter;
  typedef typename DataManager<T>::const_guide_iter const_guide_iter;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~DataObjectBase     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  DataGuide        add                 (std::string const& name,
                                        dal::DataSpace const& space);

  void             clear               ();

  void             remove              (DataGuide const& guide);

  void             read                (dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           size                () const;

  bool             empty               () const;

  bool             isValid             (DataGuide const& guide) const;

  const DataInfo<T>* dataInfo          (DataGuide const& dataGuide) const;

  std::string      name                (DataGuide const& guide) const;

  DataGuide const& dataGuide           (geo::DataGuide const& guide) const;

  T&               data                (DataGuide const& guide);

  T const&         data                (DataGuide const& guide) const;

  data_iter        data_begin          ();

  data_iter        data_end            ();

  const_data_iter  data_begin          () const;

  const_data_iter  data_end            () const;

  const_guide_iter guides_begin        () const;

  const_guide_iter guides_end          () const;

  bool             isRead              (dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

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
