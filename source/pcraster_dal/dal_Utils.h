#ifndef INCLUDED_DAL_UTILS
#define INCLUDED_DAL_UTILS



// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// #include <boost/spirit/actor/ref_value_actor.hpp>
// #include <boost/spirit/actor/ref_const_ref_actor.hpp>

#ifndef INCLUDED_BOOST_TUPLE_TUPLE
#include <boost/tuple/tuple.hpp>
#define INCLUDED_BOOST_TUPLE_TUPLE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_ARRAY
#include "dal_Array.h"
#define INCLUDED_DAL_ARRAY
#endif

#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif



namespace dal {
  // Utils declarations.
  class DataSpace;
  class DataSpaceAddress;
  class Dimension;
}



namespace dal {

void*              newBuffer           (TypeId typeId,
                                        size_t size);

void               deleteBuffer        (TypeId typeId,
                                        void* buffer);

  void             toStdMV             (TypeId typeId,
                                        void* buffer,
                                        size_t nrValues,
                                        double mv);

  template<typename T>
  void             toStdMV             (T* buffer,
                                        size_t nrValues,
                                        T mv)
  {
    std::for_each(buffer, buffer + nrValues, pcr::AlterToStdMV<T>(mv));
  }

  template<typename T>
  void             toStdMV             (T* begin,
                                        T* end,
                                        T mv)
  {
    std::for_each(begin, end, pcr::AlterToStdMV<T>(mv));
  }

  template<typename T>
  void             fromStdMV           (T* buffer,
                                        size_t nrValues,
                                        T mv)
  {
    std::for_each(buffer, buffer + nrValues, pcr::AlterFromStdMV<T>(mv));
  }

  /*! \todo correct name's? maybe copy names of std::numeric_limits or
   *        boost::type_traits
   */
  inline bool      isSignedInteger     (TypeId typeId)
  {
    return typeId == TI_INT1 || typeId == TI_INT2 || typeId == TI_INT4;
  }

  inline bool      isUnsignedInteger   (TypeId typeId)
  {
    return typeId == TI_UINT1 || typeId == TI_UINT2 || typeId == TI_UINT4;
  }

  inline bool      isInteger           (TypeId typeId)
  {
    return isSignedInteger(typeId) || isUnsignedInteger(typeId);
  }

  inline bool      isFloatingPoint     (TypeId typeId)
  {
    return typeId == TI_REAL4 || typeId == TI_REAL8;
  }

  inline bool      isNumeric           (TypeId typeId)
  {
    return isInteger(typeId) || isFloatingPoint(typeId);
  }

//! Actor which can be attached to a spirit parser.
/*!
  This actor can be used in cases where a vector of strings should be filled
  with strings parsed.
*/
struct PushBack {
private:
  //! Assignment operator. NOT IMPLEMENTED.
  PushBack&       operator=           (PushBack const& rhs);

public:
  //! Reference to the collection to fill.
  std::vector<std::string>& d_strings;

  //! Constructor.
  /*!
    \param     strings Collection to fill.
  */
  PushBack(std::vector<std::string>& strings)
    : d_strings(strings)
  {}

  //! This operator will be called by the parser.
  /*!
    \param     first Iterator to start of parsed string.
    \param     last  Iterator to one-past-the-end location of parsed string.

    A string with the characters pointed to by the iterators is stored in
    the layered container.
  */
  template <typename Iterator>
  void operator() (Iterator const& first, Iterator const& last) const {
    d_strings.push_back(std::string(first, last - first));
  }
};

PCR_DAL_DECL CSF_VS typeIdToValueScale(TypeId typeId);

PCR_DAL_DECL std::string valueScaleToString(CSF_VS valueScale);

PCR_DAL_DECL std::string typeIdToString(TypeId typeId);

PCR_DAL_DECL bool isDatasetType       (std::string const& name);

PCR_DAL_DECL std::string  datasetTypeToString(
                                        DatasetType type);

PCR_DAL_DECL DatasetType  stringToDatasetType(
                                        std::string const& name);

PCR_DAL_DECL std::string  filenameConventionToString(
                                        FilenameConvention convention);

PCR_DAL_DECL std::string  dimensionToString(
                                        Dimension const& dimension);

PCR_DAL_DECL std::string dataSpaceToString(DataSpace const& space);

std::string        nameAndSpaceToString(std::string const& name,
                                        DataSpace const& space);

std::string        nameToString        (std::string const& name,
                                        DataSpace const& space,
                                        DatasetType type);

PCR_DAL_DECL std::string  coordinateToString(
                                        Dimension const& dimension,
                                        size_t index);

PCR_DAL_DECL std::string  coordinateToString(
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        size_t index);

PCR_DAL_DECL std::string  dataSpaceAddressToString(
                                        DataSpace const& space,
                                        DataSpaceAddress const& address);

std::set<std::string> dataSpaceToFieldNames(
                                        DataSpace const& space);

std::set<std::string> dataSpaceAddressToFieldNames(
                                        DataSpace const& space,
                                        DataSpaceAddress const& address);

std::string        dataSpaceAddressToSqlQuery(
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& tableName,
                                        std::string const& fieldName);

std::string        dataSpaceAddressToSqlQuery(
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& tableName,
                                        std::vector<std::string> fieldNames);

  void             throwDataSourceError(std::string const& name,
                                        std::string const& description);

  void             throwDataSourceError(std::string const& name,
                                        DataSpace const& space,
                                        std::string const& description);

  void             throwDataSourceError(std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& description);

  void             throwDataSourceError(std::string const& name,
                                        DatasetType type,
                                        DataSpace const& space,
                                        std::string const& description);

  void             throwDataSourceError(std::string const& name,
                                        DatasetType type,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& description);

  void             throwDataSourceError(std::string const& name,
                                        DatasetType type,
                                        std::string const& description);

PCR_DAL_DECL void  throwCannotBeOpened (std::string const& name);

PCR_DAL_DECL void  throwCannotBeOpened (std::string const& name,
                                        DatasetType type,
                                        std::string const& reason = "");

PCR_DAL_DECL void  throwCannotBeOpened (std::string const& name,
                                        DataSpace const& space,
                                        DatasetType type);

/*
  void             throwCannotBeOpened (std::string const& name,
                                        DatasetType type,
                                        size_t timeStep);
                                        */

PCR_DAL_DECL void throwCannotBeOpened  (std::string const& name,
                                        DataSpace const& space);

PCR_DAL_DECL void throwCannotBeOpened  (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address);

PCR_DAL_DECL void throwCannotBeOpened  (std::string const& name,
                                        DatasetType type,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address);

  void             throwCannotBeCreated(std::string const& name,
                                        DatasetType type,
                                        std::string const& reason = "");

/*
  void             throwCannotBeOpened (std::string const& name,
                                        DatasetType type,
                                        std::string const& reason = "");
                                        */

  void             throwCannotBeClosed (std::string const& name,
                                        DatasetType type);

  void             throwCannotBeRead   (std::string const& name,
                                        DatasetType type,
                                        std::string const& reason = "");

  void             throwCannotReadHeader(std::string const& name,
                                        DatasetType type);

  void             throwCannotReadCell (std::string const& name,
                                        DatasetType type);
                                       // hack CW see CSFMapTest::testError std::string driverSpecificMsg="");

  void             throwCannotReadCell (std::string const& name,
                                        DatasetType type,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address);

  void             throwCannotReadCells(std::string const& name,
                                        DatasetType type);

  void             throwCannotReadCells(std::string const& name,
                                        DatasetType type,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address);

  void             throwCannotReadRecord(std::string const& name,
                                        DatasetType type,
                                        size_t record);

  void             throwCannotWrite    (std::string const& name,
                                        DatasetType type,
                                        std::string const& reason = "");

  void             throwCannotWriteCells(std::string const& name,
                                        DatasetType type);

  void             throwCannotWriteRecord(std::string const& name,
                                        DatasetType type,
                                        size_t record,
                                        std::string const& reason);

  void             throwCannotBeDeleted(std::string const& name,
                                        DatasetType type,
                                        std::string const& reason);

  void             throwCannotReadLegend(std::string const& name);

/*
  void             throwUnsupportedFormat(std::string const& name);

  void             throwUnsupportedDatasetType(std::string const& name,
                                        DatasetType type);
                                        */

  // DatasetType      datasetType         (std::string const& name);

PCR_DAL_DECL std::vector<std::string>  scenarios(
                                        DataSpace const& space);

PCR_DAL_DECL std::set<size_t> timeSteps(DataSpace const& space);

  std::set<float>  quantiles           (DataSpace const& space);

  template<typename T>
PCR_DAL_DECL T     timeStep            (DataSpace const& space,
                                        DataSpaceAddress const& address);

  /// template<typename T>
  /// T                rowIndex            (DataSpace const& space,
  ///                                       DataSpaceAddress const& address);

  /// template<typename T>
  /// T                colIndex            (DataSpace const& space,
  ///                                       DataSpaceAddress const& address);


PCR_DAL_DECL boost::tuple<std::string, std::vector<std::string> >
                   splitNameAndSelection(
                                        std::string name);

  /*
  std::string      replaceEnvironmentVariables(
                                       std::string const& string);
                                       */

  /*
template<typename ValueT>
struct PushBackAction
{
  ValueT d_missingValue;

  PushBackAction()
    // : d_missingValue(missingValue)  -> use traits
  {
  }

  void setMissingValue(ValueT missingValue)
  {
    d_missingValue = missingValue;
  }

  template<typename T>
  void act(T& ref_, ValueT const& value_) const
  {
    ref_.push_back( value_ );
    if(value == d_missingValue) {
      pcr::setMV(ref_.back());
    }
  }
  */

  /*
  template<
      typename T,
      typename IteratorT
  >
  void act(
      T& ref_,
      IteratorT const& first_,
      IteratorT const& last_
      ) const
  {
      typedef typename T::value_type value_type;
      value_type value(first_,last_);

      ref_.push_back( value );
  }
  */
// };

/*
template<typename T>
inline boost::spirit::ref_value_actor<T, PushBackAction<typename T::valuetype> > pushBackActor(
         T& ref_)
{
  return boost::spirit::ref_value_actor<T, PushBackAction<typename T::valuetype> >(ref_);
}
*/

/*
template<typename T, typename ValueT>
inline boost::spirit::ref_const_ref_actor<T,ValueT, PushBackAction<typename T::valuetype> > pushBackActor(
         T& ref_,
         ValueT const& value_)
{
  return boost::spirit::ref_const_ref_actor<T,ValueT, PushBackAction<typename T::valuetype> >(ref_,value_);
}
*/

//! as std::copy but with MV handling
/*!
 * CW Was eerst ::copy nu dal::copyCells. Anders is de template signature
 *  gelijk aan de std::copy en wordt deze ook ergens anders op gepikt.
 *  b.v. in de implementatie (<vector>) van std::vector<Dimension>
 * \todo
 *   Is dit echt nodig? Waarom zou de std::copy niet voldoen?
 *   als Input en Output hetzelfde type hebben? Mits de compiler
 *   Nan (flt MV's) goed copieert. Dat wordt getest in com_csfcell.cc o.i.d.
 *   \todo http://www.boost.org/doc/html/boost_typetraits/examples.html#boost_typetraits.copy
 */
template <class InputIterator, class OutputIterator>
inline OutputIterator copyCells(
         InputIterator first,
         InputIterator last,
         OutputIterator result)
{
  for(; first != last; ++first, ++result) {
    if(pcr::isMV(*first)) {
      pcr::setMV(*result);
    }
    else {
      *result = *first;
    }
  }

  return result;
}

PCR_DAL_DECL DataSpace  dataSpaceWithNarrowedDimension(
                                        DataSpace const& space,
                                        size_t indexOfDimension,
                                        size_t indexOfCoordinate);

void               eraseDimension      (DataSpace& space,
                                        DataSpaceAddress& address,
                                        Meaning meaning);

} // namespace dal


#endif
