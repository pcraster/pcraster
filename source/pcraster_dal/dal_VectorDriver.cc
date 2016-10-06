#ifndef INCLUDED_DAL_VECTORDRIVER
#include "dal_VectorDriver.h"
#define INCLUDED_DAL_VECTORDRIVER
#endif

// External headers.
#include <memory>

#ifndef INCLUDED_BOOST_FILESYSTEM
#include <boost/filesystem.hpp>
#define INCLUDED_BOOST_FILESYSTEM
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

#ifndef INCLUDED_BOOST_LEXICAL_CAST
#include <boost/lexical_cast.hpp>
#define INCLUDED_BOOST_LEXICAL_CAST
#endif

#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TUPLE_TUPLE
#include <boost/tuple/tuple.hpp>
#define INCLUDED_BOOST_TUPLE_TUPLE
#endif

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif

#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif

#ifndef INCLUDED_DAL_RASTERDAL
#include "dal_RasterDal.h"
#define INCLUDED_DAL_RASTERDAL
#endif

#ifndef INCLUDED_DAL_REGULAREXPRESSIONS
#include "dal_RegularExpressions.h"
#define INCLUDED_DAL_REGULAREXPRESSIONS
#endif

#ifndef INCLUDED_DAL_TYPES
#include "dal_Types.h"
#define INCLUDED_DAL_TYPES
#endif



/*!
  \file
  This file contains the implementation of the VectorDriver class.
*/

namespace {

} // Anonymous namespace



namespace dal {

//! Data class for VectorDriver class.
/*!
*/
class VectorDriver::Data: private boost::noncopyable
{

private:

  //! Object used to do the actual I/O.
  RasterDal        _dal;

public:

  static std::string name(
         std::string const& name,
         std::string const& direction)
  {
    assert(!name.empty());

    boost::filesystem::path path(name);

    std::string result;
    std::string basename = path.stem().string();
    std::string extension = path.extension().string();

    if(extension.empty()) {
      result = basename + "_" + direction;
    }
    else {
      result = basename + "_" + direction + extension;
    }

    return (path.parent_path() / result).string();
  }

  //! Returns the name of the raster attribute representing the magnitude in x-direction.
  /*!
    \param     Name of the vector attribute.
    \return    Name of the raster attribute.
  */
  static std::string nameX(
         std::string const& name)
  {
    return Data::name(name, "x");
  }

  //! Returns the name of the raster attribute representing the magnitude in y-direction.
  /*!
    \param     Name of the vector attribute.
    \return    Name of the raster attribute.
  */
  static std::string nameY(
         std::string const& name)
  {
    return Data::name(name, "y");
  }

  //! Default constructor.
  /*!
    All available raster drivers are added to the layered RasterDal object.
  */
  Data()
    : _dal(true)
  {
  }

  //! Returns the layered RasterDal object.
  /*!
    \return    Layered RasterDal object.
  */
  RasterDal& dal()
  {
    return _dal;
  }

  //! Returns whether the vector attribute \name exists at \a address in \a space.
  /*!
    \param     name Name of the vector attribute.
    \param     space Enclosing data space.
    \param     address Address in \a space.
    \return    true or false
  */
  bool exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
  {
    return _dal.exists(nameX(name), space, address) &&
         _dal.exists(nameY(name), space, address);
  }

  //! Opens vector attribute \a name at \a address in \a space.
  /*!
    \param     name Name of the vector attribute.
    \param     space Enclosing data space.
    \param     address Address in \a space.
    \param     throw_ Whether in case of an error an exception must be thrown.
    \return    Tuple with raster attributes representing magnitudes in x- and
               y-direction.
    \exception dal::Exception In case of an error and in case \a throw_ is
               true.
  */
  boost::tuple< boost::shared_ptr<Raster>, boost::shared_ptr<Raster> >
  open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         bool throw_) const
  {
    boost::shared_ptr<Raster> x, y;
    boost::tie(x, boost::tuples::ignore) = _dal.open(nameX(name), space,
        address);

    if(x) {
      // Don't try opening y if opening x already fails.
      boost::tie(y, boost::tuples::ignore) = _dal.open(nameY(name), space,
          address);
    }

    if(!x || !y) {
      if(throw_) {
        throwCannotBeOpened(name, VECTOR, space, address);
      }
    }

    if(x && !y) {
      x.reset();
    }

    return boost::make_tuple(x, y);
  }

  //! Checks whether \a x and \a y are valid raster attributes for representing the magnitudes in x- and y-direction.
  /*!
    \param     x Raster attribute representing the magnitude in x-direction.
    \param     y Raster attribute representing the magnitude in y-direction.
    \param     throw_ Whether in case of an error an exception must be thrown.
    \return    true or false.
    \exception dal::Exception In case of an error and in case \a throw_ is
               true.
  */
  bool validate(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         Raster const& x,
         Raster const& y,
         bool throw_) const
  {
    bool result = true;

    // Raster properties must be the same.
    if(x.dimensions() != y.dimensions()) {
      result = false;

      if(throw_) {
        // TODO other error message.
        throwCannotBeOpened(name, VECTOR, space, address);
      }
    }

    if(result) {
      if(x.typeId() != y.typeId() ||
         (x.typeId() != TI_REAL4 && x.typeId() != TI_REAL8)) {
        result = false;

        if(throw_) {
          // TODO other error message.
          throwCannotBeOpened(name, VECTOR, space, address);
        }
      }
    }

    return result;
  }

  //! Reads vector attribute name at \a address in \a space into \a x and \a y.
  /*!
    \param     x Raster attribute representing the magnitude in x-direction.
    \param     y Raster attribute representing the magnitude in y-direction.
    \param     name Name of the vector attribute.
    \param     space Enclosing data space.
    \param     address Address in \a space.
    \exception dal::Exception in case of an error.
    \warning   The \a typeId's of \a x and \a y passed in must be either
               TI_REAL4 or TI_REAL8.
  */
  void read(
         Raster& x,
         Raster& y,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
  {
    assert(x.typeId() == TI_REAL4 || x.typeId() == TI_REAL8);
    assert(y.typeId() == x.typeId());

    try {
      _dal.read(x, nameX(name), space, address);
      _dal.read(y, nameY(name), space, address);
    }
    catch(Exception const& exception) {
      // TODO
      throwCannotBeRead(name, VECTOR, exception.message());
    }
  }

  template<typename T>
  void read(
         T* cell,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
  {
    T x, y;

    try {
      _dal.read(&x, TypeTraits<T>::typeId, nameX(name), space, address);
      _dal.read(&y, TypeTraits<T>::typeId, nameY(name), space, address);
    }
    catch(Exception const& exception) {
      // TODO
      throwCannotBeRead(name, VECTOR, exception.message());
    }

    if(!pcr::isMV(x) && !pcr::isMV(y)) {
      *cell = std::sqrt(x * x + y * y);
    }
    else {
      pcr::setMV(*cell);
    }
  }

  void read(
         void* cell,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
  {
    switch(typeId) {
      case TI_REAL4: {
        read(static_cast<REAL4*>(cell), name, space, address);
        break;
      }
      case TI_REAL8: {
        read(static_cast<REAL8*>(cell), name, space, address);
        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }

};



//------------------------------------------------------------------------------
// DEFINITION OF STATIC VECTORDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VECTORDRIVER MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
*/
VectorDriver::VectorDriver()

  : Driver(Format("Vector", "Raster based vector file format",
         VECTOR, Format::File, Format::Attribute)),
    _data(new VectorDriver::Data())

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;

  // Collect the extensions of the layered raster drivers.
  std::vector<std::string> extensions;

  BOOST_FOREACH(Driver const* driver, _data->dal().drivers()) {
    Format const& format(driver->format());
    std::vector<std::string> const& formatExtensions(format.extensions());
    extensions.insert(extensions.end(), formatExtensions.begin(),
         formatExtensions.end());
  }

  format().setExtensions(extensions);
}



//! Destructor.
/*!
*/
VectorDriver::~VectorDriver()
{
}



bool VectorDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return _data->exists(name, space, address);
}



Vector* VectorDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  boost::shared_ptr<Raster> x, y;
  boost::tie(x, y) = _data->open(name, space, address, false);

  if(!x || !y) {
    return 0;
  }

  if(!_data->validate(name, space, address, *x, *y, false)) {
    return 0;
  }

  return new Vector(x->dimensions(), x->typeId());
}



DataSpace VectorDriver::dataSpace(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  boost::shared_ptr<Vector> vector(open(name, space, address));

  if(!vector) {
    // TODO make sure this function supports VECTOR.
    throwCannotBeOpened(name, VECTOR, space, address);
  }

  DataSpace result;

  result.addDimension(Dimension(Space, RegularDiscretisation,
         vector->dimensions()));

  return result;
}



Vector* VectorDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  boost::shared_ptr<Raster> x, y;
  boost::tie(x, y) = _data->open(name, space, address, true);
  assert(x && y);
  _data->validate(name, space, address, *x, *y, true);
  _data->read(*x, *y, name, space, address);

#ifdef __GXX_EXPERIMENTAL_CXX0X__
  std::unique_ptr<Vector> result(new Vector(x->dimensions(), x->typeId()));
#else
  std::auto_ptr<Vector> result(new Vector(x->dimensions(), x->typeId()));
#endif
  result->transfer(*x, *y);

  return result.release();
}



void VectorDriver::read(
         Vector& vector,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  boost::shared_ptr<Raster> x, y;
  boost::tie(x, y) = _data->open(name, space, address, true);
  assert(x && y);
  _data->validate(name, space, address, *x, *y, true);
  assert(vector.dimensions() == x->dimensions());
  assert(vector.typeId() == x->typeId());

  if(!vector.cellsAreCreated()) {
    vector.createCells();
  }

  // Let x and y use vector's buffers to prevent a copy.
  x->transfer(vector.xCells(), Matrix::DoNotTakeOwnerShip);
  y->transfer(vector.yCells(), Matrix::DoNotTakeOwnerShip);

  _data->read(*x, *y, name, space, address);
}



void VectorDriver::read(
         void* cell,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(space.hasSpace());

  _data->read(cell, typeId, name, space, address);
}



template<typename T>
bool VectorDriver::extremes(
       T& min,
       T& max,
       std::string const& name,
       DataSpace const& space) const
{
  assert(!space.hasSpace());

  bool initialised = false;
  Vector* vector = 0;

  if(space.isEmpty()) {
    vector = open(name);

    if(vector) {
      vector->setTypeId(TypeTraits<T>::typeId);

      if(!vector->hasExtremes()) {
        // Extremes unknown (no header, too expensive for open(), ...).
        read(*vector, name);
        vector->calculateExtremes();
      }

      if(vector->hasExtremes()) {
        min = vector->template min<T>();
        max = vector->template max<T>();
        initialised = true;
      }
    }
  }
  else {
    for(DataSpaceIterator it = space.begin(); it != space.end(); ++it) {
      vector = open(name, space, *it);

      if(vector) {
        vector->setTypeId(TypeTraits<T>::typeId);

        if(!vector->hasExtremes()) {
          // Extremes unknown (no header, too expensive for open(), ...).
          read(*vector, name, space, *it);
          vector->calculateExtremes();
        }

        if(vector->hasExtremes()) {
          if(!initialised) {
            min = vector->template min<T>();
            max = vector->template max<T>();
            initialised = true;
          }
          else {
            min = std::min(min, vector->template min<T>());
            max = std::max(max, vector->template max<T>());
          }
        }
      }
    }
  }

  return initialised;
}



bool VectorDriver::extremes(
         boost::any& min,
         boost::any& max,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space) const
{
  assert(!space.hasSpace());
  assert(typeId == TI_REAL4 || typeId == TI_REAL8);

  bool result = false;

  switch(typeId) {
    case TI_REAL4: {
      REAL4 i, a;

      if(extremes<REAL4>(i, a, name, space)) {
        min = i;
        max = a;
        result = true;
      }

      break;
    }
    case TI_REAL8: {
      REAL8 i, a;

      if(extremes<REAL8>(i, a, name, space)) {
        min = i;
        max = a;
        result = true;
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



void VectorDriver::browse(
         std::vector<BrowseInfo>& attributes,
         std::string const& location) const
{
  // Determine list of candidate file names of files to consider.
  boost::filesystem::path path(location);
  std::vector<std::string> leaves;
  possibleFileBasedAttributeFileNames(path, leaves);

  std::string name, step, extension;
  std::vector<size_t> ids;
  std::set<size_t> steps;
  std::regex regex;
  std::smatch match;
  Vector* vector;

  std::vector<std::string> const& extensions(format().extensions());

  // Temporal vector.
  for(int i = 0; i < int(leaves.size()); ++i) {
    // First find x component.
    if(std::regex_match(leaves[i], match, temporalVectorXRegex)) {
      name = std::string(match[1].first, match[1].second);
      step = std::string(match[2].first, match[2].second);
      extension = match[3].matched ?
         std::string(match[3].first, match[3].second) : "";

      regex = std::regex((boost::format(
         "%1%_y_%2%%3%")
         % name
         % step
         % extension).str());

      for(int j = i + 1; j < int(leaves.size()); ++j) {
        // See whether there is a matching y component.
        if(std::regex_match(leaves[j], match, regex)) {
          // Vector found.
          // Find the ids of all members of the stack.

          regex = std::regex((boost::format(
              "%1%_(?:x|y)_(%2%)%3%")
              % name
              % timeStepPattern
              % extension).str());

          steps.clear();
          ids.clear();

          for(int k = i; k < int(leaves.size()); ++k) {
            if(std::regex_match(leaves[k], match, regex)) {
              step = std::string(match[1].first, match[1].second);
              steps.insert(boost::lexical_cast<size_t>(step));
              ids.push_back(k);
            }
          }

          assert(!steps.empty());
          DataSpace space(Dimension(Time, *steps.begin(), *steps.begin(),
              size_t(1)));
          DataSpaceAddress address(space.address());
          address.setCoordinate<size_t>(0, *steps.begin());

          if(std::find(extensions.begin(), extensions.end(),
              extension) == extensions.end()) {
            // Not a default extension, add it to the attribute name.
            name += extension;
          }

          vector = open((path / name).string(), space, address);

          if(vector) {
            size_t first, last, interval;

            if(isRegularIncreasingRange(first, last, interval, steps.begin(),
                   steps.end())) {
              space.dimension(0).setValues(first, last, interval);
            }

            Properties const& properties(vector->properties());

            CSF_VS valueScale = properties.hasValue(DAL_CSF_VALUESCALE)
              ? properties.value<CSF_VS>(DAL_CSF_VALUESCALE)
              : VS_NOTDETERMINED;

            attributes.push_back(BrowseInfo(name, space, vector->type(),
              vector->typeId(), valueScale, this->name()));
          }

          // Erase all file names that belong to the stack.
          for(int k = ids.size() - 1; k >= 0; --k) {
            leaves.erase(leaves.begin() + ids[k]);
          }

          --i;
        }
      }
    }
  }

  // Vector.
  for(int i = 0; i < int(leaves.size()); ++i) {
    if(std::regex_match(leaves[i], match, vectorXRegex)) {
      name = std::string(match[1].first, match[1].second);
      extension = match[2].matched ?
         std::string(match[2].first, match[2].second) : "";

      regex = std::regex((boost::format(
         "%1%_y%2%")
         % name
         % extension).str());

      for(int j = i + 1; j < int(leaves.size()); ++j) {
        if(std::regex_match(leaves[j], match, regex)) {

          if(std::find(extensions.begin(), extensions.end(),
              extension) == extensions.end()) {
            // Not a default extension, add it to the attribute name.
            name += extension;
          }

          vector = open((path / name).string());

          if(vector) {
            Properties const& properties(vector->properties());

            CSF_VS valueScale = properties.hasValue(DAL_CSF_VALUESCALE)
              ? properties.value<CSF_VS>(DAL_CSF_VALUESCALE)
              : VS_NOTDETERMINED;

            attributes.push_back(BrowseInfo(name, DataSpace(), vector->type(),
              vector->typeId(), valueScale, this->name()));
          }

          leaves.erase(leaves.begin() + j);
          leaves.erase(leaves.begin() + i);
          --i;
          break;
        }
      }
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

