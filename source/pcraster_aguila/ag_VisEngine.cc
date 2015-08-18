#include "ag_VisEngine.h"
#include <vector>
#include <boost/format.hpp>
#include <QPoint>
#include "dal_Utils.h"
#include "com_exception.h"
#include "geo_DataType.h"
#include "ag_DataObject.h"
#include "ag_DataProperties.h"
#include "ag_DataProperty.h"
#include "ag_RangeDrawProps.h"
#include "ag_RasterDataSources.h"



/*!
  \file
  brief

  more elaborated
*/



namespace ag {

//------------------------------------------------------------------------------

class VisEnginePrivate
{
public:

  std::vector<DataGuide> _dataGuides;   // Data guides of vis.
  DataProperties   _properties;
  bool             _folowCursor;
  dal::DataSpace   _dataSpace;
  dal::DataSpaceAddress _dataSpaceAddress;

  double           _map2DZoom;
  double           _map2DScale;
  QPointF          _map2DOffset;
  size_t           _quadLength;
  double           _map3DScale;

  MapAction        _mapAction;

  DataGuide const* _height;

  boost::any       _selectedValue;
  /// std::map<DataGuide, boost::any> _selectedValues;

  QColor           _backgroundColour;

  unsigned int     _change;

  VisEnginePrivate()
    : _folowCursor(true),
      _map2DZoom(-1e32), _map2DScale(-1e32),
      _map2DOffset(),
      _quadLength(0), _map3DScale(-1e32),
      _mapAction(NR_MAP_ACTIONS),
      _height(0),
      _selectedValue(),
      _change(0)
  {
  }

  ~VisEnginePrivate()
  {
    if(_height) {
      delete _height;
    }
  }

};



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

/*!
  The visualisation will default respond to cursor changes.
*/
VisEngine::VisEngine()

  : _data(new VisEnginePrivate())

{
}



VisEngine::~VisEngine()
{
  delete _data;
}



/*
VisEngine& VisEngine::operator=(const VisEngine& aEngine)
{
  if(this != &aEngine) {
    _data->_folowCursor = aEngine._data->_folowCursor;

    _data->_change |= aEngine._data->_change;
    _data->_change |= VisEngine::OTHERATTRIB;

    // yepyep: What about all those pointers in _data? -> copy them too!
  }

  return *this;
}
*/



/*!
  \param     s If true, the visualisation will respond to cursor changes.
  \sa        folowCursor()
*/
void VisEngine::setFolowCursor(
         bool s)
{
  _data->_folowCursor = s;
}



/*!
  \return    True if the visualisation responds to cursor changes.
  \sa        setFolowCursor(bool)
*/
bool VisEngine::folowCursor()
{
  return _data->_folowCursor;
}



unsigned int VisEngine::change() const
{
  return _data->_change;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   This function should not update the internal state of the Engine.
             Use finishedScanning(DataObject const&) for that purpose. That
             way the user always has the previous and the new state at his
             disposal.
  \sa        .
*/
void VisEngine::rescan(
         DataObject const& object)
{
  // See whether data has been removed.
  for(size_t i = 0; i < _data->_dataGuides.size(); ++i) {
    if(!object.isAvailable(_data->_dataGuides[i])) {

      // Remove our references to this data guide.
      _data->_properties.remove(_data->_dataGuides[i]);
      _data->_dataGuides.erase(_data->_dataGuides.begin() + i);

      // Set change status.
      _data->_change |= OTHERATTRIB;

      --i;
    }
  }

  if(object.dataSpace() != _data->_dataSpace) {
    // The whole data configuration changed, our address is not valid anymore.
    // TODO Make this more efficient by explicitly checking whether the
    // TODO coordinates of the address realy changed.
    _data->_change |= CURSOR;
    _data->_change |= QUANTILE;
    _data->_change |= TIME;
    _data->_change |= RASTER_CELL;
  }
  else if(_data->_dataSpaceAddress.size() != object.dataSpace().size() ||
         !object.dataSpace().equal(object.dataSpaceAddress(),
         _data->_dataSpaceAddress)) {
    _data->_change |= CURSOR;

    if(object.dataSpace().hasCumProbabilities() !=
         _data->_dataSpace.hasCumProbabilities()) {
      _data->_change |= QUANTILE;
    }
    else if(object.dataSpace().hasCumProbabilities()) {
      size_t index = object.dataSpace().indexOf(
         dal::CumulativeProbabilities);
      if(!dal::comparable<float>(
         object.dataSpaceAddress().coordinate<float>(index),
         _data->_dataSpaceAddress.coordinate<float>(index))) {
        _data->_change |= QUANTILE;
      }
    }

    if(object.dataSpace().hasTime() != _data->_dataSpace.hasTime()) {
      _data->_change |= TIME;
    }
    else if(object.dataSpace().hasTime()) {
      size_t index = object.dataSpace().indexOf(dal::Time);
      if(object.dataSpaceAddress().coordinate<size_t>(index) !=
            _data->_dataSpaceAddress.coordinate<size_t>(index)) {
        _data->_change |= TIME;
      }
    }

    /// if(object.dataSpace().hasRaster() != _data->_dataSpace.hasRaster()) {
    ///   _data->_change |= RASTER_CELL;
    /// }
    /// else if(...) {
    if(object.dataSpace().hasSpace()) {
      size_t index = object.dataSpace().indexOf(dal::Space);

      if(object.dataSpaceAddress().isValid(index) !=
         _data->_dataSpaceAddress.isValid(index)) {
        _data->_change |= RASTER_CELL;
      }
      else if(object.dataSpaceAddress().isValid(index) &&
         object.dataSpaceAddress().coordinate<dal::SpatialCoordinate>(index) !=
         _data->_dataSpaceAddress.coordinate<dal::SpatialCoordinate>(index)) {
        _data->_change |= RASTER_CELL;
      }
    }
  }

#ifdef DEBUG_DEVELOP
  if(_data->_change & TIME || _data->_change & QUANTILE ||
         _data->_change &  RASTER_CELL) {
    assert(_data->_change & CURSOR);
  }
#endif

  if(_data->_map2DZoom != object.map2DZoom()) {
    _data->_change |= MAP2DZOOM;
  }

  if(_data->_map2DScale != object.map2DScale()) {
    _data->_change |= MAP2DSCALE;
  }

  if(_data->_map2DOffset != object.map2DOffset()) {
    _data->_change |= MAP2DMOVE;
  }

  if(_data->_height) {
    // Check if the stack we use as height is still available.
    if(!object.rasterDataSources().isValid(*_data->_height)) {
      delete _data->_height;
      _data->_height = 0;
      _data->_change |= OTHERHEIGHT;
    }
  }

  if(_data->_quadLength != object.quadLength()) {
    _data->_quadLength = object.quadLength();
    _data->_change |= QUADLENGTH;
  }

  if(_data->_map3DScale != object.map3DScale()) {
    _data->_map3DScale = object.map3DScale();
    _data->_change |= MAP3DSCALE;
  }

  /// if(_data->_mapAction != object.mapAction()) {
  ///   _data->_mapAction = object.mapAction();
  ///   _data->_change |= MAP_ACTION;
  /// }

  // Loop over the properties.
  for(DataProperties::const_iterator it = _data->_properties.begin();
                   it != _data->_properties.end(); ++it) {

    // Let's see if data is selected or unselected since the last time.
    if(_data->_properties.isSelected(*it) != object.isSelected(*it)) {
      // _data->_properties.setSelected(*it, object.isSelected(*it));
      _data->_change |= SELECTION;
    }

    if(_data->_properties.isEnabled(*it) != object.isEnabled(*it)) {
      // _data->_properties.setEnabled(*it, object.isEnabled(*it));
      _data->_change |= VISIBILITY;
    }
  }

  /// BOOST_FOREACH(DataGuide const& guide, _data->_dataGuides) {
  ///   if(guide.isRangeData()) {
  ///     if(_data->_properties.rangeDrawProperties(guide).algorithm() !=
  ///          object.properties().rangeDrawProperties(guide).algorithm()) {
  ///       _data->_change |= CLASSIFICATION_ALGORITHM;
  ///       break;
  ///     }
  ///   }
  /// }

  for(const_dataguide_iterator it = begin(); it != end(); ++it) {
    if(properties().palette(*it) != object.properties().palette(*it)) {
      properties().setPalette(*it, object.properties().palette(*it));
      _data->_change |= DRAWPROPS;
    }

    if((*it).isRangeData()) {
      RangeDrawProps const& newDrawProperties =
         object.properties().rangeDrawProperties(*it);
      RangeDrawProps& oldDrawProperties =
         _data->_properties.rangeDrawProperties(*it);

      if(oldDrawProperties != newDrawProperties) {
        _data->_properties.copy(*it, object.properties());
        assert(oldDrawProperties == newDrawProperties);
        _data->_change |= DRAWPROPS;
      }
    }
  }

  if(object.hasSelectedValue()) {
    if(_data->_selectedValue.empty() ||
         !dal::comparable(object.selectedValue(),
              boost::any_cast<REAL4>(_data->_selectedValue))) {
      _data->_selectedValue = object.selectedValue();
      _data->_change |= VALUE_SELECTION;
    }
  }
  else {
    if(!_data->_selectedValue.empty()) {
      _data->_selectedValue = boost::any();
      _data->_change |= VALUE_SELECTION;
    }
  }

  if(_data->_backgroundColour != object.backgroundColour()) {
    _data->_change |= BACKGROUND_COLOUR;
  }
}



void VisEngine::finishedScanning(
         DataObject const& object)
{
  _data->_dataSpace = object.dataSpace();
  assert(_data->_dataSpace == object.dataSpace());
  _data->_dataSpaceAddress = object.dataSpaceAddress();
  _data->_map2DZoom = object.map2DZoom();
  _data->_map2DScale = object.map2DScale();
  _data->_map2DOffset = object.map2DOffset();
  _data->_quadLength = object.quadLength();
  _data->_map3DScale = object.map3DScale();
  _data->_backgroundColour = object.backgroundColour();
  _data->_change = 0;

  for(DataProperties::const_iterator it = _data->_properties.begin();
                   it != _data->_properties.end(); ++it) {
    _data->_properties.setSelected(*it, object.isSelected(*it));
    _data->_properties.setEnabled(*it, object.isEnabled(*it));
    /// _data->_properties.rangeDrawProperties(*it).setAlgorithm(
    ///      object.properties().rangeDrawProperties(*it).algorithm());
  }
}



// const CursorPos& VisEngine::cursorPos() const
// {
//   return _data->_cursorPos;
// }



//! Adds a data guide for data which is visualized.
/*!
  \param     aDataGuide Data guide for data.
  \warning   Make sure you call this function for every piece of data which is
             visualized.
*/
void VisEngine::addDataGuide(
         DataGuide const& aDataGuide)
{
  _data->_dataGuides.push_back(aDataGuide);
  // KDJ: It is possible that the same data is visualised more than once.
  // assert(_data->_selectedValues.find(aDataGuide) ==
  //        _data->_selectedValues.end());
  /// _data->_selectedValues[aDataGuide] = boost::any();
}



void VisEngine::addDataProperties(
         DataObject const& dataObject,
         DataGuide const& dataGuide)
{
  _data->_properties.copy(dataGuide, dataObject.properties());
}



//! Adds attribute pointed to by \a guide to the list of attributes.
/*!
  \param     dataObject Data object with the data.
  \param     g Guide to stack.
*/
void VisEngine::addAttribute(
         DataObject const& object,
         DataGuide const& guide)
{
  addDataGuide(guide);
  addDataProperties(object, guide);
  _data->_change |= OTHERATTRIB;
}



void VisEngine::clear()
{
  for(size_t i = 0; i < _data->_dataGuides.size(); ++i) {

    // Remove our references to this data guide.
    _data->_properties.remove(_data->_dataGuides[i]);
    _data->_dataGuides.erase(_data->_dataGuides.begin() + i);

    // Set change status.
    _data->_change |= OTHERATTRIB;

    --i;
  }

  /// _data->_selectedValues.clear();
}



VisEngine::const_dataguide_iterator VisEngine::begin() const
{
  return _data->_dataGuides.begin();
}



VisEngine::const_reverse_dataguide_iterator VisEngine::rbegin() const
{
  return _data->_dataGuides.rbegin();
}



VisEngine::const_dataguide_iterator VisEngine::end() const
{
  return _data->_dataGuides.end();
}



VisEngine::const_reverse_dataguide_iterator VisEngine::rend() const
{
  return _data->_dataGuides.rend();
}



const std::vector<DataGuide>& VisEngine::dataGuides() const
{
  return _data->_dataGuides;
}



DataProperties& VisEngine::properties()
{
  return _data->_properties;
}



bool VisEngine::cursorChanged() const
{
  return _data->_change & CURSOR;
}



bool VisEngine::timeChanged() const
{
  return _data->_change & TIME;
}



//! Sets the height stack to the stack pointed to by \a guide.
/*!
  \exception com::Exception when the value scale of \a guide is not VS_SCALAR.

  The height stack is the stack which is used for height values. All attributes
  are draped on top of the height stack.
*/
void VisEngine::setHeight(
         DataGuide const& guide)
{
  if(guide.valueScale() != VS_SCALAR) {
    std::string message = (boost::format(
         "Value scale %1%: Not a valid value scale for height data.\n"
         "Valid value scale is: %2%.")
         % dal::valueScaleToString(guide.valueScale())
         % dal::valueScaleToString(VS_SCALAR)).str();
    throw com::Exception(message);
  }

  if(_data->_height) {
    delete _data->_height;
  }
  _data->_height = new DataGuide(guide);
  _data->_change |= OTHERHEIGHT;
}



DataGuide const* VisEngine::heightDataGuide() const
{
  return _data->_height;
}



/*
bool VisEngine::hasSelectedData() const
{
  bool result = false;

  for(const_dataguide_iterator it = begin(); it != end(); ++it) {
    if((*it).isSelected()) {
      result = true;
      break;
    }
  }

  return result;
}



std::vector<DataGuide> VisEngine::selectedData() const
{
  std::vector<DataGuide> dataGuides;

  for(const_dataguide_iterator it = begin(); it != end(); ++it) {
    if((*it).isSelected()) {
      dataGuides.push_back(*it);
    }
  }

  return dataGuides;
}
*/



//! Updates the guide of sub-classes.
/*!
  \param     aDataGuide Data guide which has changed.

  The default does nothing.
*/
/*
void VisEngine::updateGuide(const DataGuide& aDataGuide)
{
}
*/



// #ifdef DEBUG_DEVELOP
std::string VisEngine::changeToString() const
{
  std::string result;

  if(_data->_change & CURSOR) {
    if(!result.empty()) {
      result += "|";
    }
    result += "cursor";
  }
  if(_data->_change & QUANTILE) {
    if(!result.empty()) {
      result += "|";
    }
    result += "quantile";
  }
  if(_data->_change & TIME) {
    if(!result.empty()) {
      result += "|";
    }
    result += "time";
  }
  if(_data->_change & SELECTION) {
    if(!result.empty()) {
      result += "|";
    }
    result += "selection";
  }
  if(_data->_change & VISIBILITY) {
    if(!result.empty()) {
      result += "|";
    }
    result += "visibility";
  }
  if(_data->_change & OTHERATTRIB) {
    if(!result.empty()) {
      result += "|";
    }
    result += "otherattrib";
  }
  if(_data->_change & DRAWPROPS) {
    if(!result.empty()) {
      result += "|";
    }
    result += "drawprops";
  }
  if(_data->_change & MAP2DZOOM) {
    if(!result.empty()) {
      result += "|";
    }
    result += "map2dzoom";
  }
  if(_data->_change & MAP2DSCALE) {
    if(!result.empty()) {
      result += "|";
    }
    result += "map2dscale";
  }
  if(_data->_change & MAP2DMOVE) {
    if(!result.empty()) {
      result += "|";
    }
    result += "map2dmove";
  }
  if(_data->_change & QUADLENGTH) {
    if(!result.empty()) {
      result += "|";
    }
    result += "quadlength";
  }
  if(_data->_change & MAP3DSCALE) {
    if(!result.empty()) {
      result += "|";
    }
    result += "map3dscale";
  }
  if(_data->_change & OTHERHEIGHT) {
    if(!result.empty()) {
      result += "|";
    }
    result += "otherheight";
  }
  /// if(_data->_change & MAP_ACTION) {
  ///   if(!result.empty()) {
  ///     result += "|";
  ///   }
  ///   result += "map_action";
  /// }
  if(_data->_change & VALUE_SELECTION) {
    if(!result.empty()) {
      result += "|";
    }
    result += "value_selection";
  }
  // if(_data->_change & CLASSIFICATION_ALGORITHM) {
  //   if(!result.empty()) {
  //     result += "|";
  //   }
  //   result += "classification_algorithm";
  // }

  return result;
}
// #endif



size_t VisEngine::size() const
{
  return _data->_dataGuides.size();
}



bool VisEngine::isEmpty() const
{
  return _data->_dataGuides.empty();
}



DataGuide const& VisEngine::guide(size_t index) const
{
  assert(index <= _data->_dataGuides.size());
  return _data->_dataGuides[index];
}



dal::DataSpace const& VisEngine::dataSpace() const
{
  return _data->_dataSpace;
}



dal::DataSpaceAddress const& VisEngine::dataSpaceAddress() const
{
  return _data->_dataSpaceAddress;
}



double VisEngine::map2DZoom() const
{
  return _data->_map2DZoom;
}



double VisEngine::map2DScale() const
{
  return _data->_map2DScale;
}



QPointF VisEngine::map2DOffset() const
{
  return _data->_map2DOffset;
}



QColor const& VisEngine::backgroundColour() const
{
  return _data->_backgroundColour;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag
