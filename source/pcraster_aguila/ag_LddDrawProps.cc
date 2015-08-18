#include "ag_LddDrawProps.h"
#include "pcrtypes.h"
#include <algorithm>
#include <cassert>
#include <QImage>
#include "com_classclassifier.h"
#include "ag_ColourSelector.h"

// Bitmaps: little endian byte order: most sign. byte at lowest address offset.
// Created with xpaint (linux).
#include "ldd1.xbm"
#include "ldd2.xbm"
#include "ldd3.xbm"
#include "ldd4.xbm"
#include "ldd5.xbm"
#include "ldd6.xbm"
#include "ldd7.xbm"
#include "ldd8.xbm"
#include "ldd9.xbm"



/*!
  \file
  This file contains the implementation of the LddDrawProps class.
*/



//------------------------------------------------------------------------------
// namespace ag {
//  class LddDrawPropsPrivate;
// }
// 
// namespace com {
// template class CountedObject<ag::LddDrawPropsPrivate>;
// }

namespace ag {


class LddDrawPropsPrivate // : public com::CountedObject<LddDrawPropsPrivate>
{
public:

  static size_t    _nrCreated;

  //! Default label.
  static const std::string _label;

  static GLuint    _textures[2304];   // Array with texture names.

  static const unsigned char *_lddTextures[9];
  static const unsigned char *_gddTextures[8];

  //! Class info object.
  const com_ClassClassifier<UINT1>* _classifier;

  LddDrawPropsPrivate()
  {
    ++_nrCreated;

    if(_nrCreated == 1) {

      std::fill(&_textures[0], _textures + 2304, 0);

      _lddTextures[0] = ldd1_bits;
      _lddTextures[1] = ldd2_bits;
      _lddTextures[2] = ldd3_bits;
      _lddTextures[3] = ldd4_bits;
      _lddTextures[4] = ldd5_bits;
      _lddTextures[5] = ldd6_bits;
      _lddTextures[6] = ldd7_bits;
      _lddTextures[7] = ldd8_bits;
      _lddTextures[8] = ldd9_bits;

      _gddTextures[0] = ldd1_bits;
      _gddTextures[1] = ldd2_bits;
      _gddTextures[2] = ldd3_bits;
      _gddTextures[3] = ldd4_bits;
      _gddTextures[4] = ldd6_bits;
      _gddTextures[5] = ldd7_bits;
      _gddTextures[6] = ldd8_bits;
      _gddTextures[7] = ldd9_bits;
    }
  }

  ~LddDrawPropsPrivate()
  {
    assert(_nrCreated > 0);

    --_nrCreated;

    // Check if this is the last object to be deleted.
    // According to the specs it is not illegal to glDeleteTexture a texture
    // with name 0 (we probably have some of them in the array).
    // yepyep: dumps: maybe because opengl is already gone and has already
    // yepyep: deleted the textures? Opengl removes the textures autom.
/*
    if(nrObjectsCreated() == 1) {
      glDeleteTextures(..., _textures);
    }
*/
  }

  // ldd [1 - 9]
  // gdd [0 - 255]
  // id [0 - 2303]
  static size_t id(unsigned char gdd, UINT1 ldd)
  {
    return (256 * --ldd) + gdd;
  }

};

size_t ag::LddDrawPropsPrivate::_nrCreated = 0;
const std::string ag::LddDrawPropsPrivate::_label = "flow direction";
GLuint ag::LddDrawPropsPrivate::_textures[2304];
const unsigned char* ag::LddDrawPropsPrivate::_lddTextures[9];
const unsigned char* ag::LddDrawPropsPrivate::_gddTextures[8];

}



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

//! Returns the default label.
/*!
  \return    Label.
  \sa        label(size_t)
*/
const std::string& ag::LddDrawProps::label()
{
  return LddDrawPropsPrivate::_label;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     p Palette.
  \param     c Class info object.
*/
ag::LddDrawProps::LddDrawProps(const std::string& title,
         const com::RawPalette* p, const com_ClassClassifier<UINT1>* c)

  : ClassDrawProps(title, p),
    _data(new LddDrawPropsPrivate())

{
  assert(c);

  _data = new LddDrawPropsPrivate();
  _data->_classifier = c;

  // From the base class. Messy. Should be passed into a function (constructor).
  reMapColours();
  _nrClasses = _data->_classifier->nrClasses();
  for(size_t i = 0; i < _data->_classifier->nrClasses(); ++i) {
    _labels.push_back(_data->_classifier->descr(i));
  }
}



ag::LddDrawProps::LddDrawProps(const LddDrawProps& properties)

  : ClassDrawProps(properties),
    _data(new LddDrawPropsPrivate())

{
  _data->_classifier = properties._data->_classifier;
}



//! Destructor.
/*!
  The classifier is for use only and is not deleted here.
*/
ag::LddDrawProps::~LddDrawProps()
{
  delete _data;
}



void ag::LddDrawProps::reMapColours()
{
  _colours = mapSequential(*palette(), _data->_classifier->nrClasses());
}



//! Returns the class info object.
/*!
  \return    Class info object.
*/
const com_ClassClassifier<UINT1>& ag::LddDrawProps::classifier() const
{
  assert(_data->_classifier);
  return *(_data->_classifier);
}



GLuint ag::LddDrawProps::texture(unsigned char gdd, UINT1 ldd) const
{
  // Calculate unique id of configuration.
  // size_t uniqueId = ldd - 1;
  size_t uniqueId = _data->id(gdd, ldd);
  assert(uniqueId < 2304);

  if(_data->_textures[uniqueId] == 0) {

    // Create new texture.
    static unsigned char texture[512];

    // Set ldd texture.
    // Loop over rows in bitmap.
    for(size_t j = 0; j < 64; ++j) {
      // Loop over bytes in row.
      for(size_t k = 0; k < 8; ++k) {
        texture[j * 8 + k] = _data->_lddTextures[ldd - 1][j * 8 + k];
      }
    }

    // Add gdd texture.
    // Loop over gdd directions.
    for(size_t i = 0; i < 8; ++i) {
      if(gdd & (1 << i)) {
        // Loop over rows in bitmap.
        for(size_t j = 0; j < 64; ++j) {
          // Loop over bytes in row.
          for(size_t k = 0; k < 8; ++k) {
            // 'or' the current with an existing one for the target direction.
            texture[j * 8 + k] |= _data->_gddTextures[i][j * 8 + k];
          }
        }
      }
    }

    QImage image(&texture[0], 64, 64, QImage::Format_Mono);
    image.setColor(0, QColor(Qt::black).rgb());
    image.setColor(1, QColor(Qt::white).rgb());
    image = image.convertToFormat(QImage::Format_RGB32);
    image = image.mirrored();
    image = image.rgbSwapped();

    glGenTextures(1, &_data->_textures[uniqueId]);
    glBindTexture(GL_TEXTURE_2D, _data->_textures[uniqueId]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 64, 64, 0,
                   GL_RGBA, GL_UNSIGNED_BYTE, image.bits());
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  }

  assert(_data->_textures[uniqueId] != 0);
  return _data->_textures[uniqueId];
}



std::string ag::LddDrawProps::label(
         UINT1 const& value) const
{
  std::string result = "mv";

  if(!pcr::isMV(value)) {
    size_t index = classifier().index(value);
    result = classifier().descr(index);
  }

  return result;
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


