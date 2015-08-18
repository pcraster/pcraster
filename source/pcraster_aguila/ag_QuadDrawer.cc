#include "dal_Matrix.h"
#include "dal_Type.h"
#include "ag_QuadDrawer.h"
#include <QImage>
#include "com_const.h"
#include "ag_BooleanDrawProps.h"
#include "ag_LddDrawProps.h"
#include "ag_NominalDrawProps.h"
#include "ag_OrdinalDrawProps.h"
#include "ag_RangeDrawProps.h"



//------------------------------------------------------------------------------
ag::QuadDrawer::QuadDrawer(size_t l)

  : d_quadLength(l)

{
}



ag::QuadDrawer::~QuadDrawer()
{
}



void ag::QuadDrawer::setQuadLength(size_t l)
{
  d_quadLength = l;
}



size_t ag::QuadDrawer::quadLength() const
{
  return d_quadLength;
}



//------------------------------------------------------------------------------
ag::ClassQuadDrawer::ClassQuadDrawer(size_t l)

  : QuadDrawer(l)

{
}



ag::ClassQuadDrawer::~ClassQuadDrawer()
{
}



//------------------------------------------------------------------------------
ag::RangeQuadDrawer::RangeQuadDrawer(const RangeDrawProps& dp, size_t l)

  : QuadDrawer(l),
    d_props(dp)

{
}



ag::RangeQuadDrawer::~RangeQuadDrawer()
{
}



//------------------------------------------------------------------------------
ag::BooleanQuadDrawer::BooleanQuadDrawer(Raster const& r,
                   const BooleanDrawProps& dp, size_t l)

  : ClassQuadDrawer(l),
    d_raster(r), d_dp(dp)

{
  assert(d_dp.palette()->nrColours() >= 2);
}



ag::BooleanQuadDrawer::~BooleanQuadDrawer()
{
}



void ag::BooleanQuadDrawer::draw(size_t r, size_t c) const
{
  static const GLfloat m = 255.0;
  static const QColor& trueColour = d_dp.colourByIndex(1);
  static GLfloat diffuse_true[] = {
    trueColour.red() / m,
    trueColour.green() / m,
    trueColour.blue() / m,
    1.0
  };
  static GLfloat specular_true[] = {
    GLfloat(0.5) * (trueColour.red() / m),
    GLfloat(0.5) * (trueColour.green() / m),
    GLfloat(0.5) * (trueColour.blue() / m),
    1.0
  };
  static GLfloat shininess_true[] = { 25.0 };
  static GLfloat emission_true[] = {
    GLfloat(0.1) * (trueColour.red() / m),
    GLfloat(0.1) * (trueColour.green() / m),
    GLfloat(0.1) * (trueColour.blue() / m),
    1.0
  };
  static const QColor& falseColour = d_dp.colourByIndex(0);
  static GLfloat diffuse_false[] = {
    falseColour.red() / m,
    falseColour.green() / m, falseColour.blue() / m,
    1.0
  };
  static GLfloat specular_false[]  = {
    GLfloat(0.5) * (falseColour.red() / m),
    GLfloat(0.5) * (falseColour.green() / m),
    GLfloat(0.5) * (falseColour.blue() / m),
    1.0
  };
  static GLfloat shininess_false[] = { 25.0 };
  static GLfloat emission_false[]  = {
    GLfloat(0.1) * (falseColour.red() / m),
    GLfloat(0.1) * (falseColour.green() / m),
    GLfloat(0.1) * (falseColour.blue() / m),
    1.0
  };

  UINT1 v = d_raster.cell<UINT1>(r, c);

  if(!pcr::isMV(v)) {
    if(v) {
      glColor3f(trueColour.red() / m, trueColour.green() / m,
                   trueColour.blue() / m);
      glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, diffuse_true);
      glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, specular_true);
      glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, shininess_true);
      glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, emission_true);
    }
    else {
      glColor3f(falseColour.red() / m, falseColour.green() / m,
                   falseColour.blue() / m);
      glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, diffuse_false);
      glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, specular_false);
      glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, shininess_false);
      glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, emission_false);
    }
  }
}



//------------------------------------------------------------------------------

ag::NominalQuadDrawer::NominalQuadDrawer(Raster const& r,
                   const NominalDrawProps& dp, size_t l)

  : ClassQuadDrawer(l),
    d_raster(r), d_dp(dp)

{
}



ag::NominalQuadDrawer::~NominalQuadDrawer()
{
}



void ag::NominalQuadDrawer::draw(size_t r, size_t c) const
{
  static const GLfloat m = 255.0;

  INT4 v = d_raster.cell<INT4>(r, c);

  if(!pcr::isMV(v)) {
    size_t i = d_dp.classifier().index(v);
    const QColor& colour = d_dp.colourByIndex(i);

    GLfloat diffuse[] = {
      colour.red() / m, colour.green() / m,
      colour.blue() / m,
      1.0
    };
    GLfloat specular[] = {
      GLfloat(0.5) * (colour.red() / m),
      GLfloat(0.5) * (colour.green() / m),
      GLfloat(0.5) * (colour.blue() / m),
      1.0
    };
    GLfloat shininess[] = { 25.0 };
    GLfloat emission[] = {
      GLfloat(0.1) * (colour.red() / m),
      GLfloat(0.1) * (colour.green() / m),
      GLfloat(0.1) * (colour.blue() / m),
      1.0
    };

    glColor3f(colour.red() / m, colour.green() / m, colour.blue() / m);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, diffuse);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, specular);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, shininess);
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, emission);
  }
}



//------------------------------------------------------------------------------

ag::OrdinalQuadDrawer::OrdinalQuadDrawer(Raster const& r,
                   const OrdinalDrawProps& dp, size_t l)

  : ClassQuadDrawer(l),
    d_raster(r), d_dp(dp)

{
}



ag::OrdinalQuadDrawer::~OrdinalQuadDrawer()
{
}



void ag::OrdinalQuadDrawer::draw(size_t r, size_t c) const
{
  static const GLfloat m = 255;

  INT4 v = d_raster.cell<INT4>(r, c);

  if(!pcr::isMV(v)) {
    size_t i = d_dp.classifier().index(v);
    const QColor& colour = d_dp.colourByIndex(i);

    GLfloat diffuse[] = {
      colour.red() / m,
      colour.green() / m,
      colour.blue() / m,
      1.0
    };
    GLfloat specular[] = {
      GLfloat(0.5) * (colour.red() / m),
      GLfloat(0.5) * (colour.green() / m),
      GLfloat(0.5) * (colour.blue() / m),
      1.0
    };
    GLfloat shininess[] = { 25.0 };
    GLfloat emission[] = {
      GLfloat(0.1) * (colour.red() / m),
      GLfloat(0.1) * (colour.green() / m),
      GLfloat(0.1) * (colour.blue() / m),
      1.0
    };

    glColor3f(colour.red() / m, colour.green() / m, colour.blue() / m);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, diffuse);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, specular);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, shininess);
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, emission);
  }
}



//------------------------------------------------------------------------------

ag::ScalarQuadDrawer::ScalarQuadDrawer(Raster const& r,
                   const RangeDrawProps& dp, size_t l)

  : RangeQuadDrawer(dp, l),
    d_raster(r)

{
}



ag::ScalarQuadDrawer::~ScalarQuadDrawer()
{
}



void ag::ScalarQuadDrawer::draw(size_t r, size_t c) const
{
  static const GLfloat m = 255.0;

  REAL4 v = d_raster.cell<REAL4>(r, c);

  if(!pcr::isMV(v)) {
    QColor colour;
    if(drawProps().nrClasses() > 0) {
      colour = drawProps().colour(v);
    }
    else {
      colour = QColor(150, 50, 50);
    }

    GLfloat diffuse[] = {
      colour.red() / m,
      colour.green() / m,
      colour.blue() / m,
      1.0
    };
    GLfloat specular[] = {
      GLfloat(0.5) * (colour.red() / m),
      GLfloat(0.5) * (colour.green() / m),
      GLfloat(0.5) * (colour.blue() / m),
      1.0
    };
    GLfloat shininess[] = { 25.0 };
    GLfloat emission[] = {
      GLfloat(0.1) * (colour.red() / m),
      GLfloat(0.1) * (colour.green() / m),
      GLfloat(0.1) * (colour.blue() / m),
      1.0
    };

    glColor3f(colour.red() / m, colour.green() / m, colour.blue() / m);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, diffuse);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, specular);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, shininess);
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, emission);
  }
}



//------------------------------------------------------------------------------

ag::DirectionalQuadDrawer::DirectionalQuadDrawer(Raster const& r,
                   const RangeDrawProps& dp, size_t l)

  : RangeQuadDrawer(dp, l),
    d_raster(r)

{
}



ag::DirectionalQuadDrawer::~DirectionalQuadDrawer()
{
}



void ag::DirectionalQuadDrawer::draw(size_t r, size_t c) const
{
  static const GLfloat m = 255.0;

  REAL4 v = d_raster.cell<REAL4>(r, c);

  if(!pcr::isMV(v)) {
    QColor colour;

    if(v == -1.0) {
      colour = QColor(255, 0, 0);
    }
    else {
      if(drawProps().nrClasses() > 0) {
        colour = drawProps().colour(v);
      }
      else {
        colour = QColor(150, 50, 50);
      }
    }

    GLfloat diffuse[] = {
      colour.red() / m,
      colour.green() / m,
      colour.blue() / m,
      1.0
    };
    GLfloat specular[] = {
      GLfloat(0.5) * (colour.red() / m),
      GLfloat(0.5) * (colour.green() / m),
      GLfloat(0.5) * (colour.blue() / m),
      1.0
    };
    GLfloat shininess[] = { 25.0 };
    GLfloat emission[] = {
      GLfloat(0.1) * (colour.red() / m),
      GLfloat(0.1) * (colour.green() / m),
      GLfloat(0.1) * (colour.blue() / m),
      1.0
    };

    glColor3f(colour.red() / m, colour.green() / m, colour.blue() / m);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, diffuse);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, specular);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, shininess);
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, emission);
  }
}



//------------------------------------------------------------------------------

// Next to the ldd raster in d_raster we create a 'global drainage direction'
// raster. This raster contains bit patterns which are representative for the
// immediate surroundings of each ldd cell. In other words: based on the gdd
// pattern we know for each ldd cell which surrounding cells drain to it.
// The bit pattern is 8 bits in size. All zero's means that no surrounding
// cell drains into the current cell. All one's means that all surrounding
// cells drain into the current cell. If a surrounding cell drains into the
// current one, we set its bit to one as folows:
// pattern |= 1 << code_of_surrounding_cell
// All surrounding cells have a unique code [1-8]:
//
//               |     |
//            6  |  7  |  8
//               |     |
//          -----+-----+-----
//               |     |
//            4  |     |  5
//               |     |
//          -----+-----+-----
//               |     |
//            1  |  2  |  3
//               |     |
//

ag::LddQuadDrawer::LddQuadDrawer(Raster const& r,
                   const LddDrawProps& dp, size_t l)

  : ClassQuadDrawer(l),
    d_raster(r), d_dp(dp),
    d_gdd(d_raster.dimensions().nrRows(), d_raster.dimensions().nrCols(),
         d_raster.dimensions().cellSize(),
         d_raster.dimensions().west(), d_raster.dimensions().north(),
         dal::TypeTraits<unsigned char>::typeId)

{
  // Bit patterns for single gdd directions.
  static const unsigned char ll    = 1 << 0;
  static const unsigned char down  = 1 << 1;
  static const unsigned char lr    = 1 << 2;
  static const unsigned char left  = 1 << 3;
  static const unsigned char right = 1 << 4;
  static const unsigned char ul    = 1 << 5;
  static const unsigned char up    = 1 << 6;
  static const unsigned char ur    = 1 << 7;

  // Initialise all global drainage direction codes with zero.
  d_gdd.fill(static_cast<unsigned char>(0));

  if(d_raster.dimensions().nrRows() > 2 && d_raster.dimensions().nrCols() > 2) {
    UINT1 v;
    // Loop over rows, skip first and last one.
    for(size_t r = 1; r < d_raster.dimensions().nrRows() - 1; ++r) {
      // Loop over cols, skip first and last one.
      for(size_t c = 1; c < d_raster.dimensions().nrCols() - 1; ++c) {

        // We arrived at a cell with neighbours at all sides.
        v = d_raster.cell<UINT1>(r + 1, c - 1);         // Value of lower left neighb.
        if(!pcr::isMV(v) && v == 9) {
          d_gdd.cell<unsigned char>(r, c) |= ll;
        }
        v = d_raster.cell<UINT1>(r + 1, c);             // Value of lower cell neighb.
        if(!pcr::isMV(v) && v == 8) {
          d_gdd.cell<unsigned char>(r, c) |= down;
        }
        v = d_raster.cell<UINT1>(r + 1, c + 1);         // Value of lower right neighb.
        if(!pcr::isMV(v) && v == 7) {
          d_gdd.cell<unsigned char>(r, c) |= lr;
        }
        v = d_raster.cell<UINT1>(r, c - 1);             // Value of left neighb.
        if(!pcr::isMV(v) && v == 6) {
          d_gdd.cell<unsigned char>(r, c) |= left;
        }
        v = d_raster.cell<UINT1>(r, c + 1);             // Value of right neighb.
        if(!pcr::isMV(v) && v == 4) {
          d_gdd.cell<unsigned char>(r, c) |= right;
        }
        v = d_raster.cell<UINT1>(r - 1, c - 1);         // Value of upper left neighb.
        if(!pcr::isMV(v) && v == 3) {
          d_gdd.cell<unsigned char>(r, c) |= ul;
        }
        v = d_raster.cell<UINT1>(r - 1, c);             // Value of upper neighb.
        if(!pcr::isMV(v) && v == 2) {
          d_gdd.cell<unsigned char>(r, c) |= up;
        }
        v = d_raster.cell<UINT1>(r - 1, c + 1);         // Value of upper right neighb.
        if(!pcr::isMV(v) && v == 1) {
          d_gdd.cell<unsigned char>(r, c) |= ur;
        }
      }
    }
  }
}



ag::LddQuadDrawer::~LddQuadDrawer()
{
}



void ag::LddQuadDrawer::draw(size_t r, size_t c) const
{
  UINT1 v = d_raster.cell<UINT1>(r, c);
  if(!pcr::isMV(v)) {
    unsigned char gdd = d_gdd.cell<unsigned char>(r, c);
    GLuint tex = d_dp.texture(gdd, v);
    glBindTexture(GL_TEXTURE_2D, tex);
  }
}
