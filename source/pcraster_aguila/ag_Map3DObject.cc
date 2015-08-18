#include "ag_Map3DObject.h"
#include <vector>
#include "geo_DataType.h"
#include "geo_mathlib.h"
#include "ag_DataProperties.h"
#include "ag_QuadDrawer.h"
#include "ag_RasterDataSources.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

namespace ag {
template<class T>
// void quadInfo(T const* cells, dal::RasterDimensions const& dim,
void quadInfo(Raster const& raster,
         size_t row, size_t col, size_t length,
         GLfloat* zul, GLfloat* zur, GLfloat* zlr, GLfloat* zll)
{
  *zul = dal::average<Raster, T>(raster, row - length, col - length, length + 1);
  *zur = dal::average<Raster, T>(raster, row - length, col         , length + 1);
  *zlr = dal::average<Raster, T>(raster, row         , col         , length + 1);
  *zll = dal::average<Raster, T>(raster, row         , col - length, length + 1);
}



template<class T>
void triangleStripInfo(T const* cells, dal::RasterDimensions const& dim,
         size_t row, size_t col,
         size_t length,
         GLfloat* currentHeight, GLfloat* frontHeight, GLfloat* rightHeight)
{
  *currentHeight = cells[dim.index(row, col)];
  *frontHeight   = cells[dim.index(row + length, col)];
  *rightHeight   = cells[dim.index(row, col + length)];
}



// Instantiate the template functions using explicit instantiation declarations.
// template void quadInfo<float>(float const*, dal::RasterDimensions const&,
template void quadInfo<float>(Raster const&,
         size_t, size_t, size_t,
         GLfloat*, GLfloat*, GLfloat*, GLfloat*);
// template void quadInfo<double>(double const*, dal::RasterDimensions const&,
template void quadInfo<double>(Raster const&,
         size_t, size_t, size_t,
         GLfloat*, GLfloat*, GLfloat*, GLfloat*);
template void triangleStripInfo<float>(float const*, dal::RasterDimensions const&,
         size_t, size_t, size_t, GLfloat*, GLfloat*, GLfloat*);
template void triangleStripInfo<double>(double const*, dal::RasterDimensions const&,
         size_t, size_t, size_t, GLfloat*, GLfloat*, GLfloat*);

} // namespace ag

//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructs a Map3DObject object.
/*!
  \sa        ag::SceneObject(GLfloat, GLfloat, GLfloat, GLfloat, GLfloat,
             GLfloat)

  The default quad length is 1 and default no fishnet will be shown.
*/
ag::Map3DObject::Map3DObject(GLfloat x, GLfloat y, GLfloat z,
                   GLfloat yaw, GLfloat pitch, GLfloat roll)

  : ag::SceneObject(x, y, z, yaw, pitch, roll),
    d_quadLength(1), d_scale(1.0), d_list(0),
    d_showFishnet(false)

{
}



//! Destructs a Map3DObject object.
/*!
*/
ag::Map3DObject::~Map3DObject()
{
  deleteScene();
}



void ag::Map3DObject::renderObject()
{
  if(d_list > 0) {
    GLfloat m[16];
    matrix(m);
    glMultMatrixf(m);
    glTranslatef(x(), y(), z());

    glColor3f(0.75, 0.75, 0.75);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    //glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    // Offset the scene polygons, otherwise the outline will be stiched.
//    glEnable(GL_POLYGON_OFFSET_FILL);
//    glPolygonOffset(1.0, 1.0);
    glCallList(d_list);
//    glDisable(GL_POLYGON_OFFSET_FILL);

/*
    if(d_showFishnet) {
      // The fishnet will be shown with lights off.
      GLboolean light;
      glGetBooleanv(GL_LIGHT0, &light);
      if(light) {
        glDisable(GL_LIGHTING);
        glDisable(GL_LIGHT0);
      }
      glColor3f(0.0, 0.0, 0.0);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glCallList(d_list);
      if(light) {
        glEnable(GL_LIGHTING);
        glEnable(GL_LIGHT0);
      }
    }
*/

  }
  setDirty(false);
}



void ag::Map3DObject::createScene(const ag::DataObject& o,
         const ag::DataGuide& h, const std::vector<DataGuide>& dataGuides)
{
#ifdef DEBUG_DEVELOP
  assert(d_scale > 0.0);
#endif

  deleteScene();
  setDirty(true);

  createDrapeScene(o, h, dataGuides);

  setValid(true);
}



void ag::Map3DObject::createHeightScene(const ag::DataObject& o,
                   const ag::DataGuide& h)
{
  assert(o.isValid(h));

  Raster const& heightRaster(o.rasterDataSources().data(h));

  // Check if there're enough cells in the raster.
  if(!(heightRaster.dimensions().nrCols() >= 2 * d_quadLength + 1 &&
       heightRaster.dimensions().nrRows() >= 2 * d_quadLength + 1)) {
    return;
  }

  // Check if the min and max are defined (if there're values in the raster).
  REAL4 minHeight, maxHeight;       // Extreme heights.
  if(!heightRaster.min(minHeight) || !heightRaster.max(maxHeight)) {
    // All MV's.
    return;
  }

  GLfloat cLeft, cRight, cFront, cBack; // Triangle coordinates.
  GLfloat zCurrent, zFront, zRight;    // Heights of corners of triangles.
  GLfloat nx, ny, nz;                  // Normal vector of quad.
  GLfloat dx, dy, dz;
  GLfloat scale = d_scale;             // Scale for height values.

  dx = d_quadLength * heightRaster.dimensions().cellSize();
  dy = minHeight + 0.5 * (maxHeight - minHeight);
  dz = d_quadLength * heightRaster.dimensions().cellSize();

  setSize(heightRaster.dimensions().longitudinalExtent(), heightRaster.dimensions().latitudinalExtent(), maxHeight - minHeight);

  // Calculate default scale.
  if(height()) {
    scale *= ((width() + depth()) / 8) / height();
  }
  dy *= scale;
  setSize(width(), depth(), height() * scale);

  static GLfloat mat_diffuse[]   = { 0.75f, 0.75f, 0.75f, 1.0f };
  static GLfloat mat_specular[]  = { 0.5f, 0.5f, 0.5f, 1.0f };
  static GLfloat mat_shininess[] = { 25.0f };
  static GLfloat mat_emission[]  = { 0.1f, 0.1f, 0.1f, 1.0f };

  //----------------------------------------------------------------------------

  assert(d_list <= 0);
  d_list = glGenLists(1);
  assert(d_list > 0);
  glNewList(d_list, GL_COMPILE);

  // Why and_back?
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess);
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission);

  cBack = -0.5 * heightRaster.dimensions().latitudinalExtent() + 0.5 * (d_quadLength + 1) * heightRaster.dimensions().cellSize();

  for(size_t row = 0; row < (heightRaster.dimensions().nrRows() - d_quadLength);
         row += d_quadLength) {

    glBegin(GL_TRIANGLE_STRIP);
    cLeft  = -0.5 * heightRaster.dimensions().longitudinalExtent() +
         0.5 * (d_quadLength + 1) * heightRaster.dimensions().cellSize();
    cFront = cBack + dz;

    for(size_t col = 0; col < (heightRaster.dimensions().nrCols() - d_quadLength);
         col += d_quadLength) {

      cRight = cLeft + dx;

      triangleStripInfo<REAL4>(heightRaster.cells<REAL4>(),
         heightRaster.dimensions(), row, col, d_quadLength,
         &zCurrent, &zFront, &zRight);

      if(pcr::isMV(zCurrent) || pcr::isMV(zFront) || pcr::isMV(zRight)) {
        glEnd();
        glBegin(GL_TRIANGLE_STRIP);
      }
      else {

        zCurrent = zCurrent * scale - dy;
        zFront   = zFront * scale - dy;
        zRight   = zRight * scale - dy;

        geo::normal<GLfloat>(cLeft, cBack, zCurrent, cLeft, cFront, zFront,
                   cRight, cBack, zRight, &nx, &ny, &nz);
        glNormal3d(-nx, -nz, -ny);

        glVertex3d(cLeft,  zCurrent, cBack);
        glVertex3d(cLeft,  zFront,  cFront);
      }

      cLeft = cRight;
    }

    glEnd();
    cBack = cFront;
  }

  glEndList();

/*
  GLfloat cLeft, cRight, cFront, cBack;          // Quad coordinates.
  GLfloat zul, zur, zlr, zll;          // Heights of corners of quad.
  GLfloat nx, ny, nz;                  // Normal vector of quad.
  GLfloat dx, dy, dz;
  GLfloat scale = d_scale;             // Scale for height values.

  dx = d_quadLength * heightRaster.dimensions().cellSize();
  dy = minHeight + 0.5 * (maxHeight - minHeight);
  dz = d_quadLength * heightRaster.dimensions().cellSize();

  setSize(heightRaster.dimensions().width(), heightRaster.dimensions().height(), maxHeight - minHeight);

  // Calculate default scale.
  if(height()) {
    scale *= ((width() + depth()) / 8) / height();
  }
  dy *= scale;
  setSize(width(), depth(), height() * scale);

  size_t firstRow, lastRow, firstCol, lastCol;
  firstRow = d_quadLength + 1;
  lastRow  = heightRaster.dimensions().nrRows() - (d_quadLength + 1);
  firstCol = d_quadLength + 1;
  lastCol  = heightRaster.dimensions().nrCols() - (d_quadLength + 1);

  static GLfloat mat_diffuse[]   = { 0.75, 0.75, 0.75, 1.0 };
  static GLfloat mat_specular[]  = { 0.5, 0.5, 0.5, 1.0 };
  static GLfloat mat_shininess[] = { 25.0 };
  static GLfloat mat_emission[]  = { 0.1, 0.1, 0.1, 1.0 };

  //----------------------------------------------------------------------------

  assert(d_list <= 0);
  d_list = glGenLists(1);
  assert(d_list > 0);
  glNewList(d_list, GL_COMPILE);

  // Why and_back?
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess);
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission);

  glBegin(GL_QUADS);

  // Draw the quads.
  cBack = -0.5 * heightRaster.dimensions().height() +
                   0.5 * (d_quadLength + 1) * heightRaster.dimensions().cellSize();

  for(size_t r = firstRow; r <= lastRow; r += d_quadLength) {

    cLeft  = -0.5 * heightRaster.dimensions().width() +
                   0.5 * (d_quadLength + 1) * heightRaster.dimensions().cellSize();
    cFront = cBack + dz;

    for(size_t c = firstCol; c <= lastCol; c += d_quadLength) {

      cRight = cLeft + dx;
      quadInfo<REAL4>(heightRaster, r, c, d_quadLength,
                   &zul, &zur, &zlr, &zll);
      if(!pcr::isMV(zul) && !pcr::isMV(zur) && !pcr::isMV(zlr) &&
                   !pcr::isMV(zll)) {

        zul *= scale;
        zur *= scale;
        zlr *= scale;
        zll *= scale;

        geo::normal<GLfloat>(cLeft, cBack, zul - dy, cLeft, cFront, zll - dy,
                   cRight, cFront, zlr - dy, &nx, &ny, &nz);

        // Draw the quad.
        glNormal3d(-nx, -nz, -ny);
        glVertex3d(cLeft,  zul - dy, cBack);
        glVertex3d(cLeft,  zll - dy, cFront);
        glVertex3d(cRight, zlr - dy, cFront);
        glVertex3d(cRight, zur - dy, cBack);
      }
      cLeft = cRight;
    }
    cBack = cFront;
  }

  glEnd();
  glEndList();
*/
}



void ag::Map3DObject::createDrapeScene(const ag::DataObject& dataObject,
         const ag::DataGuide& h, const std::vector<DataGuide>& dataGuides)
{
  assert(dataObject.isValid(h));

  Raster const& heightRaster(dataObject.rasterDataSources().data(h));

  // Check if there're enough cells in the raster.
  if(!(heightRaster.dimensions().nrCols() >= 2 * d_quadLength + 1 &&
       heightRaster.dimensions().nrRows() >= 2 * d_quadLength + 1)) {
    return;
  }

  // Check if the min and max are defined (if there're values in the raster).
  REAL4 minHeight, maxHeight;       // Extreme heights.
  if(!heightRaster.min(minHeight) || !heightRaster.max(maxHeight)) // All MV's.
    return;

  // Create the quad drawers.
  std::vector<QuadDrawer *> drawers;
  std::vector<QuadDrawer *>::const_iterator draw_it;
  std::vector<QuadDrawer *>::reverse_iterator rdraw_it;
  for(std::vector<DataGuide>::const_iterator it = dataGuides.begin();
         it != dataGuides.end(); ++it) {

    if(dataObject.isEnabled(*it)) {

      ag::DataGuide guide = *it;
      assert(dataObject.isValid(guide));
      assert(guide.type() == geo::STACK);

      Raster const& raster(dataObject.rasterDataSources().data(guide));

      if(guide.valueScale() == VS_BOOLEAN) {
        // UINT1 min, max;
        // if(stack.min(&min) && stack.max(&max)) {
          const BooleanDrawProps& props =
                   dataObject.properties().booleanDrawProperties(guide);
          BooleanQuadDrawer* drawer = new BooleanQuadDrawer(raster, props,
                   d_quadLength);
          drawers.push_back(drawer);
        // }
      }
      else if(guide.valueScale() == VS_NOMINAL) {
        // INT4 min, max;
        // if(stack.min(&min) && stack.max(&max)) {
          const NominalDrawProps& props =
                   dataObject.properties().nominalDrawProperties(guide);
          NominalQuadDrawer* drawer = new NominalQuadDrawer(raster, props,
                   d_quadLength);
          drawers.push_back(drawer);
        // }
      }
      else if(guide.valueScale() == VS_ORDINAL) {
        // INT4 min, max;
        // if(stack.min(&min) && stack.max(&max)) {
          const OrdinalDrawProps& props =
                   dataObject.properties().ordinalDrawProperties(guide);
          OrdinalQuadDrawer* drawer = new OrdinalQuadDrawer(raster, props,
                   d_quadLength);
          drawers.push_back(drawer);
        // }
      }
      else if(guide.valueScale() == VS_SCALAR) {
        // REAL4 min, max;
        // if(stack.min(&min) && stack.max(&max)) {
          const RangeDrawProps& props =
                   dataObject.properties().rangeDrawProperties(guide);
          ScalarQuadDrawer* drawer =
                   new ScalarQuadDrawer(raster, props, d_quadLength);
          drawers.push_back(drawer);
        // }
      }
      else if(guide.valueScale() == VS_DIRECTION) {
        // geo::DirectType min, max;
        // if(stack.min(&min) && stack.max(&max)) {
          const RangeDrawProps& props =
                   dataObject.properties().rangeDrawProperties(guide);
          DirectionalQuadDrawer* drawer =
                   new DirectionalQuadDrawer(raster, props, d_quadLength);
          drawers.push_back(drawer);
        // }
      }
      else if(guide.valueScale() == VS_LDD) {
        // UINT1 min, max;
        // if(stack.min(&min) && stack.max(&max)) {
          const LddDrawProps& props =
                   dataObject.properties().lddDrawProperties(guide);
          LddQuadDrawer* drawer = new LddQuadDrawer(raster, props,
                   d_quadLength);
          drawers.push_back(drawer);
        // }
      }
    }
  }

  if(drawers.empty()) {
    // No attributes or all attributes disabled.
    createHeightScene(dataObject, h);
  }
  else {

    GLfloat cLeft, cRight, cFront, cBack;          // Quad coordinates.
    GLfloat zul, zur, zlr, zll;          // Heights of corners of quad.
    GLfloat nx, ny, nz;                  // Normal vector of quad.
    GLfloat dx, dy, dz;
    GLfloat scale = d_scale;             // Scale for height values.

    dx = d_quadLength * heightRaster.dimensions().cellSize();
    dy = minHeight + 0.5 * (maxHeight - minHeight);
    dz = d_quadLength * heightRaster.dimensions().cellSize();

    setSize(heightRaster.dimensions().longitudinalExtent(), heightRaster.dimensions().latitudinalExtent(), maxHeight - minHeight);

    // Calculate default scale.
    if(height()) {
      scale *= ((width() + depth()) / 8) / height();
    }
    dy *= scale;
    setSize(width(), depth(), height() * scale);

    size_t firstRow, lastRow, firstCol, lastCol;
    firstRow = d_quadLength + 1;
    lastRow  = heightRaster.dimensions().nrRows() - (d_quadLength + 1);
    firstCol = d_quadLength + 1;
    lastCol  = heightRaster.dimensions().nrCols() - (d_quadLength + 1);

    static GLfloat mat_diffuse[]   = { 0.75f, 0.75f, 0.75f, 1.0f };
    static GLfloat mat_specular[]  = { 0.5f, 0.5f, 0.5f, 1.0f };
    static GLfloat mat_shininess[] = { 25.0f };
    static GLfloat mat_emission[]  = { 0.1f, 0.1f, 0.1f, 1.0f };

    //--------------------------------------------------------------------------

    assert(d_list <= 0);
    d_list = glGenLists(1);
    assert(d_list > 0);
    glNewList(d_list, GL_COMPILE);

    // Draw the quads.
    cBack = -0.5 * heightRaster.dimensions().latitudinalExtent() +
                     0.5 * (d_quadLength + 1) * heightRaster.dimensions().cellSize();

    for(size_t r = firstRow; r <= lastRow; r += d_quadLength)
    {
      cLeft  = -0.5 * heightRaster.dimensions().longitudinalExtent() +
                     0.5 * (d_quadLength + 1) * heightRaster.dimensions().cellSize();
      cFront = cBack + dz;

      for(size_t c = firstCol; c <= lastCol; c += d_quadLength)
      {
        cRight = cLeft + dx;
        // quadInfo<REAL4>(heightRaster.cells<REAL4>(),
        //              heightRaster.rasterSpace(),
        quadInfo<REAL4>(heightRaster,
                     r, c, d_quadLength,
                     &zul, &zur, &zlr, &zll);
        if(!pcr::isMV(zul) && !pcr::isMV(zur) && !pcr::isMV(zlr) &&
                     !pcr::isMV(zll))
        {
          zul *= scale;
          zur *= scale;
          zlr *= scale;
          zll *= scale;

          geo::normal<GLfloat>(cLeft, cBack, zul - dy, cLeft, cFront, zll - dy,
                     cRight, cFront, zlr - dy, &nx, &ny, &nz);

          glNormal3d(-nx, -nz, -ny);

          // Draw the quad.
          // For efficiency we look for the top attribute which fills the quad
          // and start calling that drawer and the ones after it (possibly an
          // ldd which doesnt fill the quad). This results in less drawn quads
          // and prevends the situation that only the first filled quad is
          // visible.
          for(rdraw_it = drawers.rbegin(); rdraw_it != drawers.rend();
                     ++rdraw_it) {
            if((*rdraw_it)->willFill(r, c)) {
              ++rdraw_it;
              break;
            }
          }
          --rdraw_it;

          // Remove possibly set texture.
          glBindTexture(GL_TEXTURE_2D, 0);

          // Set default colour props.
          glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_diffuse);
          glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
          glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess);
          glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission);

          // Conversion from reverse to normal iterator shifts the logical
          // position of the iterator.
          draw_it = (++rdraw_it).base();
          while(draw_it != drawers.end()) {
            (*draw_it)->draw(r, c);
            ++draw_it;
          }

          glBegin(GL_QUADS);
            glTexCoord2f(0.0, 0.0); glVertex3f(cLeft,  zll - dy, cFront); // ll
            glTexCoord2f(1.0, 0.0); glVertex3f(cRight, zlr - dy, cFront); // lr
            glTexCoord2f(1.0, 1.0); glVertex3f(cRight, zur - dy, cBack);  // ur
            glTexCoord2f(0.0, 1.0); glVertex3f(cLeft,  zul - dy, cBack);  // ul
          glEnd();
        }
        cLeft = cRight;
      }
      cBack = cFront;
    }

    glEndList();

    // yepyep: Totally unguarded!
    for(draw_it = drawers.begin(); draw_it != drawers.end(); ++draw_it) {
      delete *draw_it;
    }
  }
}



void ag::Map3DObject::deleteScene()
{
  if(d_list > 0) {
    glDeleteLists(d_list, 1);
    d_list = 0;
    setSize(0.0, 0.0, 0.0);
  }
}



bool ag::Map3DObject::showFishnet() const
{
  return d_showFishnet;
}



void ag::Map3DObject::setShowFishnet(bool s)
{
  if(s != d_showFishnet)
  {
    d_showFishnet = s;
    setDirty(true);
  }
}



//! Returns the current scale.
/*!
  \return    Scale.
  \sa        setScale(GLfloat)
*/
GLfloat ag::Map3DObject::scale() const
{
  return d_scale;
}



//! Sets the scale of the height values to \a s.
/*!
  \param     s Scale. It will be clamped to [0.1 - 10.0]
  \warning   The scale will be clamped to [0.1 - 10.0]. This means you can
             rescale the heights to 0.1 - 10 times the default height.
  \sa        scale()

  The default scale is determined by the horizontal extension of the stacks.

  The new scale \a s will be multiplied with the default one, so a new scale
  of 2.0 will result in height values twice as large as the default ones.
*/
void ag::Map3DObject::setScale(GLfloat s)
{
  s = std::min<GLfloat>(s, 10.0f);
  s = std::max<GLfloat>(s, 0.1f);
  if(d_scale != s) {
    d_scale = s;
    setValid(false);
  }
}



void ag::Map3DObject::setQuadLength(size_t l)
{
  if(d_quadLength != l) {
    d_quadLength = l;
    setValid(false);
  }
}



size_t ag::Map3DObject::quadLength() const
{
  return d_quadLength;
}



void ag::Map3DObject::reset()
{
  ag::SceneObject::reset();
  setScale(1.0);
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


