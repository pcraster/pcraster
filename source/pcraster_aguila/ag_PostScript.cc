#include "ag_PostScript.h"

// Std
#include <algorithm>
#include <functional>
#include <fstream>

// Pcr
#include "com_exception.h"



/*!
  \file
  This file contains the implementation of the PostScript class.

  \todo the C-library psill contains code for EPS/Illustrator support:
  has primitives
*/



//------------------------------------------------------------------------------

namespace ag {

struct Feedback3DColor {
  GLfloat          d_x;
  GLfloat          d_y;
  GLfloat          d_z;
  GLfloat          d_red;
  GLfloat          d_green;
  GLfloat          d_blue;
  GLfloat          d_alpha;
};

struct DepthIndex {
  Feedback::const_iterator d_it;
  GLfloat d_depth;
};

struct furtherThan: public std::binary_function<DepthIndex, DepthIndex, bool> {
  bool operator()(DepthIndex i1, DepthIndex i2) {
    return i1.d_depth > i2.d_depth;
  }
};

class PostScriptPrivate
{
public:
  const Feedback&  d_feedback;         // OpenGL's feedback buffer.
  double           d_boundingBox[4];
  DepthIndex*      d_primitives;
  size_t           d_nrPrimitives;

  PostScriptPrivate(const Feedback& f, double llx, double lly, double urx,
                   double ury)
    : d_feedback(f), d_primitives(0), d_nrPrimitives(0)
  {
    d_boundingBox[0] = llx;
    d_boundingBox[1] = lly;
    d_boundingBox[2] = urx;
    d_boundingBox[3] = ury;
  }

  ~PostScriptPrivate()
  {
  }

};

}



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

ag::PostScript::PostScript(const Feedback& f, double llx, double lly,
                   double urx, double ury)

  : d_data(0)

{
  d_data = new PostScriptPrivate(f, llx, lly, urx, ury);
}



ag::PostScript::~PostScript()
{
  deletePrimitivesArray();
  delete d_data;
}



size_t ag::PostScript::nrPrimitives() const
{
  size_t nrVertices;
  size_t nrPrimitives = 0;
  int token;

  Feedback::const_iterator it = d_data->d_feedback.begin();

  while(it != d_data->d_feedback.end()) {

    token = static_cast<int>(*it);
    ++it;

    switch(token) {
      case GL_POLYGON_TOKEN:
        nrVertices = static_cast<size_t>(*it);
        ++it;
        ++nrPrimitives;
        it += 7 * nrVertices;
        break;

      default:
        assert(false);
        break;                 // Never reached.
    }
  }

  return nrPrimitives;
}



void ag::PostScript::sort()
{
  deletePrimitivesArray();
  createPrimitivesArray();
  fillPrimitivesArray();
  sortPrimitivesArray();
}



void ag::PostScript::deletePrimitivesArray()
{
  if(d_data->d_primitives) {
    delete[] d_data->d_primitives;
    d_data->d_primitives = 0;
  }

  d_data->d_nrPrimitives = 0;
}



void ag::PostScript::createPrimitivesArray()
{
  assert(!d_data->d_primitives);

  d_data->d_nrPrimitives = nrPrimitives();
  d_data->d_primitives = new DepthIndex[d_data->d_nrPrimitives];
}



void ag::PostScript::fillPrimitivesArray()
{
  assert(d_data->d_primitives);

  size_t nrVertices;
  size_t item = 0;
  int token;
  GLfloat depthSum;
  const Feedback3DColor* vertex;

  Feedback::const_iterator it = d_data->d_feedback.begin();

  while(it != d_data->d_feedback.end()) {

    d_data->d_primitives[item].d_it = it;
    token = static_cast<int>(*it);
    ++it;

    switch(token) {
      case GL_POLYGON_TOKEN:
        nrVertices = static_cast<size_t>(*it);
        ++it;
        vertex = (const Feedback3DColor*)it;

        depthSum = vertex[0].d_z;
        for(size_t i = 1; i < nrVertices; ++i) {
          depthSum += vertex[i].d_z;
        }
        d_data->d_primitives[item].d_depth = depthSum / nrVertices;

        it += 7 * nrVertices;
        break;

      default:
        assert(false);
        break;                 // Never reached.
    }

    ++item;
  }

  assert(item == d_data->d_nrPrimitives);
}



void ag::PostScript::sortPrimitivesArray()
{
  assert(d_data->d_primitives);

  std::sort(d_data->d_primitives,
                   d_data->d_primitives + d_data->d_nrPrimitives,
                   furtherThan());
}



void ag::PostScript::writeHeader(std::ostream& os)
{
  os << "%!PS-Adobe-3.0 EPSF-3.0\n"
        "%%BoundingBox: " << d_data->d_boundingBox[0] << " "
                   << d_data->d_boundingBox[1] << " "
                   << d_data->d_boundingBox[2] << " "
                   << d_data->d_boundingBox[3] << "\n"
     << "%EndComments\n"
        "gsave\n";
}



void ag::PostScript::writeBody(std::ostream& os)
{
  if(d_data->d_primitives) {
    for(size_t i = 0; i < d_data->d_nrPrimitives; ++i) {
      (void)writePrimitive(os, d_data->d_primitives[i].d_it);
    }
  }
  else {
    Feedback::const_iterator it = d_data->d_feedback.begin();

    while(it != d_data->d_feedback.end()) {
      it = writePrimitive(os, it);
    }
  }
}



void ag::PostScript::writeFooter(std::ostream& os)
{
  os << "grestore\n"
        "showpage\n";
}



ag::Feedback::const_iterator ag::PostScript::writePrimitive(std::ostream& os,
                   Feedback::const_iterator it)
{
  size_t nrVertices;
  int token;
  const Feedback3DColor* vertex;

  token = static_cast<int>(*it);
  ++it;

  switch(token) {
    case GL_POLYGON_TOKEN:
      nrVertices = static_cast<size_t>(*it);
      ++it;
      vertex = (const Feedback3DColor*)it;

      os << "newpath\n"
         << vertex[0].d_red << " " << vertex[0].d_green << " "
                   << vertex[0].d_blue << " setrgbcolor\n"
         << vertex[0].d_x << " " << vertex[0].d_y << " moveto\n";
      for(size_t i = 1; i < nrVertices; ++i) {
        os << vertex[i].d_x << " " << vertex[i].d_y << " lineto\n";
      }
      os << "closepath fill\n";

      it += 7 * nrVertices;
      break;

    default:
      assert(false);
      break;                 // Never reached.
  }

  return it;
}



void ag::PostScript::save(
         boost::filesystem::path const& path)
{
  std::ofstream fs;

  // com::open(fs, path.string());

  fs.open(path.string().c_str()); // , std::ios::out);

  if(!fs) {
    throw com::OpenFileErrnoMsg(path.string(),
         "could not open file for reading");
  }

  writeHeader(fs);
  writeBody(fs);
  writeFooter(fs);
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


