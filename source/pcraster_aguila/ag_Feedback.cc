#include "ag_Feedback.h"

// Std
#include <cassert>

#ifdef DEBUG_DEVELOP
  #ifndef INCLUDED_IOSTREAM
#include <iostream>
  #define INCLUDED_IOSTREAM
  #endif
#endif

// Pcr



/*!
  \file
  This file contains the implementation of the Feedback class.
*/



//------------------------------------------------------------------------------

namespace ag {

class FeedbackPrivate
{
public:
  GLint            d_sizeOfBuffer;
  GLint            d_valuesStored;
  GLfloat*         d_buffer;

  FeedbackPrivate(GLint size)
    : d_sizeOfBuffer(size), d_valuesStored(0),
      d_buffer(new GLfloat[size])
  {
  }

  ~FeedbackPrivate()
  {
    delete[] d_buffer;
  }

};

}



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

ag::Feedback::Feedback(GLint size)

  : d_data(0)

{
  d_data = new FeedbackPrivate(size);
  glFeedbackBuffer(size, GL_3D_COLOR, d_data->d_buffer);
}



ag::Feedback::~Feedback()
{
  delete d_data;

  int m;
  glGetIntegerv(GL_RENDER_MODE, &m);
  if(m == GL_FEEDBACK) {
    glRenderMode(GL_RENDER);
  }
}



void ag::Feedback::start() const
{
  d_data->d_valuesStored = 0;
  glRenderMode(GL_FEEDBACK);
}



void ag::Feedback::stop() const
{
  d_data->d_valuesStored = glRenderMode(GL_RENDER);
  assert(d_data->d_valuesStored >= 0);
  assert(d_data->d_valuesStored <= d_data->d_sizeOfBuffer);
}



GLint ag::Feedback::nrValuesStored() const
{
  return d_data->d_valuesStored;
}



const GLfloat* ag::Feedback::buffer() const
{
  return d_data->d_buffer;
}



ag::Feedback::const_iterator ag::Feedback::begin() const
{
  return d_data->d_buffer;
}



ag::Feedback::const_iterator ag::Feedback::end() const
{
  return d_data->d_buffer + d_data->d_valuesStored;
}



#ifdef DEBUG_DEVELOP
void ag::Feedback::describe() const
{
  GLint count = d_data->d_valuesStored;
  int token, nrVertices;

  while(count) {
    token = static_cast<int>(d_data->d_buffer[d_data->d_valuesStored - count]);
    --count;

    switch(token) {
      case GL_POINT_TOKEN:
      case GL_LINE_TOKEN:
      case GL_LINE_RESET_TOKEN:
      case GL_BITMAP_TOKEN:
      case GL_DRAW_PIXEL_TOKEN:
      case GL_COPY_PIXEL_TOKEN:
      case GL_PASS_THROUGH_TOKEN:
        // std::cout << "unhandled token" << std::endl;
        break;
      case GL_POLYGON_TOKEN:
        // std::cout << "POLYGON" << std::endl;
        nrVertices = static_cast<int>(
                   d_data->d_buffer[d_data->d_valuesStored - count]);
        // std::cout << "nr vertices: " << nrVertices << std::endl;
        --count;
        count -= 7 * nrVertices;
        break;
      default:
        // std::cout << "unknown token" << std::endl;
        break;
    }
  }
}
#endif



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


