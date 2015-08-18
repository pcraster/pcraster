#include "ag_GLVisualisation.h"

// Library headers.
#include <sstream>

/*
#if defined(Q_WS_X11)
#include <GL/glx.h>
#endif
*/


// PCRaster library headers.
#include "com_exception.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the GLVisualisation class.
*/



//------------------------------------------------------------------------------

/*
namespace ag {

class CustomContext: public QGLContext
{

protected:

#if defined(Q_WS_WIN)
  int              choosePixelFormat   (void *p,
                                        HDC hdc);
#elif defined(Q_WS_X11)
  void*            chooseVisual        ();
#elif defined(Q_WS_MAC)
  void*            chooseMacVisual     (GDHandle gdev);
#endif

public:

                   CustomContext       (const QGLFormat &fmt,
                                        QPaintDevice *dev);


};


CustomContext::CustomContext(const QGLFormat &fmt, QPaintDevice *dev)
  : QGLContext(fmt, dev) {}
 
#if defined(Q_WS_WIN)
#include <windows.h> // DescribePixelFormat

// In the Windows implementation, we loop through all the available pixel
// formats until we find one that has a 32-bit depth buffer. In a real-world
// application we would probably have some other criteria that we'd check
// for as well. If a 32-bit depth buffer can't be found we fall back to the
// default QGLContext implementation. 
int CustomContext::choosePixelFormat(void *p, HDC pdc)
{
  PIXELFORMATDESCRIPTOR *pfd = (PIXELFORMATDESCRIPTOR *)p;
  int pfiMax = DescribePixelFormat(pdc, 0, 0, NULL);
  int pfi;
  for (pfi = 1; pfi <= pfiMax; pfi++) {
    DescribePixelFormat(pdc, pfi, sizeof(PIXELFORMATDESCRIPTOR), pfd);
    if (pfd->cDepthBits == 32) {
      return pfi;
    }
  }

  pfi = QGLContext::choosePixelFormat(pfd, pdc);
  // qWarning("32-bit depth unavailable: using %d bits", pfd->cDepthBits);

  return pfi;
}
#endif

#if defined(Q_WS_X11)
// Under X11 we can request a visual that supports a 32-bit depth buffer
// directly. But even if the glXChooseVisual() function succeeds we're not
// guaranteed to have obtained a visual that has the buffer depth we
// requested. glXChooseVisual() will return the visual that best meets the
// specification we pass in with the attribs array. If no visual can be
// obtained at all we fall back to the default implementation. 
void *CustomContext::chooseVisual()
{
  GLint attribs[] = {GLX_RGBA, GLX_DEPTH_SIZE, 32, None};
  XVisualInfo *vis = glXChooseVisual(device()->x11Display(),
         device()->x11Screen(), attribs);

  if (vis) {
    GLint depth = 0;
    glXGetConfig(device()->x11Display(), vis, GLX_DEPTH_SIZE, &depth);
    // if (depth != 32)
    //     qWarning("32-bit depth unavailable: using %d bits", depth);

    return vis;
  } 

  return QGLContext::chooseVisual();
}
#endif

#if defined(Q_WS_MAC)
// The aglChoosePixelFormat() function is similar to X11's glXChooseVisual():
// It returns a pixel format that most closely matches our specification. If
// it doesn't return anything we fall back to the default implementation. 
void *CustomContext::chooseMacVisual(GDHandle gdev)
{
  GLint attribs[] = {AGL_ALL_RENDERERS, AGL_RGBA, AGL_DEPTH_SIZE, 32, AGL_NONE};
  AGLPixelFormat fmt = aglChoosePixelFormat(NULL, 0, attribs);

  if (fmt) {
    GLint depth;
    aglDescribePixelFormat(fmt, AGL_DEPTH_SIZE, &depth);

    // if (depth != 32)
    //   qWarning("32-bit depth unavailable: using %d bits", depth);

    return fmt;
  }

  return QGLContext::chooseMacVisual(gdev);
}
#endif

} // namespace ag
*/




//------------------------------------------------------------------------------
// DEFINITION OF STATIC GLVISUALISATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GLVISUALISATION MEMBERS
//------------------------------------------------------------------------------

ag::GLVisualisation::GLVisualisation(DataObject* object,
         const std::string& visualisationName,
         QWidget* parent)

  : SceneView(parent),
    IVisualisation(object, visualisationName)

{
  // Is opengl installed?
  if(!QGLFormat::hasOpenGL()) {
    std::ostringstream stream;
    stream << this->visualisationName() << " uses OpenGL for 3D rendering.\n"
           << "Your window system does not seem to have OpenGL installed.\n"
           << "Please make sure OpenGL is installed properly.";
    throw com::Exception(stream.str());
  }

  // QGLFormat format(DoubleBuffer);
  // CustomContext* context = new CustomContext(format, this);
  // setContext(context);

  // Everything should be ok now.
  assert(isValid());
}



ag::GLVisualisation::~GLVisualisation()
{
}



bool ag::GLVisualisation::close()
{
  return SceneView::close();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



