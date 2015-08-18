#include "ag_Scene.h"

// Qt
#include <QEvent>

// Pcr
#include "com_const.h"


// App
#include "ag_SceneView.h"



/*!
  \file
  This file contains the implementation of the Scene class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SCENE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SCENE MEMBERS
//------------------------------------------------------------------------------

ag::Scene::Scene()
{
}



ag::Scene::~Scene()
{
}



bool ag::Scene::keyPressHandled(QKeyEvent* e)
{
  double d;                  // A distance.

  if(e->modifiers() & Qt::ShiftModifier)
  {
    switch(e->key())
    {
      case Qt::Key_Up:
      {
        // Move camera to the front.
        d = -sceneView().step();
        sceneView().moveHead(0.0, 0.0, d);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_Down:
      {
        // Move camera to the back.
        d = sceneView().step();
        sceneView().moveHead(0.0, 0.0, d);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_Left:
      {
        // Move camera to the left.
        d = -sceneView().step();
        sceneView().moveHead(d, 0.0, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_Right:
      {
        // Move camera to the right.
        d = sceneView().step();
        sceneView().moveHead(d, 0.0, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_H:
      {
        // Move camera to the left.
        d = -sceneView().step();
        sceneView().moveHead(d, 0.0, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_L:
      {
        // Move camera to the right.
        d = sceneView().step();
        sceneView().moveHead(d, 0.0, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_K:
      {
        // Move camera to the top.
        d = sceneView().step();
        sceneView().moveHead(0.0, d, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_J:
      {
        // Move camera to the bottom.
        d = -sceneView().step();
        sceneView().moveHead(0.0, d, 0.0);
        sceneView().updateGL();
        return true;
      }
    }
  }
  else if(e->modifiers() & Qt::ControlModifier)
  {
    switch(e->key())
    {
      case Qt::Key_K:
      {
        // Move camera to the front.
        d = -sceneView().step();
        sceneView().moveHead(0.0, 0.0, d);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_J:
      {
        // Move camera to the back.
        d = sceneView().step();
        sceneView().moveHead(0.0, 0.0, d);
        sceneView().updateGL();
        return true;
      }
    }
  }
  else
  {
    switch(e->key())
    {
      case Qt::Key_0:
      {
        // Install user camera.
        sceneView().installCamera(SceneView::USER);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_2:
      {
        // Install static front camera.
        sceneView().installCamera(SceneView::FRONT);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_4:
      {
        // Install static left camera.
        sceneView().installCamera(SceneView::LEFT);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_5:
      {
        // Install static top camera.
        sceneView().installCamera(SceneView::TOP);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_6:
      {
        // Install static right camera.
        sceneView().installCamera(SceneView::RIGHT);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_8:
      {
        // Install static back camera.
        sceneView().installCamera(SceneView::BACK);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_Left:
      {
        // Rotate scene around the z-axes.
        sceneView().rotateScene(0.0, -2.0 * com::DEG2RAD, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_Right:
      {
        // Rotate scene around the z-axes.
        sceneView().rotateScene(0.0, 2.0 * com::DEG2RAD, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_Up:
      {
        // Rotate scene around the x-axes.
        sceneView().rotateScene(-2.0 * com::DEG2RAD, 0.0, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_Down:
      {
        // Rotate scene around the x-axes.
        sceneView().rotateScene(2.0 * com::DEG2RAD, 0.0, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_H:
      {
        // Aim camera left.
        sceneView().rotateHead(0.0, -2.0 * com::DEG2RAD, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_J:
      {
        // Aim camera down. 
        sceneView().rotateHead(2.0 * com::DEG2RAD, 0.0, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_K:
      {
        // Aim camera up.
        sceneView().rotateHead(-2.0 * com::DEG2RAD, 0.0, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_L:
      {
        // Aim camera right.
        sceneView().rotateHead(0.0, 2.0 * com::DEG2RAD, 0.0);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_M:
      {
        // Roll camera
        sceneView().rotateHead(0.0, 0.0, -2.0 * com::DEG2RAD);
        sceneView().updateGL();
        return true;
      }

      case Qt::Key_N:
      {
        // Roll camera
        sceneView().rotateHead(0.0, 0.0, 2.0 * com::DEG2RAD);
        sceneView().updateGL();
        return true;
      }
    }
  }

  return false;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



