#include "ag_ScriptEditWindow.h"

// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_ScriptEdit.h"



/*!
  \file
  This file contains the implementation of the ScriptEditWindow class.
*/



//------------------------------------------------------------------------------

/*
namespace ag {

class ScriptEditWindowPrivate
{
public:

  ScriptEditWindowPrivate()
  {
  }

  ~ScriptEditWindowPrivate()
  {
  }

};

} // namespace ag
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SCRIPTEDITWINDOW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SCRIPTEDITWINDOW MEMBERS
//------------------------------------------------------------------------------

ag::ScriptEditWindow::ScriptEditWindow(const qt::AppWindowProperties& props,
         DataObject* object)

  : VisualisationWindow(props, "Script Edit", object, Qt::Window)

{
  createInterface();
}



ag::ScriptEditWindow::~ScriptEditWindow()
{
}



void ag::ScriptEditWindow::createInterface()
{
  VisualisationWindow::createInterface();

  d_edit = new ScriptEdit(&dataObject(), this);

  setCentralWidget(d_edit);
}



void ag::ScriptEditWindow::addAttribute(const DataGuide& dataGuide)
{
  d_edit->setModelScript(dataGuide);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



