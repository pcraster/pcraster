#include "ag_ScriptEdit.h"

// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_DataGuide.h"
#include "ag_DataObject.h"
#include "ag_ScriptSyntaxHighlighter.h"



/*!
  \file
  This file contains the implementation of the ScriptEdit class.
*/



//------------------------------------------------------------------------------

/*
namespace ag {

class ScriptEditPrivate
{
public:

  ScriptEditPrivate()
  {
  }

  ~ScriptEditPrivate()
  {
  }

};

} // namespace ag
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SCRIPTEDIT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SCRIPTEDIT MEMBERS
//------------------------------------------------------------------------------

ag::ScriptEdit::ScriptEdit(DataObject* object,
         QWidget* parent)

  : QTextEdit(parent),
    d_dataObject(object)
{
  // setPaper(QBrush(Qt::lightGray));
  // Doesn't compile anymore after port to Qt4.
  // new ScriptSyntaxHighlighter(this);
}



ag::ScriptEdit::~ScriptEdit()
{
}



void ag::ScriptEdit::setModelScript(const DataGuide&
#ifdef DEBUG_DEVELOP
  guide
#endif
  )
{
  // Check guide.
  // testDataGuide(dataGuide);
  assert(guide.type() == geo::MODELSCRIPT);

  // Get data.
  // const QString& script = d_dataObject->modelScripts().data(guide);

  // Push data into editor.
  // setText(script); -> setPlainText(script);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



