#include "qt_AnimationProgBar.h"



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------


qt::AnimationProgBar::AnimationProgBar(QWidget* p)

  : QProgressBar(p),
    d_firstStep(0)

{
}



qt::AnimationProgBar::~AnimationProgBar()
{
}



bool qt::AnimationProgBar::setIndicator(QString& label, int progress,
                   int /* totalSteps */)
{
  if(progress < 0) {
    d_label.setNum(d_firstStep);
  }
  else {
    d_label.setNum(d_firstStep + progress - 1);
  }

  bool status;

  if(d_label == label) {
    status = false;
  }
  else {
    label = d_label;
    status = true;
  }

  return status;
}



void qt::AnimationProgBar::setFirstStep(size_t t)
{
  d_firstStep = t;
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


