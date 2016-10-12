#include "ag_AnimationControl.h"

// Library headers.
#include <QtWidgets>

// PCRaster library headers.
#include "dal_DataSpace.h"
#include "dal_Utils.h"
#include "qt_Animation.h"
#include "qt_AnimationProgBar.h"
#include "qt_Const.h"

// Module headers.
#include "ag_DataObject.h"
#include "ag_VisEngine.h"



// Icons.
#include "icons/begin.xpm"
#include "icons/end.xpm"
#include "icons/fastforward.xpm"
#include "icons/play.xpm"
#include "icons/rewind.xpm"
#include "icons/pause.xpm"



/*!
  \file
  This file contains the implementation of the AnimationDialog class.
*/



//------------------------------------------------------------------------------

namespace ag {

class AnimationControlPrivate
{
public:

  QPushButton*     d_start;
  QPushButton*     d_pause;
  QPushButton*     d_begin;
  QPushButton*     d_backwards;
  QPushButton*     d_forewards;
  QPushButton*     d_end;
  QCheckBox*       d_loop;
  QLineEdit*       d_stepEdit;
  QIntValidator*   d_stepValidator;
  QSpinBox*        d_interval;

  qt::AnimationProgBar* d_progressBar;
  VisEngine        d_engine;

  AnimationControlPrivate()
    : d_start(0), d_pause(0), d_begin(0), d_backwards(0), d_forewards(0),
      d_end(0),
      d_loop(0),
      d_stepEdit(0), d_stepValidator(0),
      d_interval(0),
      d_progressBar(0),
      d_engine()
  {
  }

  ~AnimationControlPrivate()
  {
  }

};



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ANIMATIONDIALOG MEMBERS
//------------------------------------------------------------------------------

AnimationControl* AnimationControl::instance(DataObject* object)
{
  AnimationControl* dialog =
         VisualisationDialog<DataObject*, AnimationControl>::instance(
         object, object);

  if(dialog) {
    dialog->raise();
  }
  else {
    // Create and add instance.
    dialog = new AnimationControl(object);
    addInstance(object, object, dialog);
  }

  assert(dialog);

  return dialog;
}





//------------------------------------------------------------------------------
// DEFINITION OF ANIMATIONDIALOG MEMBERS
//------------------------------------------------------------------------------

AnimationControl::AnimationControl(DataObject* object)

  : VisualisationDialog<DataObject*, AnimationControl>(
         object, "Animation Dialog"), // , 0, false, Qt::WindowStaysOnTopHint),
    d_data(new AnimationControlPrivate())

{
  createInterface();
  configureInterface();
}



AnimationControl::~AnimationControl()
{
  delete d_data;
}



void AnimationControl::createInterface()
{
  QBoxLayout *vbox, *hbox, *top;
  QGroupBox *gb;

  QWidget* widget = new QWidget(this);
  top = new QVBoxLayout(widget);

  //----------------------------------------------------------------------------
  gb = new QGroupBox(widget);
  top->addWidget(gb);

  d_data->d_progressBar = new qt::AnimationProgBar(gb);

  QPixmap startIcon = QPixmap((const char **)play_xpm);
  QPixmap pauseIcon = QPixmap((const char **)pause_xpm);
  QPixmap forewardsIcon = QPixmap((const char **)fastforward_xpm);
  QPixmap backwardsIcon = QPixmap((const char **)rewind_xpm);
  QPixmap beginIcon = QPixmap((const char **)begin_xpm);
  QPixmap endIcon = QPixmap((const char **)end_xpm);

  d_data->d_start = new QPushButton(gb);
  d_data->d_start->setToolTip("Start animation");
  // Turn auto default off.
  // Otherwise start is pressed whenever return is pressed in the edit.
  // If a user selects a time step en presses enter he doesn't want an
  // animation to start.
  d_data->d_start->setAutoDefault(false);
  d_data->d_start->setIcon(startIcon);
  connect(d_data->d_start, SIGNAL(clicked()), SLOT(start()));

  d_data->d_pause = new QPushButton(gb);
  d_data->d_pause->setToolTip("Pause animation");
  d_data->d_pause->setIcon(pauseIcon);
  connect(d_data->d_pause, SIGNAL(clicked()), SLOT(pause()));

  d_data->d_begin = new QPushButton(gb);
  d_data->d_begin->setToolTip("Go to the first time step");
  d_data->d_begin->setIcon(beginIcon);
  connect(d_data->d_begin, SIGNAL(clicked()), SLOT(toBegin()));

  d_data->d_backwards = new QPushButton(gb);
  d_data->d_backwards->setToolTip("Go one step backwards");
  d_data->d_backwards->setIcon(backwardsIcon);
  connect(d_data->d_backwards, SIGNAL(clicked()), SLOT(backwards()));

  d_data->d_forewards = new QPushButton(gb);
  d_data->d_forewards->setToolTip("Go one step forewards");
  d_data->d_forewards->setIcon(forewardsIcon);
  connect(d_data->d_forewards, SIGNAL(clicked()), SLOT(forewards()));

  d_data->d_end = new QPushButton(gb);
  d_data->d_end->setToolTip("Go to last time step");
  d_data->d_end->setIcon(endIcon);
  connect(d_data->d_end, SIGNAL(clicked()), SLOT(toEnd()));

  vbox = new QVBoxLayout(gb);
  vbox->addWidget(d_data->d_progressBar);

  hbox = new QHBoxLayout();
  vbox->addLayout(hbox);

  hbox->setSpacing(0);
  hbox->addStretch(1);
  hbox->addWidget(d_data->d_start);
  hbox->addWidget(d_data->d_pause);
  hbox->addSpacing(qt::SPACING);
  hbox->addWidget(d_data->d_begin);
  hbox->addWidget(d_data->d_backwards);
  hbox->addWidget(d_data->d_forewards);
  hbox->addWidget(d_data->d_end);
  hbox->addStretch(1);

  connect(&dataObject().animationManager(),
         SIGNAL(stopped()), SLOT(updateInterface()));

  //----------------------------------------------------------------------------
  d_data->d_loop = new QCheckBox("Loop animation", widget);
  connect(d_data->d_loop, SIGNAL(toggled(bool)), this, SLOT(loop(bool)));
  d_data->d_loop->setToolTip("Select looping if you want continous animation");
  top->addWidget(d_data->d_loop);

  QLabel* label = 0;

  d_data->d_stepEdit = new QLineEdit(widget);
  d_data->d_stepEdit->setToolTip("Select time step to show");
  connect(d_data->d_stepEdit, SIGNAL(returnPressed()),
         this, SLOT(timeStepChanged()));
  connect(d_data->d_stepEdit, SIGNAL(editingFinished()),
         this, SLOT(timeStepChanged()));
  d_data->d_stepValidator = new QIntValidator(d_data->d_stepEdit);
  d_data->d_stepEdit->setValidator(d_data->d_stepValidator);

  label = new QLabel("Time step:", widget);

  hbox = new QHBoxLayout();
  top->addLayout(hbox);

  hbox->addWidget(label);
  hbox->addStretch(1);
  hbox->addWidget(d_data->d_stepEdit);

  d_data->d_interval = new QSpinBox(widget);
  d_data->d_interval->setMinimum(0);
  d_data->d_interval->setMaximum(1000);
  d_data->d_interval->setSingleStep(100);

  d_data->d_interval->setSuffix("ms");
  connect(d_data->d_interval, SIGNAL(valueChanged(int)),
         this, SLOT(intervalChanged(int)));
  d_data->d_interval->setToolTip("Decrease interval for faster animation");
  label = new QLabel("Animation interval:", widget);

  hbox = new QHBoxLayout();
  top->addLayout(hbox);

  hbox->addWidget(label);
  hbox->addStretch(1);
  hbox->addWidget(d_data->d_interval);
  // hbox->addStretch(1);

  QPushButton* close = new QPushButton("Close", widget);
  close->setFixedSize(qt::BUTTONWIDTH, qt::BUTTONHEIGHT);
  connect(close, SIGNAL(clicked()), SLOT(close()));

  hbox = new QHBoxLayout();
  top->addLayout(hbox);

  hbox->addStretch(1);
  hbox->addWidget(close);
  hbox->addStretch(1);

  top->addStretch(1);

  QBoxLayout* box = new QVBoxLayout(this);
  box->setMargin(0);
  box->addWidget(widget);

  setMaximumSize(sizeHint());
}



void AnimationControl::configureInterface()
{
  d_data->d_loop->setChecked(dataObject().animationManager().loop());
  d_data->d_interval->setValue(static_cast<int>(
         dataObject().animationManager().interval()));
}



void AnimationControl::updateInterface()
{
  qt::Animation& animation = dataObject().animationManager();

  d_data->d_progressBar->setFirstStep(animation.firstStep());

  if(animation.isRunning()) {
    d_data->d_start->setEnabled(false);
    d_data->d_pause->setEnabled(true);

    d_data->d_begin->setEnabled(false);
    d_data->d_backwards->setEnabled(false);
    d_data->d_forewards->setEnabled(false);
    d_data->d_end->setEnabled(false);

    d_data->d_stepEdit->setReadOnly(true);
  }
  else {
    d_data->d_pause->setEnabled(false);

    if(animation.currentStep() > animation.firstStep()) {
      d_data->d_begin->setEnabled(true);
      d_data->d_backwards->setEnabled(true);
    }
    else {
      d_data->d_begin->setEnabled(false);
      d_data->d_backwards->setEnabled(false);
    }

    if(animation.currentStep() < animation.lastStep()) {
      d_data->d_start->setEnabled(true);
      d_data->d_forewards->setEnabled(true);
      d_data->d_end->setEnabled(true);
    }
    else {
      d_data->d_start->setEnabled(false);
      d_data->d_forewards->setEnabled(false);
      d_data->d_end->setEnabled(false);
    }

    d_data->d_stepEdit->setReadOnly(false);
  }


  int progress = static_cast<int>(animation.currentStep() -
         animation.firstStep() + 1);
  int total = static_cast<int>(animation.timeSpan() + 1);

  d_data->d_progressBar->setMaximum(total);
  d_data->d_progressBar->setValue(progress);

  assert(d_data->d_stepValidator);
  d_data->d_stepValidator->setRange(animation.firstStep(),
         animation.lastStep());
  d_data->d_stepEdit->setText(QString::number(animation.currentStep()));
}



void AnimationControl::start()
{
  dataObject().animationManager().start();
  updateInterface();
}



void AnimationControl::pause()
{
  dataObject().animationManager().pause();
  updateInterface();
}



void AnimationControl::rescan()
{
  // VisualisationWindow::rescan();

  d_data->d_engine.rescan(dataObject());
}



void AnimationControl::process()
{
}



void AnimationControl::visualise()
{
  if(d_data->d_engine.timeChanged()) {
    updateInterface();
  }

  d_data->d_engine.finishedScanning(dataObject());
}



void AnimationControl::toBegin()
{
  dataObject().setTimeStep(dataObject().animationManager().firstStep());
}



void AnimationControl::toEnd()
{
  dataObject().setTimeStep(dataObject().animationManager().lastStep());
}



void AnimationControl::backwards()
{
  dataObject().setTimeStep(dataObject().animationManager().prevTimeStep());
}



void AnimationControl::forewards()
{
  dataObject().setTimeStep(dataObject().animationManager().nextTimeStep());
}



void AnimationControl::loop(bool setting)
{
  dataObject().animationManager().setLoop(setting);
}



void AnimationControl::intervalChanged(int interval)
{
  assert(interval >= 0);

  dataObject().animationManager().setInterval(static_cast<size_t>(interval));
}



void AnimationControl::timeStepChanged()
{
  assert(d_data->d_stepEdit);
  assert(dataObject().dataSpace().hasTime());
  assert(dataObject().dataSpace().isValid(dataObject().dataSpaceAddress()));
  assert(dataObject().firstTimeStep() >= 0);
  assert(dataObject().lastTimeStep() >= 0);

  if(!d_data->d_stepEdit->hasAcceptableInput()) {
    d_data->d_stepEdit->setText(QString::number(
         dal::timeStep<size_t>(
         dataObject().dataSpace(), dataObject().dataSpaceAddress())));
  }
  else {
    qt::Animation& animation = dataObject().animationManager();
    bool ok(false);
    size_t timeStep = d_data->d_stepEdit->text().toUInt(&ok);
    assert(ok);
    assert(timeStep >= animation.firstStep());
    assert(timeStep <= animation.lastStep());
    timeStep = animation.closestStep(timeStep);

    if(animation.currentStep() != timeStep) {
      dataObject().setTimeStep(timeStep);
    }
  }
}

} // namespace ag

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



