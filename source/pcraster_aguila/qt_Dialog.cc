#include "qt_Dialog.h"
#include <cassert>
#include <iostream>
#include <QDialog>
#include <QLayout>
#include <QPoint>
#include <QPushButton>
#include <QSize>
#include <QWidget>
#include "qt_Const.h"



namespace qt {

class DialogPrivate
{
  public:

    QBoxLayout *   top;                // Toplevel layout.
    QWidget *      cw;                 // Central widget.
    QPushButton *  ok;                 // Ok button.
    QPushButton *  cancel;             // Cancel button.

    DialogPrivate()
      : top(0), cw(0), ok(0), cancel(0)
    {
    }

    ~DialogPrivate()
    {
    }
};



//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------



Dialog::Dialog(QWidget *p, bool m, Qt::WindowFlags f)

  : QDialog(p, f), d_data(0)

{
  setModal(m);

  try
  {
    d_data         = new DialogPrivate();
    d_data->ok     = new QPushButton(this);
    d_data->cancel = new QPushButton(this);

    d_data->ok->setText("Ok");
    // d_data->ok->setAutoDefault(true);
    d_data->ok->setDefault(true);
    d_data->cancel->setText("Cancel");

    QSize s1 = d_data->cancel->sizeHint();
    QSize s2 = d_data->ok->sizeHint();
    s1 = QSize(std::max(s1.width(), s2.width()),
               std::max(s1.height(), s2.height()));
    d_data->cancel->setFixedSize(s1);
    d_data->ok->setFixedSize(s1);

    connect(d_data->ok, SIGNAL(clicked()), SLOT(acceptSettings()));
    connect(d_data->cancel, SIGNAL(clicked()), SLOT(reject()));

    resetLayout();
  }
  catch(...)
  {
    clean();
    throw;
  }
}



Dialog::~Dialog()
{
  clean();
}



void Dialog::clean()
{
  delete d_data, d_data = 0;
}



void Dialog::resetLayout()
{
  assert(d_data);

  delete d_data->top;

  d_data->top = new QVBoxLayout(this);

  if(centralWidget())
    d_data->top->addWidget(centralWidget(), 1);
  else
    d_data->top->addStretch(1);

  QHBoxLayout *hbox = new QHBoxLayout();
  d_data->top->addLayout(hbox);
  hbox->addStretch();
  hbox->addWidget(d_data->ok);
  hbox->addSpacing(SPACING);
  hbox->addWidget(d_data->cancel);
  hbox->addStretch();

  d_data->top->setSizeConstraint(QLayout::SetFixedSize);
  d_data->top->activate();
}



void Dialog::setCentralWidget(QWidget *w)
{
  assert(d_data);
  assert(w);

  // dumps on x
  // w->recreate(this, 0, QPoint(0, 0), false);
  d_data->cw = w;
  resetLayout();
}



/*
void Dialog::adjustSize()
{
  assert(d_data);

  int w = 0;
  int h = 0;
  
  w = d_data->ok->width() + d_data->ok->width() + SPACING;
  h = MAX(d_data->ok->height(), d_data->cancel->height());

  if(d_data->cw)
  {
    w = MAX(w, d_data->cw->minimumSize().width());
    h += d_data->cw->minimumSize().height() + SPACING;
  }

  w += 2 * SPACING;
  h += 2 * SPACING;

  resize(w, h);
}
*/



void Dialog::acceptSettings()
{
  QDialog::accept();
}



QWidget *Dialog::centralWidget() const
{
  assert(d_data);

  return d_data->cw;
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------

} // namespace qt
