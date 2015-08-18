#include "qtd_SelectPalette.h"
#include <cassert>
#include <string>
#include <vector>
#include <QLabel>
#include <QLayout>
#include <QMouseEvent>
#include <QWidget>
#include "com_rawpalette.h"
#include "qt_Const.h"
#include "qtw_PaletteBar.h"



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------

namespace qtd {

class SelectPalettePrivate: public QWidget
{

public:

  std::vector<qtw::PaletteBar *> d_pb;

  SelectPalettePrivate(QWidget *p)
    : QWidget(p)
  {
    resetLayout();
    adjustSize();
  }

  ~SelectPalettePrivate()
  {
    d_pb.erase(d_pb.begin(), d_pb.end());
  }

  void addPalette(qtw::PaletteBar *pb)
  {
    d_pb.push_back(pb);

    resetLayout();
    adjustSize();
  }

  void resetLayout()
  {
    if(layout())
      delete layout();

    QVBoxLayout *vbox = new QVBoxLayout(this);
    vbox->addStretch(1);

    std::vector<qtw::PaletteBar *>::const_iterator it;
    for(it = d_pb.begin(); it != d_pb.end(); it++)
    {
      vbox->addWidget(*it);
      vbox->addStretch(1);
    }
  }

  void adjustSize()
  {
    int w = 0;
    int h = 0;

    std::vector<qtw::PaletteBar *>::iterator it;
    for(it = d_pb.begin(); it != d_pb.end(); it++)
    {
      // w = std::max<int>(w, (*it)->minimumSize().width());
      w = std::max<int>(w, (*it)->sizeHint().width());
      h = std::max<int>(h, (*it)->sizeHint().height());
    }

    for(it = d_pb.begin(); it != d_pb.end(); it++)
      (*it)->setMinimumSize(w, h);

    h += d_pb.size() * h;

    if(!d_pb.empty())
      h += (d_pb.size() - 1) * layout()->spacing();

    h += 2 * layout()->margin();
    w += 2 * layout()->margin();

    setMinimumSize(w, h);
  }

};

} // namespace qtd



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

qtd::SelectPalette::SelectPalette(QWidget *p, const char *n)

  : qt::Dialog(p, n),
    d_cw(0)

{
  d_cw = new SelectPalettePrivate(this);
  setCentralWidget(d_cw);
}



qtd::SelectPalette::~SelectPalette()
{
  clean();
}



void qtd::SelectPalette::clean()
{
  delete d_cw; d_cw = 0;
}



void qtd::SelectPalette::addPalette(const com::RawPalette *p)
{
  assert(p);

  qtw::PaletteBar *pb = new qtw::PaletteBar(p, d_cw);
  connect(pb, SIGNAL(mousePressed(qtw::PaletteBar *, QMouseEvent *)),
          this, SLOT(selectPaletteBar(qtw::PaletteBar *, QMouseEvent *)));
  d_cw->addPalette(pb);
}



void qtd::SelectPalette::selectPaletteBar(qtw::PaletteBar *pb,
                                               QMouseEvent *e)
{
  if(e->button() == Qt::LeftButton)
  {
    std::vector<qtw::PaletteBar *>::iterator it;
    for(it = d_cw->d_pb.begin(); it != d_cw->d_pb.end(); it++)
    {
      if((*it)->outline())
      {
        (*it)->setOutline(false);
        (*it)->repaint();
      }
    }

    pb->setOutline(true);
    pb->repaint();
  }
}



const com::RawPalette *qtd::SelectPalette::selected() const
{
  const com::RawPalette *p = 0;

  std::vector<qtw::PaletteBar *>::const_iterator it;
  for(it = d_cw->d_pb.begin(); it != d_cw->d_pb.end(); it++)
  {
    if((*it)->outline())
    {
      p = (*it)->palette();
      break;
    }
  }

  return p;
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


