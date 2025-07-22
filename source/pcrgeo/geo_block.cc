#include "stddefx.h"
#include "geo_block.h"
#include "com_file.h"
#include "geo_util.h"



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

/*
geo::Block::Block()

  : Raster<VoxelStack>()

{
}
*/



geo::Block::Block(size_t nr, size_t nc, REAL8 cellSize,
                  REAL8 left, REAL8 top, Projection proj)

  : Raster<VoxelStack>(nr, nc, cellSize, left, top, proj)

{
}



geo::Block::~Block()
{
}



void geo::Block::clean()
{
}



void geo::Block::addLayer(size_t n, INT4 s, REAL8 t)
{
  for(auto & it : *this)
    it.add(n, s, t);
}



REAL8 geo::Block::thickness() const
{
  REAL8 t = 0;

  for(const auto & it : *this)
    t = MAX(t, it.thickness());

  return t;
}



REAL8 geo::Block::low() const
{
  REAL8 b = 0;

  for(const auto & it : *this)
    b = MIN(b, it.bottom());

  return b;
}



REAL8 geo::Block::high() const
{
  REAL8 h = 0;

  for(const auto & it : *this)
    h = MAX(h, it.top());

  return h;
}



size_t geo::Block::nrVoxels() const
{
  size_t n = 0;

  for(const auto & it : *this)
    n += it.nrVoxels();

  return n;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! DISABLED: Input operator for reading Block objects from an input stream.
std::istream &operator>>(std::istream &s, geo::Block &/*b*/)
{
  PRECOND(false);

/*
  char ch;
  size_t cellSize, nr, nc;
  REAL8 left, top, bottom, thickness, origThickness;
  std::string line;
  int sedType;
  geo::Projection proj;

  // Read the number of rows.
  s >> nr;

  // Read the number of columns.
  s >> nc;

  // Read the cellsize.
  s >> cellSize;

  // Read the top left coordinate.
  s >> left >> top;

  while(s.get() != '\n')
    ;

  // Read the projection.
  std::getline(s, line);
  proj = geo::strToProj(line);

  // Configure the block.
  b.configure(nr, nc, cellSize, left, top, proj);

  // Read each stack.
  for(size_t r = 0; r < b.nrRows(); r++)
  {
    for(size_t c = 0; c < b.nrCols(); c++)
    {
      geo::VoxelStack &stack = b.cell(r, c);
      s >> bottom;
      stack.setBottom(bottom);
      while(1)
      {
        com::eat(s, '\t');

        // Check if there's another voxel to read.
        ch = s.get();
        if(ch == '\n')
        {
          break;
        }
        else
        {
          s.putback(ch);
          s >> ch >> sedType >> ch;
          com::eat(s, ' ');
          s >> thickness;
          com::eat(s, ' ');
          s >> ch >> origThickness >> ch >> ch;
          com::eat(s, ' ');
          geo::Voxel voxel(static_cast<INT4>(sedType), thickness,
                           origThickness);
          stack.add(1, voxel);
        }
      }
    }
  }
*/

  return s;
}



//! DISABLED: Output operator for writing Block objects to an output stream.
std::ostream &operator<<(std::ostream &s, const geo::Block & /*b*/)
{
  PRECOND(false);

/*
  // Write the number of rows.
  s << b.nrRows() << '\n';

  // Write the number of columns.
  s << b.nrCols() << '\n';

  // Write the cellsize.
  s << b.cellSize() << '\n';

  // Write the top left coordinate.
  s << b.left() << ' ' << b.top() << '\n';

  // Write the projection.
  s << geo::projToStr(b.projection()) << '\n';

  // Write each column.
  geo::Block::const_iterator blockit;
  geo::VoxelStack::const_iterator stackit;
  for(blockit = b.begin(); blockit != b.end(); blockit++)
  {
    s << (*blockit).bottom() << '\t';

    if((*blockit).nrVoxels() > 0)
    {
      for(stackit = (*blockit).begin(); stackit != (*blockit).end(); stackit++)
      {
        s << '[' <<  static_cast<int>((*stackit).sedType()) << ": "
          << (*stackit).thickness() << " ("
          << (*stackit).origThickness() << ")] \t";
      }
      s << '\n';
    }
  }
*/

  return s;
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


