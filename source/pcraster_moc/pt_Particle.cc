#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PT_PARTICLE
#include "pt_Particle.h"
#define INCLUDED_PT_PARTICLE
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the Particle class.
*/



//------------------------------------------------------------------------------

/*
namespace pt {

class ParticlePrivate
{
public:

  ParticlePrivate()
  {
  }

  ~ParticlePrivate()
  {
  }

};

} // namespace pt
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PARTICLE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PARTICLE MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     x x-coordinate
  \param     y y-coordinate
  \param     concentration Initial concentration
*/
pt::Particle::Particle(size_t birthRow, size_t birthCol, double x, double y,
         double concentration)

  : geo::Point<double, 2>(),
    d_birthRow(birthRow), d_birthCol(birthCol),
    d_concentration(concentration)

{
  operator[](0) = x;
  operator[](1) = y;
}



//! Destructor.
/*!
*/
pt::Particle::~Particle()
{
}



void pt::Particle::setBirthCell(size_t row, size_t col)
{
  d_birthRow = row;
  d_birthCol = col;
}



//! Sets the concentration to \a concentration.
/*!
  \param     concentration New concentration.
  \sa        concentration()
*/
void pt::Particle::setConcentration(double concentration)
{
  d_concentration = concentration;
}



size_t pt::Particle::birthRow() const
{
  return d_birthRow;
}



size_t pt::Particle::birthCol() const
{
  return d_birthCol;
}



bool pt::Particle::born(size_t row, size_t col) const
{
  return d_birthRow == row && d_birthCol == col;
}



bool pt::Particle::born(const geo::CellLoc& loc) const
{
  return d_birthRow == loc.row() && d_birthCol == loc.col();
}



//! Returns the concentration.
/*!
  \return    Concentration.
  \sa        setConcentration(double)
*/
double pt::Particle::concentration() const
{
  return d_concentration;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



