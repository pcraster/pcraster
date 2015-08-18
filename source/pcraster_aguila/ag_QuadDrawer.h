#ifndef INCLUDED_AG_QUADDRAWER
#define INCLUDED_AG_QUADDRAWER



#include <QtOpenGL>
#include "dal_Raster.h"
#include "com_classifier.h"
#include "com_rawpalette.h"
#include "ag_Raster.h"



class QImage;
namespace ag {
  class BooleanDrawProps;
  class LddDrawProps;
  class NominalDrawProps;
  class OrdinalDrawProps;
  class RangeDrawProps;
}



namespace ag {



//! This is the abstract base class of all value scale specific quad drawers.
/*!
  A quad drawer is an object which can be used to draw a quad. A quad is a
  square piece of a raster.

  \todo This code is temporarily inefficient, update.
*/
class QuadDrawer
{

private:

  size_t           d_quadLength;

  //! Assignment operator. NOT IMPLEMENTED.
  QuadDrawer&      operator=           (const QuadDrawer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   QuadDrawer          (const QuadDrawer&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   QuadDrawer          (size_t l);

  virtual          ~QuadDrawer         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setQuadLength       (size_t l);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           quadLength          () const;

  virtual bool     willFill            (size_t r,
                                        size_t c) const = 0;

  virtual void     draw                (size_t r,
                                        size_t c) const = 0;

};



//------------------------------------------------------------------------------

//! Specialised QuadDrawer for drawing class quads.
/*!
  Class raster layers can be drawn with this quad drawer.
*/
//       1         2         3         4         5         6         7         8
class ClassQuadDrawer: public QuadDrawer
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ClassQuadDrawer& operator=           (const ClassQuadDrawer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ClassQuadDrawer     (const ClassQuadDrawer&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ClassQuadDrawer     (size_t l);

  virtual         ~ClassQuadDrawer     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------

//! Specialised QuadDrawer for drawing range quads.
/*!
  Range raster layers can be drawn with this quad drawer.
*/
//       1         2         3         4         5         6         7         8
class RangeQuadDrawer: public QuadDrawer
{

private:

  const RangeDrawProps& d_props;

  //! Assignment operator. NOT IMPLEMENTED.
  RangeQuadDrawer& operator=           (const RangeQuadDrawer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   RangeQuadDrawer     (const RangeQuadDrawer&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RangeQuadDrawer     (const RangeDrawProps& dp,
                                        size_t l);

  virtual         ~RangeQuadDrawer     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const RangeDrawProps& drawProps      () const;

};



inline const RangeDrawProps& RangeQuadDrawer::drawProps() const
{
  return d_props;
}


//------------------------------------------------------------------------------

//! Specialised QuadDrawer for drawing boolean quads.
/*!
  Boolean raster layers can be drawn with this quad drawer.
*/
//       1         2         3         4         5         6         7         8
class BooleanQuadDrawer: public ClassQuadDrawer
{

private:

  Raster const&    d_raster;

  const BooleanDrawProps& d_dp;

  //! Assignment operator. NOT IMPLEMENTED.
  BooleanQuadDrawer&operator=          (const BooleanQuadDrawer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   BooleanQuadDrawer   (const BooleanQuadDrawer&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BooleanQuadDrawer   (Raster const& raster,
                                        const BooleanDrawProps& dp,
                                        size_t l);

  /* virtual */    ~BooleanQuadDrawer  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             willFill            (size_t r,
                                        size_t c) const;

  void             draw                (size_t r,
                                        size_t c) const;

};



inline bool BooleanQuadDrawer::willFill(size_t r, size_t c) const
{
  return !pcr::isMV(d_raster.cell<UINT1>(r, c));
}

//------------------------------------------------------------------------------

//! Specialised QuadDrawer for drawing nominal quads.
/*!
  Nominal raster layers can be drawn with this quad drawer.
*/
//       1         2         3         4         5         6         7         8
class NominalQuadDrawer: public ClassQuadDrawer
{

private:

  Raster const& d_raster;

  const NominalDrawProps& d_dp;

  //! Assignment operator. NOT IMPLEMENTED.
  NominalQuadDrawer&operator=          (const NominalQuadDrawer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   NominalQuadDrawer   (const NominalQuadDrawer&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   NominalQuadDrawer   (Raster const& raster,
                                        const NominalDrawProps& dp,
                                        size_t l);

  /* virtual */    ~NominalQuadDrawer  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             willFill            (size_t r,
                                        size_t c) const;

  void             draw                (size_t r,
                                        size_t c) const;

};



inline bool NominalQuadDrawer::willFill(size_t r, size_t c) const
{
  return !pcr::isMV(d_raster.cell<INT4>(r, c));
}

//------------------------------------------------------------------------------

//! Specialised QuadDrawer for drawing ordinal quads.
/*!
  Ordinal raster layers can be drawn with this quad drawer.
*/
//       1         2         3         4         5         6         7         8
class OrdinalQuadDrawer: public ClassQuadDrawer
{

private:

  Raster const& d_raster;

  const OrdinalDrawProps& d_dp;

  //! Assignment operator. NOT IMPLEMENTED.
  OrdinalQuadDrawer& operator=         (const OrdinalQuadDrawer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   OrdinalQuadDrawer   (const OrdinalQuadDrawer&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   OrdinalQuadDrawer   (Raster const& raster,
                                        const OrdinalDrawProps& dp,
                                        size_t l);

  /* virtual */    ~OrdinalQuadDrawer  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             willFill            (size_t r,
                                        size_t c) const;

  void             draw                (size_t r,
                                        size_t c) const;

};



inline bool OrdinalQuadDrawer::willFill(size_t r, size_t c) const
{
  return !pcr::isMV(d_raster.cell<INT4>(r, c));
}

//------------------------------------------------------------------------------

//! Specialised QuadDrawer for drawing scalar quads.
/*!
  Scalar raster layers can be drawn with this quad drawer.
*/
class ScalarQuadDrawer: public RangeQuadDrawer
{

private:

  Raster const& d_raster;

  //! Assignment operator. NOT IMPLEMENTED.
  ScalarQuadDrawer&operator=           (const ScalarQuadDrawer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ScalarQuadDrawer    (const ScalarQuadDrawer&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ScalarQuadDrawer    (Raster const& raster,
                                        const RangeDrawProps& dp,
                                        size_t l);

  /* virtual */    ~ScalarQuadDrawer   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             willFill            (size_t r,
                                        size_t c) const;

  void             draw                (size_t r,
                                        size_t c) const;

};



inline bool ScalarQuadDrawer::willFill(size_t r, size_t c) const
{
  return !pcr::isMV(d_raster.cell<REAL4>(r, c));
}



//------------------------------------------------------------------------------

//! Specialised QuadDrawer for drawing directional quads.
/*!
  Directional raster layers can be drawn with this quad drawer.
*/
class DirectionalQuadDrawer: public RangeQuadDrawer
{

private:

  Raster const& d_raster;

  //! Assignment operator. NOT IMPLEMENTED.
  DirectionalQuadDrawer&operator=      (const DirectionalQuadDrawer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   DirectionalQuadDrawer(const DirectionalQuadDrawer&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DirectionalQuadDrawer(Raster const& raster,
                                        const RangeDrawProps& dp,
                                        size_t l);

  /* virtual */    ~DirectionalQuadDrawer();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             willFill            (size_t r,
                                        size_t c) const;

  void             draw                (size_t r,
                                        size_t c) const;

};



inline bool DirectionalQuadDrawer::willFill(size_t r, size_t c) const
{
  return !pcr::isMV(d_raster.cell<REAL4>(r, c));
}

//------------------------------------------------------------------------------

//! Specialised QuadDrawer for drawing ldd quads.
/*!
  Ldd raster layers can be drawn with this quad drawer.
*/
//       1         2         3         4         5         6         7         8
class LddQuadDrawer: public ClassQuadDrawer
{

private:

  Raster const& d_raster;

  const LddDrawProps& d_dp;

  dal::Raster      d_gdd;

  //! Assignment operator. NOT IMPLEMENTED.
  LddQuadDrawer&   operator=           (const LddQuadDrawer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   LddQuadDrawer       (const LddQuadDrawer&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LddQuadDrawer       (Raster const& raster,
                                        const LddDrawProps& dp,
                                        size_t l);

  /* virtual */    ~LddQuadDrawer      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             willFill            (size_t r,
                                        size_t c) const;

  void             draw                (size_t r,
                                        size_t c) const;

};



inline bool LddQuadDrawer::willFill(size_t /* r */, size_t /* c */) const
{
  return false;
}

} // namespace ag

#endif
