#ifndef INCLUDED_AG_RANGELEGENDBODY
#define INCLUDED_AG_RANGELEGENDBODY



#include <boost/noncopyable.hpp>
#include "ag_LegendBody.h"



namespace ag {
  class DataGuide;
  class DataObject;
  class RangeDrawProps;
}



namespace ag {



//! The RangeLegendBody class is for body widgets for RangeLegend s.
/*!
*/
class RangeLegendBody: private boost::noncopyable,
                       public LegendBody
{

private:

  //! Nr of objects created.
  static size_t    _nrCreated;

  //! Key box offset from upper left corner of body.
  static QSize     _keyBoxOffset;

  //! Maximum key box height.
  static int       _maxKeyBoxHeight;

  RangeDrawProps const& _drawProperties;

  RangeDrawProps const& drawProperties () const;

  int              keyBoxHeight        () const;

  int              width               () const;

  int              height              () const;

  int              maxLengthLabel      () const;

  void             paintLineLegend     ();

  void             paintKeyLegend      ();

  void             paintVectorLegend   ();

  void             paintLabels         (QPainter& painter,
                                        QTransform const& map,
                                        std::vector<double> const& borders,
                                        bool drawTics=true) const;

  QString          label               (size_t id) const;

protected:

  void             paintEvent          (QPaintEvent* event) override;

  void             showEvent           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RangeLegendBody     (DataObject const& object,
                                        DataGuide const& guide,
                                        ViewerType type,
                                        QWidget* parent = nullptr);

  /* virtual */    ~RangeLegendBody    () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  static QSize const& keyBoxOffset     ();

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
