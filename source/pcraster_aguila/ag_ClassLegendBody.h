#ifndef INCLUDED_AG_CLASSLEGENDBODY
#define INCLUDED_AG_CLASSLEGENDBODY



#include "ag_DataGuide.h"
#include "ag_LegendBody.h"



namespace ag {
  class ClassDrawProps;
  class DataObject;
}



namespace ag {



//! The ClassLegendBody class is for body widgets for ClassLegend s.
/*!
  \ todo No virtual functions.
*/
class ClassLegendBody: public LegendBody
{

private:

  //! Space between keys.
  static const int d_keyOffset;

  static int       keyOffset           ();

  DataGuide        d_guide;

  ClassDrawProps const& d_drawProperties;

  //! Assignment operator. NOT IMPLEMENTED.
  ClassLegendBody& operator=           (ClassLegendBody const&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ClassLegendBody     (ClassLegendBody const&);

  int              maxWidthLabel       () const;

  void             paintLineLegend     ();

  void             paintKeyLegend      ();

protected:

  virtual int      width               () const;

  virtual int      height              () const;

  virtual void     paintEvent          (QPaintEvent* event);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ClassLegendBody     (DataObject const& object,
                                        DataGuide const& guide,
                                        ViewerType type,
                                        QWidget* parent = 0);

  virtual          ~ClassLegendBody    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
