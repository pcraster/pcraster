#ifndef INCLUDED_AG_LEGENDBODY
#define INCLUDED_AG_LEGENDBODY



#include <QWidget>
#include "ag_Types.h"



namespace ag {



//! The LegendBody class is a base class for legend body widgets.
/*!
  A Legend consists of a title and a LegendBody widget.
*/
class LegendBody: public QWidget
{

private:

  //! Length of a tic.
  static int       d_ticLength;

  //! Size of a key.
  static QSize     d_keySize;

  //! Label offset from end of tic.
  static QSize     d_labelOffset;

  ViewerType       d_type;

  //! Assignment operator. NOT IMPLEMENTED.
  LegendBody&      operator=           (LegendBody const&);

  //! Copy constructor. NOT IMPLEMENTED.
                   LegendBody          (LegendBody const&);

protected:

                   LegendBody          (ViewerType type,
                                        QWidget* parent);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~LegendBody         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  static int       ticLength           ();

  static QSize const& keySize          ();

  static QSize const& labelOffset      ();

  ViewerType       viewerType          () const;

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
