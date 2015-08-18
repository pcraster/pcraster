#ifndef INCLUDED_QT_ANIMATIONPROGBAR
#define INCLUDED_QT_ANIMATIONPROGBAR



#include <QProgressBar>



namespace qt {



//! Specialized progress bar for use in the AnimationDlg dialog.
/*!
*/
class AnimationProgBar: public QProgressBar
{

private:

  Q_OBJECT

  QString          d_label;

  size_t           d_firstStep;

  //! Assignment operator. NOT IMPLEMENTED.
  AnimationProgBar& operator=          (const AnimationProgBar&);

  //! Copy constructor. NOT IMPLEMENTED.
                   AnimationProgBar    (const AnimationProgBar&);

protected:

  bool             setIndicator        (QString& label,
                                        int progress,
                                        int totalSteps);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AnimationProgBar    (QWidget* p);

  /* virtual */    ~AnimationProgBar   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setFirstStep        (size_t t);

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



} // namespace qt

#endif
