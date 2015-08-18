#ifndef INCLUDED_AG_ANIMATIONCONTROL
#define INCLUDED_AG_ANIMATIONCONTROL



// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_VisualisationDialog.h"



namespace ag {
  // AnimationControl declarations.
  class AnimationControlPrivate;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class AnimationControl:
         public ag::VisualisationDialog<DataObject*, AnimationControl>
{

private:

  Q_OBJECT

  AnimationControlPrivate* d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  AnimationControl& operator=           (const AnimationControl&);

  //! Copy constructor. NOT IMPLEMENTED.
                   AnimationControl     (const AnimationControl&);

  void             createInterface     ();

  void             configureInterface  ();

  void             updateAnimation     ();

private Q_SLOTS:

  void             start               ();

  void             pause               ();

  void             toBegin             ();

  void             toEnd               ();

  void             backwards           ();

  void             forewards           ();

  void             loop                (bool setting);

  void             intervalChanged     (int interval);

  void             updateInterface     ();

  void             timeStepChanged     ();

protected:

                   AnimationControl     (DataObject* object);

  void             rescan              ();

  void             process             ();

  void             visualise           ();

public:

  static AnimationControl* instance    (DataObject* object);

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~AnimationControl    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
