#ifndef INCLUDED_AG_TIMEPLOTWINDOW
#define INCLUDED_AG_TIMEPLOTWINDOW



#include <boost/filesystem/path.hpp>
#include "ag_Configure.h"
#include "ag_VisualisationWindow.h"



namespace ag {
  // class TimePlotWindowPrivate;
  class TimePlot;
}



namespace ag {



//! The TimePlotWindow class is for timeplot application windows.
class PCR_AG_DECL TimePlotWindow: public VisualisationWindow
{

private:

  Q_OBJECT

  TimePlot*        d_plot;

  //! Assignment operator. NOT IMPLEMENTED.
  TimePlotWindow & operator=           (const TimePlotWindow &);

  //! Copy constructor. NOT IMPLEMENTED.
                   TimePlotWindow      (const TimePlotWindow &);

  void             createInterface     ();

  void             saveAsPNG           (boost::filesystem::path const& path);

protected:

  // void             showEvent           (QShowEvent* event);

  std::string      windowName          () const;

  bool             dataVisualised      () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   TimePlotWindow      (const qt::AppWindowProperties& props,
                                        ag::DataObject *o);

  //! Destructor.
  /* virtual */    ~TimePlotWindow     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             rescan              ();

  void             addAttribute        (const ag::DataGuide& dataGuide);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  // TimePlotWindow * copy                (ag::DataObject *o) const;

  // void             saveAsPNG           (const com::PathName& pathName);

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
