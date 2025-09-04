#ifndef INCLUDED_AG_TIMEPLOTWINDOW
#define INCLUDED_AG_TIMEPLOTWINDOW



#include "ag_Configure.h"
#include "ag_VisualisationWindow.h"

#include <filesystem>


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

  TimePlot*        d_plot{};

  //! Assignment operator. NOT IMPLEMENTED.
  TimePlotWindow & operator=           (const TimePlotWindow &);

  //! Copy constructor. NOT IMPLEMENTED.
                   TimePlotWindow      (const TimePlotWindow &);

  void             createInterface     ();

  void             saveAsPNG           (std::filesystem::path const& path) override;

protected:

  // void             showEvent           (QShowEvent* event);

  std::string      windowName          () const override;

  bool             dataVisualised      () const override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   TimePlotWindow      (const qt::AppWindowProperties& props,
                                        ag::DataObject *o);

  //! Destructor.
  /* virtual */    ~TimePlotWindow     () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             rescan              () override;

  void             addAttribute        (const ag::DataGuide& dataGuide) override;

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
