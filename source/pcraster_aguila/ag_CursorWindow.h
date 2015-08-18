#ifndef INCLUDED_AG_CURSORWINDOW
#define INCLUDED_AG_CURSORWINDOW



// Library headers.
#include <boost/filesystem/path.hpp>

// PCRaster library headers.

// Module headers.
#include "ag_VisualisationDialog.h"



namespace ag {
  // CursorWindow declarations.
  class CursorView;
}
namespace boost {
 namespace filesystem {
  class filesystem;
 }
}
class QPushButton;


namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class CursorWindow: public VisualisationDialog<DataObject*, CursorWindow>
{

  friend class CursorWindowTest;

private:

  Q_OBJECT

  CursorView*      d_cursorView;
  QPushButton*     d_save;
  QPushButton*     d_get;

  boost::filesystem::path d_cursorValueMonitorPath;
  boost::filesystem::path d_fileToGetCursorValue;

  //! Assignment operator. NOT IMPLEMENTED.
  CursorWindow&    operator=           (CursorWindow const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CursorWindow        (CursorWindow const& rhs);

  void             createInterface     ();

  void             saveToText          (boost::filesystem::path const& path);

  void             appendToCursorValueMonitorFile();

  void             rescan              ();

  void             process             ();

  void             visualise           ();

private Q_SLOTS:

  void             save                ();
  void             get                 ();

protected:

                   CursorWindow        (DataObject* object);

public:

  static CursorWindow* instance        (DataObject* object);

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~CursorWindow       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& guide);

  void             setCursorIO         (
                                        std::string const& cursorValueMonitorFile,
                                        std::string const& fileToGetCursorValue);

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
