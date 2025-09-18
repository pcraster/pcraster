#ifndef INCLUDED_AG_CURSORWINDOW
#define INCLUDED_AG_CURSORWINDOW


#include "ag_VisualisationDialog.h"
#include <filesystem>



namespace ag {
  // CursorWindow declarations.
  class CursorView;
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

  CursorView*      d_cursorView{nullptr};
  QPushButton*     d_save{nullptr};
  QPushButton*     d_get{nullptr};

  std::filesystem::path d_cursorValueMonitorPath;
  std::filesystem::path d_fileToGetCursorValue;

  //! Assignment operator. NOT IMPLEMENTED.
  CursorWindow&    operator=           (CursorWindow const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CursorWindow        (CursorWindow const& rhs);

  void             createInterface     ();

  void             saveToText          (std::filesystem::path const& path);

  void             appendToCursorValueMonitorFile();

  void             rescan              () override;

  void             process             () override;

  void             visualise           () override;

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

  /* virtual */    ~CursorWindow       () override;

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
