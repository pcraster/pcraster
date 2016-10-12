#ifndef INCLUDED_AG_SAVEVIEWASDIALOG
#define INCLUDED_AG_SAVEVIEWASDIALOG



// Library headers.
#include <vector>
#include <QDialog>

// PCRaster library headers.
#ifndef Q_MOC_RUN
#include "dal_DataSpace.h"
#include "com_fileformatinfo.h"
#endif

// Module headers.
#include "ui_SaveViewAsDialogBase.h"



namespace ag {
  // SaveViewAsDialog declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class SaveViewAsDialog: public QDialog
{

  friend class SaveAsDialogTest;

private:

  Q_OBJECT

  Ui::SaveViewAsDialogBase d_ui;

  std::string      d_applicationName;

  std::vector<com::FileFormatInfo> d_formats;

  //! Data space in which the data to save is defined in.
  dal::DataSpace   d_space;

  //! Data space address which is visualised when starting the dialog.
  dal::DataSpaceAddress d_address;

  //! Assignment operator. NOT IMPLEMENTED.
  SaveViewAsDialog& operator=          (SaveViewAsDialog const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   SaveViewAsDialog    (SaveViewAsDialog const& rhs);

  bool             nameIsValid         (QString const& name) const;

private Q_SLOTS:

  void             browse              ();

  void             nameChanged         (const QString& name);

  void             accept              ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SaveViewAsDialog    (std::string const& applicationName,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address,
                                        std::vector<com::FileFormatInfo> const& formats,
                                        // std::string const& defaultName,
                                        QWidget* parent = 0,
                                        bool modal = false,
                                        Qt::WindowFlags flags = Qt::Widget);

  /* virtual */    ~SaveViewAsDialog   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  dal::DataSpace   iterationSpace      () const;

  com::FileFormatInfo const& selectedFormat() const;

  std::string      name                () const;

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
