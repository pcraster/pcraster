#ifndef INCLUDED_AG_IVISUALISATION
#define INCLUDED_AG_IVISUALISATION



#include <string>
#include <vector>
#ifndef Q_MOC_RUN
#include <boost/filesystem/path.hpp>
#include "dal_DataSpace.h"
#include "com_fileformatinfo.h"
#include "geo_dataguide.h"
#endif

#ifndef INCLUDED_AG_VISOBSERVER
#include "ag_VisObserver.h"
#endif



namespace ag {
  class DataGuide;
  class DataObject;
  class IVisualisationPrivate;
  class VisEngine;
}



namespace ag {



/*!
  \class IVisualisation
  \brief The IVisualisation class is a base class for all visualisations.

  ...
*/

class IVisualisation: public ag::VisObserver
{

private:

  static size_t    d_nrCreated;

  IVisualisationPrivate *d_data;

  //! Cursor position of this visualisation.
  // CursorPos        d_cursor;

  //! Assignment operator. NOT IMPLEMENTED.
  IVisualisation &  operator=           (const IVisualisation &);

  //! Copy constructor. NOT IMPLEMENTED.
                   IVisualisation       (const IVisualisation &);

  //! Frees dynamically allocated memory.
  void             clean               ();

/*
  //! enable animation controls
  qt::EnableContainer d_animationEnable;
  */

  void             testDataType        (geo::DataType dataType) const;

  void             testValueScale      (CSF_VS valueScale) const;

protected:

  //! Constructor.
                   IVisualisation       (ag::DataObject* object,
                                        const std::string& visualisationName);

/*
  bool             eventFilter         (QObject* object,
                                        QEvent* event);
*/

/*
  void             closeEvent          (QCloseEvent *e);
*/

/*
  virtual bool     mousePressHandled   (QObject *o,
                                        QMouseEvent *e);

  virtual bool     mouseReleaseHandled (QObject *o,
                                        QMouseEvent *e);

  virtual bool     mouseDoubleClickHandled(QObject *o,
                                        QMouseEvent *e);

  virtual bool     mouseMoveHandled    (QObject *o,
                                        QMouseEvent *e);
*/

  void             setSupportedDataTypes(
                         const std::vector<geo::DataType>& dataTypes);

  void             setSupportedValueScales(
                         const std::vector<CSF_VS>& valueScales);

  void             setSaveAsFileFormats(
                         const std::vector<com::FileFormatInfo>& fileFormats);

  virtual void     saveAsPNG           (boost::filesystem::path const& path);

  virtual void     saveAsEPS           (boost::filesystem::path const& path);

  void             testDataGuide       (const geo::DataGuide& dataGuide) const;

  static size_t    nrCreated           ();

  const std::vector<com::FileFormatInfo>& fileFormats() const;

// protected Q_SLOTS:

/*
  void             showDataPropertiesDialog(const ag::DataGuide& dataGuide);
*/

  VisEngine&       visualisationEngine ();

  dal::DataSpace   visualisationDataSpace() const;

  dal::DataSpace   dataSpace           () const;

  dal::DataSpaceAddress const& dataSpaceAddress() const;

  // void             setProfileDataSpace (dal::DataSpace const& space);

  // dal::DataSpace const& profileDataSpace() const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Destructor.
  virtual          ~IVisualisation      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  virtual bool     close               ()=0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns a copy of the visualisation.
  // virtual IVisualisation *copy          (ag::DataObject * /* o */) const;

  //! Returns the cursor.
  // const CursorPos &cursor              () const;

  //! Returns the data object observed.
  ag::DataObject&  dataObject          () const;

  const std::string& visualisationName () const;

  VisEngine const& visualisationEngine () const;

  // Q_SIGNALS:

  // void             closed              (ag::IVisualisation *v);

  // void             copy                (ag::IVisualisation *v);

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
