#ifndef INCLUDED_AG_FILEICONPROVIDER
#define INCLUDED_AG_FILEICONPROVIDER



#include <q3filedialog.h>
#include <QPixmap>



namespace ag {



/*!
  \class FileIconProvider
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
class PCR_AG_DECL FileIconProvider: public Q3FileIconProvider
{

private:

  QPixmap          d_pcrIcon;

  //! Assignment operator. NOT IMPLEMENTED.
  FileIconProvider &operator=          (const FileIconProvider &);

  //! Copy constructor. NOT IMPLEMENTED.
                   FileIconProvider    (const FileIconProvider &);

  //! Frees dynamically allocated memory.
  void             clean               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   FileIconProvider    (QObject *p = 0,
                                        const char *n = 0);

  //! Destructor.
  /* virtual */    ~FileIconProvider   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const QPixmap *  pixmap              (const QFileInfo &i);

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
