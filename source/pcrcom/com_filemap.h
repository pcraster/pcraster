#ifndef INCLUDED_COM_FILEMAP
#define INCLUDED_COM_FILEMAP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace com {
  class PathName;
  class FileMapPrivate;
}



namespace com {



//! open a file and maps its contents to memory
/*!
   This is a portable implementation of mmap (Posix) and CreateFileMapping (WIN32)
   It currently always uses an existing file, whose contents can be accessed in both
   read and read-write mode, no appending or shrinking of the file 
   is possible or known to work.

*/
class FileMap
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  FileMap&           operator=           (const FileMap&);

  //! Copy constructor. NOT IMPLEMENTED.
                   FileMap               (const FileMap&);

  FileMapPrivate  *d_data;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   FileMap               (const PathName& pn,
                                          bool            update=false,
                                          size_t          offset=0,
                                          size_t          len=0);

  /* virtual */    ~FileMap              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  //! return the address pointer, can be 0 if file-size is 0
  void      *pointer()  const;
  static size_t pageSize();

  //! can be 0 if file-size is 0
  const char *begin() const;
  //! can be 0 if file-size is 0
  const char *end()   const;

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



} // namespace com

#endif
