#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_FILEMAP
#include "com_filemap.h"
#define INCLUDED_COM_FILEMAP
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
// Module headers.



/*!
  \file
  This file contains the implementation of the FileMap class.
*/


#ifdef WIN32
# include <windows.h>
#ifndef INCLUDED_COM_WIN32
#include "com_win32.h"
#define INCLUDED_COM_WIN32
#endif

static int getPageSize() {
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return si.dwPageSize;
}

namespace com {
class FileMapPrivate
  {
    void   *d_ptr;
    HANDLE  d_fd;
    HANDLE  d_map;
    size_t  d_mappedLen;
    std::string d_fileName;

    void clean() {
      int r;
      if (d_ptr) {
       r = UnmapViewOfFile(d_ptr);
       POSTCOND(r);
       d_ptr=0;
      }
      if (d_map != NULL) {
        r = CloseHandle(d_map);
        POSTCOND(r);
        d_map=NULL;
      }
      if (d_fd != INVALID_HANDLE_VALUE) {
        r=CloseHandle(d_fd);
        POSTCOND(r);
        d_fd=INVALID_HANDLE_VALUE;
      }
   }

   void throwError(const std::string& msg) {
      clean();
      throw com::OpenFileError(d_fileName,msg+" win32msg: "+win32GetLastError());
   }

   public:

    FileMapPrivate(
      const char *fileName,
      bool            update,
      size_t          offset,
      size_t          len):
       d_ptr(0), // default for empty file
       d_fd(INVALID_HANDLE_VALUE),
       d_map(NULL), // default for empty file
       d_mappedLen(len),
       d_fileName(fileName)
    {
      DWORD    prot = GENERIC_READ;
      prot |= (update?GENERIC_WRITE:0);
      d_fd = CreateFile(fileName, prot,0 /* no share r|w*/,NULL,
                OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,NULL); 
      if (d_fd == INVALID_HANDLE_VALUE)
        throwError("CreateFile failed");
      if (!len) {
        // compute length
        DWORD size = GetFileSize(d_fd,NULL);
        if (size  == 0xFFFFFFFF)
          throwError("GetFileSize failed");
        d_mappedLen = (size_t)size-offset;
      }
      if (d_mappedLen) {
        prot = PAGE_READONLY;
        if (update)
          prot = PAGE_READWRITE;
        d_map = CreateFileMapping(d_fd,NULL,prot,0,len,NULL);
        if (d_map == NULL)
          throwError("CreateFileMapping failed");
        prot = FILE_MAP_READ;
        if (update)
          prot = FILE_MAP_WRITE;
        d_ptr = (void * )MapViewOfFile(d_map,prot,0,offset,len);
        if (d_ptr == NULL) // this one fails if it is too big
          throwError("Too large to map in memory");
      }
    }

    ~FileMapPrivate()
    {
      clean();
    }
    void *pointer() const {
      return d_ptr;
    }
    size_t mappedLen() const {
      return d_mappedLen;
    }

  };

} // namespace com

#else
  // linux/posix

# include <unistd.h>
# include <sys/mman.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sys/fcntl.h>

static int getPageSize() {
  return getpagesize();
}

namespace com {
class FileMapPrivate
  {
    std::string d_fileName;
    // the memory ptr of the map
    void   *d_ptr;
    // length of the mapping
    size_t d_mappedLen;
    // file descriptor
    int    d_fd;

    void throwError(const char *msg) {
      clean();
      throw com::OpenFileError(d_fileName,msg);
    }

    void clean() {
      if (d_fd != -1)
        close(d_fd);
      d_fd=-1;
      if (d_ptr) {
       int r = msync(d_ptr,d_mappedLen,0);
       POSTCOND(!r);
       r = munmap(d_ptr, d_mappedLen);
       POSTCOND(!r);
       (void)r;  // Shut up compiler
      }
      d_ptr=0;
    }
   public:

    FileMapPrivate(
      const char *fileName,
      bool            update,
      size_t          offset,
      size_t          len):
        d_fileName(fileName),d_ptr(0),d_mappedLen(0),d_fd(-1)
    {
      d_fd = ::open(fileName, (update ? O_RDWR:O_RDONLY)|O_NONBLOCK,0);
      if (d_fd == -1)
        throwError("open failed");
      if (!len) {
        // compute length
        off_t size = lseek(d_fd,0,SEEK_END);
        if (size  == -1)
         throwError("lseek failed");
        len = (size_t)size-offset;
        if (!len)
         throwError("mmap does not support 0 sized files");
      }
      int prot = PROT_READ;
      if (update)
        prot |= PROT_WRITE;
      d_ptr = mmap(0,len,prot,(update?MAP_SHARED:MAP_PRIVATE),d_fd,(off_t)offset);
      if (d_ptr == MAP_FAILED)
        throwError("mmap failed");
      d_mappedLen = len;
    }

    ~FileMapPrivate()
    {
      clean();
    }
    void *pointer() const {
      return d_ptr;
    }
    size_t mappedLen() const {
      return d_mappedLen;
    }

  };

} // namespace com

#endif

//------------------------------------------------------------------------------
// DEFINITION OF STATIC FILEMAP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FILEMAP MEMBERS
//------------------------------------------------------------------------------

/*! ctor, opens the file and maps it
 *  Note that the \a offset and \a len have not been tested beyond their default
 *  values.
 * \param fn     file to open
 * \param update open file for updating, appending is not yet tested!
 * \param offset offset in file, must be getpagesize(2) in Posix.
 *                and SYSTEM_INFO::dwAllocationGranularity on Win32
 *                set to 0, or use with care and understanding.
 * \param len    length of map, if 0 the whole file is mapped
 * \throws       com::OpenFileError in case of access errors or if the file is
 *               too large to map (2 Gb limit or a bit less in practice).
 *               On linux it also throws OpenFileError if a 0 sized file is mapped
 */
com::FileMap::FileMap(
    const PathName& pn,
    bool            update,
    size_t          offset,
    size_t          len)
{
  if (update)
    testOpenForWriting(pn);
  else {
    if (size(pn) > gigaByte<size_t>(2)-1) {
      throw com::OpenFileError(pn,"Too large to map in memory");
    }
    testOpenForReading(pn);
  }
  d_data = new FileMapPrivate(pn.toString().c_str(), update,offset,len);
}

com::FileMap::~FileMap()
{
  delete d_data;
}

void *com::FileMap::pointer() const
{
  return d_data->pointer();
}

//! page size of underlying memory architecture
size_t com::FileMap::pageSize()
{
  return (size_t)getPageSize();
}

const char* com::FileMap::begin() const
{
  return (const char *)pointer();
}

const char* com::FileMap::end() const
{
  return begin()+d_data->mappedLen();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



