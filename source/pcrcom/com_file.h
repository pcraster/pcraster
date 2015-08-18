#ifndef INCLUDED_COM_FILE
#define INCLUDED_COM_FILE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif



/*!
  \file 
  functions to open, create and remove files

  functions to open, create and remove files, see also
  notes on file io.
*/

namespace com {
  class PathName;
//       1         2         3         4         5         6         7         8

  void             testOpenForReading  (const std::string &fn);

  void             testOpenForReading  (const PathName    &pn);

  void             testOpenForWriting  (const std::string &fileName);

  void             testOpenForWriting  (const PathName    &pn);

  void             open                (std::ifstream &fs,
                                        const PathName &fileName,
                                        std::ios::openmode flags=std::ios::in);

  void             open                (std::ofstream &fs,
                                        const PathName &fn,
                                        std::ios::openmode flags=std::ios::out);


  void             create              (const std::string &fileName);

  void             create              (const com::PathName &fileName);

  void             move                (const PathName& from,
                                        const PathName& to);

  void             remove              (const PathName &fn);
  void             copy                (const PathName &src,
                                        const PathName &dest);
  void             cat                 (const PathName &src,
                                        std::ostream   &dest);

  size_t           size                (const PathName &fn);

  void             eat                 (std::istream &s,
                                        char c);

  void             skipWhiteSpace      (std::istream &stream);

  void             toNextLine          (std::istream& stream);

  void             expectExistingFile  (const std::string &p);

  bool             filesEqual          (const std::string& file1,
                                        const std::string& file2,
                                        std::ios::openmode cmpMode=std::ios::in);
  bool             filesExistsAndEqual (const std::string& fileName1,
                                        const std::string& fileName2,
                                        std::ios::openmode cmpMode=std::ios::in);

  bool             exists              (const PathName& fileName);

  void             read                (std::string& fillThis,
                                        const PathName &fileName,
                                        std::ios::openmode m=std::ios::in);

  void             write               (const std::string& contents,
                                        const PathName &fileName);
  void             write               (const void *data,
                                        size_t      ndata,
                                        const PathName &fileName);



  //! copy a file at ctor, and remove result at dtor
  class  ScopedCopy {
    com::PathName d_dest;
  public:
    ScopedCopy(const com::PathName& src,
               const com::PathName& dest);
   ~ScopedCopy();
  };

  //! copy a file at ctor, and remove result at dtor
  class  ScopedRename {
    com::PathName d_src;
    com::PathName d_dest;
  public:
    ScopedRename(const com::PathName& src,
                 const com::PathName& dest);
   ~ScopedRename();
  };


} // namespace com

#endif
