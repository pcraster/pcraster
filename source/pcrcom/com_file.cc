#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif

#ifndef INCLUDED_CERRNO
#include <cerrno>
#define INCLUDED_CERRNO
#endif

#ifndef INCLUDED_CSTDIO
#include <cstdio>
#define INCLUDED_CSTDIO
#endif

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_CCTYPE
#include <cctype>
#define INCLUDED_CCTYPE
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#include "boost/filesystem/operations.hpp"
#define INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

/*!
  \file
  functions to open, create and remove files

  functions to open, create and remove files, see also
  notes on file io.
*/

//! std::string version of com::testOpenForReading(const PathName &pn)
void com::testOpenForReading(const std::string &fileName)
{
  PRECOND(!fileName.empty());
  PathInfo pi(fileName);
  pi.testOpenForReading();
}

//! test if \a fileName is an existing regular file that can be read
/*!
   If no exceptions are thrown the file is garantueed to be a file ready
    for opening in read only mode.
    Details are in com::PathInfo::testOpenForReading().
*/
void  com::testOpenForReading(const PathName    &pn)
{
  testOpenForReading(pn.toString());
}

//! open existing regular file for reading
/*!
   If no exceptions are thrown \a fs is garantueed to be a readable stream
   \param     fs Input filestream to open.
   \param     fileName Name of file to open.
   \param     flags Stream open flags, default supplied
   \exception com::OpenFileError calls com::testOpenForReading for regular checks
*/
void com::open(std::ifstream &fs, const PathName &fileName,
                   std::ios::openmode flags)
{
  PRECOND(!fileName.isEmpty());
  testOpenForReading(fileName);

  fs.open(fileName.toString().c_str(), flags);

  if(!fs) { // a reason not expected in testOpenForReading()
    throw OpenFileErrnoMsg(fileName,"could not open file for reading");
  }
}



//! test if \a fileName can be written
/*!
   If no exceptions are thrown the file is garantueed to be a file ready
    for creating in write mode
    Details are in com::PathInfo::testOpenForWriting().
*/
void com::testOpenForWriting(const std::string &fileName)
{
  PRECOND(!fileName.empty());
  PathInfo pi(fileName);
  pi.testOpenForWriting();
}

void  com::testOpenForWriting(const PathName    &pn)
{
  testOpenForWriting(pn.toString());
}



//! Opens a file stream for writing. Filename is \a fileName.
/*!
  \param     fs Output filestream to open.
  \param     fileName Name of file to open.
  \param     flags Stream open flags, default supplied
  \exception com::OpenFileError calls com::testOpenForWriting for regular checks
  \sa        create(), remove()
*/
void com::open(std::ofstream &fs, const PathName &fileName,
                                  std::ios::openmode flags)
{
  PRECOND(!fileName.isEmpty());
  testOpenForWriting(fileName.toString());

  fs.open(fileName.toString().c_str(),flags);

  if(!fs) { // a reason not expected in testOpenForWriting()
    throw OpenFileErrnoMsg(fileName,"could not open/create file for writing");
  }
}



/*! Creates a file if it does not yet exists. A file is created of zero bites size.
  \param     fileName Name of file to create.
  \exception com::OpenFileError as in com::open(std::ofstream &fs, const std::string &fileName)
*/
void com::create(const std::string &fileName)
{
  PRECOND(!fileName.empty());
  PathInfo info(fileName);

  if(!info.exists())
  {
    std::ofstream fs;
    open(fs,fileName);
  }
}

//! wrapper around com::create(const std::string &fileName)
void com::create(const com::PathName &fileName)
{
  create(fileName.toString());
}


//! Moves file \a from to \a to.
/*!
  \param     from Name of file to move.
  \param     to Name of new file.
  \exception com::FileError File or directory could not be moved.
*/
void com::move(const PathName& from, const PathName& to)
{
  if(std::rename(from.toString().c_str(), to.toString().c_str())) {
    throw FileErrnoMsg(from,"while moving to "+to.toString());
  }
}

/*!
  Remove a file object (file or an empty directory). Nothing is done
  if \a fileName does not exits
  \param     fileName Name of file to remove.
  \exception com::FileError File or directory could not be removed.
  \warning   \a fileName must be a normalized path name.
*/
void com::remove(const PathName &fileName)
{
  PRECOND(!fileName.toString().empty());
  namespace fs=boost::filesystem;
  fs::path pn(fileName.path());

  if(fs::exists(pn)) {
    char const* msg= "could not remove file";
    if (fs::is_directory(pn))
      msg="could not remove directory";
    try {
     fs::remove(pn);
    } catch(...) {
     throw FileErrnoMsg(fileName,msg);
    }
  }
}

/*!  Check if path name is an <b>existing regular</b> file
  \throws com::OpenFileError if it is not an <b>existing regular</b> file i
*/
void com::expectExistingFile(const std::string &name)
{
  PathInfo p(name);
  if (!p.exists())
   throw OpenFileError(name," does not exists");
  if (p.isDirectory())
   throw OpenFileError(name," is a directory");
  if (! p.isFile())
   throw OpenFileError(name," is not a (regular) file");
}

//! Eats \a c characters from \a s and throws them away.
void com::eat(std::istream &s, char c)
{
  int ch;

  while(s) {
    if((ch = s.get()) != (int)c) {
      s.putback((char)ch);
      break;
    }
  }
}



/*!
 * eat up all white space chars in stream until a non white space
 * char is found that is put back
 */
void com::skipWhiteSpace(std::istream &stream)
{
    if(stream) {
        char ch;
        while(stream.get(ch)) {
            if(!std::isspace(ch)) {
                stream.putback(ch);
                break;
            }
        }
    }
}



//! Skips whitespace and \n.
/*!
  \exception com::Exception If there's some other stuff than whitespace and \n
             left on the current line. Other stuff is put back on the stream.
*/
void com::toNextLine(std::istream& stream)
{
  int ch = stream.get();

  while(ch != '\n') {
    if(!std::isspace(ch)) {
      stream.putback((char)ch);
      std::ostringstream s;
      s << "Expecting whitespace but character read was '" << ch << '\'';
      throw com::Exception(s.str());
    }

    ch = stream.get();
  }

#ifdef DEBUG_DEVELOP
  POSTCOND(stream);
#endif
}



//! compare two files on contents
/*! return true if contents is equal.
 *  \a cmpMode argument to open the streams is only half of the story.
 *   To really compare in text or binary we must implement discarding
 *   of the \r character
 *  \param cmpMode set to std::ios::binary if binary cmp is needed,
 *           text compare is default
 *  \throws exception if files not found
 */
bool  com::filesEqual   ( const std::string& fileName1,
                          const std::string& fileName2,
                          std::ios::openmode cmpMode)
{
   std::ifstream i1,i2;
   open(i1,fileName1,cmpMode);
   open(i2,fileName2,cmpMode);
   while (i1 && i2) {
      // TODO skip \r here if (cmpMode&std::ios::binary)==0
      if (i1.get() != i2.get())
        return false;
   }
   return i1.eof() && i2.eof();
}

//! test two files on existance and identical contents contents
/*! return true if both existant and contents is equal.
    \sa bool  com::filesEqual ( const std::string& fileName1, const std::string& fileName2, std::ios::openmode cmpMode);
 */
bool com::filesExistsAndEqual(
    const std::string& fileName1,
    const std::string& fileName2,
    std::ios::openmode cmpMode)
{
  if (!PathInfo(fileName1).exists() ||
      !PathInfo(fileName2).exists() )
        return false;
  return filesEqual(fileName1, fileName2, cmpMode);
}

//! returns nr of bytes in file
size_t com::size(const PathName &fn)
{
   std::ifstream ifs;
   open(ifs,fn);
   ifs.seekg(0,std::ios::end);
   return static_cast<size_t>(ifs.tellg());
}

//! read file contents in string
void com::read(
    std::string& fillThis,
    const PathName &fileName,
    std::ios::openmode m)
{
   std::ifstream ifs;
   open(ifs,fileName,m);
   fillThis.erase();
   char c;
   while (ifs.get(c))
     fillThis += c;
   POSTCOND(ifs.eof());
}

//! write string as sole contents to file
void com::write(
    const std::string& contents,
    const PathName &fileName)
{
   std::ofstream ofs;
   open(ofs,fileName);
   ofs << contents;
}

//! write (binary) data of dataLen bytes  as sole contents to file
void com::write(
    const void *data,
    size_t      dataLen,
    const PathName &fileName)
{
   std::ofstream ofs;
   open(ofs,fileName,std::ios::binary);
   ofs.write((const char *)data,dataLen);
}

//! wrapper for PathInfo::exists()
bool  com::exists(const PathName& fileName)
{
  PathInfo pi(fileName);
  return pi.exists();
}

//! make a binary copy of \a src
/*!
 * works like a shell copy, if dest is a directory it is copied
 * to that directory.
 */
void com::copy(
    const PathName &src,
    const PathName &destArg)
{
  testOpenForReading(src);

  PathName dest(destArg);
  if (PathInfo(dest).isDirectory())
    dest.join(src.baseName());
  testOpenForWriting(dest);
  std::string buf;
  read(buf,src,std::ios::binary);
  write(buf.c_str(),buf.size(),dest);
}

//! write contents of \a src to \a dest
void com::cat(const PathName &src,
              std::ostream   &dest)
{
  testOpenForReading(src);

  std::string buf;
  read(buf,src,std::ios::binary);

  dest << buf;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! copy a file at ctor, and remove result at dtor
/*! be aware that the dtor may fail while exceptions
     can not be caught (Soustrup 14.4.7)
 */
com::ScopedCopy::ScopedCopy(
    const com::PathName& src,
    const com::PathName& dest):
     d_dest(dest)
{
  com::copy(src, dest);
}

com::ScopedCopy::~ScopedCopy()
{
  try{
  com::remove(d_dest);
  } catch(...) {
#ifdef DEBUG_DEVELOP
    std::cout << "com::~ScopedCopy failed on " << d_dest << std::endl;
#endif
  }
}

//! rename src to dest at ctor, and rename dest back to src at ctor
/*! be aware that the dtor may fail while exceptions
     can not be caught (Soustrup 14.4.7)
 */
com::ScopedRename::ScopedRename(
    const com::PathName& src,
    const com::PathName& dest):
     d_src(src),
     d_dest(dest)
{
  com::remove(d_dest);
  com::move(d_src,d_dest);
}

com::ScopedRename::~ScopedRename()
{
  try{
  com::remove(d_src);
  com::move(d_dest,d_src);
  } catch(...) {
#ifdef DEBUG_DEVELOP
    std::cout << "com::~ScopedRename failed" << std::endl;
#endif
    abort();
  }
}

//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


