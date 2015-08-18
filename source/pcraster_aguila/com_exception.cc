#include "com_exception.h"
#include <cerrno>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <boost/algorithm/string.hpp>



/*!
  \file
  \brief Exception hierarchy
*/

namespace com {
class SystemMessages {
 std::map<Errno,std::string> d_msg;
 typedef std::pair<Errno,std::string> Pair;
public:
  SystemMessages() {
   d_msg.insert(Pair(com::E_NOMEM,       "Not enough memory"));
   d_msg.insert(Pair(com::E_NODISKSPACE, "Not enough disk space"));
   d_msg.insert(Pair(com::E_NOENT,       "No such file or directory"));
   d_msg.insert(Pair(com::E_ISDIR,       "Is a directory"));

   d_msg.insert(Pair(com::E_NOTREGFILE,  "Is not a (regular) file"));
   d_msg.insert(Pair(com::E_ACCESREAD,   "Permission denied for reading"));
   d_msg.insert(Pair(com::E_ACCESWRITE,  "Permission denied for (over)writing"));
   d_msg.insert(Pair(com::E_ACCESCREATE, "Permission denied for creating"));
   d_msg.insert(Pair(com::E_DIRPARTNOENT,"Directory/Folder part of file does not exist"));
   d_msg.insert(Pair(com::E_EXIST,       "File exists"));
  };
  const std::string& operator[](Errno nr) const {
   std::map<Errno,std::string>::const_iterator f = d_msg.find(nr);
   assert(f != d_msg.end());
   return (*f).second;
  }
};

  static const SystemMessages systemMessages;
}


/*!
  Constructor takes man error message.
  Note that that all messages put into the Exception should be 
  sensible, and non empty . The Exception formatter removes all
  leading and trailing spaces
*/
com::Exception::Exception(const std::string &message):
  d_no(E_NOSYSTEMMESSAGE)
{
  add(message,true);
}

//! add the string trimmed
void com::Exception::add(const std::string& m, bool atEnd)
{
 std::string t(m);
 boost::trim(t);
 assert(!t.empty());
 if (atEnd)
  d_messages.push_back(t);
 else
  d_messages.insert(d_messages.begin(), t);
}


/*!
  Adds an error message to the back of the collection.
  \a m is trimmed for space at end and start
*/
void com::Exception::append(const std::string &m)
{
  add(m,true);
}



/*!
  Adds an errormessage to the front of the collection.
  \a m is trimmed for space at end and start
*/
void com::Exception::prepend(const std::string &m)
{
  add(m,false);
}

/*!
  reset errormessage collection to m only
  \a m is trimmed for space at end and start
*/
void com::Exception::reset(const std::string &m)
{
  d_messages.clear();
  add(m,false);
}



/*!
  Returns the errormessage(s) as a string.
  \sa messages()
*/
std::string com::Exception::getMessages() const
{
  return messages();
}

/*!
  \return A string containing all messages
  A newline is printed after each message.
*/
std::string com::Exception::messages() const
{
  std::string m;

  std::vector<std::string>::const_iterator it;
  it = d_messages.begin();

  while(it != d_messages.end())
  {
    m += *it;
    m += '\n';
    it++;
  }

  // a least one char and newline :-)
  assert(m.size() > 2); 
  // ending newline
  assert(m[m.size()-1] == '\n'); 

  return m;
}



size_t com::Exception::size() const
{
  return d_messages.size();
}



std::string const& com::Exception::operator[](size_t i) const
{
  assert(i < size());
  return d_messages[i];
}



com::Exception::const_iterator com::Exception::begin() const
{
  return d_messages.begin();
}



com::Exception::const_iterator com::Exception::end() const
{
  return d_messages.end();
}



//------------------------------------------------------------------------------

//! thrown in case of not enough memory
com::BadAllocException::BadAllocException()
  : com::Exception(com::systemMessages[E_NOMEM])
{
  // DO NOT RENAME BACK to BadAlloc X11 has a macro named BadAlloc
}

com::CommandLineException::CommandLineException(const std::string &m)

  : com::Exception(m)

{
}



com::CommandLineException::CommandLineException(const Exception& exception)

  : com::Exception(exception)

{
}



com::CommandLineException::~CommandLineException()
{
}

//------------------------------------------------------------------------------

com::OutOfRangeException::OutOfRangeException(const std::string &m)

  : com::Exception(m)

{
}



com::OutOfRangeException::~OutOfRangeException()
{
}

//------------------------------------------------------------------------------

com::BadStreamFormat::BadStreamFormat(const std::string &m)

  : com::Exception(m)

{
}

com::BadStreamFormat::~BadStreamFormat()
{
}


std::string com::FileError::makeFileDiagnose(
        const std::string& fileName,
        const std::string& diagnosis)
{
        std::string str = "File '"+fileName+"': "+diagnosis;
        return str;
}

std::string com::FileError::makeErrnoDiagnose(
      const std::string& msg)
{
      return msg+": "+std::string(::strerror(errno));
}

com::FileError::FileError(
        const std::string& fileName,
        const std::string& diagnosis)
  : Exception(makeFileDiagnose(fileName,diagnosis)),
    d_fileName(fileName), d_diagnosis(diagnosis)
{
}

com::FileError::FileError(
        const char*        fileName,
        const std::string& diagnosis)
  : Exception(makeFileDiagnose(std::string(fileName),diagnosis)),
    d_fileName(fileName), d_diagnosis(diagnosis)
{
}

// com::FileError::FileError(
//         const PathName&    fileName,
//         const std::string& diagnosis)
//   : Exception(makeFileDiagnose(fileName.toString(),diagnosis)),
//     d_fileName(fileName.toString()), d_diagnosis(diagnosis)
// {
// }


com::FileError::~FileError()
{
}

//------------------------------------------------------------------------------

com::OpenFileError::OpenFileError(
        const std::string& fileName,
        const std::string& diagnosis)
  : FileError(fileName,diagnosis)
{
}

// com::OpenFileError::OpenFileError(
//         const com::PathName& fileName,
//         const std::string& diagnosis)
//   : FileError(fileName,diagnosis)
// {
// }

com::OpenFileError::OpenFileError(
        const std::string& fileName,
        Errno nr)
  : FileError(fileName,systemMessages[nr])
{
 d_no=nr;
}

com::OpenFileError::~OpenFileError()
{
}

//------------------------------------------------------------------------------

com::FileFormatError::FileFormatError(
        const std::string& fileName,
        const std::string& diagnosis)
  : FileError(fileName,diagnosis)
{
}

// com::FileFormatError::FileFormatError(
//         const com::PathName& fileName,
//         const std::string& diagnosis)
//   : FileError(fileName,diagnosis)
// {
// }

com::FileFormatError::~FileFormatError()
{
}

std::string com::FilePositionError::makePositionDiagnose(
      size_t lineNr, size_t columnNr,
      const std::string& msg)
{
  std::ostringstream s;
  s << "at line '" << lineNr << "'";
  if (columnNr)
    s << " column '" << columnNr << "'";
  s << ": " << msg;
  return s.str();
}

//! column nr info is only printed in text, if \a columnNr is non-zero
com::FilePositionError::FilePositionError(
        const std::string& fileName, size_t lineNr, size_t columnNr,
        const std::string& diagnosis)
  : FileFormatError(fileName,makePositionDiagnose(lineNr,columnNr,diagnosis)),
    d_lineNr(lineNr),d_columnNr(columnNr)
{
}

com::FilePositionError::FilePositionError(
        const char*        fileName, size_t lineNr, size_t columnNr,
        const std::string& diagnosis)
  : FileFormatError(std::string(fileName),makePositionDiagnose(lineNr,columnNr,diagnosis)),
    d_lineNr(lineNr),d_columnNr(columnNr)
{
}

// com::FilePositionError::FilePositionError(
//         const PathName&    fileName, size_t lineNr, size_t columnNr,
//         const std::string& diagnosis)
//   : FileFormatError(fileName.toString(),makePositionDiagnose(lineNr,columnNr,diagnosis)),
//     d_lineNr(lineNr),d_columnNr(columnNr)
// {
// }

// //! ctor
// com::FileErrnoMsg::FileErrnoMsg(
//         const com::PathName& fileName,
//         const std::string& msg):
//   FileError(fileName,makeErrnoDiagnose(msg))
// {
// }

//! ctor
com::FileErrnoMsg::FileErrnoMsg(
        const std::string& fileName,
        const std::string& msg):
  FileError(fileName,makeErrnoDiagnose(msg))
{
}

// //! ctor
// com::OpenFileErrnoMsg::OpenFileErrnoMsg(
//         const com::PathName& fileName,
//         const std::string& msg):
//   OpenFileError(fileName.toString(),makeErrnoDiagnose(msg))
// {
// }

//! ctor
com::OpenFileErrnoMsg::OpenFileErrnoMsg(
        const std::string& fileName,
        const std::string& msg):
  OpenFileError(fileName,makeErrnoDiagnose(msg))
{
}
