#ifndef INCLUDED_COM_EXCEPTION
#define INCLUDED_COM_EXCEPTION

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif


namespace  com {

class PathName;

//! Predefined system messages \sa com::SystemMessages for explanation.
enum Errno {
       E_NOSYSTEMMESSAGE,
       E_NOMEM,
       E_NODISKSPACE,
       E_NOENT,
       E_ISDIR,
       E_NOTREGFILE,
       E_ACCESREAD,
       E_ACCESWRITE,
       E_ACCESCREATE, // WARNING DO NOT DEPEND ON THIS ONE see bugzilla #82
       E_DIRPARTNOENT,
       E_EXIST
};

//! Base class for exceptions which can be thrown in case an error occurs.
/*!
  This class provides functionality for storing more than one error message.
  No prefix string like ERROR,etc. is added.

  \sa com_exception.cc for full details.
  \todo Help! can not get docs of com::Errno working
*/
class Exception
{

private:

  void add(const std::string& m, bool atEnd);

   //! Error message collection.
   std::vector<std::string> d_messages;


protected:

  Errno       d_no;

  /*!
   * In order to link cppunit with catching of all exception both ctor and dtor
   * must be inline
   */
  Exception   ():
   d_no(E_NOSYSTEMMESSAGE) {};

public:

  typedef std::vector<std::string>::const_iterator const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Exception           (const std::string &m);

  /*!
   * In order to link cppunit with catching of all exception both ctor and dtor
   * must be inline
   */
  virtual          ~Exception          () {};

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             append              (const std::string &m);

  void             prepend             (const std::string &m);

  void             reset               (const std::string &m);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           size                () const;

  std::string      getMessages         () const;

  std::string      messages            () const;

  std::string const& operator[]        (size_t i) const;

  //! Returns iterator to first message.
  const_iterator   begin               () const;

  //! Returns iterator to 'one past the last' message.
  const_iterator   end                 () const;

  Errno            errorNr             () const
  { return d_no; };

  //! inline version of messages() for cppunit only
  /*!
   * trikcy but needed so we do not have to link in pcrcom
   * in cppunit (dependency cycle)
   */
  std::string messagesInLine() const
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
    return m;
  }
};

//------------------------------------------------------------------------------

class BadAllocException : public Exception {
public:
                    BadAllocException();
};

/*!
  \class CommandLineException
  \brief The CommandLineException class is for exceptions which can be
         thrown in case an error occurse while parsing the commandline.
*/
class CommandLineException: public Exception
{

public:

                   CommandLineException(const std::string &m);

                   CommandLineException(const Exception& exception);

  virtual          ~CommandLineException();

};

//------------------------------------------------------------------------------

/*!
  \class OutOfRangeException
  \brief The OutOfRangeException class is for exceptions which can be
         thrown in case a given value is not within the range of valid values.
*/
class OutOfRangeException: public Exception
{

public:

                   OutOfRangeException(const std::string &m);
  virtual          ~OutOfRangeException();

};

//------------------------------------------------------------------------------

/*!
  \class BadStreamFormat
  \brief The BadStreamFormat class is for exceptions which can be
         thrown in case an input stream is badly formatted.
*/
//       1         2         3         4         5         6         7         8
class BadStreamFormat: public Exception
{

public:

                   BadStreamFormat (const std::string &m);
  virtual          ~BadStreamFormat();

};

//------------------------------------------------------------------------------

/*!The FileError class is for exceptions which can be
   thrown in case something goes wrong opening/reading/writing a file.
*/
class FileError: public Exception
{

        std::string d_fileName;
        std::string d_diagnosis;
protected:
  static std::string makeErrnoDiagnose(const std::string& msg);
  static std::string makeFileDiagnose( const std::string& fileName,
                                       const std::string& diagnosis);
public:
                   FileError       (const char*        fileName,
                                    const std::string& diagnosis);

                   FileError       (const std::string& fileName,
                                    const std::string& diagnosis);
                   FileError       (const PathName&    fileName,
                                    const std::string& diagnosis);

  virtual          ~FileError      ();

  const std::string& fileName()  const { return d_fileName;  };
  const std::string& diagnosis() const { return d_diagnosis; };
};

//------------------------------------------------------------------------------

/*!
  \class OpenFileError
  \brief The OpenFileError class is for exceptions which can be
         thrown in case something goes wrong opening a file.
*/
//       1         2         3         4         5         6         7         8
class OpenFileError: public FileError
{

public:

                   OpenFileError   (const std::string& fileName,
                                    const std::string& diagnosis);
                   OpenFileError   (const com::PathName& fileName,
                                    const std::string& diagnosis);
                   OpenFileError   (const std::string& fileName,
                                    Errno nr);
  virtual         ~OpenFileError   ();

};

//------------------------------------------------------------------------------

/*!
  \class FileFormatError
  \brief The FileFormatError class is for exceptions which can be
         thrown in case a file is badly formatted.
*/
//       1         2         3         4         5         6         7         8
class FileFormatError: public FileError
{

public:

                   FileFormatError (const std::string& fileName,
                                    const std::string& diagnosis);
                   FileFormatError (const PathName   & fileName,
                                    const std::string& diagnosis);
  virtual          ~FileFormatError();

};

/*!A FileFormatError with positional info
*/
class FilePositionError: public FileFormatError
{
  //! 1-based
  size_t d_lineNr;
  //! 1-based
  size_t d_columnNr;

  static std::string makePositionDiagnose(
      size_t lineNr, size_t columnNr,
      const std::string& msg);
 public:
  FilePositionError       (const char*        fileName, size_t lineNr, size_t columnNr,
                           const std::string& diagnosis);

  FilePositionError       (const std::string& fileName, size_t lineNr, size_t columnNr,
                           const std::string& diagnosis);
  FilePositionError       (const PathName&    fileName, size_t lineNr, size_t columnNr,
                           const std::string& diagnosis);

  size_t lineNr()   const { return d_lineNr;   };
  size_t columnNr() const { return d_columnNr; };
};

//! file error with strerror(errno) info included
/*! assumes that errno is set to a relevant value
    as com::FileError() with diagnose is \a msg+": "+strerror()
 */
class FileErrnoMsg: public FileError
{
public:

  FileErrnoMsg(
        const    PathName& fileName,
        const std::string& msg);
  FileErrnoMsg(
        const std::string& fileName,
        const std::string& msg);
};

//! open file error with strerror(errno) info included
/*! assumes that errno is set to a relevant value
    as com::OpenFileError() with diagnose is \a msg+": "+strerror()
 */
class OpenFileErrnoMsg: public OpenFileError
{
public:
  OpenFileErrnoMsg(
        const    PathName& fileName,
        const std::string& msg);
  OpenFileErrnoMsg(
        const std::string& fileName,
        const std::string& msg);
};

} // namespace

#endif
