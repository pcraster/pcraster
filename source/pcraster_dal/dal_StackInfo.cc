#ifndef INCLUDED_DAL_STACKINFO
#include "dal_StackInfo.h"
#define INCLUDED_DAL_STACKINFO
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_CCTYPE
#include <cctype>  // std::isdigit on current locale
#define INCLUDED_CCTYPE
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_EXCEPTION
#include <boost/filesystem/exception.hpp>
#define INCLUDED_BOOST_FILESYSTEM_EXCEPTION
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#include <boost/filesystem/operations.hpp>
#define INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

#ifndef INCLUDED_BOOST_LEXICAL_CAST
#include <boost/lexical_cast.hpp>
#define INCLUDED_BOOST_LEXICAL_CAST
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif



/*!
  \file
  This file contains the implementation of the StackInfo class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC STACKINFO MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STACKINFO MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     name Name of the stack.
  \param     scan If true, and the stack is dynamic, then the stack will be
             scanned to see which timesteps are available. If false, or the
             stack is static, then scanning is skipped.
  \warning   If \a scan is false the stack won't be scanned for available
             steps and size() will return 0. This doesn't necesarely mean
             that there aren't any timesteps available in the stack.

  There are two types of stacks: static stack e.g. a map and dynamic stacks,
  a series of maps for different timesteps.

  The name of a static stack is restricted only by the rules of the platform.

  The name of a dynamic stack name must obey the folowing naming rules:
  <ol>
    <li> The name of each map (stack item) should obey the naming conventions
         of DOS: 8 chars, period, 3 chars.
    <li> Structure: base name + first timestep + '+' + last timestep.<p>
         eg: dem00000.001+1000 for a stack with timestep 1 untill and
         including timestep 1000 possibly available.
         This means a stack comprised of possibly 1000 maps, named
         dem00000.001, dem00000.002 up to dem00001.000
  </ol>
  A dynamic stack name is parsed in such a way, that all digits that can not
  be part of the first timestep, are part of the name. This is done on the
  assumption that the user sets the last timestep correct, the last time step
  denotes the maximum number of digits for the first timestep. Thus
  lisw9700.001+999, yields the name lisw9700, which is consistent with pcrcalc,
  a stack name may end with digits.
*/
dal::StackInfo::StackInfo(
         std::string const& name,
         bool scan)

  : d_first(0), d_last(0), d_isScanned(false)

{
  // assert(name.find('\\') == std::string::npos);
  // Split filename from the parent path.
  boost::filesystem::path path(name);
  std::string parent = path.parent_path().string();
  std::string filename = path.filename().string();

  wrongFormatIf(filename.empty());

  size_t pos = filename.rfind('+');

  if( (
        filename.length() < 12 ||
        filename[8] != '.'
      ) ||
      (
        // No + sign.
        pos == std::string::npos &&
        (
          !std::isdigit(*filename.rbegin()) ||
          filename.length() != 12
        )
      ) ||
      (
        // With + sign.
        pos != std::string::npos &&
        (
          filename.length() <= 13 // ||
          // std::count(name.begin(), name.end(), '.') != 1 ||
          // std::count(name.begin(), name.end(), '+') != 1
        )
      )
    ) {
    // Wrong format for dynamic stack: static stack.
    wrongFormatIf(pos != std::string::npos);
    // testPathnameIsNative(name);
    d_name = boost::filesystem::path(name);
  }
  else {
    // Right format for dynamic stack.

    if(pos == std::string::npos) {
      // No plus sign: one step of a dynamic stack.
      // Erase dot.
      filename.erase(8, 1);
      size_t pos = startOfStep(filename);

      if(pos == filename.length()) {
        // Filename ends with only zero's, name of static stack.
        // testPathnameIsNative(name);
        d_name = boost::filesystem::path(name);
      }
      else {
        if(!parent.empty()) {
          // testPathnameIsNative(parent);
        }

        d_name = boost::filesystem::path(parent);

        // soil0000.010
        // soil0000010
        // 0123456789
        std::string number(filename.begin() + pos, filename.end());
        filename.erase(pos, filename.length() - pos + 1);
        if(filename.length() >= 9) {
          filename.insert(8, 1, '.');
        }
        d_name /= filename;
        d_first = boost::lexical_cast<size_t>(number);
        d_last = d_first;
      }
    }
    else {
      // Plus sign: dynamic stack.

      // Erase dot. Keep pos on dot.
      filename.erase(8, 1);
      --pos;

      std::string lastStr = filename.substr(pos + 1);
      assert(!lastStr.empty());        // Checked above.

      std::string firstStr;
      do {
        // On break, pos is always the last char to be included in d_path.
        --pos;

        if(firstStr.size() == lastStr.size()) {
          // Can never be longer than last because last is a bigger number.
          break;
        }

        if(std::isdigit(filename[pos])) {
          firstStr += filename[pos];
        }
        else {
          // Break on the character being end of stack name.
          break;
        }
      } while(pos);

      std::reverse(firstStr.begin(), firstStr.end());
      wrongFormatIf(firstStr.empty());

      d_name = boost::filesystem::path(parent);
      // Add dot again, if necessary.
      // xxxxxxxx.xxx
      // 012345678
      if(pos >= 8) {
        filename.insert(8, 1, '.');
        ++pos;
      }
      d_name /= filename.substr(0, pos + 1);

      // Given the DOS naming rules of PCRaster stacknames we can determine
      // the maximum value for the last time step:
      // blablabl.001+... -> MAX(last) = 999
      // blablab0.001+... -> MAX(last) = 9999
      // If the user provides an insane large number for the last time step as
      // in blablab0.001+88988999933 -> we know last is maximal 9999.
      // Thus according to the naming rules, we reset last to the maximum
      // possible value.
      if (lastStr.size() > firstStr.size()) {
        lastStr.assign(firstStr.size(), '9');
      }

      try {
        d_first = boost::lexical_cast<size_t>(firstStr);
        d_last  = boost::lexical_cast<size_t>(lastStr);
      }
      catch(boost::bad_lexical_cast&) {
        wrongFormatIf(true);
      }

      checkFirstLastOrder();

      assert(d_last >= d_first);
    }
  }

  if(isDynamic() && scan) {
    this->scan();
  }
}



//! Destructor.
/*!
*/
dal::StackInfo::~StackInfo()
{
}



//! Throws an exception if the first step set is larger than the last.
/*!
  \exception Exception If the first step set is larget than the last.
  \todo      Improved message.
*/
void dal::StackInfo::checkFirstLastOrder() const
{
  if(d_first > d_last) {
    throw Exception("Last time step must be larger than or equal to first time step");
  }
}



//! Throws an exception if \a test is false.
/*!
  \param     test Result of a boolean test.
  \exception Exception If \test is false.
  \todo      Improved message.
*/
void dal::StackInfo::wrongFormatIf(bool test) const
{
  if(test) {
    throw Exception("Wrong format for stack name");
  }
}



//! Determines whether \a path could part of the stack.
/*!
  \param     path A pathname.
  \return    true or false
  \warning   The stack must be dynamic.
  \sa        scan()

  The decision is based upon the whether the filename of \a path starts with the
  same filename of the layered name of the stack and whether the folowing
  characters are all digits.
*/
bool dal::StackInfo::isMemberOfStack(boost::filesystem::path const& path) const
{
  assert(isDynamic());

  std::string name = path.filename().string();

  if(name.length() == 12) {
    if(name.find(d_name.filename().string()) == 0) {

      // Check if stuff after base name is numbers and optionally one ".".
      std::string numbers = path.filename().string().substr(d_name.filename().string().size());
      bool dotSeen = false;
      for(std::string::iterator it = numbers.begin(); it != numbers.end();
              ++it) {
        if(!std::isdigit(*it)) {

          if(*it != '.') {
            // Not a digit, should be the one extension separator.
            return false;
          }
          else if(dotSeen) {
            // Second dot seen.
            return false;
          }
          else {
            dotSeen = true;
          }
        }
      }

      return true;
    }
  }

  return false;
}



size_t dal::StackInfo::step(boost::filesystem::path const& path) const
{
  assert(path.filename().string().length() == 12);
  std::string number = path.filename().string().substr(d_name.filename().string().size());
  if(number.length() > 3) {
    assert(number[number.length() - 1 - 3] == '.');
    number.erase(number.length() - 1 - 3, 1);
  }
  return boost::lexical_cast<size_t>(number);
}



//! Determines the position of the first number which is part of the time step.
/*!
  \param     name Name of the stack.
  \return    Position of first number or npos.
  \warning   This function assumes that \a name has the right format for a
             dynamic stack: 8.3 DOS convention, but with the dot removed.
*/
size_t dal::StackInfo::startOfStep(std::string const& name) const
{
  assert(name.length() == 11);
  assert(std::find(name.begin(), name.end(), '.') == name.end());

  size_t pos = name.length();

  do {
    --pos;
    if(!std::isdigit(name[pos])) {
      break;
    }
  } while(pos != 0);

  ++pos;

  assert(pos != name.length());

  // Skip leading zero's which are not considered part of the number, but of
  // the name.
  if(pos != name.length() && name[pos] == '0') {
    while(pos != name.length() && name[pos] == '0') {
      ++pos;
    }
    --pos;
  }

  return ++pos;
}



//! Scans the filesystem for filenames of steps which are part of the stack.
/*!
  \exception Exception If the parent path of the layered stack name is not a
                       directory or can not be searched.
  \warning   The stack must be dynamic. Previous found steps are erased.
  \sa        .

  d_first and d_last are untouched, whether steps are found or not.
*/
void dal::StackInfo::scan()
{
  assert(isDynamic());

  d_steps.erase(d_steps.begin(), d_steps.end());
  d_isScanned = false;
  boost::filesystem::directory_iterator iterator, end;

  try {
    if(d_name.has_parent_path()) {
      iterator = boost::filesystem::directory_iterator(d_name.parent_path());
    }
    else {
      iterator = boost::filesystem::directory_iterator(
         boost::filesystem::current_path());
    }
  }
  catch(boost::filesystem::filesystem_error const& exception) {
    throw Exception(exception.what());
  }

  // Loop over all filenames in the directory.
  size_t step;
  while(iterator != end) {
    if(isMemberOfStack(*iterator)) {
      step = this->step(*iterator);
      if(step >= d_first && step <= d_last) {
        d_steps.push_back(step);
      }
    }
    ++iterator;
  }

  // Order of iteration is undefined. Sort time steps found above.
  std::sort(d_steps.begin(), d_steps.end());

  d_isScanned = true;
}



void dal::StackInfo::scanFirst()
{
  assert(isDynamic());

  if(!isScanned()) {
    d_steps.erase(d_steps.begin(), d_steps.end());

    for(size_t step = d_first; step <= d_last; ++step) {
      if(boost::filesystem::exists(timeStepPath(d_name, step, PCRConvention))) {
        d_steps.push_back(step);
        break;
      }
    }
  }
}



size_t dal::StackInfo::first() const
{
  assert(isDynamic());

  return d_first;
}



size_t dal::StackInfo::last() const
{
  assert(isDynamic());

  return d_last;
}



//! Returns the number of steps found during scanning.
/*!
  \return    Number of steps.
  \warning   The stack must be dynamic. The stack must be scanned already.
  \sa        scan(), isScanned()
*/
size_t dal::StackInfo::size() const
{
  assert(isDynamic());
  assert(isScanned());

  return d_steps.size();
}



//! Returns a const iterator to the first step found during scanning.
/*!
  \return    Const iterator.
  \warning   The stack must be dynamic.
*/
dal::StackInfo::const_iterator dal::StackInfo::begin() const
{
  assert(isDynamic());

  return d_steps.begin();
}



//! Returns a const iterator to the one-past-the-last step found during scanning.
/*!
  \return    Const iterator.
  \warning   The stack must be dynamic.
*/
dal::StackInfo::const_iterator dal::StackInfo::end() const
{
  assert(isDynamic());

  return d_steps.end();
}



//! Returns the name of the stack.
/*!
  \return    Name.

  The name of a static stack is the same name as given to the constructor.
  The name of a dynamic stack is the part of the name given to the constructor
  before the time steps. The name returned is a valid filename on the runtime
  platform.
*/
std::string dal::StackInfo::name() const
{
  return d_name.string();
}



//! Returns the filename of the stack.
/*!
  \warning   The stack must be static.
  \return    Filename.

  The name returned is the same name as given to the constructor.
  The name returned is a valid filename on the runtime platform.
*/
boost::filesystem::path const& dal::StackInfo::filename() const
{
  assert(!isDynamic());
  return d_name;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \sa        .
*/
boost::filesystem::path dal::StackInfo::filename(size_t item) const
{
  assert(!d_name.empty());
  return isDynamic() ? timeStepPath(d_name, item, PCRConvention) : filename();
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   The stack must be dynamic.
  \sa        .
*/
bool dal::StackInfo::isScanned() const
{
  assert(isDynamic());

  return d_isScanned;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
bool dal::StackInfo::isDynamic() const
{
  return d_first != 0;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   The stack must be dynamic.
  \sa        .
*/
bool dal::StackInfo::contains(size_t item) const
{
  assert(isDynamic());
  assert(isScanned());
  return std::find(d_steps.begin(), d_steps.end(), item) != d_steps.end();
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
std::string dal::StackInfo::toString() const
{
  if(!isDynamic()) {
    return d_name.string();
  }
  else {
    return (boost::format("%1%+%2%")
         % timeStepPath(d_name, d_first, PCRConvention).string()
         % d_last).str();
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



