#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CSFSTACKNAME
#include "geo_csfstackname.h"
#define INCLUDED_GEO_CSFSTACKNAME
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#include "boost/filesystem/operations.hpp"
#define INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#endif


#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

#ifndef INCLUDED_ITERATOR
#include <iterator>
#define INCLUDED_ITERATOR
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_CCTYPE
#include <cctype>
#define INCLUDED_CCTYPE
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif



//------------------------------------------------------------------------------

namespace geo {


class CSFStackNamePrivate
{
private:
  void checkFirstLastOrder() {
    if(first > last)
      throw com::Exception("last timestep must be larger than first timestep");
  }
  void wrongFormatIf(bool test) {
    if(test)
      throw com::Exception("wrong format for stack name");
  }
public:

  com::PathName d_path;                // Path name without time step nrs.
  size_t      first;                   // First timestep available.
  size_t      last;                    // Last timestep available.
  bool        dynamic;                 // Stack is dynamic or not.

  CSFStackNamePrivate(const com::PathName& pn, size_t f, size_t l)
    : d_path(pn), first(f), last(l), dynamic(true)
  {
    checkFirstLastOrder();
  }

  CSFStackNamePrivate(const com::PathName &n)
    : first(0), last(0)
  {
    std::string str(n.toString());
    // position of plus
    size_t pos = str.rfind('+');
    // presence of plus means dynamic
    dynamic = pos != std::string::npos;
    if (!dynamic)
      d_path  = n;
    else {
      std::string lastStr = str.substr(pos+1);
      wrongFormatIf(lastStr.empty());

      std::string firstStr;
      do { 
          // on break, pos is always the last char
          // to be included in d_path
          --pos;
          if (firstStr.size() == lastStr.size())
            break; // can never be longer than last
                   // because last is a bigger number
          if(str[pos] == '.') 
             continue; // skip the . in between the DOS 8+3
          else {
             if(std::isdigit(str[pos])) 
               firstStr += str[pos];
             else
               break; // break on the character being end of stackname
          }
      } while(pos);
      std::reverse(firstStr.begin(),firstStr.end());
      wrongFormatIf(firstStr.empty());

      // The code above might still result in a point at the end of the
      // pathname. Remove it if so.
      if(pos && str[pos] == '.') {
        --pos;
      }
      d_path = str.substr(0,pos+1);

      /*
       * Given the DOS naming rules of PCRaster stacknames we can determine
       * the maximum value for the last time step:
       * blablabl.001+... -> MAX(last) = 999
       * blablab0.001+... -> MAX(last) = 9999
       * If the user provides an insane large number for the last time step as in
       * blablab0.001+88988999933 -> we know last is maximal 9999
       * this according to the naming rules, we reset last to the maximum
       * possible value.
       */
      if (lastStr.size() > firstStr.size())
        lastStr.assign(firstStr.size(),'9');

      try {
       first = com::strToSize_t(firstStr);
       last  = com::strToSize_t(lastStr);
      } catch(...) {
        wrongFormatIf(true);
      }
      checkFirstLastOrder();
    }
  }

};

} // namespace geo



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

std::string geo::CSFStackName::asAguilaArgument(
         std::string stackName, int start, int stop)
{
  DEVELOP_PRECOND(!stackName.empty());
  DEVELOP_PRECOND(stackName.find(".") != stackName.size() - 1);
  std::string arg(dal::timeStepPath83(boost::filesystem::path(
         stackName), start).string());
  arg+="+";
  arg+=com::intToStr(stop);
  return arg;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

/*!
  \brief   Constructor.
  \param   n Name of the stack, static or dynamic.
  \param   scanIt If true, and the stack is dynamic, then the stack will be
           scanned to see which timesteps are available. If false, or the stack
           is static, then scanning is skipped.
  \warning If \a scanNow is false the stack won't be scanned for available
           layers and nrLayers() will return 0. This doesn't necesarely mean
           that there aren't any timesteps available in the stack.
  \sa      geo::CSFStackName(const geo::CSFStackName &)

  There are two types of stacks: static stack e.g. a map and dynamic stacks,
  a series of maps for different timesteps.

  The name of a static stack is restricted only by the rules of the shell.

  The name of a dynamic stack name must obey the folowing naming rules:
  <ol>
    <li> Structure: base name + first timestep + '+' + last timestep.<p>
         eg: dem00000.001+1000 for a stack with timestep 1 untill and
         including timestep 1000 possibly available.
         This means a stack comprised of possibly 1000 maps, named 
         dem00000.001, dem00000.002 up to dem00001.000
    <li> The name of each map (stack item) should obey the naming conventions of DOS:
         8 chars, period, 3 chars.
  </ol>
  A dynamic stack name is parsed in such a way, that all digits that can not be part
  of the first timestep, are part of the base name. This is done on the assumption
  that the user sets the last timestep correct, the last time step denots the maximum
  number of digits for the first timestep. Thus lisw9700.001+999, yields
  the base name lisw9700, which consistent with pcrcalc, a stack name may end with 
  digits.
*/
geo::CSFStackName::CSFStackName(const com::PathName &n, bool scanIt)

  : d_data(0)

{
  try {

    init(n);

    if(isDynamic() && scanIt)
      scan();

  }
  catch(...) {

    clean();
    throw;

  }
}



//! Constructs a CSFStackName object for a dynamic stack.
/*!
  \param     f First time step.
  \param     l Last time step.
  \param     pn Path name of the stack. This path name should not contain any
             timesteps.
  \param     scanIt If true, then the stack will be scanned to see which
             timesteps are available. If false, then scanning is skipped.
  \warning   \a f and \a l must both be larger than 0 and \a f must be smaller
             than \a l.

  Example:

  \code
    // Create a stack name object for the stack named 'dem' which contains
    // rasterlayers for time steps [1, 100].
    CSFStack ds("/home/tjalling/data/dem",1,100);
  \endcode
*/
geo::CSFStackName::CSFStackName(const com::PathName& pn, size_t f, size_t l,
                   bool scanIt)
{
  PRECOND(f > 0 && l > 0 && f < l);

  try {
    d_data = new CSFStackNamePrivate(pn, f, l);
    d_scanned = false;

    if(scanIt)
      scan();

    POSTCOND(isDynamic());
  }
  catch(...) {
    clean();
    throw;
  }
}



/*!
  \brief   Copy constructor.
  \param   n Stack name object to copy properties from.
  \sa      geo::CSFStackName(const std::string &, bool)
*/
geo::CSFStackName::CSFStackName(const geo::CSFStackName &n)

  : d_data(0)

{
  d_data    = new CSFStackNamePrivate(*(n.d_data));
  d_scanned = n.d_scanned;
  d_steps   = n.d_steps;
}



/*!
  \brief   Destructor.
*/
geo::CSFStackName::~CSFStackName()
{
  clean();
}



/*!
  \brief   Initialises an object.
*/
void geo::CSFStackName::init(const com::PathName &n)
{
  d_data    = new CSFStackNamePrivate(n);
  d_scanned = false;
}



/*!
  \brief   Frees dynamically allocated memory.
*/
void geo::CSFStackName::clean()
{
  delete d_data; d_data = 0;
}



/*!
  \brief   Assignment operator.
  \param   rhs Stack name object to copy properties from.
*/
geo::CSFStackName &geo::CSFStackName::operator=(const geo::CSFStackName &rhs)
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_data);
#endif

  if(this != &rhs) {
    *d_data   = *(rhs.d_data);
    d_scanned = rhs.d_scanned;
    d_steps   = rhs.d_steps;
  }

  return *this;
}



//! Returns true if \a fileName is a member of the stack.
/*!
  \param     fileName Name of file to check.
  \return    true or false.
  \warning   fileName must be equal to its base name (must not contain a
             directory name or a drive letter).
*/
bool geo::CSFStackName::isMemberOfStack(const com::PathName& fileName) const
{
    PRECOND(fileName == fileName.baseName());

    // Check if fileName is smaller then or equal to 8 + 1 + 3 in length.
    if(!(fileName.length() <= 12)) {
      return false;
    }

    // Check if fileName starts with base name.
    if(!fileName.startsWith(baseName().baseName())) {
      return false;
    }

    // Check if stuff after base name is numbers and optionally one ".".
    size_t i = baseName().baseName().length();
    size_t n = fileName.length() - baseName().baseName().length();
    std::string numbers = fileName.toString().substr(i, n);

    bool extensionSeen = false;
    for(std::string::iterator it = numbers.begin(); it != numbers.end(); ++it) {

      if(!std::isdigit(*it)) {
        // Not a digit, should be the one extension separator.
        if(*it == '.') {

          if(extensionSeen) {

            // Second extension separator seen.
            return false;
          }
          else {

            if(std::distance(it, numbers.end()) != 4) {
              // Extension to small or large (according to PCRaster specs).
              return false;
            }

            extensionSeen = true;
          }
        }
      }
    }

    return true;
}



//! Fills \a pool with interesting file names which might be part of the stack.
/*!
  \param     pool Collection which will get filled.
  \return    Nothing, but \a pool will be filled.
  \warning   The stack must be dynamic.
  \sa        .

  This function creates the smallest possible pool of file names which might
  be interesting to us. It's important that this pool is of the smallest
  possible size because performance will drop fast if it doesn't.

  The file names in \a pool are sorted according to the
  com::PathName::LessThen strickt weak ordering.

  \todo
     NOTE CEES: the directory-iterator is inefficient, waarom niet:
     for(i=firstStep; i <= lastStep; i++) 
      if (file.exists(makeName(i))
        pool.push_back;
     EN pool.size <= aantal directory entries -> kan ook last verkleinen.
*/
void geo::CSFStackName::stackNamePool(std::vector<com::PathName>& pool) const
{
  PRECOND(isDynamic());

  // Determine and check directory name. If empty, use current working
  // directory.
  com::PathName directoryName;

  if(baseName().hasDirectoryName()) {
    directoryName = baseName().directoryName();
  }
  else {
    directoryName = com::currentWorkingDirectory();
  }

  if(!com::PathInfo(directoryName).isDirectory()) {
    throw com::OpenFileError(directoryName.toString(), "Not a directory");
  }

  // Directory iterator to retrieve file names.
  // Loop over all file names.
  boost::filesystem::directory_iterator end_iter;

  for (boost::filesystem::directory_iterator f(directoryName.path());
       f != end_iter; ++f) {
    if(isMemberOfStack(f->path().filename()))
      pool.push_back(f->path().filename());
  }

  // Sort pool of interesting file names. This sort takes into account that
  // on some platforms file names are case sensitive and on others not.
  std::sort(pool.begin(), pool.end(), com::PathName::LessThen());
}



//! Determines if a file name for time step \a step is in \a pool.
/*!
  \param     pool Pool of file names to search in.
  \param     step Time step to look for.
  \return    true or false.
*/
bool geo::CSFStackName::findTimeStep(std::vector<com::PathName>& pool,
                   size_t& step) const
{
  bool result = false;
  com::PathName::Equals equals;
  com::PathName::LessThen lessThen;

  // File name to search for.
  com::PathName searchName = fileName(step).baseName();

  // If pool.back() < searchName then there's no chance that this
  // or the next file(s) will be found.
  if(lessThen(pool.back(), searchName)) {

    pool.clear();
  }
  else {

    // Check if there's a chance that the pool contains the file name.
    // If searchName < pool.front() than there's no such chance.
    if(!lessThen(searchName, pool.front())) {

      std::vector<com::PathName>::iterator poolIter = pool.begin();

      // Advance through pool.
      while(poolIter != pool.end() && lessThen(*poolIter, searchName)) {
        ++poolIter;
      }

      if(poolIter != pool.end() && equals(searchName, *poolIter)) {
        // Found a match!
        ++poolIter;

        // Remember this time step.
        result = true;
      }

      // Make the pool of file names to search in smaller.
      pool.erase(pool.begin(), poolIter);
    }
  }

  return result;
}



//! Finds the first available time step of the stack.
/*!
  \param     pool Pool of file names to search in.
  \param     step Time step found.
  \return    Nothing, time step found through \a step. If no time step was
             found, then \a step will be 0.
*/
void geo::CSFStackName::firstAvailableTimeStep(
                   std::vector<com::PathName>& pool, size_t& step) const
{
  // Initial setting signals that no file name was found.
  step = 0;

  // Stop this loop as soon as there's no more hope to do something useful!
  for(size_t i = d_data->first; i <= d_data->last; ++i) {

    // Stop if the pool is empty.
    if(pool.empty()) {
      break;
    }

    // Find the time step. This changes pool. Stop if the first time step
    // is found.
    if(findTimeStep(pool, i)) {
      step = i;
      break;
    }
  }
}



//! Finds all available time steps of the stack.
/*!
  \param     pool Pool of file names to search in.
  \param     steps Time steps found.
  \return    Nothing, time steps found through \a steps. If no time steps
             were found, then \a steps will not be changed.
*/
void geo::CSFStackName::availableTimeSteps(std::vector<com::PathName>& pool,
                   std::vector<size_t>& steps) const
{
  PRECOND(steps.empty());

  // The idea:
  // Search for file names in pool. During the search the pool of interesting
  // file names gets smaller and smaller. This is because we search in an
  // orderly fasion in an ordered pool, and every time we find something we
  // can discard all file names which are before and including the found one,
  // before we start searching for a new stack name. Also, before we start
  // the search, we can determine if the search will be hopeful. If not,
  // the search is not performed for that particular file name.

  // Stop this loop as soon as there's no more hope to do something useful!
  for(size_t i = d_data->first; i <= d_data->last; ++i) {

    // Stop if the pool is empty.
    if(pool.empty()) {
      break;
    }

    // Find the time step. This changes pool.
    if(findTimeStep(pool, i)) {
      steps.push_back(i);
    }
  }
}



/*!
  \brief   Scans the stack for available timesteps.
  \warning Scanning the filesystem for existing files is an expensive
           operation. Only call this function the first time or if you think
           that timesteps have been added/deleted to/from the stack.

  This function inspects the filesystem for the existance of files with names
  based on the name given to the constructor.

  For example, if soil0000.001+100 is given to the constructor, this function
  will check soil0000.001, soil0000.002, ... soil0000.099 and soil0000.100 for
  existance.
*/
void geo::CSFStackName::scan()
{
  // Erase current settings.
  d_steps.erase(d_steps.begin(), d_steps.end());

  if(isDynamic()) {

    // Create pool of interesting file names.
    std::vector<com::PathName> pool;
    stackNamePool(pool);

    // Determine which file names in the pool are a member of the stack and
    // remember its time step.
    availableTimeSteps(pool, d_steps);
  }

  d_scanned = true;
}



/*!
  \brief   Returns the number of layers in the stack.
  \return  Number of timesteps in the stack. If the stack is not dynamic, than
           this function returns 1.
  \warning If a stack isDynamic(), then you must call scan() before calling
           this function. Otherwise this function will return 0.
*/
size_t geo::CSFStackName::nrLayers() const
{
  if(isDynamic())
    return d_steps.size();
  else
    return 1;
}



/*!
  \brief   Returns the base name of the stack.
  \return  The base name of the stack.
  \sa      fileName()

  If the stack is dynamic, then the base name is part of the file name that together
  with a timestep nr makes up a map name.
  For example, the base name of soil0000.001+100 is soil0000.
  
  If the stack is not dynamic, then the base name is the file name of the static layer.
  The base name of soil.csf is soil.csf.

  The full path is returned, including leading directory names.

  Note that this definition differs slightly from com::PathName's definition of
  the base name (but not much).
*/
com::PathName geo::CSFStackName::baseName() const
{
  PRECOND(d_data);
  return d_data->d_path;
}

//! Returns the name of the file for the first timestep of the stack.
/*!
  \return    File name.

  In case the stack is dynamic, the name of the file of the first existing
  time step is returned. So then you know it exists.

  In case the stack is static, you cannot be sure that the returned file name
  belongs to an existing file.
*/
com::PathName geo::CSFStackName::fileName() const
{
  com::PathName pn;

  if(!isDynamic()) {
    pn = d_data->d_path;
  }
  else {

    if(scanned()) {
      pn = fileName(*begin());
    }
    else {

      // Create pool of interesting file names.
      std::vector<com::PathName> pool;
      stackNamePool(pool);

      // Determine time step of first file in the pool which is a member of the
      // stack.
      size_t step;
      firstAvailableTimeStep(pool, step);

      if(!step) {
        std::ostringstream s;
        s << "Stack '" << baseName().toString() << "': is empty";
        throw com::Exception(s.str());
      }

      pn = fileName(step);
    }
  }

  return pn;
}

/*!
  \brief   Returns the name of the file for timestep \a t of the stack.
  \param   t Timestep of stack to create file name for.
  \return  Filename of timestep \a t of the stack.
  \warning Don't expect the file with the returned file name to exist. This
           function only does some string manipulation; no existence checking.
  \sa      baseName()

  If the stack is not dynamic, then baseName() is returned.
*/
com::PathName geo::CSFStackName::fileName(size_t t) const
{
  if(isDynamic()) {
    DEVELOP_PRECOND(!d_data->d_path.toString().empty());
    DEVELOP_PRECOND(d_data->d_path.toString().find(".") != d_data->d_path.toString().size() - 1);
    return dal::timeStepPath83(d_data->d_path.path(), t);
  }
  return d_data->d_path;
}



/*!
  \brief   Returns an iterator to the first timestep available.
  \return  An iterator to the first timestep.
  \sa      end()

  If scan() is not called or if there're no timestep available, then this
  function returns end().
*/
geo::CSFStackName::const_iterator geo::CSFStackName::begin() const
{
  return d_steps.begin();
}



/*!
  \brief   Returns an iterator to the one-past-the-last timestep available.
  \return  An iterator to the one-past-the-last timestep.
  \sa      begin()
*/
geo::CSFStackName::const_iterator geo::CSFStackName::end() const
{
  return d_steps.end();
}



/*!
  \brief   Returns true if the stack has been scanned for available layers.
  \return  True if the scan() function has been called.
  \sa      scan()
*/
bool geo::CSFStackName::scanned() const
{
  return d_scanned;
}



/*!
  \brief   Returns true if the stack is dynamic.
  \return  True if the stack is dynamic.
  \warning Don't expect any of the timesteps of the stack to exist. This
           function only inspects the name given to the constructor.

  A stack is dynamic if, according to the name given to the constructor,
  there's possibly more than one timestep available in the stack. For example,
  soil0000.001 is a static stack, soil0000.001+1 and soil0000.001+100 are
  dynamic and soil.csf is static.
*/
bool geo::CSFStackName::isDynamic() const
{
  return d_data->dynamic;
}



//! Returns true if data for time step \a t is available.
/*!
  \param     t Time step.
  \return    true if data for \a t exists.
*/
bool geo::CSFStackName::isAvailable(size_t t) const
{
  PRECOND(scanned());

  if(!isDynamic()) {
    return true;
  }
  else {
    return std::find(d_steps.begin(), d_steps.end(), t) != d_steps.end();
  }
}



//------------------------------------------------------------------------------
// IMPLEMENTATION OF FREE OPERATORS
//------------------------------------------------------------------------------

#ifdef DEBUG_DEVELOP
namespace geo {
std::ostream &operator<<(std::ostream &s, const CSFStackName &n)
{
  PRECOND(n.d_data);

  s << n.d_data->d_path.baseName() << '\n'
    << "  directory         " << n.d_data->d_path.directoryName() << '\n';
  if(n.d_data->dynamic)
  {
    s << "  dynamic stack\n";
    if(n.d_scanned)
    {
      s << "  first timestep:   " << n.d_data->first << '\n'
        << "  last timestep :   " << n.d_data->last << '\n'
        << "  nr available  :   " << n.d_steps.size() << '\n';
    }
    else
    {
      s << "  first timestep:   unknown\n"
        << "  last timestep :   unknown\n";
    }
  }
  else
  {
    s << "  static stack\n";
  }

  s << std::flush;
  return s;
}
}
#endif



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------
