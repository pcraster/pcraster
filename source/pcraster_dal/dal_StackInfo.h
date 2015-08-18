#ifndef INCLUDED_DAL_STACKINFO
#define INCLUDED_DAL_STACKINFO



// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

// PCRaster library headers.

// Module headers.



namespace dal {
  // StackInfo declarations.
}



namespace dal {



//! Class for keeping track of the names of a stack of data.
/*!
  Stacks of data which are stored in individual files have filenaming rules
  which make it possible to name the stack by a short name and determine
  which individual files are part of the stack. This class does all the
  string magick and file searching needed to determine which files are part
  of a stack with a certain name and what their filenames are.
*/
class StackInfo
{

  friend class StackInfoTest;

private:

  //! First step, given or scan() ed.
  size_t           d_first;

  //! Last step in the stack, given or scan() ed.
  size_t           d_last;

  //! Stack is scanned for available steps or not.
  bool             d_isScanned;

  //! Contains the steps which the stack contains. Valid after scan() ing.
  std::vector<size_t> d_steps;

  //! Name of the stack.
  boost::filesystem::path d_name;

  void             checkFirstLastOrder () const;

  void             wrongFormatIf       (bool test) const;

  bool             isMemberOfStack     (boost::filesystem::path const& path) const;

  size_t           step                (boost::filesystem::path const& path) const;

  size_t           startOfStep         (std::string const& name) const;

public:

  //! Type of the iterator for timesteps.
  typedef std::vector<size_t>::const_iterator const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   StackInfo           (std::string const& name,
                                        bool scan = true);

  /* virtual */    ~StackInfo          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             scan                ();

  void             scanFirst           ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           first               () const;

  size_t           last                () const;

  size_t           size                () const;

  const_iterator   begin               () const;

  const_iterator   end                 () const;

  std::string      name                () const;

  boost::filesystem::path const& filename() const;

  boost::filesystem::path filename     (size_t item) const;

  bool             isScanned           () const;

  bool             isDynamic           () const;

  bool             contains            (size_t item) const;

  std::string      toString            () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (StackInfo& lhs,
                                        StackInfo& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
