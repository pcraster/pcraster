#ifndef INCLUDED_COM_COMMANDLINEARGUMENTS
#define INCLUDED_COM_COMMANDLINEARGUMENTS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.

// Module headers.



namespace com {
  // CommandLineArguments declarations.
  class CommandLineArgument;
}



namespace com {



//! This class is for handling collections of command line arguments.
/*!
  Command line arguments can be add()ed to this collection, they can be
  pars()ed, and afterwards check()ed.
*/
class CommandLineArguments
{

  friend class CommandLine;

private:

  //! Vector with the arguments.
  std::vector<CommandLineArgument*> d_arguments;

  typedef std::set<const CommandLineArgument*> DefaultArguments;

  //! Set with addresses of default arguments. These are also in \a d_arguments.
  DefaultArguments d_defaultArguments;

  bool             isDefaultArgument   (const CommandLineArgument* arg) const;

  void             clear               ();

public:

  typedef std::vector<CommandLineArgument*>::iterator iterator;

  typedef std::vector<CommandLineArgument*>::const_iterator const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CommandLineArguments();

  /* virtual */    ~CommandLineArguments();

                   CommandLineArguments(const CommandLineArguments& arguments);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  CommandLineArguments& operator=      (const CommandLineArguments& rhs);

  void             add                 (CommandLineArgument *argument,
                                        bool isDefaultArgument = false);

  void             parse               (size_t argc,
                                        char* const* argv);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const_iterator   begin               () const;

  const_iterator   end                 () const;

  void             check               () const;

  void             printSynopsis       (std::ostream& stream) const;

  void             printDescription    (std::ostream& stream,
                                        size_t offset,
                                        size_t width) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

/*
CommandLineArguments::const_iterator CommandLineArguments::begin() const
{
  return d_arguments.begin();
}



CommandLineArguments::const_iterator CommandLineArguments::end() const
{
  return d_arguments.end();
}
*/



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
