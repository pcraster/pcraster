#ifndef INCLUDED_COM_EXCLUSIVEARGUMENT
#define INCLUDED_COM_EXCLUSIVEARGUMENT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_COMMANDLINEARGUMENT
#include "com_commandlineargument.h"
#define INCLUDED_COM_COMMANDLINEARGUMENT
#endif



namespace com {
  // ExclusiveArgument declarations.
}



namespace com {



//! An exclusive argument is a set of arguments which exclude each other.
/*!
  For example, a command could support the options -a and -b but they shouldn't
  appear both on the command line:

  \code
  $ doit -a        # OK
  $ doit -b        # OK
  $ doit -a -b     # NOT OK
  $ doit           # Depends on the isRequired constructor argument.
  \endcode

  This class encapsulates behaviour but doesn't do any command line argument
  parsing itself. It is filled with arguments (through the constructors or
  using addArgument(ArgumentType*) which do the actual testing and parsing.
*/
template<class ArgumentType>
class ExclusiveArgument: public CommandLineArgument
{

private:

  //! Possible command line arguments.
  std::vector<ArgumentType*> d_arguments;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ExclusiveArgument   (const std::string& description,
                                        bool isRequired);

                   ExclusiveArgument   (const std::string& description,
                                        bool isRequired,
                                        ArgumentType* argument);

                   ExclusiveArgument   (const std::string& description,
                                        bool isRequired,
                                        ArgumentType* argument1,
                                        ArgumentType* argument2);

                   ExclusiveArgument   (const std::string& description,
                                        bool isRequired,
                                        ArgumentType* argument1,
                                        ArgumentType* argument2,
                                        ArgumentType* argument3);

  /* virtual */    ~ExclusiveArgument  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addArgument         (ArgumentType* argument);

  size_t           parse               (size_t argc,
                                        char* const* argv);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isParsed            () const;

  bool             canParse            (const char* token) const;

  void             check               () const;

/*
  const ArgumentType& value            () const;
*/

  void             printSynopsis       (std::ostream& stream) const;

  void             printDescription    (std::ostream& stream,
                                        size_t offset,
                                        size_t width) const;

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
