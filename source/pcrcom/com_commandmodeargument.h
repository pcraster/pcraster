#ifndef INCLUDED_COM_COMMANDMODEARGUMENT
#define INCLUDED_COM_COMMANDMODEARGUMENT

#include "stddefx.h"
#include "com_commandlineargument.h"
#include "com_commandlinearguments.h"

#include <string>



namespace com {
  // CommandModeArgument declarations.
}



namespace com {

//! This class is for command mode command line arguments.
/*!
  A command mode is a state in which a command is started. Having command
  modes is similar to having more than one command. An advantage of having
  one executable supporting different command modes is that only one
  executable needs to be distributed and that it is logical to group very
  related, but differing, functionality into one command.

  Different command modes need to support possibly totally different sets
  of command line arguments. That's what this class is for. This class is
  comparable to the CommandLine class except for e.g. the command
  name / version stuff.

  For the end user it works as follows:
  \code

  # Command goforit supports two command modes: calculate and analyze. Each
  # of these modes has it own, differing, set of command line arguments.

  # Calculate something.
  $ goforit --calculate file1 34 file2 --fast --optimize-level 9

  # Analyze something.
  $ goforit --analyze file1 file2 file3

  \endcode
*/
class CommandModeArgument: public ValueArgument<CommandLineArguments, ArgumentParser<std::string> >,
                           public Option
{

private:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CommandModeArgument (char character,
                                        const std::string& name,
                                        const std::string& valueDescription,
                                        const std::string& description,
                                        bool isRequired);

                   CommandModeArgument (const std::string& name,
                                        const std::string& valueDescription,
                                        const std::string& description,
                                        bool isRequired);

  /* virtual */    ~CommandModeArgument() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setArguments        (const CommandLineArguments& arguments);

  size_t           parse               (size_t argc,
                                        char* const* argv) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isParsed            () const override;

  void             printSynopsis       (std::ostream& stream) const override;

  void             printDescription    (std::ostream& stream,
                                        size_t offset,
                                        size_t width) const override;

  void             check               () const override;

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
