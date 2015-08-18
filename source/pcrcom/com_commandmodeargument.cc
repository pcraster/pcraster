#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_COMMANDMODEARGUMENT
#include "com_commandmodeargument.h"
#define INCLUDED_COM_COMMANDMODEARGUMENT
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the CommandMode class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class CommandModeArgumentPrivate
{
public:

  CommandModeArgumentPrivate()
  {
  }

  ~CommandModeArgumentPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMANDMODE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF COMMANDMODE MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     character Character of option.
  \param     name Name of option.
  \param     valueDescription Description of value.
  \param     description Description of option.
  \param     isRequired Whether the argument is required or not.
*/
com::CommandModeArgument::CommandModeArgument(
         char character, const std::string& name,
         const std::string& valueDescription,
         const std::string& description, bool isRequired)

  : ValueArgument<CommandLineArguments, ArgumentParser<std::string> >(valueDescription),
    Option(character, name, description, isRequired)

{
}



//! Constructor.
/*!
  \param     name Name of option.
  \param     valueDescription Description of value.
  \param     description Description of option.
  \param     isRequired Whether the argument is required or not.
*/
com::CommandModeArgument::CommandModeArgument(
         const std::string& name,
         const std::string& valueDescription,
         const std::string& description, bool isRequired)

  : ValueArgument<CommandLineArguments, ArgumentParser<std::string> >(valueDescription),
    Option(name, description, isRequired)

{
}



//! Destructor.
/*!
*/
com::CommandModeArgument::~CommandModeArgument()
{
}



//! Sets the arguments of the command mode to \a arguments.
/*!
  \param     arguments Supported arguments.
*/
void com::CommandModeArgument::setArguments(
         const CommandLineArguments& arguments)
{
  setDefaultValue(arguments);
}



size_t com::CommandModeArgument::parse(size_t argc, char* const* argv)
{
  if(com::Option::parse(argc, argv) && argc >= 2) {

    // Let command line arguments parse themselves.
    CommandLineArguments arguments(value());

    // Tries to parse rest of command line. Skip option name/character.
    arguments.parse(argc - 1, argv + 1);
    setValue(arguments);
  }

  // All arguments are parsed, else an exception would be thrown.
  return argc;
}



void com::CommandModeArgument::check() const
{
  // If the user tries to use the command mode, it should provide the right
  // arguments.
  if(Option::isParsed()) {
    value().check();
  }

  Option::check();
}



bool com::CommandModeArgument::isParsed() const
{
  return Option::isParsed();
}



void com::CommandModeArgument::printSynopsis(std::ostream& stream) const
{
  Option::printSynopsis(stream);
  value().printSynopsis(stream);
}



void com::CommandModeArgument::printDescription(std::ostream& stream,
                   size_t offset, size_t width) const
{
  Option::printDescription(stream, offset, width);
  value().printDescription(stream, offset, width);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



