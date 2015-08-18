#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_COMMANDLINEARGUMENTS
#include "com_commandlinearguments.h"
#define INCLUDED_COM_COMMANDLINEARGUMENTS
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_COMMANDLINEARGUMENT
#include "com_commandlineargument.h"
#define INCLUDED_COM_COMMANDLINEARGUMENT
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif



/*!
  \file
  This file contains the implementation of the CommandLineArguments class.
*/



//------------------------------------------------------------------------------

namespace com {

struct IsWillingToParse:
                   public std::unary_function<CommandLineArgument*, bool>
{
  const char*      d_token;

  IsWillingToParse(const char* token)
    : std::unary_function<CommandLineArgument*, bool>(),
      d_token(token)
  {
    POSTCOND(token);
  }

  bool operator()(CommandLineArgument* arg) {
    PRECOND(arg);
    return arg->canParse(d_token);
  }

};

}



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMANDLINEARGUMENTS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF COMMANDLINEARGUMENTS MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
*/
com::CommandLineArguments::CommandLineArguments()
{
}



//! Copy constructor.
/*!
  \param     arguments Object to copy.
*/
com::CommandLineArguments::CommandLineArguments(
                   const CommandLineArguments& arguments)

  : d_arguments(arguments.d_arguments)

{
}



//! Destructor.
/*!
*/
com::CommandLineArguments::~CommandLineArguments()
{
}



//! Assignment operator.
/*!
  \param     rhs Object to assign from.

  Shallow copy of CommandLineArgument's.
*/
com::CommandLineArguments& com::CommandLineArguments::operator=(
                   const CommandLineArguments& rhs)
{
  if(this != &rhs) {
    d_arguments = rhs.d_arguments;
  }

  return *this;
}



bool com::CommandLineArguments::isDefaultArgument(
         const CommandLineArgument* arg) const
{
//  this generates a compile error on bcc32:
//  return d_defaultArguments.find(arg) != d_defaultArguments.end();

  DefaultArguments::const_iterator pos(d_defaultArguments.find(arg));
  return pos != d_defaultArguments.end();
}



//! Adds \a arg to the collection.
/*!
  \param     arg Object to add.
  \warning   Make sure \a arg stays alive! Only the pointer is copied.
*/
void com::CommandLineArguments::add(CommandLineArgument *argument,
         bool isDefaultArgument)
{
  PRECOND(argument);

  addArgument(d_arguments, argument);

  if(isDefaultArgument) {
    PRECOND(d_defaultArguments.find(argument) == d_defaultArguments.end());
    d_defaultArguments.insert(argument);
  }
}



//! Tries to parse the \a argc arguments in \a argv.
/*!
  \param     argc Argument count.
  \param     argv Argument vector.
*/
void com::CommandLineArguments::parse(size_t argc, char* const* argv)
{
  size_t nrTokensParsed;
  iterator it;

  for(size_t i = 0; i < argc; i += nrTokensParsed) {

    // Find first argument who's willing to parse.
    IsWillingToParse f(argv[i]);
    it = std::find_if(d_arguments.begin(), d_arguments.end(), f);

    // No argument wants to parse.
    if(it == d_arguments.end()) {
      if(com::CommandLineArgument::isOption(argv[i])) {
        std::ostringstream stream;
        stream << "Command line argument '" << argv[i]
               << "': Invalid option";
        throw com::CommandLineException(stream.str());
      }
      else {
        std::ostringstream stream;
        stream << "Command line argument '" << argv[i]
               << "': Argument too much";
        throw com::CommandLineException(stream.str());
      }
    }

    try {
      nrTokensParsed = (*it)->parse(static_cast<size_t>(argc - i),
                 argv + i);
    }
    catch(com::Exception& exception) {
      std::ostringstream stream;
      stream << "Command line argument '" << *(argv + i)
             << "': Error";
      exception.prepend(stream.str());
      throw com::CommandLineException(exception);
    }
    catch(std::range_error& /* e */) {
      std::ostringstream stream;
      stream << "Command line argument '" << *(argv + i)
             << "': Range error";
      throw com::CommandLineException(stream.str());
    }
  }
}



//! Checks if all required arguments have been parsed.
/*!
  \exception CommandLineException If a required argument is missing.
*/
void com::CommandLineArguments::check() const
{
  for(const_iterator it = d_arguments.begin(); it != d_arguments.end(); ++it) {
    (*it)->check();
  }
}



void com::CommandLineArguments::printSynopsis(std::ostream& stream) const
{
  for(const_iterator it = d_arguments.begin(); it != d_arguments.end(); ++it) {
    if(!isDefaultArgument(*it)) {
      stream << ' ';
      if(!(*it)->isRequired()) {
        stream << '[';
      }
      (*it)->printSynopsis(stream);
      if(!(*it)->isRequired()) {
        stream << ']';
      }
    }
  }

/*
  // Non-positionals.
  for(const_iterator it = d_arguments.begin(); it != d_arguments.end(); ++it) {
    if(!(*it)->isPositional()) {
      stream << ' ';
      if(!(*it)->isRequired()) {
        stream << '[';
      }
      (*it)->printSynopsis(stream);
      if(!(*it)->isRequired()) {
        stream << ']';
      }
    }
  }

  // Positionals.
  for(const_iterator it = d_arguments.begin(); it != d_arguments.end(); ++it) {
    if((*it)->isPositional()) {
      stream << ' ';
      if(!(*it)->isRequired()) {
        stream << '[';
      }
      (*it)->printSynopsis(stream);
      if(!(*it)->isRequired()) {
        stream << ']';
      }
    }
  }
*/
}



void com::CommandLineArguments::printDescription(std::ostream& stream,
                   size_t offset, size_t width) const
{
/*
  // Non-positionals.
  for(const_iterator it = d_arguments.begin(); it != d_arguments.end(); ++it) {
    if(!(*it)->isPositional()) {
      (*it)->printDescription(stream, offset, width);
    }
  }

  // Positionals.
  for(const_iterator it = d_arguments.begin(); it != d_arguments.end(); ++it) {
    if((*it)->isPositional()) {
      (*it)->printDescription(stream, offset, width);
    }
  }
*/

  // Non-default arguments.
  for(const_iterator it = d_arguments.begin(); it != d_arguments.end(); ++it) {
    if(!isDefaultArgument(*it)) {
      (*it)->printDescription(stream, offset, width);
    }
  }

  // Default arguments.
  bool firstDefaultArgument = true;
  for(const_iterator it = d_arguments.begin(); it != d_arguments.end(); ++it) {
    if(isDefaultArgument(*it)) {
      if(firstDefaultArgument) {
        // Only print this if the object has default arguments.
        stream << std::string("\nDefault arguments:\n");
        firstDefaultArgument = false;
      }
      (*it)->printDescription(stream, offset, width);
    }
  }
}



com::CommandLineArguments::const_iterator
com::CommandLineArguments::begin() const
{
  return d_arguments.begin();
}



com::CommandLineArguments::const_iterator
com::CommandLineArguments::end() const
{
  return d_arguments.end();
}



void com::CommandLineArguments::clear()
{
  d_arguments.clear();
  d_defaultArguments.clear();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



