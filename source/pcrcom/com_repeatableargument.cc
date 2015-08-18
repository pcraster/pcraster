#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_REPEATABLEARGUMENT
#include "com_repeatableargument.h"
#define INCLUDED_COM_REPEATABLEARGUMENT
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the RepeatableArgument class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class RepeatableArgumentPrivate
{
public:

  RepeatableArgumentPrivate()
  {
  }

  ~RepeatableArgumentPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC REPEATABLEARGUMENT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF REPEATABLEARGUMENT MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     description Description of argument.
  \param     isRequired If argument is required or not.
  \param     argument Fully configured example argument.
  \param     inserter Back inserter used to write parsed arguments to an
             external collection.

  The \a argument is used to create new ArgumentType objects for parsing.
*/
template<class ArgumentType, class CollectionType>
com::RepeatableArgument<ArgumentType, CollectionType>::RepeatableArgument(
                   const std::string& description, bool isRequired,
                   const ArgumentType& argument,
                   const std::back_insert_iterator<CollectionType>& inserter)

  : CommandLineArgument(description, isRequired),
    d_argument(argument), d_inserter(inserter), d_nrParsedArguments(0)

{
}



//! Destructor.
/*!
*/
template<class ArgumentType, class CollectionType>
com::RepeatableArgument<ArgumentType, CollectionType>::~RepeatableArgument()
{
}



template<class ArgumentType, class CollectionType>
size_t com::RepeatableArgument<ArgumentType, CollectionType>::parse(
                   size_t argc, char* const* argv)
{
  size_t totalNrTokensParsed = 0;
  size_t nrTokensParsed;

  // Parse as many arguments as possible.
  while(1) {

    // Stop if we're out of arguments.
    if(totalNrTokensParsed == argc) {
      break;
    }

    // Create new argument, based on the template, to handle the parsing.
    ArgumentType argument(d_argument);
    PRECOND(!argument.isParsed());

    // Stop if we cannot parse the next argument.
    if(!argument.canParse(*(argv + totalNrTokensParsed))) {
      break;
    }

    // Parse the next argument and skip arguments already parsed.
    nrTokensParsed = argument.parse(argc - totalNrTokensParsed,
                   argv + totalNrTokensParsed);

    // This function should only be called if canParse(...) returns true.
    POSTCOND(nrTokensParsed > 0);
    totalNrTokensParsed += nrTokensParsed;

    // Output the parsed argument.
    *d_inserter = argument;
  }

  // Keep track of number of tokens parsed in this function.
  d_nrParsedArguments += totalNrTokensParsed;

  return totalNrTokensParsed;
}



template<class ArgumentType, class CollectionType>
bool com::RepeatableArgument<ArgumentType, CollectionType>::isParsed() const
{
  return d_nrParsedArguments > 0;
}



template<class ArgumentType, class CollectionType>
bool com::RepeatableArgument<ArgumentType, CollectionType>::canParse(
                   const char* token) const
{
  return d_argument.canParse(token);
}



template<class ArgumentType, class CollectionType>
void com::RepeatableArgument<ArgumentType, CollectionType>::check() const
{
  CommandLineArgument::check();

  // Mmm, how do we do this? We don't have access to the actual (parsed)
  // arguments.
}



template<class ArgumentType, class CollectionType>
void com::RepeatableArgument<ArgumentType, CollectionType>::printSynopsis(
                   std::ostream& stream) const
{
  if(!isRequired()) {
    stream << '[';
  }

  d_argument.printSynopsis(stream);

  if(!isRequired()) {
    stream << ']';
  }

  stream << "*";
}



template<class ArgumentType, class CollectionType>
void com::RepeatableArgument<ArgumentType, CollectionType>::printDescription(
                   std::ostream& stream,
                   size_t offset, size_t width) const
{
  stream << std::string(offset, ' ')
         << description() << std::string(":\n");
  d_argument.printDescription(stream, offset, width);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

