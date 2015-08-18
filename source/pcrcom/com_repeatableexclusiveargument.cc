#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_REPEATABLEEXCLUSIVEARGUMENT
#include "com_repeatableexclusiveargument.h"
#define INCLUDED_COM_REPEATABLEEXCLUSIVEARGUMENT
#endif

// Library headers.
#ifndef INCLUDED_UTILITY
#include <utility>
#define INCLUDED_UTILITY
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the RepeatableExclusiveArgument class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class RepeatableExclusiveArgumentPrivate
{
public:

  RepeatableExclusiveArgumentPrivate()
  {
  }

  ~RepeatableExclusiveArgumentPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC REPEATABLEEXCLUSIVEARGUMENT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF REPEATABLEEXCLUSIVEARGUMENT MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     description Description of argument.
  \param     isRequired Whether the argument is required or not.
  \param     argument1 Argument to add.
  \param     inserter1 Corresponding back inserter of argument1 objects to add.
  \param     argument2 Argument to add.
  \param     inserter2 Corresponding back inserter of argument2 objects to add.
*/
template<class ArgumentType, class CollectionType>
com::RepeatableExclusiveArgument<ArgumentType, CollectionType>::
         RepeatableExclusiveArgument(
              const std::string& description, bool isRequired,
              const ArgumentType& argument1, const back_inserter& inserter1,
              const ArgumentType& argument2, const back_inserter& inserter2)

  : CommandLineArgument(description, isRequired),
    d_nrParsedArguments(0)

{
  addArgument(argument1, inserter1);
  addArgument(argument2, inserter2);
}



//! Constructor.
/*!
  \param     description Description of argument.
  \param     isRequired Whether the argument is required or not.
  \param     argument1 Argument to add.
  \param     inserter1 Corresponding back inserter of argument1 objects to add.
  \param     argument2 Argument to add.
  \param     inserter2 Corresponding back inserter of argument2 objects to add.
  \param     argument3 Argument to add.
  \param     inserter3 Corresponding back inserter of argument3 objects to add.
*/
template<class ArgumentType, class CollectionType>
com::RepeatableExclusiveArgument<ArgumentType, CollectionType>::
         RepeatableExclusiveArgument(
              const std::string& description, bool isRequired,
              const ArgumentType& argument1, const back_inserter& inserter1,
              const ArgumentType& argument2, const back_inserter& inserter2,
              const ArgumentType& argument3, const back_inserter& inserter3)

  : CommandLineArgument(description, isRequired),
    d_nrParsedArguments(0)

{
  addArgument(argument1, inserter1);
  addArgument(argument2, inserter2);
  addArgument(argument3, inserter3);
}



//! Constructor.
/*!
  \param     description Description of argument.
  \param     isRequired Whether the argument is required or not.
  \param     argument1 Argument to add.
  \param     inserter1 Corresponding back inserter of argument1 objects to add.
  \param     argument2 Argument to add.
  \param     inserter2 Corresponding back inserter of argument2 objects to add.
  \param     argument3 Argument to add.
  \param     inserter3 Corresponding back inserter of argument3 objects to add.
  \param     argument4 Argument to add.
  \param     inserter4 Corresponding back inserter of argument4 objects to add.
*/
template<class ArgumentType, class CollectionType>
com::RepeatableExclusiveArgument<ArgumentType, CollectionType>::
         RepeatableExclusiveArgument(
              const std::string& description, bool isRequired,
              const ArgumentType& argument1, const back_inserter& inserter1,
              const ArgumentType& argument2, const back_inserter& inserter2,
              const ArgumentType& argument3, const back_inserter& inserter3,
              const ArgumentType& argument4, const back_inserter& inserter4)

  : CommandLineArgument(description, isRequired),
    d_nrParsedArguments(0)

{
  addArgument(argument1, inserter1);
  addArgument(argument2, inserter2);
  addArgument(argument3, inserter3);
  addArgument(argument4, inserter4);
}



//! Destructor.
/*!
*/
template<class ArgumentType, class CollectionType>
com::RepeatableExclusiveArgument<ArgumentType, CollectionType>::
         ~RepeatableExclusiveArgument()
{
}



//! Adds a argument and inserter pair to the collection.
/*!
  \param     argument Argument to add.
  \param     inserter Back inserter of \a argument objects to add.
*/
template<class ArgumentType, class CollectionType>
void com::RepeatableExclusiveArgument<ArgumentType, CollectionType>::
         addArgument(
                   const ArgumentType& argument, const back_inserter& inserter)
{
  d_inserters.push_back(std::make_pair(argument, inserter));
}



template<class ArgumentType, class CollectionType>
size_t com::RepeatableExclusiveArgument<ArgumentType, CollectionType>::
         parse(size_t argc, char* const* argv)
{
  size_t totalNrTokensParsed = 0;
  size_t nrTokensParsed;

  // Parse as many arguments as possible.
  while(1) {

    // Stop if we're out of arguments.
    if(totalNrTokensParsed == argc) {
      break;
    }

    nrTokensParsed = 0;

    // Try all arguments to parse the current token.
    for(typename Inserters::iterator it = d_inserters.begin();
         it != d_inserters.end(); ++it) {

      // Create new argument, based on the template, to handle the parsing.
      ArgumentType argument((*it).first);
      PRECOND(!argument.isParsed());

      // Skip if we cannot parse the next token with the current argument.
      if(!argument.canParse(*(argv + totalNrTokensParsed))) {
        continue;
      }

      // Parse the next argument and skip arguments already parsed.
      nrTokensParsed = argument.parse(argc - totalNrTokensParsed,
                   argv + totalNrTokensParsed);

      // This function should only be called if canParse(...) returns true.
      POSTCOND(nrTokensParsed > 0);
      totalNrTokensParsed += nrTokensParsed;

      // Output the parsed argument, using the right inserter.
      (*((*it).second)) = argument;

      // Stop parsing the current token.
      break;
    }

    // Stop if none of the arguments can parse the current token.
    if(nrTokensParsed == 0) {
      break;
    }
  }

  // Keep track of number of tokens parsed in this function.
  d_nrParsedArguments += totalNrTokensParsed;

  return totalNrTokensParsed;
}



template<class ArgumentType, class CollectionType>
bool com::RepeatableExclusiveArgument<ArgumentType, CollectionType>::
         isParsed() const
{
  return d_nrParsedArguments > 0;
}



template<class ArgumentType, class CollectionType>
bool com::RepeatableExclusiveArgument<ArgumentType, CollectionType>::
         canParse(const char* token) const
{
  // See if one of the possible arguments can parse the token.
  for(typename Inserters::const_iterator it = d_inserters.begin();
         it != d_inserters.end(); ++it) {
    if((*it).first.canParse(token)) {
      return true;
    }
  }

  return false;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      If required: add +, else *. Change to (bla | bli | blo)* instead of
             (bla) | (bli) | (blo)
*/
template<class ArgumentType, class CollectionType>
void com::RepeatableExclusiveArgument<ArgumentType, CollectionType>::
         printSynopsis(std::ostream& stream) const
{
  if(!d_inserters.empty()) {

    if(d_inserters.size() > 1) {
      for(typename Inserters::const_iterator it = d_inserters.begin();
                   it != d_inserters.end() - 1; ++it) {
        stream << " (";
        (*it).first.printSynopsis(stream);
        stream << ") | ";
      }
    }

    stream << " (";
    d_inserters.back().first.printSynopsis(stream);
    stream << ")";
  }
}



template<class ArgumentType, class CollectionType>
void com::RepeatableExclusiveArgument<ArgumentType, CollectionType>::
         printDescription(std::ostream& stream,
                   size_t offset, size_t width) const
{
  stream << std::string(offset, ' ')
         << description() << std::string(":\n");

  for(typename Inserters::const_iterator it = d_inserters.begin();
         it != d_inserters.end(); ++it) {
    stream << std::string(offset, ' ');
    (*it).first.printDescription(stream, offset, width);
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



