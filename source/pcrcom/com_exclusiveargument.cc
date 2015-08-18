#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_EXCLUSIVEARGUMENT
#include "com_exclusiveargument.h"
#define INCLUDED_COM_EXCLUSIVEARGUMENT
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ExclusiveArgument class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class ExclusiveArgumentPrivate
{
public:

  ExclusiveArgumentPrivate()
  {
  }

  ~ExclusiveArgumentPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC EXCLUSIVEARGUMENT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF EXCLUSIVEARGUMENT MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     description Description of the nested arguments.
  \param     isRequired Whether the arguments is required or not.
*/
template<class ArgumentType>
com::ExclusiveArgument<ArgumentType>::ExclusiveArgument(
                   const std::string& description, bool isRequired)

  : CommandLineArgument(description, isRequired)

{
}



//! Constructor.
/*!
  \param     description Description of the nested arguments.
  \param     isRequired Whether the arguments is required or not.
  \param     argument Command line argument to add.
*/
template<class ArgumentType>
com::ExclusiveArgument<ArgumentType>::ExclusiveArgument(
                   const std::string& description, bool isRequired,
                   ArgumentType* argument)

  : CommandLineArgument(description, isRequired)

{
  addArgument(argument);
}



//! Constructor.
/*!
  \param     description Description of the nested arguments.
  \param     isRequired Whether the arguments is required or not.
  \param     argument1 Command line argument to add.
  \param     argument2 Command line argument to add.
*/
template<class ArgumentType>
com::ExclusiveArgument<ArgumentType>::ExclusiveArgument(
                   const std::string& description, bool isRequired,
                   ArgumentType* argument1, ArgumentType* argument2)

  : CommandLineArgument(description, isRequired)

{
  addArgument(argument1);
  addArgument(argument2);
}



//! Constructor.
/*!
  \param     description Description of the nested arguments.
  \param     isRequired Whether the arguments is required or not.
  \param     argument1 Command line argument to add.
  \param     argument2 Command line argument to add.
  \param     argument3 Command line argument to add.
*/
template<class ArgumentType>
com::ExclusiveArgument<ArgumentType>::ExclusiveArgument(
                   const std::string& description, bool isRequired,
                   ArgumentType* argument1, ArgumentType* argument2,
                   ArgumentType* argument3)

  : CommandLineArgument(description, isRequired)

{
  addArgument(argument1);
  addArgument(argument2);
  addArgument(argument3);
}



//! Destructor.
/*!
*/
template<class ArgumentType>
com::ExclusiveArgument<ArgumentType>::~ExclusiveArgument()
{
}



//! Adds an argument to the list of nested arguments.
/*!
  \param     argument Argument to add.
  \warning   \a argument should not be 0.
*/
template<class ArgumentType>
void com::ExclusiveArgument<ArgumentType>::addArgument(ArgumentType* argument)
{
  PRECOND(argument);

  com::addArgument(d_arguments, argument);
}



template<class ArgumentType>
size_t com::ExclusiveArgument<ArgumentType>::parse(
                   size_t argc, char* const* argv)
{
  size_t nrTokensParsed = 0;

  if(!d_arguments.empty()) {

    // See if one of the possible arguments can parse the token.
    for(typename std::vector<ArgumentType*>::iterator it = d_arguments.begin();
         it != d_arguments.end(); ++it) {
      if((*it)->canParse(argv[0])) {
        nrTokensParsed = (*it)->parse(argc, argv);
        POSTCOND(nrTokensParsed > 0);
        break;
      }
    }
  }

  return nrTokensParsed;
}



template<class ArgumentType>
bool com::ExclusiveArgument<ArgumentType>::isParsed() const
{
  // See if one of the possible arguments is parsed.
  for(typename std::vector<ArgumentType*>::const_iterator it =
         d_arguments.begin(); it != d_arguments.end(); ++it) {
    if((*it)->isParsed()) {
      return true;
    }
  }

  return false;
}



template<class ArgumentType>
bool com::ExclusiveArgument<ArgumentType>::canParse(const char* token) const
{
  // See if one of the possible arguments can parse the token.
  for(typename std::vector<ArgumentType*>::const_iterator it =
         d_arguments.begin(); it != d_arguments.end(); ++it) {
    if((*it)->canParse(token)) {
      return true;
    }
  }

  return false;
}



template<class ArgumentType>
void com::ExclusiveArgument<ArgumentType>::check() const
{
  // The first argument which is parsed is checked.
  for(typename std::vector<ArgumentType*>::const_iterator it =
         d_arguments.begin(); it != d_arguments.end(); ++it) {
    if((*it)->isParsed()) {
      (*it)->check();
      break;
    }
  }

  CommandLineArgument::check();
}



/*
template<class ArgumentType>
const ArgumentType& com::ExclusiveArgument<ArgumentType>::value() const
{
  std::vector<ArgumentType*>::const_iterator it = d_arguments.end();

  for(it = d_arguments.begin(); it != d_arguments.end(); ++it) {
    if((*it)->isParsed()) {
      break;
    }
  }

  POSTCOND(it != d_arguments.end());
  return **it;
}
*/



template<class ArgumentType>
void com::ExclusiveArgument<ArgumentType>::printSynopsis(
                   std::ostream& stream) const
{
  if(!d_arguments.empty()) {

    if(d_arguments.size() > 1) {
      for(typename std::vector<ArgumentType*>::const_iterator it =
              d_arguments.begin(); it != d_arguments.end() - 1; ++it) {
        stream << "(";
        (*it)->printSynopsis(stream);
        stream << ") | ";
      }
    }

    stream << "(";
    d_arguments.back()->printSynopsis(stream);
    stream << ")";
  }
}



template<class ArgumentType>
void com::ExclusiveArgument<ArgumentType>::printDescription(
                   std::ostream& stream,
                   size_t offset, size_t width) const
{
  stream << std::string(offset, ' ')
         << description() << std::string(":\n");

  for(typename std::vector<ArgumentType*>::const_iterator it =
         d_arguments.begin(); it != d_arguments.end(); ++it) {
    stream << std::string(offset, ' ');
    (*it)->printDescription(stream, offset, width);
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

