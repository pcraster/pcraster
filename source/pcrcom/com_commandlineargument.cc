#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_COMMANDLINEARGUMENT
#include "com_commandlineargument.h"
#define INCLUDED_COM_COMMANDLINEARGUMENT
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_IOMANIP
#include <iomanip>
#define INCLUDED_IOMANIP
#endif

#ifndef INCLUDED_CCTYPE
#include <cctype>
#define INCLUDED_CCTYPE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"
#define INCLUDED_COM_STRCONV
#endif



/*!
  \file
  This file contains the implementation of the CommandLineArgument class.
*/



//------------------------------------------------------------------------------

//! Returns true if \a token can be considered as a value.
/*!
  \param     token Token to check.
  \return    true or false.
  \sa        isOption(const char*)

  A token is a value or an option.
*/
bool com::CommandLineArgument::isValue(const char* token)
{
  return !isOption(token);
}



//! Returns true if \a token can be considered as an option.
/*!
  \param     token Token to check.
  \return    true or false.
  \sa        isValue(const char*), isShortOption(const char*),
             isLongOption(const char*)

  A token is a value or an option.

  A token is an option if isShortOption(const char*) or
  isLongOption(const char*) returns true.
*/
bool com::CommandLineArgument::isOption(const char* token)
{
  return isShortOption(token) || isLongOption(token);
}



//! Returns true if \a token is an option character.
/*!
  \param     token Token to check.
  \return    true or false.
  \sa        isLongOption(const char*)

  An option character is a character prepended by '-'. For example, given the
  option '-v', the option character is 'v'.
*/
bool com::CommandLineArgument::isShortOption(const char* token)
{
  return std::strlen(token) >= 2 && token[0] == '-' && token[1] != '-';
}



//! Returns true if \a token is an option name.
/*!
  \param     token Token to check.
  \return    true or false.
  \sa        isShortOption(const char*)

  An option name is a string prepended by '--'. For example, given the option
  '--verbose', the option name is 'verbose'. The option name must have more
  than one character.
*/
bool com::CommandLineArgument::isLongOption(const char* token)
{
  return std::strlen(token) > 2 && token[0] == '-' && token[1] == '-' &&
                   token[2] != '-';
}



//! Returns the option shortOption if matched, else '\0'.
/*!
  \param     token Token to parse.
  \return    Option character or '\0'.
  \sa        determineLongOption(const char*)
*/
char com::CommandLineArgument::determineShortOption(const char* token)
{
  return isShortOption(token) ? token[1] : '\0';
}



//! Returns the option name if matched, else 0.
/*!
  \param     token Token to parse.
  \return    Option name or 0.
  \sa        determineShortOption(const char*)
*/
const char* com::CommandLineArgument::determineLongOption(const char* token)
{
  return isLongOption(token) ? token + 2 : 0;
}



void com::CommandLineArgument::throwCanNotParseValueException(
         std::string const& argument, size_t length)
{
  std::ostringstream stream;
  com::CommandLineException exception("Can not parse value:");
  exception.append(argument);
  for(size_t i = 0; i < length; ++i) {
    stream << '-';
  }
  stream << '^';
  exception.append(stream.str());
  throw exception;
}



//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     description Description of the argument.
  \param     isRequired Some arguments are optional, others are required.
*/
com::CommandLineArgument::CommandLineArgument(const std::string& description,
                   bool isRequired)

  : d_description(description), d_required(isRequired)

{
}



//! Destructor.
/*!
*/
com::CommandLineArgument::~CommandLineArgument()
{
}



//! Returns true if the argument is required.
/*!
  \return    true if argument is required.

  A required argument must be given on the command line.
*/
bool com::CommandLineArgument::isRequired() const
{
  return d_required;
}



//! Returns the description of the argument.
/*!
  \return    Description.

  Each command line argument has a description.
*/
const std::string& com::CommandLineArgument::description() const
{
  return d_description;
}



//! Returns true if the argument is a positional argument.
/*!
  \return    true or false.

  The default returns false.
*/
bool com::CommandLineArgument::isPositional() const
{
  return false;
}



//! Returns true if the argument can parse \a token.
/*!
  \param     token The token to parse.
  \return    true if the argument can parse \a token.

  The default returns false: it must be re-implemented in all sub-classes.

  This is a query function and nothing is actually
  changed/parsed/read/assigned.

  This function returns false if a token has already been parsed by the
  object: no argument can parse tokens more than once.
*/
bool com::CommandLineArgument::canParse(const char* /* token */) const
{
  return false;
}



//! Checks the argument.
/*!
  \warning    Make sure you call this function when you override it.

  This function should be called after command line parsing.
*/
void com::CommandLineArgument::check() const
{
  if(isRequired() && !isParsed()) {
    // At least one argument is missing from the command line.
    std::ostringstream stream;
    stream << "Not enough arguments: Argument '";
    printSynopsis(stream);
    stream << "' is missing";
    throw com::CommandLineException(stream.str());
  }
}



//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     description Description of the positional
  \param     isRequired Whether the argument is required or not.
*/
com::Positional::Positional(const std::string& description, bool isRequired)

  : CommandLineArgument(description, isRequired)

{
}



//! Destructor.
/*!
*/
com::Positional::~Positional()
{
}



bool com::Positional::isPositional() const
{
  return true;
}



bool com::Positional::canParse(const char* token) const
{
  return !isParsed() && isValue(token);
}



//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     description Description of the option.
  \param     isRequired Whether the option is required.
  \warning   Make sure \a shortOption and \a longOption are unique within one
             application.
  \pre       \a shortOption should not be a whitespace character
             and \a name should not be empty.
*/
com::Option::Option(const std::string& longOption,
                   const std::string& description, bool isRequired)

  : CommandLineArgument(description, isRequired),
    d_parsed(false), d_shortOption('\0'), d_longOption(longOption)

{
  PRECOND(skipShortOption() || !std::isspace(d_shortOption));
  PRECOND(!d_longOption.empty());
}



//! Constructor.
/*!
  \param     shortOption Character of the option.
  \param     longOption Name of the option.
  \param     description Description of the option.
  \param     isRequired Whether the option is required.
  \warning   Make sure \a shortOption and \a longOption are unique within one
             application. \a shortOption should not be a whitespace shortOption
             and \a name should not be empty.
*/
com::Option::Option(char shortOption, const std::string& longOption,
                   const std::string& description, bool isRequired)

  : CommandLineArgument(description, isRequired),
    d_parsed(false), d_shortOption(shortOption), d_longOption(longOption)

{
  PRECOND(skipShortOption() || !std::isspace(d_shortOption));
  PRECOND(!d_longOption.empty());
}



//! Destructor.
/*!
*/
com::Option::~Option()
{
}



bool com::Option::operator<(const CommandLineArgument& arg) const
{
  try {
    const Option& option = dynamic_cast<const Option&>(arg);
    std::string shortOption1(1, shortOption());
    std::string shortOption2(1, option.shortOption());
    std::string longOption1 = longOption();
    std::string longOption2 = option.longOption();

    PRECOND(!longOption1.empty() && !longOption2.empty());

    if(!skipShortOption()) {
      if(!option.skipShortOption()) {
        PRECOND(shortOption1 != shortOption2);
        return shortOption1 < shortOption2;
      }
      else {
        PRECOND(!longOption2.empty());
        return shortOption1 < longOption2;
      }
    }
    else {
      if(!option.skipShortOption()) {
        return longOption1 < shortOption2;
      }
      else {
        return longOption1 < longOption2;
      }
    }
  }
  catch(std::bad_cast&) {
    // Argument is not an option.
    return true;
  }
}



size_t com::Option::parse(size_t argc, char* const* argv)
{
  size_t nrTokensParsed = 0;
  char* token = argv[0];

  if(argc >= 1 && matched(token)) {
    d_parsed = true;
    if(isShortOption(token) && (std::strlen(token) > 2)) {

      // There are more option characters in the token. Remove our character
      // and leave the other(s) for the next command line argument object(s)
      // to parse.
      std::copy(token + 2, token + std::strlen(token) + 1, token + 1);
    }
    else {
      ++nrTokensParsed;
    }
  }
  return nrTokensParsed;
}



//! return nr of prefix characters matched
/*!
 *  0 means not matched
 */
size_t com::Option::matched(const char *token) const
{
  return (!skipShortOption() &&
         isShortOption(token) &&
         determineShortOption(token) == shortOption()) ||
         (isLongOption(token) &&
         determineLongOption(token) == longOption());
}



/*
bool com::Option::firstOfMultipleShorts(const char *token) const
{
    return isShortOption(token) && std::strlen(token) > 2;
}
*/



//! Returns true if the option shortOption should not be regarded during parsing.
/*!
  \return    true, false
*/
bool com::Option::skipShortOption() const
{
  return d_shortOption == '\0';
}



//! Returns the character of the option.
/*!
  \return    Character.
*/
char com::Option::shortOption() const
{
  return d_shortOption;
}



//! Returns the name of the option.
/*!
  \return    Name.
  \sa        shortOption()
*/
const std::string& com::Option::longOption() const
{
  return d_longOption;
}



bool com::Option::canParse(const char* token) const
{
  return !isParsed() && matched(token);
}



bool com::Option::isParsed() const
{
  return d_parsed;
}



void com::Option::printSynopsis(std::ostream& stream) const
{
  if(!skipShortOption()) {
    stream << std::string("-") << shortOption() << std::string("/");
  }

  stream << std::string("--") << longOption();
}



void com::Option::printDescription(std::ostream& stream,
                   size_t offset, size_t /* width */) const
{
  std::ostringstream stringStream;
  stringStream << std::string(offset, ' ');

  if(!skipShortOption()) {
    stringStream << std::string("-") << shortOption() << std::string("/");
  }

  stringStream << std::string("--") << longOption();

  stream << std::setiosflags(std::ios::left) << std::setw(30) 
         << stringStream.str().c_str()
         << std::string(" ") << description() << std::string("\n");
}



//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     description Description of values.
  \warning   \a description should not be empty.
*/
template<class ValueType, class ValueParser>
com::ListArgument<ValueType, ValueParser>::ListArgument(const std::string& description)

  : d_separator('\n'), d_description(description)

{
  PRECOND(!description.empty());
}



//! Destructor.
/*!
*/
template<class ValueType, class ValueParser>
com::ListArgument<ValueType, ValueParser>::~ListArgument()
{
}



template<class ValueType, class ValueParser>
void com::ListArgument<ValueType, ValueParser>::setSeparator(char separator)
{
  d_separator = separator;
}



template<class ValueType, class ValueParser>
bool com::ListArgument<ValueType, ValueParser>::whiteSpaceSeparator () const
{
  return separator() == '\n';
}



template<class ValueType, class ValueParser>
char com::ListArgument<ValueType, ValueParser>::separator() const
{
  return d_separator;
}



//! Adds a parsed value to the collection.
/*!
  \param     value Value just parsed.
*/
template<class ValueType, class ValueParser>
void com::ListArgument<ValueType, ValueParser>::addValue(const ValueType& value)
{
  d_values.push_back(value);
}



//! Returns the description of the values.
/*!
  \return    Description.
*/
template<class ValueType, class ValueParser>
const std::string& com::ListArgument<ValueType, ValueParser>::description() const
{
  return d_description;
}



//! Parses the value in \a token.
/*!
  \param     value Token with one value.
*/
template<class ValueType, class ValueParser>
void com::ListArgument<ValueType, ValueParser>::parseValue(
         std::string const& argument)
{
  ValueType value = d_parser.parse(argument.c_str());

  if(!d_parser.full()) {
    com::CommandLineArgument::throwCanNotParseValueException(
         argument, d_parser.length());
  }

  addValue(value);
}



//! Parses the values in the argument vector \a argv and return the number of values parsed.
/*!
  \param     argc Argument count.
  \param     argv Argument vector.
  \return    Number of values parsed.
*/
template<class ValueType, class ValueParser>
size_t com::ListArgument<ValueType, ValueParser>::parseArguments(
         size_t argc, char* const* argv)
{
  size_t nrTokensParsed = 0;

  // Continue parsing untill an argument is not a value.
  for(size_t i = 0; i < argc; ++i) {
    if(!CommandLineArgument::isValue(argv[i])) {
      break;
    }

    parseArgument(std::string(argv[i]));
    ++nrTokensParsed;
  }

  return nrTokensParsed;
}



//! Parses the value(s) in \a argument.
/*!
  \param     argument Argument with at least one value.
*/
template<class ValueType, class ValueParser>
void com::ListArgument<ValueType, ValueParser>::parseArgument(
         std::string const& argument)
{
  PRECOND(!argument.empty());

  if(whiteSpaceSeparator()) {
    parseValue(argument.c_str());
  }
  else {
    typedef std::vector<std::string> Tokens;
    Tokens tokens = com::split(argument, separator());
    POSTCOND(!argument.empty());
    for(Tokens::const_iterator it = tokens.begin(); it != tokens.end(); ++it) {
      parseValue((*it).c_str());
    }
  }
}



//! Returns true if one or more values where parsed / added.
/*!
  \return    true or false.
*/
template<class ValueType, class ValueParser>
bool com::ListArgument<ValueType, ValueParser>::isParsed() const
{
  return size() > 0;
}



//! Returns the amount of added / parsed values.
/*!
  \return    Amount.
  \sa        empty()
*/
template<class ValueType, class ValueParser>
size_t com::ListArgument<ValueType, ValueParser>::size() const
{
  return d_values.size();
}



//! Returns whether the amount of parsed values is zero.
/*!
  \return    true or false
  \sa        size()
*/
template<class ValueType, class ValueParser>
bool com::ListArgument<ValueType, ValueParser>::empty() const
{
  return d_values.empty();
}



//! Returns the values at position \a index in the collection.
/*!
  \param     index Position in collection [0, size()).
  \return    Value.
  \sa        operator[](size_t)
*/
template<class ValueType, class ValueParser>
const ValueType& com::ListArgument<ValueType, ValueParser>::value(size_t index) const
{
  PRECOND(index < d_values.size());

  return d_values[index];
}



//! Returns the values at position \a index in the collection.
/*!
  \param     index Position in collection [0, size()).
  \return    Value.
  \sa        value(size_t)
*/
template<class ValueType, class ValueParser>
const ValueType& com::ListArgument<ValueType, ValueParser>::operator[](size_t index) const
{
  PRECOND(index < d_values.size());

  return d_values[index];
}



//! Returns an iterator to the first value in the collection.
/*!
  \return    Iterator.
  \sa        end()
*/
template<class ValueType, class ValueParser>
typename com::ListArgument<ValueType, ValueParser>::const_iterator
         com::ListArgument<ValueType, ValueParser>:: begin() const
{
  return d_values.begin();
}



//! Returns an iterator to the one past the last value in the collection.
/*!
  \return    Iterator.
  \sa        begin()
*/
template<class ValueType, class ValueParser>
typename com::ListArgument<ValueType, ValueParser>::const_iterator com::ListArgument<ValueType, ValueParser>::end() const
{
  return d_values.end();
}



//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     valueDescription Description of the value.
  \param     description Description of the argument.
  \param     isRequired Whether the argument is required or not.
*/
template<class ValueType, class ValueParser>
com::PositionalValue<ValueType, ValueParser>::PositionalValue(const std::string& valueDescription,
                   const std::string& description, bool isRequired)

  : ValueArgument<ValueType, ValueParser>(valueDescription),
    Positional(description, isRequired)

{
}



//! Destructor.
/*!
*/
template<class ValueType, class ValueParser>
com::PositionalValue<ValueType, ValueParser>::~PositionalValue()
{
}



template<class ValueType, class ValueParser>
size_t com::PositionalValue<ValueType, ValueParser>::parse(size_t argc, char* const* argv)
{
  PRECOND(argc && canParse(argv[0]));

  /*
    std::ostringstream stream1, stream2;
    stream1 << "Cannot parse argument: Argument '";
    printSynopsis(stream1);
    stream1 << "'";
    */

  ValueArgument<ValueType, ValueParser>::parse(argv[0]);

  return 1;
}



template<class ValueType, class ValueParser>
bool com::PositionalValue<ValueType, ValueParser>::isParsed() const
{
  return ValueArgument<ValueType, ValueParser>::isParsed();
}



template<class ValueType, class ValueParser>
void com::PositionalValue<ValueType, ValueParser>::printSynopsis(std::ostream& s) const
{
  s << ValueArgument<ValueType, ValueParser>::description();
}



template<class ValueType, class ValueParser>
void com::PositionalValue<ValueType, ValueParser>::printDescription(std::ostream& stream,
                   size_t offset, size_t /* width */) const
{
  std::ostringstream stringStream;
  stringStream << std::string(offset, ' ')
    << ValueArgument<ValueType, ValueParser>::description();

  stream << std::setiosflags(std::ios::left) << std::setw(30) 
         << stringStream.str().c_str()
         << std::string(" ") << Positional::description() << std::string("\n");
}



//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     valueDescription Description of the list of values.
  \param     description Description of the positional.
  \param     isRequired Whether the argument is required or not.
*/
template<class ValueType, class ValueParser>
com::PositionalList<ValueType, ValueParser>::PositionalList(const std::string& valueDescription,
                   const std::string& description, bool isRequired)

  : ListArgument<ValueType, ValueParser>(valueDescription),
    Positional(description, isRequired)

{
}



//! Destructor.
/*!
*/
template<class ValueType, class ValueParser>
com::PositionalList<ValueType, ValueParser>::~PositionalList()
{
}



template<class ValueType, class ValueParser>
size_t com::PositionalList<ValueType, ValueParser>::parse(
         size_t argc, char* const* argv)
{
  PRECOND(argc && canParse(argv[0]));

  size_t nrArgumentsParsed =
         ListArgument<ValueType, ValueParser>::parseArguments(argc, argv);

  POSTCOND(nrArgumentsParsed > 0);

  return nrArgumentsParsed;
}



template<class ValueType, class ValueParser>
bool com::PositionalList<ValueType, ValueParser>::isParsed() const
{
  return ListArgument<ValueType, ValueParser>::isParsed();
}



template<class ValueType, class ValueParser>
void com::PositionalList<ValueType, ValueParser>::printSynopsis(std::ostream& stream) const
{
  stream << ListArgument<ValueType, ValueParser>::description() << std::string("...");
}



template<class ValueType, class ValueParser>
void com::PositionalList<ValueType, ValueParser>::printDescription(std::ostream& stream,
                   size_t offset, size_t /* width */) const
{
  std::ostringstream stringStream;
  stringStream << std::string(offset, ' ')
    << ListArgument<ValueType, ValueParser>::description();

  stream << std::setiosflags(std::ios::left) << std::setw(30) 
         << stringStream.str().c_str()
         << std::string(" ") << Positional::description() << std::string("\n");
}



//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     longOption Long option.
  \param     valueDescription Description of the option argument.
  \param     description Description of the option.
  \param     isRequired Whether the option is required or not.
*/
template<class ValueType, class ValueParser>
com::OptionValue<ValueType, ValueParser>::OptionValue(
         const std::string& longOption, const std::string& valueDescription,
         const std::string& description, bool isRequired)

  : ValueArgument<ValueType, ValueParser>(valueDescription),
    Option(longOption, description, isRequired)

{
}



//! Constructor.
/*!
  \param     shortOption Short option.
  \param     longOption Long option.
  \param     valueDescription Description of the option argument.
  \param     description Description of the option.
  \param     isRequired Whether the option is required or not.
*/
template<class ValueType, class ValueParser>
com::OptionValue<ValueType, ValueParser>::OptionValue(char shortOption,
         const std::string& longOption, const std::string& valueDescription,
         const std::string& description, bool isRequired)

  : ValueArgument<ValueType, ValueParser>(valueDescription),
    Option(shortOption, longOption, description, isRequired)

{
}



//! Constructor.
/*!
  \param     shortOption Short option.
  \param     longOption Long option.
  \param     valueDescription Description of the option argument.
  \param     description Description of the option.
  \param     defaultValue Default value.
  \param     isRequired Whether the option is required or not.
*/
template<class ValueType, class ValueParser>
com::OptionValue<ValueType, ValueParser>::OptionValue(char shortOption,
         const std::string& longOption, const std::string& valueDescription,
         const std::string& description, ValueType const& defaultValue,
         bool isRequired)

  : ValueArgument<ValueType, ValueParser>(valueDescription, defaultValue),
    Option(shortOption, longOption, description, isRequired)

{
}



//! Destructor.
/*!
*/
template<class ValueType, class ValueParser>
com::OptionValue<ValueType, ValueParser>::~OptionValue()
{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Code to support --optionvalue or -ov. Not finished yet. Dont forget OptionValue<string>. Don't throw exceptions! Eerst test maken met string en een met integer argument.
*/

template<class ValueType, class ValueParser>
size_t com::OptionValue<ValueType, ValueParser>::parse(size_t argc, char* const* argv)
{
  PRECOND(argc && canParse(argv[0]));

  size_t nrTokensParsed = 0;
  char* token = argv[0];

  if(isShortOption(token) && (std::strlen(token) > 2)) {

    // The value of the argument is in the first argument, after the short
    // option character.
    ValueArgument<ValueType, ValueParser>::parse(token + 2);
    nrTokensParsed = 1;
  }
  else {

    // The value is in the second argument.
    if(argc < 2) {
      throw com::Exception("Missing argument value");
    }

    ValueArgument<ValueType, ValueParser>::parse(argv[1]);
    nrTokensParsed = 2;
  }

  POSTCOND(nrTokensParsed > 0);

  return nrTokensParsed;
}



template<class ValueType, class ValueParser>
bool com::OptionValue<ValueType, ValueParser>::isParsed() const
{
  return ValueArgument<ValueType, ValueParser>::isParsed();
}



template<class ValueType, class ValueParser>
void com::OptionValue<ValueType, ValueParser>::printSynopsis(std::ostream& stream) const
{
  if(!skipShortOption()) {
    stream << std::string("-") << shortOption() << std::string("/");
  }

  stream << std::string("--") << longOption()
         << std::string(" ") << ValueArgument<ValueType, ValueParser>::description();
}



template<class ValueType, class ValueParser>
void com::OptionValue<ValueType, ValueParser>::printDescription(std::ostream& stream,
                   size_t offset, size_t /* width */) const
{
  std::ostringstream stringStream;
  stringStream << std::string(offset, ' ');

  if(!skipShortOption()) {
    stringStream << std::string("-") << shortOption() << std::string("/");
  }

  stringStream << std::string("--") << longOption()
               << std::string(" ") << ValueArgument<ValueType, ValueParser>::description();

  stream << std::setiosflags(std::ios::left) << std::setw(30)
         << stringStream.str().c_str()
         << std::string(" ") << Option::description() << std::string("\n");
}



//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     valueDescription Description of argument values.
  \param     description Description of option.
*/
template<class ValueType, class ValueParser>
com::OptionList<ValueType, ValueParser>::OptionList(const std::string& longOption,
                   const std::string& valueDescription,
                   const std::string& description, bool isRequired)

  : ListArgument<ValueType, ValueParser>(valueDescription),
    Option(longOption, description, isRequired)

{
}



//! Constructor.
/*!
  \param     shortOption Character of option.
  \param     longOption Name of option
  \param     valueDescription Description of argument values.
  \param     description Description of option.
*/
template<class ValueType, class ValueParser>
com::OptionList<ValueType, ValueParser>::OptionList(char shortOption, const std::string& longOption,
                   const std::string& valueDescription,
                   const std::string& description, bool isRequired)

  : ListArgument<ValueType, ValueParser>(valueDescription),
    Option(shortOption, longOption, description, isRequired)

{
}



//! Destructor.
/*!
*/
template<class ValueType, class ValueParser>
com::OptionList<ValueType, ValueParser>::~OptionList()
{
}



template<class ValueType, class ValueParser>
size_t com::OptionList<ValueType, ValueParser>::parse(
         size_t argc, char* const* argv)
{
  PRECOND(argc && canParse(argv[0]));

  size_t nrArgumentsParsed = 0;

  std::string argument(argv[0]);

  // Handle -i 5 and -i5 cases.
  if(isShortOption(argument.c_str()) && argument.length() > 2) {

    // The value of the argument is in the first argument, after the short
    // option character.
    argument = std::string(argument.substr(2));
    ListArgument<ValueType, ValueParser>::parseArgument(argument);
  }

  ++nrArgumentsParsed;

  POSTCOND(nrArgumentsParsed == 1);

  nrArgumentsParsed += ListArgument<ValueType, ValueParser>::parseArguments(
         argc - nrArgumentsParsed, argv + nrArgumentsParsed);

  if(!this->size()) {
    // Only the option stuff is found.
    throw com::Exception("Missing argument value");
  }

  return nrArgumentsParsed;
}



template<class ValueType, class ValueParser>
bool com::OptionList<ValueType, ValueParser>::isParsed() const
{
  return ListArgument<ValueType, ValueParser>::isParsed();
}



template<class ValueType, class ValueParser>
void com::OptionList<ValueType, ValueParser>::printSynopsis(std::ostream& stream) const
{
  if(!skipShortOption()) {
    stream << std::string("-") << shortOption() << std::string("/");
  }

  stream << std::string("--") << longOption()
         << std::string(" ")
         << ListArgument<ValueType, ValueParser>::description()
         << std::string("...");
}



template<class ValueType, class ValueParser>
void com::OptionList<ValueType, ValueParser>::printDescription(std::ostream& stream,
                   size_t offset, size_t /* width */) const
{
  std::ostringstream stringStream;
  stringStream << std::string(offset, ' ');

  if(!skipShortOption()) {
    stringStream << std::string("-") << shortOption() << std::string("/");
  }

  stringStream << std::string("--") << longOption()
               << std::string(" ") << ListArgument<ValueType, ValueParser>::description();

  stream << std::setiosflags(std::ios::left) << std::setw(30) 
         << stringStream.str().c_str()
         << std::string(" ") << Option::description() << std::string("\n");
}



//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

template class com::ListArgument<std::string>;
template class com::ListArgument<int>;
template class com::ListArgument<double>;

template class com::PositionalValue<std::string>;
template class com::PositionalValue<int>;
template class com::PositionalValue<double>;

template class com::PositionalList<std::string>;
template class com::PositionalList<int>;
template class com::PositionalList<double>;

template class com::OptionValue<std::string>;
template class com::OptionValue<int>;
template class com::OptionValue<double>;

template class com::OptionList<std::string>;
template class com::OptionList<int>;
template class com::OptionList<double>;
