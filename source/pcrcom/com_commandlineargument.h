#ifndef INCLUDED_COM_COMMANDLINEARGUMENT
#define INCLUDED_COM_COMMANDLINEARGUMENT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

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
#ifndef INCLUDED_COM_ARGUMENTPARSER
#include "com_argumentparser.h"
#define INCLUDED_COM_ARGUMENTPARSER
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif



namespace com {
  // CommandLineArgument declarations.
}



namespace com {



//! The CommandLineArgument class is a base class for all kinds of arguments.
/*!
  This class has all information and functionality common to all kinds of
  command line arguments.

  CommandLineArgument objects can be used to parse (part of) the command line.
  After creating a specialized command line argument object you can ask it to
  parse a piece of the command line. If it succeeds, than it will contain all
  relevant parsed information which can be queried afterwards.

  \todo
    Comments CW

      waarom later addArgument, mischien een smaakje met  als eerste
       argument: com::App *app (dus typisch this)

      PositionalList heeft operator[] -> PositionalVector
      kan je bij PositionalList ook het minimal en maximale aantal args opgeven,
       een range van [1,1] is b.v. handig (of PostionalSingle?)

      ik denk ExclusiveArgument daar stop ik twee opties in die elkaar
      uitsluiten en krijg dan een foutmelding, nie dus of wel?
*/
class CommandLineArgument
{

public:

  static bool      isValue             (const char* token);

  static bool      isOption            (const char* token);

  static bool      isShortOption       (const char* token);

  static bool      isLongOption        (const char* token);

  static char      determineShortOption(const char* token);

  static const char* determineLongOption(const char* token);

  static void      throwCanNotParseValueException(
                                        std::string const& argument,
                                        size_t length);

private:

  //! Description of argument.
  std::string      d_description;

  //! Argument is required or not.
  bool             d_required;

protected:

                   CommandLineArgument (const std::string& description,
                                        bool isRequired);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~CommandLineArgument();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Tries to parse the tokens in \a argv.
  /*!
    \param     argc Number of tokens in \a argv
    \param     argv Argument vector with tokens.
    \return    Number of tokens parsed.
    \warning   It is assumed that canParse(char* const* argv) returns true.
               Don't call this function without checking with
               canParse(char* const* argv) first. \a argc must be > 0. Don't
               call this function more than once for an argument.
  */
  virtual size_t   parse               (size_t argc,
                                        char* const* argv) = 0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns whether this is less than \a arg.
  /*!
    The default returns false.
  */
  virtual bool     operator<           (
                                  const CommandLineArgument& /* arg */) const
  { return false; }

  bool             isRequired          () const;

  const std::string& description       () const;

  virtual bool     isPositional        () const;

  //! Returns true if the argument was parsed successfully.
  /*!
    \return    true if parsed succesfully.
  */
  virtual bool     isParsed            () const = 0;

  virtual bool     canParse            (const char* token) const;

  virtual void     check               () const;

  //! Prints the synopsis of the argument.
  /*!
    \param     stream Stream to print to.
    \sa        printDescription(std::ostream&, size_t, size_t)

    A synopsis is the summary of the command line arguments usage.
  */
  virtual void     printSynopsis       (std::ostream& stream) const = 0;

  //! Prints the description of the argument.
  /*!
    \param     stream Stream to print to.
    \param     offset Offset from left of line to start printing.
    \param     width Width of line to print on.
    \sa        printSynopsis(std::ostream&)
  */
  virtual void     printDescription    (std::ostream& stream,
                                        size_t offset,
                                        size_t width) const = 0;

};




//! Sort criterion which is used when arguments are added to the collection.
/*!
  \param     arg1 Pointer to first argument.
  \param     arg2 Pointer to second argument.
  \return    Whether the first argument is less than the second.
  \sa        CommandLineArgument::operator<(),
             addArgument(std::vector<CommandLineArgument*>&,
             CommandLineArgument*)

  Command line arguments are kept partly sorted: in the collection used to
  store the arguments options come before the positionals. Options are
  sorted according to their short and long option values. Positionals are
  not sorted.
*/
struct ArgumentSortCriterion
{
  bool operator() (const CommandLineArgument* arg1,
                   const CommandLineArgument* arg2) const {
    return *arg1 < *arg2;
  }
};



//! Adds \a argument to \a arguments while keeping \a arguments (partly) sorted.
/*!
  \param     arguments Collection with argument.
  \param     argument Argument to add to collection.
  \sa        ArgumentSortCriterion
*/
template<class ArgumentType>
extern void addArgument(std::vector<ArgumentType*>& arguments,
         ArgumentType* argument)
{
  // Sort arguments.
  typename std::vector<ArgumentType*>::iterator pos =
         std::upper_bound(arguments.begin(), arguments.end(), argument,
         ArgumentSortCriterion());

  if(pos == arguments.end()) {
    arguments.push_back(argument);
  }
  else {
    arguments.insert(pos, argument);
  }
}



//! A positional argument is an argument at a certain place on the command line.
/*!
  For example, a command line usage can define that the second argument is an
  input file name and the fourth a cell size.

  You can't create Positionals without a value so you need the specializations
  to create objects: PositionalValue and PositionalList.
*/
class Positional: public CommandLineArgument
{

private:

protected:

                   Positional          (const std::string& description,
                                        bool isRequired);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~Positional         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             canParse            (const char* token) const;

  bool             isPositional        () const;

};



//! An option is a command line argument with a single character or a so called long option.
/*!
  Options are arguments  that come in 2 forms:
  <ul>
   <li>short option: single dash and one character like in <pre>-x</pre></li>
   <li>long option: two dashes attached to a name <pre>--extended</pre></li>
  </ul>
  This type of command line arguments can appear everywhere on the command line, order
  is not important.
  This differs from Positionals which don't have a character
  or a name and must appear in on a certain position on the command line.

  Examples of options are:
  \code
   $ command --verbose
   $ command -h
  \endcode

  There are different kinds of option classes: the option without a value
  (Option), the option with one value (OptionValue), the option with a list
  of values (OptionList) and the command modes (CommandMode).

  \bug KDJ: Options can be required and thus are not always optional. Maybe the
       name isn't well chosen. I find required options handy: they are more
       verbose than positionals and they are the only way to separate different
       list of arguments: $ command --inputmaps bla.map bli.map --inputtables
       bla.tab bli.tab. Named versus Positional?
*/
class Option: public CommandLineArgument
{

private:

  //! Option is parsed or not.
  bool             d_parsed;

  //! short option
  char             d_shortOption;

  //! long option
  std::string      d_longOption;

protected:

  size_t matched(const char* token) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Option              (const std::string& longOption,
                                        const std::string& description,
                                        bool isRequired);

                   Option              (char character,
                                        const std::string& longOption,
                                        const std::string& description,
                                        bool isRequired);

  virtual          ~Option             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  bool             operator<           (const CommandLineArgument& arg) const;

  virtual size_t   parse               (size_t argc,
                                        char* const* argv);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             skipShortOption     () const;

  char             shortOption         () const;

  const std::string& longOption        () const;

  bool             canParse            (const char* token) const;

  virtual bool     isParsed            () const;

  virtual void     printSynopsis       (std::ostream& s) const;

  virtual void     printDescription    (std::ostream& s,
                                        size_t offset,
                                        size_t width) const;

};



//! This class is a base class for arguments with a value.
/*!
  Value arguments keep track of whether the value is set (=parsed) and what
  the value and the description of the value argument is.

  The ValueParser template argument must support the following functions:
  <dl>
    <dt>ValueType parse(char const* const string)</dt>
    <dd>Parses string and returns the parsed value in its type.</dd>
    <dt>bool full()</dt>
    <dd>Returns true if all characters in string where parsed. If not, something went wrong.</dd>
    <dt>size_t length()</dt>
    <dd>Returns the number of parsed characters which is smaller than the number of characters in the string in case something went wrong during the parse.</dd>
  </dl>

  \sa        ListArgument
*/
template<class ValueType, class ValueParser>
class ValueArgument
{

private:

  //! Value parser.
  ValueParser      d_parser;

  //! Description of value.
  std::string      d_description;

  //! Value is parsed or not.
  bool             d_parsed;

  //! Parsed value.
  ValueType        d_value;

protected:

                   ValueArgument       (const std::string& description,
                                        const ValueType& defaultValue = ValueType());

  void             parse               (char const* arg);

  void             setValue            (const ValueType& value);

  void             setDefaultValue     (const ValueType& value);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~ValueArgument      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const std::string& description       () const;

  bool             isParsed            () const;

  const ValueType& value               () const;

};



//! This class is a base class for arguments with a list of values.
/*!
  List arguments keep track of whether a value is set (=parsed) and what the
  values and the description of the list argument is.

  See description of ValueArgument for information about the ValueParser
  template argument.

  \sa        ValueArgument
*/
template<class ValueType, class ValueParser = ArgumentParser<ValueType> >
class ListArgument
{

private:

  //! Separator between values. Default is whitespace.
  char             d_separator;

  //! Value parser.
  ValueParser      d_parser;

  //! Description of values.
  std::string      d_description;

  //! Parsed value(s).
  std::vector<ValueType> d_values;

  bool             whiteSpaceSeparator () const;

  char             separator           () const;

protected:

                   ListArgument        (const std::string& description);

  void             addValue            (const ValueType& value);

  void             parseValue          (std::string const& argument);

  size_t           parseArguments      (size_t argc,
                                        char* const* argv);

  void             parseArgument       (std::string const& argument);

public:

  //! Iterator for the collection with values.
  typedef typename std::vector<ValueType>::const_iterator const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~ListArgument       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setSeparator        (char separator);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const std::string& description       () const;

  bool             isParsed            () const;

  size_t           size                () const;

  bool             empty               () const;

  const ValueType& value               (size_t index) const;

  const ValueType& operator[]          (size_t index) const;

  const_iterator   begin               () const;

  const_iterator   end                 () const;

};




//! A positional value is a positional with a value.
/*!
  In the next example the first argument of the command is a positional with a
  value and the second an option:
  \code
  $ doit file.txt -v
  \endcode

  \sa        PositionalList
*/
template<class ValueType, class ValueParser = ArgumentParser<ValueType> >
class PositionalValue: public ValueArgument<ValueType, ValueParser>,
                       public Positional
{

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PositionalValue     (const std::string& valueDescription,
                                        const std::string& description,
                                        bool isRequired=true);

                   ~PositionalValue    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  size_t           parse               (size_t argc,
                                        char* const* argv);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isParsed            () const;

  void             printSynopsis       (std::ostream& s) const;

  void             printDescription    (std::ostream& s,
                                        size_t offset,
                                        size_t width) const;

};



//! A positional list is positional with a list of values.
/*!
  In the next example the first three arguments of the command is a positional
  with a list of values and the fourth an option:
  \code
  $ doit file1.txt file2.txt file3.txt -v
  \endcode

  \sa        PositionalValue
*/
template<class ValueType, class ValueParser = ArgumentParser<ValueType> >
class PositionalList: public ListArgument<ValueType, ValueParser>,
                      public Positional
{

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PositionalList      (const std::string& valueDescription,
                                        const std::string& description,
                                        bool isRequired);

                   ~PositionalList    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  size_t           parse               (size_t argc,
                                        char* const* argv);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isParsed            () const;

  void             printSynopsis       (std::ostream& s) const;

  void             printDescription    (std::ostream& s,
                                        size_t offset,
                                        size_t width) const;

};



//! This class is for options which have a value argument.
/*!
  Example:
  \code
  // Option with a file name as its argument.
  $ command --file bla.txt
  \endcode
*/
template<class ValueType, class ValueParser = ArgumentParser<ValueType> >
class OptionValue: public ValueArgument<ValueType, ValueParser>,
                   public Option
{

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   OptionValue         (const std::string& longOption,
                                        const std::string& valueDescription,
                                        const std::string& description,
                                        bool isRequired = false);

                   OptionValue         (char shortOption,
                                        const std::string& longOption,
                                        const std::string& valueDescription,
                                        const std::string& description,
                                        bool isRequired = false);

                   OptionValue         (char shortOption,
                                        const std::string& longOption,
                                        const std::string& valueDescription,
                                        const std::string& description,
                                        ValueType const& defaultValue,
                                        bool isRequired = false);

                   ~OptionValue        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  size_t           parse               (size_t argc,
                                        char* const* argv);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isParsed            () const;

  void             printSynopsis       (std::ostream& s) const;

  void             printDescription    (std::ostream& s,
                                        size_t offset,
                                        size_t width) const;

};



//! This class is for arguments which have list of values.
/*!
  Example:
  \code
  $ command --inputs bla.map bli.map blo.map
  \endcode
*/
template<class ValueType, class ValueParser = ArgumentParser<ValueType> >
class OptionList: public ListArgument<ValueType, ValueParser>,
                  public Option
{

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   OptionList          (const std::string& longOption,
                                        const std::string& valueDescription,
                                        const std::string& description,
                                        bool isRequired);

                   OptionList          (char character,
                                        const std::string& longOption,
                                        const std::string& valueDescription,
                                        const std::string& description,
                                        bool isRequired);

                   ~OptionList         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  size_t           parse               (size_t argc,
                                        char* const* argv);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isParsed            () const;

  void             printSynopsis       (std::ostream& s) const;

  void             printDescription    (std::ostream& s,
                                        size_t offset,
                                        size_t width) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     description Description of the value.
  \param     defaultValue Default value.
  \warning   \a description should not be empty.

  The default value of defaultValue (!) is T().
*/
template<class ValueType, class ValueParser>
ValueArgument<ValueType, ValueParser>::ValueArgument(const std::string& description,
                   const ValueType& defaultValue)

  : d_description(description), d_parsed(false)

{
  PRECOND(!d_description.empty());

  setDefaultValue(defaultValue);
}

//! Destructor.
/*!
*/
template<class ValueType, class ValueParser>
ValueArgument<ValueType, ValueParser>::~ValueArgument()
{
}

//! Sets the value to the default value \a value.
/*!
  \param     value Default value.
  \warning   You should probably do this only when initializing the object.
             This will overwrite the previous value.
  \sa        setValue(const ValueType&)
*/
template<class ValueType, class ValueParser>
void ValueArgument<ValueType, ValueParser>::setDefaultValue(const ValueType& value)
{
  d_value = value;
}

//! Sets the value to \a value.
/*!
  \param     value New value.
  \sa        setDefaultValue(const ValueType&)

  As a side effect parsed() will return true from now on.
*/
template<class ValueType, class ValueParser>
void ValueArgument<ValueType, ValueParser>::setValue(const ValueType& value)
{
  d_value = value;
  d_parsed = true;
}

template<class ValueType, class ValueParser>
void ValueArgument<ValueType, ValueParser>::parse(char const* arg)
{
  ValueType value = d_parser.parse(arg);

  if(!d_parser.full()) {
    com::CommandLineArgument::throwCanNotParseValueException(
         arg, d_parser.length());
  }

  setValue(value);
}

//! Returns the description of the value.
/*!
  \return    Description.
*/
template<class ValueType, class ValueParser>
const std::string& ValueArgument<ValueType, ValueParser>::description() const
{
  return d_description;
}

//! Returns true if the value is parsed.
/*!
  \return    true or false.
*/
template<class ValueType, class ValueParser>
bool ValueArgument<ValueType, ValueParser>::isParsed() const
{
  return d_parsed;
}

//! Returns the value.
/*!
  \return    Value.

  If the value is not set (isParsed() returns false), than the default value
  is returned (given to the constructor or set by setDefaultValue(const ValueType&).
*/
template<class ValueType, class ValueParser>
const ValueType& ValueArgument<ValueType, ValueParser>::value() const
{
  return d_value;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
