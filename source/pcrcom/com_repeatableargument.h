#ifndef INCLUDED_COM_REPEATABLEARGUMENT
#define INCLUDED_COM_REPEATABLEARGUMENT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_ITERATOR
#include <iterator>
#define INCLUDED_ITERATOR
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
#ifndef INCLUDED_COM_COMMANDLINEARGUMENT
#include "com_commandlineargument.h"
#define INCLUDED_COM_COMMANDLINEARGUMENT
#endif



namespace com {
  // RepeatableArgument declarations.
}



namespace com {



//! RepeatableArgument object can parse a sequence of equal arguments.
/*!
  This class is for command line arguments which can be repeated on the
  command line. The parse() function continues parsing untill it finds
  encounters an argument it cannot parse or all arguments are parsed.

  Parsed arguments are written to an external collection through a
  back_insert_iterator. Valid collection types for the external collection are:
  std::vector, std::deque and list.

  The CollectionType template argument is the collection type of the external
  collection.

  The ArgumentType template argument is the Argument type of the command line
  arguments to parse. This type is used to create an object for each new
  argument that is parsed.
*/
template<class ArgumentType, class CollectionType = std::vector<ArgumentType> >
class RepeatableArgument: public CommandLineArgument
{

private:

  //! Template argument which will be used to create new ones.
  ArgumentType     d_argument;

  //! Back inserter to dump parsed arguments in.
  std::back_insert_iterator<CollectionType> d_inserter;

  //! Number of arguments parsed.
  size_t           d_nrParsedArguments;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RepeatableArgument  (const std::string& description,
                                        bool isRequired,
                                        const ArgumentType& argument,
                    const std::back_insert_iterator<CollectionType>& inserter);

  /* virtual */    ~RepeatableArgument ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  size_t           parse               (size_t argc,
                                        char* const* argv);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isParsed            () const;

  bool             canParse            (const char* token) const;

  void             check               () const;

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
