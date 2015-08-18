#ifndef INCLUDED_COM_REPEATABLEEXCLUSIVEARGUMENT
#define INCLUDED_COM_REPEATABLEEXCLUSIVEARGUMENT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_ITERATOR
#include <iterator>
#define INCLUDED_ITERATOR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_COMMANDLINEARGUMENT
#include "com_commandlineargument.h"
#define INCLUDED_COM_COMMANDLINEARGUMENT
#endif



namespace com {
  // RepeatableExclusiveArgument declarations.
}



namespace com {



//! A repeatable exclusive argument is an exclusiver argument which can be repeated.
/*!
  \sa        RepeatableArgument, ExclusiveArgument

  The default CollectionType is std::vector<ArgumentType>
*/
template<class ArgumentType, class CollectionType = std::vector<ArgumentType> >
class RepeatableExclusiveArgument: public CommandLineArgument
{

private:

  //! Type for back inserter for Arguments in the collection.
  typedef std::back_insert_iterator<CollectionType> back_inserter;

  //! Type for collection with ArgumentType, back_inserter pairs.
  typedef std::vector<std::pair<ArgumentType, back_inserter> > Inserters;

  //! For each argument an inserter.
  Inserters        d_inserters;

  //! Number of arguments parsed.
  size_t           d_nrParsedArguments;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RepeatableExclusiveArgument(
                                        const std::string& description,
                                        bool isRequired,
                                        const ArgumentType& argument1,
                                        const back_inserter& inserter1,
                                        const ArgumentType& argument2,
                                        const back_inserter& inserter2);

                   RepeatableExclusiveArgument(
                                        const std::string& description,
                                        bool isRequired,
                                        const ArgumentType& argument1,
                                        const back_inserter& inserter1,
                                        const ArgumentType& argument2,
                                        const back_inserter& inserter2,
                                        const ArgumentType& argument3,
                                        const back_inserter& inserter3);

                   RepeatableExclusiveArgument(
                                        const std::string& description,
                                        bool isRequired,
                                        const ArgumentType& argument1,
                                        const back_inserter& inserter1,
                                        const ArgumentType& argument2,
                                        const back_inserter& inserter2,
                                        const ArgumentType& argument3,
                                        const back_inserter& inserter3,
                                        const ArgumentType& argument4,
                                        const back_inserter& inserter4);

  /* virtual */    ~RepeatableExclusiveArgument();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addArgument         (const ArgumentType& argument,
                                        const back_inserter& inserter);

  size_t           parse               (size_t argc,
                                        char* const* argv);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isParsed            () const;

  bool             canParse            (const char* token) const;

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
