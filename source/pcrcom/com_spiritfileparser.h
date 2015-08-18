#ifndef INCLUDED_COM_SPIRITFILEPARSER
#define INCLUDED_COM_SPIRITFILEPARSER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#include <boost/spirit/include/classic_core.hpp>
#include <boost/spirit/include/classic_exceptions.hpp>
#include <boost/spirit/include/classic_position_iterator.hpp>
#include <boost/spirit/include/classic_file_iterator.hpp>

// PCRaster library headers.

// Module headers.



namespace com {
  class PathName;
  class FileMap;
}



namespace com {



//! wrapper for use of a parser that reads from a file
/*!
 * Implements position detection
 *
 * Implementation is with com::FileMap, using
 * file_iterator<> on win32 is unacceptable slow, however file
 * mapping is limitted to 512 Mb files or smaller to prevent
 * memory exhausting
 *
 * commented implementation is the file_iterator version
 *  bcc32 crashes with the file_iterator version
 */
class SpiritFileParser
{

public:
   // typedef boost::spirit::file_iterator<char> fit_t;
   typedef const char *fit_t;
private:
   FileMap       *d_fileMap;
   fit_t d_fbegin,d_fend;
   //! wrap the file iterator to get position_iterators
public:
   typedef boost::spirit::classic::position_iterator<fit_t> iterator;
private:
   iterator  d_begin,d_end;
   iterator  d_current;
   size_t    d_lineNr;


  //! Assignment operator. NOT IMPLEMENTED.
  SpiritFileParser&           operator=           (const SpiritFileParser& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   SpiritFileParser               (const SpiritFileParser& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SpiritFileParser             (const com::PathName& pn);

  /* virtual */    ~SpiritFileParser              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

   //! trick hack
   boost::spirit::classic::parse_info<iterator> pi;

   void advance();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
   void errorAtStop() const;

   iterator begin() const {
     return d_begin;
   }
   iterator end()  const {
     return d_end;
   }
   iterator current()  const {
     return d_current;
   }
   size_t   lineNr() const;


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
