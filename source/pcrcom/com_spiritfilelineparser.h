#ifndef INCLUDED_COM_SPIRITFILELINEPARSER
#define INCLUDED_COM_SPIRITFILELINEPARSER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif
#ifndef INCLUDED_BOOST_SPIRIT_CORE
#include <boost/spirit/core.hpp>
#define INCLUDED_BOOST_SPIRIT_CORE
#endif
#include <boost/spirit/iterator/position_iterator.hpp>

// PCRaster library headers.
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

// Module headers.



namespace com {
}



namespace com {



/*!
 * Replica of SpiritFileParser but with line-base scanning
 * with skipping non-empty lines.
 */
class SpiritFileLineParser
{

public:
   typedef const char * iterator;
private:
   enum { CAPACITY=128000 };
   com::PathName d_pn;
   //! including the current terminating in the buffer!
   /*!
    *  always matches user 1-based index
    */
   size_t      d_nrNewLinesRead;
   char        d_buffer[CAPACITY];
   char       *d_end;

   std::ifstream d_ifs;


  //! Assignment operator. NOT IMPLEMENTED.
  SpiritFileLineParser&           operator=           (const SpiritFileLineParser& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   SpiritFileLineParser               (const SpiritFileLineParser& rhs);


public:
  void skipLines(size_t nrLinesToSkip);

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SpiritFileLineParser             (const com::PathName& pn,
                                                     size_t nrLinesToSkip=0);

  /* virtual */    ~SpiritFileLineParser              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

   //! trick hack
   boost::spirit::parse_info<iterator> pi;

   void advance();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
   void errorAtStop() const;

   iterator begin() const {
     return d_buffer+0;
   }
   iterator end()  const {
     return d_end;
   }
   size_t   lineNr()    const;
   bool     fullMatch() const;
   bool     eof()       const;

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
