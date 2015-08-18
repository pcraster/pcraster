#ifndef INCLUDED_CALC_LEXINPUTSOURCE
#define INCLUDED_CALC_LEXINPUTSOURCE

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif
// PCRaster library headers.

// Module headers.

namespace com {
  class PathName;
}


namespace calc {

class PositionText;

//! how lexinput get its input
class LexInputSource {
  typedef boost::shared_ptr<std::string> StringSharedPtr;

           LexInputSource();
protected:
  StringSharedPtr d_fileName;

           LexInputSource(const std::string& fileName);
public:
  virtual ~LexInputSource();

  //! get a char
  /*! must return EOF on end of input
   */
  virtual int getChar()=0;

  virtual PositionText* createPositionText(size_t lineNr, size_t charNr)const;
};

LexInputSource *createLexInputSourceFromFile(const com::PathName& fileName);

} // namespace calc


//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------


#endif
