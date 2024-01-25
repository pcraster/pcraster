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
// PCRaster library headers.

// Module headers.
#include  <memory>

namespace com {
  class PathName;
}


namespace calc {

class PositionText;

//! how lexinput get its input
class LexInputSource {
  typedef std::shared_ptr<std::string> StringSharedPtr;

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
