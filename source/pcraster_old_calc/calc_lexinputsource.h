#ifndef INCLUDED_OLDCALC_LEXINPUTSOURCE
#define INCLUDED_OLDCALC_LEXINPUTSOURCE

#include "stddefx.h"

#include <string>
#include <memory>



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
