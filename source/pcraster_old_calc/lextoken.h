#ifndef INCLUDED_LEXTOKEN
#define INCLUDED_LEXTOKEN

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#include "tokens.h"
#include "AToken.h"

#include "major_op.h"

namespace calc {

class Position;

//! The token class for grammar
/*! derive from ANTLRRefCountToken so that tokens are automatically deleted
 * when no remaining pointers to them exist.
 * <BR>implementations is in newcalc.l
 * <BR>TODO most methods are never used, check what is really needed
 */
class LexToken : public ANTLRRefCountToken {
private:
  ANTLRTokenType       d_type;
  bool                 d_isKeyword{false};
  MAJOR_CODE           d_op;
  //! defined if d_type is TOK_INT or TOK_FLOAT
  double               d_val{};
  const std::string    d_text;
  Position            *d_pos;

public:
  LexToken(ANTLRTokenType tt, MAJOR_CODE op, const char* text, Position *ownedPos);
  ~LexToken() override;

  void setType(ANTLRTokenType t) override;
  void setText(const ANTLRChar * s) override;
  void setLine(int line) override;

  void setNrValue(double val);
  void setIsKeyword(bool isKeyword);


  MAJOR_CODE         opCode()    const;
  int                integerVal()const;
  const std::string& stringVal() const;

  const Position    *position()  const;

  int                getLine()   const override;
  ANTLRTokenType     getType()   const override;
  ANTLRChar         *getText()   const override;
  bool               isKeyword() const;

};
}
#endif
