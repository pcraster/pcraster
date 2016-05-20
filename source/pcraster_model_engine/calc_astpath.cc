#include <string>
#include <sstream>
#include <typeinfo>
#include <stdexcept>
#include "calc_astpath.h"
#include "com_exception.h"
#include "calc_pointcodeblock.h"
#include "calc_nonassexpr.h"
#include "calc_aststat.h"
#include "calc_astass.h"
// #include "calc_astnode.h"
#include "calc_astpar.h"
#include "calc_astexpr.h"
#include "calc_astnumber.h"
#include "calc_astnodevector.h"
#include "calc_astnodelist.h"
#include "calc_repeatuntil.h"
// #include "calc_stringparser.h"
#include "calc_blockentrance.h"
#include "calc_jumpnode.h"
#include "calc_code.h"



namespace calc {
  struct ASTPathCaster {
    ASTNode     *d_node;

    template <typename T>
      T *expect(const char *name) {
        T *t=dynamic_cast<T *>(d_node);
        if(!t) {
          const std::type_info& gotClass = typeid (*d_node);
          std::ostringstream s;
          s << "expecting " << name
            << " got another ASTNode subclass: "
            << std::string(gotClass.name ());

          throw com::Exception(s.str());
        }
        return t;
      }
#define EXPECT(X)  expect<X>(#X)

    ASTNode *node() {
      return d_node;
    }

    ASTPathCaster(ASTNode *n) {
      d_node=n;
    }

  void add(char c) {
    // implies notes means shortcuts
    switch(c) {
      case '/': break;
      case 'l':
        EXPECT(ASTNodeList);
        break;
      case 'a':
        d_node=EXPECT(ASTStat)->stat();
        EXPECT(ASTAss);
        break;
      case '<': // lhs of ass
        d_node=EXPECT(ASTAss)->par(0);
        break;
      case '>': // rhs of ass
        d_node=EXPECT(ASTAss)->rhs();
        break;
      case 'p':
        EXPECT(ASTPar);
        break;
      case 'n':
        EXPECT(ASTNumber);
        break;
      case 'e':
        EXPECT(ASTExpr);
        break;
      case ',':
        //  ,   implies a prefix e
        // args: ASTNodeVector of expr
        d_node=EXPECT(ASTExpr)->args();
        break;
      case 'C':
        EXPECT(Code);
        break;
      case 'r':
        EXPECT(RepeatUntil);
        break;
      case 'P':
        EXPECT(PointCodeBlock);
        break;
      case 'R':
        d_node=EXPECT(PointCodeBlock)->replacedCode();
        break;
      case 'c':// condition of RepeatUntil
        d_node=EXPECT(RepeatUntil)->condition();
        d_node=EXPECT(NonAssExpr)->expr();
        break;
      case 'b': // statements of the BasicBlock
        // b implies a prefix that is a subclass of BasicBlock
        d_node=EXPECT(BasicBlock)->statements();
        break;
      case '{': // BlockEntrance
        // { implies a prefix that is a subclass of BasicBlock
        d_node=EXPECT(BasicBlock)->blockEntrance();
        EXPECT(BlockEntrance);
        break;
      case '}': // JumpNode
        d_node=EXPECT(BasicBlock)->jumpNode();
        EXPECT(JumpNode);
        break;
      default: // a number
        //  0-9 implies a prefix l or expr argument
        if (!(c >= '0' && c <= '9'))
          throw com::Exception("Unknown path character");
        size_t pos= c-'0';
        ASTNodeList *sl=dynamic_cast<ASTNodeList *>(d_node);
        if (sl) {
          ASTNodeList::const_iterator i=sl->begin();
          size_t n=0;
          for( ; n!=pos && i!=sl->end(); ++i)
            n++;
          if (i==sl->end())
           throw com::Exception("invalid index in ASTSTatList");
          d_node=*i;
        } else {
         if (pos >= EXPECT(ASTNodeVector)->size())
           throw com::Exception("invalid index in ASTNodeVector");
         d_node=EXPECT(ASTNodeVector)->at(pos);
        }
    }
  }
};

} // namespace calc






/*!
 * \returns non-0 node found in root with pathStr
 * \throws  std::runtime_error if pathStr leads not to the correct node in root
 */
calc::ASTNode *calc::path(ASTNode* root, const char *pathStr) {
  PRECOND(root);
  ASTPathCaster current(root);

  const char *ptr;
  try {
    for(ptr=pathStr; *ptr; ++ptr)
       current.add(*ptr);
  } catch (com::Exception& e) {
    std::ostringstream s;
    s << "Error in path: " << pathStr << " at point " << ptr;
    e.prepend(s.str());
    throw std::runtime_error(e.getMessages());
  }
  POSTCOND(current.node());
  return current.node();
}




