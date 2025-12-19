#ifndef INCLUDED_CALC_ASTTESTFACTORY
#define INCLUDED_CALC_ASTTESTFACTORY

#include "stddefx.h"
#include "com_exception.h"
#include "calc_posexception.h"

#include <string>
#include <vector>


class QDomElement;

namespace calc {
class ASTPar;
class DataType;
class ASTScript;
class ASTAss;
class ASTNumber;
class ASTExpr;
class ASTNode;
class Position;
class PositionName;
class MessagesTestDB;

//! a simple factory for AST nodes for the unit tests
class ASTTestFactory
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ASTTestFactory&           operator=           (const ASTTestFactory&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ASTTestFactory               (const ASTTestFactory&);


  static MessagesTestDB& db();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ASTTestFactory               ();

  /* virtual */    ~ASTTestFactory              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  ASTPar*    createPar(const    std::string& name);
  ASTNumber* createNumber(const std::string& numericString);
  ASTExpr*   createExpr(const   std::string& opName);
  ASTNode*   createCode(const char *xmlCode);
  ASTNode*   createCode(const QDomElement& e);
  ASTNode*   createFromId(const char *id);
  ASTNode*   createFromId(const std::string& id);
  ASTAss*    createAss(const std::string& par,ASTNode  *value,
                       const Position *pos=nullptr);
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  static std::string modelFromId( const std::string& id);
  static ASTScript* createFromIdOrStr(const std::string& codeOrId);
  static bool       msgVerify(const std::string& id,
                       const com::Exception& e,
                       const std::string& prefix="");// const;
  static bool       fileVerify(const std::string& id,
                        const std::string& createdFile);//const;

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

//! tool to verify ascii output (for example a tss) stored in messagestest.xml
class TestAsciiResult: public ASTTestFactory {
  std::string d_fName;
public:
  TestAsciiResult(const std::string& fName="tmp.res");
  bool equals(const std::string& id);
};

//! macro magic for testing errors coded in messagestest.xml
#define TRY_TEST_MSG  \
    bool  catched=false; \
    (void)catched; /* Shut up compiler */ \
    try

//! macro magic for testing errors coded in messagestest.xml
/*! class must derive from ASTTestFactory for msgVerify
 *   catch expected but (re)throw the unexpected and let the Unit
 *   test logic figure out.
 *   BOOST_CHECK(s.what()) to suppress s not used in release mode
 */
#define CATCH_TEST_MSG(msgId)                \
   catch (const calc::PosException &p) {     \
   BOOST_CHECK(calc::ASTTestFactory::msgVerify(msgId,p));            \
   catched=true;                             \
 } catch (const com::Exception& s) {         \
   BOOST_CHECK(calc::ASTTestFactory::msgVerify(msgId,s,"ERROR: "));  \
   catched=true;                             \
 } catch (std::exception const& s) {         \
   BOOST_CHECK(s.what());                    \
   PRINT_VAR(s.what());                      \
   throw;                                    \
 }


void execTest(const std::string& id);

#define EXEC_ERROR_TEST(id) \
  { TRY_TEST_MSG {       \
    execTest(id);        \
    } CATCH_TEST_MSG(id);\
   BOOST_CHECK(catched);   \
  }
#define EXEC_ERROR_TEST_WARN(id) \
  { TRY_TEST_MSG {       \
    execTest(id);        \
    } CATCH_TEST_MSG(id);\
   BOOST_WARN(catched);   \
  }

} // namespace calc

#endif
