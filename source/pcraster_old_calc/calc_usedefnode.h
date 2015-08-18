#ifndef INCLUDED_USEDEFCHAIN
#define INCLUDED_USEDEFCHAIN

namespace calc {

class StatementBlock;
class FieldParameter;

//! A use-def chain consisting of Use and Def links
/*! A use-def chain is for each parameter defined to know
 *  when parameter values can be deleted
 *  A use is in a FieldLeaf, a def in a FieldLeft.
 */
class UseDefNode {
private:
  const UseDefNode *d_next;
  StatementBlock &d_inBlock;
protected:
  virtual void analyseUseDef()=0;

  bool deleteValueAtEndOfBlock(FieldParameter *par, bool forceBlockClean); 


  // ACCESSORS
  virtual bool isUse() const =0;
  bool IsDef() const { return !isUse(); };
  bool nextIsNotUse() const;
  bool nextInSameBlock() const;
  const UseDefNode *nextUseDef() const { return d_next; };
public:
  UseDefNode(StatementBlock &inBlock);
  virtual ~UseDefNode();

  // MANIPULATORS
  void addNextUseDef(const UseDefNode *l);

  // ACCESSORS
  bool inDynamic() const;
};

}

#endif
