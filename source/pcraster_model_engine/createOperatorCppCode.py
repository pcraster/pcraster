#!/usr/bin/env python

# generate C++ code to
#  manage different aspects of the operations

import string
import sys
import os.path

sys.path = sys.path + [os.path.join(os.path.split(__file__)[0], "..", "..", "environment", "script")]

import codeutils # was cgen in OLDPCRTREE

import DomUtils
import pcrxml


# implementation name
def implName(o):
  if o.hasAttribute('implName'):
    return o.getAttribute('implName')
  return o.getAttribute('name')

def exec_(e):
   assert e.hasAttribute('exec')
   return e.getAttribute('exec')

def opEnum(opName):
  return "OP_%s" % opName.upper()

def cr2Type(cr):
  Cr2Type = { '1':'UINT1', '4':'INT4', 'f':'REAL4' }
  return Cr2Type[cr]

class Generator:
  def output(self, name):
    f= codeutils.CppFileGenerator(name,'(/PCRasterModelEngine/)createOperatorCppCode')
    self.d_outputs.append(f)
    return f

  def __init__(self, xmlFile):

   root = pcrxml.readDom(xmlFile)
   # put all operations in list
   assert root.nodeName == 'Operations'
   self.operations = DomUtils.childrenWithName(root,'Operation')

   self.automatic = [ "SameUn", "SameBin", "DiffBin","DiffUn"]
   self.pointCode = [ "SameUn", "SameBin", "DiffBin","DiffUn"]
   # already supported in generation
   self.supported = self.automatic + [ "Manual" ]
   self.onExec =  ['GLOBAL', 'MRF', 'DOUBLE', 'Direct' ]

   self.d_outputs = []

   self.foArInc=self.output("calc_fopointarrayimpl.inc")
   self.biInc=self.output("calc_builtinops.inc")

   self.pntImplH=self.output("calc_fopointimpl.h")
   self.pntImplH.write("""
#ifndef INCLUDED_MATHX
#include "mathx.h"
#define INCLUDED_MATHX
#endif
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
#ifndef INCLUDED_CSTDLIB
#include <cstdlib>
#define INCLUDED_CSTDLIB
#endif
#ifndef INCLUDED_CALC_FOPOINTSPECIAL
#include "calc_fopointspecial.h"
#define INCLUDED_CALC_FOPOINTSPECIAL
#endif
#ifndef INCLUDED_CALC_FOPOINT
#include "calc_fopoint.h"
#define INCLUDED_CALC_FOPOINT
#endif
#ifndef INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#include <boost/math/special_functions/round.hpp>
#define INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#endif
""")
   self.pntImplH.write("\nnamespace calc {")
   self.pntImplH.write("\nnamespace point {")

  def execute(self):
     try:
       self.generateMajor()
       self.generateFunctionTable()
       self.generateOperationsInit()
     except:
       self.removeOutputs()
       raise

  def __del__(self):
    if self.pntImplH != None:
     self.pntImplH.write(' } // namespace calc ');
     self.pntImplH.write(' } // namespace point ');

  def removeOutputs(self):
    for i in self.d_outputs:
      i.remove()
    self.pntImplH = None

  def domainIll(self,e):
    types = [ 'onlyDomainIll','rightDomainIll', 'combDomainIll']
    for i in types:
     if e.hasAttribute(i):
      return i
    return 'noDomainIll'

  def domainFunction(self,e):
    args={
     'onlyDomainIll':'const T& v',
     'rightDomainIll':'const T& r',
     'combDomainIll':'const T& r, const T& l'}
    for i in args.keys():
     if e.hasAttribute(i):
      return "inline static bool %s(%s){return %s;} " % \
                  (i,args[i],e.getAttribute(i))
    return ""

  def pointOps(self,name,e,crs):
    templates ={
    "SameUn" :'''
      template<typename T>
      struct %s :public SameUnPoint< T_A > {
       %s
       static T f(const T& v)
       { return %s; }
       static void op(T& v)
       { v = f(v); }
       }; ''',
     "SameBin":'''
      template<typename T>
      struct %s :public SameBinPoint< T > {
       %s
       static T f(const T& a1, const T& a2)
       { return %s; }
       static void op(T& v, const T& a1, const T& a2)
       { v = f(a1,a2); }
       };''',
     "DiffBin":'''
      template<typename T>
      struct %s :public DiffBinPoint< T > {
       %s // NO_DOMAIN_CHECKS
       static UINT1 f(const T& a1, const T& a2)
       { return %s; }
       static void op(UINT1& v, const T& a1, const T& a2)
       { v = f(a1,a2); }
       };''',

     "DiffUn" :'''
      template<typename ResultType, typename InputType>
      struct %s :public DiffUnPoint< ResultType,InputType > {
       %s // NO_DOMAIN_CHECKS
       static ResultType f(const InputType& i)
       { return %s; }
       static void op(ResultType& v, const InputType& i)
       { v = f(i); }
       };'''}

    opType=e.nodeName

    template = templates[opType]

    if opType == "SameUn":
      templArgs="T"
      if self.domainFunction(e) != "":
        templArgs = "T, point::%s<T>" % name
      template=template.replace("T_A",templArgs)

    self.pntImplH.write(template % (
         name,
         self.domainFunction(e),
         e.getAttribute('function')))

    ctorArgs=[]
    for cr in crs:
     tI=cr2Type(cr)
     if opType=="DiffUn":
      rt=e.getAttribute("foResultType")
      if rt!="":
        tI=cr2Type(rt)+","+tI
      else:
        tI=tI+","+tI
     fname=name+"_"+cr
     self.foArInc.write("static const %sArray< point::%s< %s > > foAr_%s;" \
        % (opType,name,tI, fname))
     ctorArgs.append("&foAr_%s" % fname)
    return ctorArgs


  def generateMajor(self):
   class OpCode:
     def __init__(s,g):
       s.opCode=0
       s.g = g;
     def write(s,opName,sep=","):
       s.g.write("%s=%d%s" % (opEnum(opName),s.opCode,sep))
       s.opCode=s.opCode+1
   g=self.output('major_op.h')
   c=OpCode(g)
   g.write('typedef enum MAJOR_CODE {')
   c.write('NOP');
   for n in self.operations:
     if (n.getAttribute('syntax')!='MRF'):
       c.write(implName(n))
     else:
       c.write(implName(n)+'_MRF')
       mrf = DomUtils.childrenWithName(n,'Result')
       for f in mrf:
         assert f.hasAttribute('functionSuffix')
         c.write(implName(n)+f.getAttribute('functionSuffix'))
   # OP_ILL used in conversion table
   c.write('ILL','} MAJOR_CODE;');

  def generateFunctionTable(self):
   def insertEntry(name):
     g.write('{"%s",%s},' % (name,opEnum(name)))
   g=self.output('calc_functiontable.h')
   g.write('struct FuncNameOpcode {\n'+\
           '  const char *d_name;\n'          +\
           '  MAJOR_CODE  d_op;\n'            +\
           '};\n'                             +\
           'static FuncNameOpcode funcName2OpCode[] = {' )
   for n in self.operations:
     if (n.getAttribute('syntax') == 'Function'):
       insertEntry(implName(n))
     if (n.getAttribute('syntax') == 'MRF'):
       mrf = DomUtils.childrenWithName(n,'Result')
       for f in mrf:
         assert f.hasAttribute('functionSuffix')
         insertEntry(implName(n)+f.getAttribute('functionSuffix'))
   # "",OP_ILL used as a terminate indication
   g.write('{"",OP_ILL}};')

  # write to biInc: static %IOpImpl% builtIn_%name%(&foAr...);
  def operationImpl(self,n):
     if exec_(n) == "Direct":
       return # no need to generate the builtIn_%name% object
     name=implName(n)
     i = DomUtils.childInNameSet(n, self.automatic)
     ctorArgs=[]
     if i:
       type = i.nodeName
       if name in [ "sin", "cos", "tan" ]:
         type = "Trig"
       typeAndName="%s builtIn_%s" % (type,name)
       ctorArgs = self.pointOps(name,i,list(i.getAttribute("foTypes")))
     elif exec_(n) in self.onExec:
       className = { 'GLOBAL':'Global', 'MRF':'MRF', 'DOUBLE':'OneOfMRF' }
       typeAndName="%s builtIn_%s" % (className[exec_(n)],name)
       if exec_(n) == 'MRF':
         typeAndName=typeAndName+"_mrf"
       if exec_(n) == 'DOUBLE':
         assert n.hasAttribute('mrf')
         ctorArgs= [ "&builtIn_%s_mrf" % n.getAttribute('mrf') ]
       else: # GLOBAL
         ctorArgs= [ 'Do_'+name ]
     if len(ctorArgs):
       self.biInc.write("static %s(%s);" % (typeAndName,",".join(ctorArgs)))


  # create operationsInit.inc
  def generateOperationsInit(self):
   def syntax(o):
     return \
     { 'Function' : 'FUNC','Operator':'OP','MRF':'MRF','None':'NONE' }\
     [o.getAttribute('syntax')]
   def commutative(o):
     return o.getAttribute('commutative')
   def inputTailRepeat(o):
     return o.getAttribute('inputTailRepeat')

   # write single operator info
   def operator(g,n):
    name=n.getAttribute('name')
    opCode = opEnum(implName(n))
    if (exec_(n) == 'MRF'):
     name=name+'_mrf'
     opCode=opEnum(name)

    iOpImpl="0" # # FTTB, some not implemented yet, hence if:
    if DomUtils.childInNameSet(n, self.supported) != None or \
       exec_(n) in self.onExec:
        biName=implName(n)
        if exec_(n) == 'MRF':
          biName=biName+"_mrf"
        iOpImpl= '&builtIn_%s' % biName

    p=DomUtils.childInNameSet(n, self.pointCode)

    # mark by setting the domain arg to setPointOn
    setPointOn=""
    if p != None and p.hasAttribute("function") and name not in [ "sin", "cos", "tan" ]:
      setPointOn=self.domainIll(p)

    if implName(n) == "ifthenelse":
      setPointOn="noDomainIll"

    tail= int(inputTailRepeat(n))
    if (tail > 0):
     assert tail <= len(DomUtils.childrenInNameSet(n,["Input"]))

    g.write('{IOpImpl   *i= %s;' % iOpImpl)
    g.write(' Operator *c= new Operator(')
    g.write('''  "%s","%s",%s, SYNTAX_%s, EXEC_TYPE_%s, i,%s,%s);'''
            % (name,implName(n),opCode,syntax(n),exec_(n),commutative(n),tail))
    if setPointOn != "":
      g.write(' c->setPointOn(%s);' % setPointOn)
    for f in DomUtils.childrenInNameSet(n,["Input"]):
      g.write('  c->pushBackInput(%s);' % pcrxml.DataType(f).arg())
    for f in DomUtils.childrenInNameSet(n,["Result"]):
      g.write('  c->pushBackResult(%s);' % pcrxml.DataType(f).str())
    g.write(' add(c);')
    g.write('}')


   def cloneResultOperator(n,r):
      c=n.cloneNode(1)
      # replace Result child append the current result
      for d in DomUtils.childrenWithName(c,"Result"):
        c.removeChild(d)
      c.appendChild(r.cloneNode(1))
      c.setAttribute('syntax','Function')
      c.setAttribute('exec','DOUBLE')
      # HACK HACK mrf attr is not in the DTD
      c.setAttribute('mrf',n.getAttribute('name'))
      c.setAttribute('name',n.getAttribute('name')+r.getAttribute('functionSuffix'))
      return c

   g=self.output('operationsInit.inc')
   for n in self.operations:
    operator(g,n)
    self.operationImpl(n)
    if (exec_(n) == 'MRF'):
     oneOf = []
     for r in DomUtils.childrenWithName(n,"Result"):
       c = cloneResultOperator(n,r)
       operator(g,c)
       self.operationImpl(c)
       oneOf.append(opEnum(implName(c)))
     assert len(oneOf)==2, " for %s len(oneOf)==%d" % (implName(n),len(oneOf))
     g.write('add(%s_MRF,%s,%s);' % \
            ( opEnum(implName(n)),oneOf[0],oneOf[1]))


if __name__ == '__main__':
 X = Generator(sys.argv[1])
 X.execute()
