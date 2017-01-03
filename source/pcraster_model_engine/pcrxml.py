
# common code for using operation.dtd/pcraster.dtd related stuff
import DomUtils
import pcr

import xml.dom.minidom
import string

# read Dom and return the documentElement
def readDom(xmlFile):
   pcr.testNonEmptyFile(xmlFile)
   # minidom seems to skip whitespace that what I want
   # minidom is fast
   inpDoc = xml.dom.minidom.parse(xmlFile)
   #  dunno if root is document or node
   if inpDoc.nodeType == inpDoc.DOCUMENT_NODE:
       assert inpDoc.hasChildNodes()
       for i in inpDoc.childNodes:
        if i.nodeType == inpDoc.ELEMENT_NODE:
          return i
   else:
       assert inpDoc.nodeType == inpDoc.ELEMENT_NODE
       return inpDoc

# get calc::DataType from an [ 'Result','Input' ] element
#  processing fragment of operation.dtd
class DataType:
  def __init__(self,e):
    assert e.nodeName in [ 'Result','Input' ]

    self.d_argTypeSet = set()
    self.d_repeat     = "false"
    self.d_st         = "ST_NON"
    self.d_result     = e.nodeName == 'Result'
    self.d_vector     = 0

    if not DomUtils.onlyChildWithName(e,"Field"):
      if DomUtils.onlyChildWithName(e,"Table"):
        self.d_vs="VS_TABLE"
      if DomUtils.onlyChildWithName(e,"IndexTable"):
        self.d_vs="VS_INDEXTABLE"
      if DomUtils.onlyChildWithName(e,"TimeSeries"):
        self.d_vs="VS_TSS"
      if DomUtils.onlyChildWithName(e,"MapStack"):
        self.d_vs="VS_MAPSTACK"
      return

    # from DataType's Field child element
    assert DomUtils.onlyChildWithName(e,"Field")
    f = DomUtils.onlyChildWithName(e,"Field")
    if e.hasAttribute("repeat"):
     self.d_repeat=e.getAttribute("repeat");
    stMap={ 'Yes':'ST_SPATIAL','Non':'ST_NONSPATIAL','Either':'ST_EITHER'}
    self.d_st=stMap[f.getAttribute('spatial')]

    self.d_vs="VS_";
    argTypeMap={ 'S':'REAL4','D':'REAL4','N':'INT4','O':'INT4','B':'UINT1','L':'UINT1'}
    for vs in DomUtils.childrenWithName(f,"DataType"):
       char1 =vs.getAttribute("value")[0]
       self.d_vs=self.d_vs+char1
       self.d_argType = argTypeMap[char1]
       self.d_argTypeSet.add(self.d_argType)

    a=e.getAttribute("contextName")
    self.d_vector=a.find("[]") != -1
    self.d_vectorName=a;

  # pair with , for struct init for Operator
  def str(self):
    return self.d_vs+","+self.d_st;
  def arg(self):
    return self.str()+","+self.d_repeat
  def linkArgDef(self):
    assert(len(self.d_argTypeSet)==1)
    assert(self.d_st in [ 'ST_SPATIAL', 'ST_NONSPATIAL'] )
    if self.d_result and self.d_st != 'ST_SPATIAL':
        # see TODO below
        raise pcr.Exception('nonspatial results not supported')

    defStr = self.d_argType
    if self.d_st == 'ST_SPATIAL':
       defStr = defStr + " *"
       if not self.d_result:
         defStr = "const %s " % defStr
    if (self.d_vector):
         defStr = "std::vector<%s>" % defStr
    return defStr

# argument DECLARATION in wrapped method
# T = (UINT1,INT4,REAL4)
# spatial           single      vector
#   -y     result
#             -y         T*  std::vector<T*>&
#             -n   const T*  const std::vector<const T*>&
#   -n    result
#             -y         T&  std::vector<T>&
#             -n         T   const std::vector<T>&
# argument DEFINITION in wrapped method
# T = (UINT1,INT4,REAL4)
# spatial           single      vector
#   -y     result
#             -y         T*  std::vector<T*>
#             -n   const T*  std::vector<const T*>
#   -n    result
#             -y         T   std::vector<T>           1)
#             -n         T   std::vector<T>
#  1)TODO 
#     - how to return a single value?
#     - vector's of reference is not possible
#       create writable vector and assign the NonSpatial fields
#       after execution of the method
