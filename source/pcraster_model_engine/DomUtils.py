# utils for xml.dom(.minidom)

# Returns the text value of a DOM node.
def textValue(node):
  assert hasOneChild(node) and isTextNode(node.childNodes[0])
  return node.childNodes[0].nodeValue

# Returns whether the node is a document node.
def isDocumentNode(node):
  return node.nodeType == node.DOCUMENT_NODE

# Returns whether the node is an element node.
def isElementNode(node):
  return node.nodeType == node.ELEMENT_NODE

# Returns whether the node is a text node.
def isTextNode(node):
  return node.nodeType == node.TEXT_NODE

# Returns whether the node is an element node and has a local name of name.
def isElementNodeWithName(node, name):
  return isElementNode(node) and node.localName == name

# Returns whether the node has one child.
def hasOneChild(node):
  return len(node.childNodes) == 1

def nrChildrenWithName(node, name):
  nr = 0
  for childNode in node.childNodes:
    if childNode.localName == name:
      nr = nr + 1
  return nr

# Returns whether the number of child nodes of with name is 1.
def hasOneChildWithName(node, name):
  return nrChildrenWithName(node, name) == 1

# Returns the only child with name or None if no such child
# precond not multiple children with name
def onlyChildWithName(node, name):
  c = childrenWithName(node,name)
  assert len(c) < 2
  if len(c):
     return c[0]
  return None

def attributeWithName(node, name):
  assert isElementNode(node)
  assert node.hasAttributes()
  value = node.getAttribute(name)
  assert len(value) > 0
  return value

# Returns a list of children with name. Possibly empty.
def childrenWithName(node, name):
  resultNodes = []
  for childNode in node.childNodes:
    if childNode.localName == name:
      resultNodes.append(childNode)
  return resultNodes

# return the single child element having a name in nameSet
# multiple elements within nameSet asserts, none returns None
def childInNameSet(node,nameSet):
  c = childrenInNameSet(node,nameSet)
  assert len(c) < 2
  if len(c):
   return c[0]
  return None

# return the children having a name in nameSet or empty list if none
def childrenInNameSet(node,nameSet):
  c = []
  for n in childrenElements(node):
    if n.nodeName in nameSet:
     c.append(n)
  return c

# return list of all element children
def childrenElements(node):
  c = []
  for n in node.childNodes[:] :
   if n.nodeType == n.ELEMENT_NODE:
     c.append(n)
  return c

def appendTextNode(doc, parent, value):
  textNode = doc.createTextNode(value)
  parent.appendChild(textNode)

def appendNewline(doc, parent):
  textNode = doc.createTextNode("\n")
  parent.appendChild(textNode)

def allText(l):
    """ concat of all text children (minidom tested) """
    textContents=""
    for c in l.childNodes:
      if c.nodeType in [ c.TEXT_NODE,  c.CDATA_SECTION_NODE]:
       textContents += c.nodeValue
    return textContents
