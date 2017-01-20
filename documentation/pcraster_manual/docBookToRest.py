#!/usr/bin/env python
import os
import sys

import lxml.etree as ElementTree

# Issues
# b.v.


# http://www.sagehill.net/livedtd/docbook45/


def openFile(fileName, perm):
  # print "Writing to ", fileName
  return file(fileName, perm)


class Element(object):

  ids = []

  def __init__(self,
    element):
    self.element = element

    if "id" in self.element.attrib:
      self.id = self.element.attrib["id"]
      assert not self.element.attrib["id"] in self.ids
      self.ids.append(self.id)

  def baseName(self):
    assert "id" in self.element.attrib
    return self.id

  def fileName(self):
    return "%s.rst" % (self.baseName())



def _graphicToRest(
  element):
  assert element.tag == "graphic", element.tag
  return ".. figure:: ../%s" % (element.attrib["fileref"])



def _programListingToRest(
  element):
  assert element.tag == "programlisting", element.tag
  include = element.find("{http://www.w3.org/2001/XInclude}include")

  if include is None:
    result = "`%s`" % (_paragraphToRest(element))
  else:
    # TODO This doesn't come through in the table.
    result = ".. include:: ../%s" % (include.attrib["href"])

  return result


# <listitem><informalexample><programlisting role='pcrcalc-statement'>map2asc -m -m PCRmap.map AscFile1.txt</programlisting><informaltable frame='none' role='example-data'><tgroup cols='3'><tbody><row>
# 
# <entrytbl cols='1' role='pcr-txt-example'><tbody><row><entry><programlisting><varname>AscFile1.txt</varname></programlisting></entry></row><row><entry><programlisting><!--           <inlinegraphic format='linespecific' fileref='examples/map2asc.AscFile1.txt' /> --><xi:include href="examples/map2asc.AscFile1.txt" parse="text" xmlns:xi="http://www.w3.org/2001/XInclude"/>
#         </programlisting></entry></row></tbody></entrytbl>
# 
# <entrytbl cols='1' role='pcr-omap'><tbody><row><entry><varname>PCRmap.map</varname></entry></row><row><entry><graphic format='GIF' fileref='examples/map2asc.PCRmap.gif' /></entry></row></tbody></entrytbl>
# 
# </row></tbody></tgroup></informaltable></informalexample></listitem>
# 
# 
# <listitem><informalexample><programlisting role='pcrcalc-statement'>map2asc -m m -s s -f 3.1f PCRmap.map AscFile2.txt</programlisting><informaltable frame='none' role='example-data'><tgroup cols='3'><tbody><row>
# 
# <entrytbl cols='1' role='pcr-txt-example'><tbody><row><entry><programlisting><varname>AscFile2.txt</varname></programlisting></entry></row><row><entry><programlisting><!--<inlinegraphic format='linespecific' fileref='examples/map2asc.AscFile2.txt' /> --><xi:include href="examples/map2asc.AscFile2.txt" parse="text" xmlns:xi="http://www.w3.org/2001/XInclude"/>
#         </programlisting></entry></row></tbody></entrytbl>
# 
# <entrytbl cols='1' role='pcr-omap'><tbody><row><entry><varname>PCRmap.map</varname></entry></row><row><entry><graphic format='GIF' fileref='examples/map2asc.PCRmap.gif' /></entry></row></tbody></entrytbl>
# 
# </row></tbody></tgroup></informaltable></informalexample></listitem>

def _informalTableToRest(
  element,
  level):
  assert element.tag == "informaltable"
  result = []
  # nrRows = int(element.find("tgroup/tbody/row/entrytbl/tbody/row"))
  nrCols = int(element.find("tgroup").attrib["cols"])
  rows = element.findall("tgroup/tbody/row")
  nrRows = len(rows)
  table = [[] for i in range(nrRows)]
  colWidths = [0 for i in range(nrCols)]

  for i in range(nrRows):
    # row[i] is row in final table.
    # Each of these consists of two `sub-rows` (yes, this is weird).
    entryTables = rows[i].findall("entrytbl")
    table[i] = [[] for j in range(len(entryTables))]

    for j in range(len(entryTables)):
      # entryTables[j] is two cells on top of each other in final table.
      # This is a `sub-column` in the current row.
      entries = entryTables[j].findall("tbody/row/entry")
      assert len(entries) == 2
      table[i][j] = [[] for k in range(len(entries))]

      for k in range(len(entries)):
        # entries[k] is one cell in final table.
        assert len(entries[k].getchildren()) == 1
        child = entries[k].getchildren()[0]

        if child.tag == "varname":
          table[i][j][k] = "`%s`" % (child.text)
        elif child.tag == "graphic":
          table[i][j][k] = "%s" % (_graphicToRest(child))
        elif child.tag == "programlisting":
          table[i][j][k] = "%s" % (_programListingToRest(child))
        else:
          assert False, child.tag

  assert len(table) == nrRows
  for i in range(nrRows):
    assert len(table[i]) == nrCols
    for j in range(nrCols):
      assert len(table[i][j]) == 2
      for k in range(2):
        colWidths[j] = max(colWidths[j], len(table[i][j][k]))

  def separator(
    colWidths):
    return " ".join([line for line in [width * "=" for width in colWidths]])

  result.append("%s" % (separator(colWidths)))
  for i in range(nrRows):
    for k in range(2):
      result.append("%s" % (" ".join(["{0:{1}}".format(table[i][j][k],
        colWidths[j]) for j in range(nrCols)])))
  result.append("%s" % (separator(colWidths)))

  return level * "   " + ("\n%s" % (level * "   ")).join(result) + "\n\n"



def _orderedListToRest(
  element):
  assert element.tag == "orderedlist", element.tag

  result = ""

  for child in element.getchildren():
    assert child.tag in ["listitem"]

    assert len(child.getchildren()) == 1
    informalExample = child.getchildren()[0]
    assert informalExample.tag == "informalexample", informalExample.tag
    assert len(child.xpath("informalexample/programlisting")) == 1

    # TODO get rid of rest stuff inside this result. We only need the text here
    #      (_paragraphToText()?).
    listing = _paragraphToRest(child.find("informalexample/programlisting")).strip().split("\n")

    # Main list item.
    result += "#. ::\n\n      %s\n\n" % ("\n      ".join(listing))

    assert len(informalExample.getchildren()) <= 2

    if len(informalExample.getchildren()) == 2:
      if isinstance(informalExample.getchildren()[1], ElementTree._Comment):
        continue

      assert informalExample.getchildren()[1].tag == "informaltable", \
        informalExample.getchildren()[1].tag
      informalTable = informalExample.getchildren()[1]

      result += _informalTableToRest(child.find(
        "informalexample/informaltable"), level=1)

  return result



# def _informalExampleToRest(
#   element):
#   assert element.tag == "informalexample"
#   print "Warning: postprocess _informalExampleToRest"
#   return element.findtext(".//*")



def _literalLayoutToRest(
  element):
  assert element.tag == "literallayout"
  # Indent block.
  # result  = ".. parsed-literal::\n\n   "
  # result += _paragraphToRest(element).replace("\n", "\n   ")
  result  = _paragraphToRest(element).replace("\n", "\n  | ")
  result += "\n"
  return result



def _itemizedListToRest(
  element):
  assert element.tag == "itemizedlist"
  result = "\n"
  for item in element.findall("listitem"):
    result += "* %s\n\n" % ("\n  ".join(_paragraphToRest(item).strip().split("\n")))
  result += "\n"
  return result



def _paragraphToRest(
  element):
  result = ""

  if not element.text is None:
    result += element.text # .replace("\n", " ") # CW

  for child in element.getchildren(): # findall("*"):
    if isinstance(child, ElementTree._Comment):
      continue

    if child.tag == "para":
      result += _paragraphToRest(child)
    elif child.tag == "anchor":
      pass
    elif child.tag == "indexterm":
      pass
    # elif child.tag == "primary":
    # elif child.tag == "secondary":
    # elif child.tag == "tgroup":
    # elif child.tag == "colspec":
    # elif child.tag == "thead":
    # elif child.tag == "row":
    # elif child.tag == "entry":
    # elif child.tag == "tbody":
    elif child.tag == "xref":
      assert not "role" in child.attrib or \
        child.attrib["role"] == "bibRef", child.attrib["role"]
      if "role" in child.attrib and child.attrib["role"] == "bibRef":
        result += ":ref:`%s <bibliography>`" % (child.attrib["linkend"])
      else:
        result += ":ref:`%s`" % (child.attrib["linkend"])
    elif child.tag == "figure":
      result += "\n\n.. _%s:\n\n" % (child.attrib["id"])
      assert not child.find("title") is None
      if child.find("graphic") is None:
        print("Warning: figure with title \"%s\" has no graphic" % (
          child.findtext("title").strip().replace("\n", " ")))
      else:
        result += ".. figure:: ../%s\n\n" % (
          child.find("graphic").attrib["fileref"])
      result += "   %s" % (_paragraphToRest(child.find("title")).strip().replace("\n", " "))
    elif child.tag == "graphic":
      print("Warning: graphic outside of figure: %s" % (child.attrib["fileref"]))
    elif child.tag == "emphasis":
      result += ":emphasis:`%s`" % (_paragraphToRest(child))
    elif child.tag == "subscript":
      result += "\ :sub:`%s`" % (_paragraphToRest(child))
    elif child.tag == "superscript":
      result += "\ :sup:`%s`" % (_paragraphToRest(child))
    elif child.tag == "informalexample":
      result += _paragraphToRest(child)
    elif child.tag == "pcr-op":
      result += _paragraphToRest(child)
    elif child.tag == "varname":
      result += _paragraphToRest(child)
    elif child.tag == "variablelist":
      result += "\n\n" # CW
      for entry in child.findall("varlistentry"):
        term = _paragraphToRest(entry.find("term"))
        definition = _paragraphToRest(entry.find("listitem")).strip().replace(
          "\n", "n")
        result += "%s\n   %s\n\n" % (term, definition)
    # elif child.tag == "varlistentry":
    #   result += _paragraphToRest(child)
    elif child.tag == "screen":
      result += _paragraphToRest(child)
    elif child.tag == "term":
      result += _paragraphToRest(child)
    # elif child.tag == "orderedlist":
    #   result += _paragraphToRest(child)
    # elif child.tag == "listitem":
    #   result += _paragraphToRest(child)
    elif child.tag == "table":
      # TODO table links don't seem to work.
      result += "\n\n.. _%s:\n\n" % (child.attrib["id"])
      result += ".. table:: %s\n\n" % (
        _paragraphToRest(child.find("title")).strip().replace("\n", " "))

      # Parse the whole table, than output to rest.
      tgroup = child.find("tgroup")
      nrCols = int(tgroup.attrib["cols"])
      nrRows = 0
      values = [[] for i in range(nrCols)]
      if not tgroup.find("thead") is None:
        headings = [_paragraphToRest(entry) for entry in
          tgroup.findall("thead/row/entry")]
        assert len(headings) == nrCols, "%d != %d" % (len(headings), nrCols)
        for i in range(nrCols):
          values[i].append(headings[i])
        nrRows += 1
      if not tgroup.find("tbody") is None:
        rows = tgroup.findall("tbody/row")
        for row in rows:
          entries = [_paragraphToRest(entry) for entry in row.findall("entry")]
          assert len(entries) == nrCols, "%d != %d" % (len(entries), nrCols)
          for i in range(nrCols):
            values[i].append(entries[i])
          nrRows += 1

      colWidths = [0 for i in range(nrCols)]
      for i in range(nrCols):
        for j in range(len(values[i])):
          colWidths[i] = max(colWidths[i], len(values[i][j]))

      def separator(
        colWidths):
        return " ".join([line for line in [width * "=" for width in colWidths]])

      if nrRows > 0:
        result += "  %s\n" % (separator(colWidths))
        if not tgroup.find("thead") is None:
          result += "  %s\n" % (" ".join(["{0:{1}}".format(values[c][0],
            colWidths[c]) for c in range(nrCols)]))
          result += "  %s\n" % (separator(colWidths))
          for r in range(1, nrRows):
            result += "  %s\n" % (" ".join(["{0:{1}}".format(values[c][r],
              colWidths[c]) for c in range(nrCols)]))
        else:
          # No header.
          for r in range(nrRows):
            result += "  %s\n" % (" ".join(["{0:{1}}".format(values[c][r],
              colWidths[c]) for c in range(nrCols)]))
        result += "  %s\n" % (separator(colWidths))
    elif child.tag == "br":
      result += "\n" + _paragraphToRest(child)

    elif child.tag == "link":
      result += ":ref:`%s <%s>`" % (_paragraphToRest(child),
        child.attrib["linkend"])
    elif child.tag == "title":
      result += _paragraphToRest(child)

    elif child.tag == "literallayout":
      result += _literalLayoutToRest(child)
    elif child.tag == "programlisting":
      result += ".. parsed-literal::\n\n   "
      result += _paragraphToRest(child).replace("\n", "\n   ")
    elif child.tag == "pcr-go":
      result += ":literal:`--%s`" % (_paragraphToRest(child))
    elif child.tag == "pcr-keyword":
      result += _paragraphToRest(child)
    elif child.tag == "example":
      # result += _paragraphToRest(child)
      result += Section(child, depth=1).asRest()

    elif child.tag == "itemizedlist":
      result += _itemizedListToRest(child)

    elif child.tag == "glossseealso":
      print("Warning: skipping 'glossseealso' of {0}".format(child.attrib["otherterm"]))
    else:
      assert False, child.tag

    if not child.tail is None:
      result += child.tail.replace("\n", " ") # CW

  if len(result.strip()) == 0:
    result = ""

  return result # .strip()


class BookInfo(Element):

  def __init__(self,
    element):
    Element.__init__(self, element)
    self.title = self.element.findtext("title")

  def asRest(self):
    return "%s\n%s\n%s\n" % (len(self.title) * "#", self.title,
      len(self.title) * "#")



def _anchorsToRest(
  element,
  recurse):
  anchor = []

  if recurse:
    anchorElements = element.findall("anchor")
  else:
    anchorElements = element.xpath("anchor")

  for anchorElement in anchorElements:
    if not anchorElement is None:
      assert anchorElement.attrib["role"] == "indexterm-concept"
    anchor.append("\n\n.. _%s:" % (anchorElement.attrib["id"]))

  anchor = ("\n\n".join(anchor) + "\n\n").strip()

  return anchor  + "\n\n" if anchor else ""



def _indexToRest(
  element,
  recurse):
  index = []

  if recurse:
    indexTermElements = element.findall("indexterm")
  else:
    indexTermElements = element.xpath("indexterm")

  for indexTermElement in indexTermElements:
    if not indexTermElement is None:
      assert indexTermElement.attrib["role"] == "concept", \
        indexTermElement.attrib["role"]
      assert not indexTermElement.find("primary") is None, \
        indexTermElement.text
      if indexTermElement.find("secondary") is None:
        index.append(".. index::\n   single: %s" % (
          indexTermElement.findtext("primary")))
      else:
        index.append(".. index::\n   single: %s; %s" % (
          indexTermElement.findtext("primary"),
          indexTermElement.findtext("secondary")))

  index = ("\n\n".join(index) + "\n\n").strip()

  return index + "\n\n" if index else ""



class Para(Element):

  def __init__(self,
    element):
    Element.__init__(self, element)

  def asRest(self):
    anchor = _anchorsToRest(self.element, recurse=True)
    index = _indexToRest(self.element, recurse=True)

    paragraph = _paragraphToRest(self.element) # .strip() # .replace("\n", " ")
    # assert len(paragraph) > 0, str(dir(self.element)) + self.element.base
    # paragraph = " ".join(paragraph)
    # paragraph = paragraph.replace("\n", " ").replace("  ", " ").replace(
    #   " ,", ",")
    paragraph += "\n\n"

    return anchor + index + paragraph



class Section(Element):

  def __init__(self,
    element,
    depth=0):
    Element.__init__(self, element)
    self.depth = depth
    self.title = self.element.findtext("title").strip().replace("\n", " ")
    ### self.paras = [Para(element) for element in
    ###   self.element.findall("para")]
    ### self.sections = [Section(element) for element in
    ###   self.element.findall("section")]
    ### print len(self.paras), len(self.sections)
    ### assert (len(self.paras) > 0) ^ (len(self.sections) > 0), self.sections[0].element.findtext("title")

  def asRest(self):
    ### print "  %s%s" % (self.depth * "  ", self.title)

    underline = {
      0: "=",
      1: "-",
      2: "^",
    }

    result = ""

    if "id" in self.element.attrib:
      result = "\n\n.. _%s:\n\n" % (self.element.attrib["id"])

    result += "%s\n%s\n" % (self.title, len(self.title) * underline[self.depth])

    for child in self.element.getchildren():
      if not isinstance(child, ElementTree._Comment):
        assert child.tag in ["title", "anchor", "indexterm", "para",
          "section", "orderedlist", "informalexample", "literallayout"], \
            child.tag

    result += _anchorsToRest(self.element, recurse=False)
    result += _indexToRest(self.element, recurse=False)

    for child in self.element.xpath("para|section|orderedlist|informalexample|literallayout"):
      if child.tag == "section":
        result += Section(child, self.depth + 1).asRest()
      elif child.tag == "para":
        result += Para(child).asRest()
      elif child.tag == "orderedlist":
        result += _orderedListToRest(child)
      elif child.tag == "informalexample":
        result += Para(child).asRest()
        # result += _informalExampleToRest(child)
      elif child.tag == "literallayout":
        result += _literalLayoutToRest(child)
      else:
        assert False, child.tag

    # for para in self.paras:
    #   result += para.asRest()

    return result



class Chapter(Element):

  def __init__(self,
    element):
    Element.__init__(self, element)
    self.title = self.element.findtext("title").strip().replace("\n", " ")
    self.sections = [Section(element) for element in
      self.element.findall("section")]

  def writeRest(self,
    root):
    ### print self.title

    file_ = openFile(os.path.join(root, self.fileName()), "w")

    file_.write("\n\n.. _%s:\n\n%s\n%s\n%s\n" % (self.element.attrib["id"],
      len(self.title) * "*", self.title, len(self.title) * "*"))

    for section in self.sections:
      file_.write(section.asRest())



class Synopsis(Element):

  class Operation(object):
    def __init__(self,
      names,
      inputs,
      outputs):
      self.names = names
      self.inputs = inputs
      self.outputs = outputs

  class Operator(Operation):
    def __init__(self,
      names,
      inputs,
      outputs):
      assert len(names) == 1
      Synopsis.Operation.__init__(self, names, inputs, outputs)

    def asRest(self):
      assert len(self.inputs) == 2, self.names
      assert len(self.outputs) == 1, self.names
      return "::\n\n  %s = %s %s %s\n\n" % (
        self.outputs[0].asRest(),
        self.inputs[0].asRest(),
        self.names[0],
        self.inputs[1].asRest())

  class Function(Operation):
    def __init__(self,
      names,
      inputs,
      outputs):
      Synopsis.Operation.__init__(self, names, inputs, outputs)

    def asRest(self):
      return "::\n\n  %s = %s(%s)\n\n" % (
        " ".join(parameter.asRest() for parameter in self.outputs),
        ", ".join(self.names),
        ", ".join(parameter.asRest() for parameter in self.inputs))

  class Parameter(object):
    def __init__(self,
      name,
      dataType,
      valueType):
      self.name = name
      self.dataType = dataType
      self.valueType = valueType

    def asRest(self):
      return self.name

  def __init__(self,
    element):
    Element.__init__(self, element)
    assert element.tag == "refsynopsisdiv"

    # Parse synopses to obtain information about the syntax.
    synopses = []

    for synopsis in self.element.findall("synopsis"):
      children = synopsis.getchildren()
      assert children[0].tag == "pcr-op", children[0].tag
      assert children[0].text in ["asc2map", "col2map", "legend", "map2asc", "map2col", "mapattr", "pcrcalc", "resample", "table", "timeplot"], children[0].text

      outputVariables = []
      inputVariables = []
      operations = []

      if children[0].text in ["asc2map", "col2map", "map2asc", "map2col"]:
        # asc2map [options] asciifile PCRresult
        synopses.append([[children[2].text], [children[0].text],
          [children[1].text]])
      elif children[0].text in ["legend", "mapattr"]:
        # legend [options] PCRmap1 PCRmap2.....PCRmapn
        synopses.append([["PCRmap1", "PCRmap2", "...", "PCRmapn"],
          [children[0].text], []])
      elif children[0].text in ["resample", "table"]:
        # resample [options] Map1 Map2....PCRmapn Result
        synopses.append([["PCRmap1", "PCRmap2", "...", "PCRmapn"],
          [children[0].text], ["Result"]])
      elif children[0].text in ["timeplot"]:
        # timeplot [options] timeseries1 timeseries2.....timeseriesn
        synopses.append([["timeseries1", "timeseries2", "...", "timeseriesn"],
          [children[0].text], []])
      else:
        assert children[0].text == "pcrcalc"

        # Output variables.
        for i in range(1, len(children)):
          if children[i].tag != "varname":
            break
          outputVariables.append(children[i].text)
          if children[i].tail.strip() == "=":
            i += 1
            break
        assert len(outputVariables) > 0

        # Left operands of operators.
        if children[i].tag == "varname":
          inputVariables.append(children[i].text)
          i += 1

        # Operator symbol or function name(s).
        for j in range(i, len(children)):
          if children[j].tag != "pcr-op":
            break
          operations.append(children[j].text)
        assert len(operations) > 0

        for k in range(j, len(children)):
          if children[k].tag != "varname":
            break
          inputVariables.append(children[k].text)

        synopses.append([outputVariables, operations, inputVariables])

    assert len(synopses) > 0

    # Parse the table with information about the parameters.
    # Search for name, valueType and dataType.
    parameters = {}
    rows = self.element.findall("informaltable/tgroup/tbody/row")
    i = 0
    while i < len(rows):
      entries = rows[i].getchildren()
      assert len(entries) >= 1
      name = entries[0].findtext("varname")

      if len(entries) == 1:
        assert synopses[0][1][0] in ["map2col"], synopses[0][1]
        ## dataType = _paragraphToRest(entries[0]).strip().replace("\n", " ")
        ## valueType = None
        ## assert len(entries) == 3, len(entries)
        print("Warning: skipped doc (%s) in synopsis of %s" % (_paragraphToRest(entries[0]).strip().replace("\n", " "), name))
        i += 1
        continue
      else:
        valueType = _paragraphToRest(entries[1]).strip().replace("\n", " ")

        if i + 1 == len(rows):
          assert synopses[0][1][0] in ["map2asc", "map2col", "timeplot"], synopses[0][1]
          dataType = valueType
          valueType = None
          i += 1
        else:
          i += 1
          entries = rows[i].getchildren()

          if len(entries) == 2:
            assert synopses[0][1][0] in ["asc2map", "col2map", "lookupboolean"], synopses[0][1]
            dataType = valueType
            valueType = None
          else:
            dataType = _paragraphToRest(entries[0]).strip().replace("\n", " ")
            i += 1

        # if len(entries) > 2:
        #   assert synopses[0][1][0] in ["map2col"], synopses[0][1]
        #   assert len(entries) == 3, len(entries)
        #   assert i == 2
        #   i += 1
        #   print "Warning: skipped doc (%s) in synopsis of %s" % (_paragraphToRest(entries[2].strip().replace("\n", " ")), name)

      assert not name in parameters
      parameters[name] = Synopsis.Parameter(name, dataType, valueType)

    self.operations = []

    def operatorSyntax(
      operations):
      return len(operations) == 1 and not operations[0][0].isalpha()

    for synopsis in synopses:
      outputVariables, operations, inputVariables = synopsis

      inputs = []
      outputs = []

      for variable in inputVariables:
        # assert variable in parameters, "%s: %s" % (
        #  variable, parameters.keys())
        if not variable in parameters:
          print("Warning: input parameter %s not documented" % (variable))
          inputs.append(Synopsis.Parameter(variable, [], []))
        else:
          inputs.append(parameters[variable])

      for variable in outputVariables:
        # assert variable in parameters, "%s: %s" % (
        #  variable, parameters.keys())
        if not variable in parameters:
          print("Warning: output parameter %s not documented" % (variable))
          outputs.append(Synopsis.Parameter(variable, [], []))
        else:
          outputs.append(parameters[variable])

      if operatorSyntax(operations):
        assert len(outputVariables) == 1
        assert len(operations) == 1
        assert len(inputVariables) <= 2
        self.operations.append(Synopsis.Operator(operations, inputs, outputs))
      else:
        self.operations.append(Synopsis.Function(operations, inputs, outputs))

  def asRest(self):
    result = ""

    inputs = set()
    outputs = set()

    for operation in self.operations:
      result += operation.asRest()
      inputs |= set(operation.inputs)
      outputs |= set(operation.outputs)

    # TODO make sure the order is conform the original db table.
    for parameter in inputs:
      if not parameter.valueType is None:
        result += "%s\n   %s\n   %s\n\n" % (parameter.name, parameter.dataType,
          parameter.valueType)
      else:
        result += "%s\n   %s\n\n" % (parameter.name, parameter.dataType)

    for parameter in outputs:
      result += "%s\n   %s\n   %s\n\n" % (parameter.name, parameter.dataType,
        parameter.valueType)

    return result



class RefEntry(Element):

  def __init__(self,
    element):
    Element.__init__(self, element)
    assert element.tag == "refentry"

    self.id = self.element.attrib["id"]

    # refmeta
    self.title = self.element.findtext(
      "refmeta/refentrytitle").strip().replace("\n", " ")
    assert self.title
    if self.title in ["+", "-", "*"]:
      # These symbols are also used in headings. Escape the first character.
      self.title = "\\" + self.title
    self.name = self.element.findtext("refnamediv/refname").strip()
    assert self.name, self.title
    self.purpose = _paragraphToRest(self.element.find(
      "refnamediv/refpurpose")).strip().replace("\n", " ")
    assert self.purpose, self.name
    self.synopsis = Synopsis(self.element.find("refsynopsisdiv"))
    self.sections = [Section(element) for element in
      self.element.findall("refsect1")]

  def writeRest(self,
    root):
    ### print self.title

    if self.id in ["map2asc", "map2col", "table"]:
      print("Warning: skipping refentry for %s" % (self.id))
      return

    file_ = openFile(os.path.join(root, self.fileName()), "w")
    result = "\n\n.. _%s:\n\n%s\n%s\n%s\n" % (self.element.attrib["id"],
      len(self.title) * "*", self.title, len(self.title) * "*")
    file_.write(result)
    file_.write(".. topic:: %s\n\n   %s\n\n" % (self.name, self.purpose))
    file_.write(self.synopsis.asRest())

    for section in self.sections:
      file_.write(section.asRest())



class Reference(Element):

  def __init__(self,
    element):
    Element.__init__(self, element)
    self.title = self.element.findtext("title").strip().replace("\n", " ")
    self.refEntries = [RefEntry(element) for element in
      self.element.findall("refentry")]

  def writeRest(self,
    root):
    ### print self.title

    if self.title.find("applications") != -1:
      fileName = "applications.rst"
    elif self.title.find("Operators") != -1:
      fileName = "operations.rst"
    else:
      assert False, self.title

    file_ = openFile(os.path.join(root, fileName), "w")

    file_.write("%s\n%s\n%s\n" % (len(self.title) * "#", self.title,
      len(self.title) * "#"))
    file_.write("""\

Contents:

.. toctree::
   :maxdepth: 1

   %s
""" % (
      "\n   ".join([refEntry.baseName() for refEntry in self.refEntries])
      ))

    for refEntry in self.refEntries:
      refEntry.writeRest(root)


class GlossEntry(Element):

  def __init__(self,
    element):
    Element.__init__(self, element)

  def asRest(self):
    print("Warning: glossary terms must be referred by using 'term', %s" % (
      self.element.findtext("glossterm")))
    return "{0}\n   {1}\n".format(self.element.findtext("glossterm"),
      _paragraphToRest(self.element.find("glossdef")).strip().replace(
        "\n", " "))



class Glossary(Element):

  def __init__(self,
    element):
    assert element.tag == "glossary"
    Element.__init__(self, element)
    self.title = self.element.findtext("title").strip().replace("\n", " ")

  def writeRest(self,
    root):
    ### print self.title

    fileName = "glossary.rst"
    file_ = openFile(os.path.join(root, fileName), "w")

    file_.write("%s\n%s\n%s\n" % (len(self.title) * "#", self.title,
      len(self.title) * "#"))
    file_.write(".. glossary::\n   :sorted:\n\n")
    for glossEntry in [GlossEntry(element) for element in
      self.element.findall("glossdiv/glossentry")]:
      file_.write("\n{0}".format(glossEntry.asRest()))



class BiblioEntry(Element):

  def __init__(self,
    element):
    assert element.tag == "biblioentry"
    Element.__init__(self, element)

  def asRest(self):
    result = ""
    result += ", ".join(["{0}, {1}".format(author.findtext("surname"),
      author.findtext("firstname")) for author in
        self.element.findall(".//author")])
    result += ", {0}".format(self.element.findtext(".//copyright/year"))
    result += ", {0}.".format(self.element.findtext(".//title"))

    biblioset = self.element.find("biblioset")

    if biblioset is None:
      publisher = self.element.find("publisher")

      if not publisher is None:
        result += ", {0}".format(publisher.findtext("publishername"))
    else:
      pass

    assert not "None" in result, result

    return "{0}\n   {1}\n".format(self.element.attrib["id"],
      result.replace("\n", " "))



class Bibliography(Element):

  def __init__(self,
    element):
    assert element.tag == "bibliography"
    Element.__init__(self, element)

  def writeRest(self,
    root):
    fileName = "bibliography.rst"
    file_ = openFile(os.path.join(root, fileName), "w")

    file_.write(".. _bibliography:\n\n")
    title = self.element.findtext("title").strip().replace("\n", " ")
    file_.write("%s\n%s\n%s\n" % (len(title) * "#", title, len(title) * "#"))
    file_.write("\n".join(["{0}".format(biblioEntry.asRest()) for biblioEntry in
      [BiblioEntry(element) for element in
        self.element.findall("biblioentry")]]))



class DocBook(Element):

  def __init__(self,
    element):
    Element.__init__(self, element)

  def writeRest(self,
    root):
    file_ = openFile(os.path.join(root, "manual.rst"), "w")

    chaptersHandled = False

    for child in self.element.getchildren():
      if child.tag == "bookinfo":
        file_.write(BookInfo(child).asRest())
      elif child.tag == "chapter" and not chaptersHandled:
        chapters = [Chapter(chapter) for chapter in \
          self.element.findall("chapter")]
        file_.write("""\

Contents:

.. toctree::
   :maxdepth: 2

   %s
""" % ("\n   ".join([chapter.baseName() for chapter in chapters])))

        for chapter in chapters:
          chapter.writeRest(root)
        chaptersHandled = True
      elif child.tag == "reference":
        Reference(child).writeRest(root)
      elif child.tag == "glossary":
        Glossary(child).writeRest(root)
      elif child.tag == "bibliography":
        Bibliography(child).writeRest(root)
      elif child.tag == "index":
        pass
      else:
        assert child.tag == "chapter", child.tag



def docBookDocument(
  fileName):
  parser = ElementTree.XMLParser(remove_blank_text=True, remove_comments=True)
  return DocBook(ElementTree.parse(fileName, parser).getroot())



docBookDocument("manual.xml").writeRest("sphinx")
sys.exit(0)

