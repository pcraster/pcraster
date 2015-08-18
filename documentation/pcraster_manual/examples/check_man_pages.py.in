#!/usr/bin/env python
import sys
import os
import xml.etree.ElementTree as ElementTree


class AllEntities:
  """
  Support class for unknown entities
  http://stackoverflow.com/questions/7237466/
  python-elementtree-support-for-parsing-unknown-xml-entities
  """
  def __getitem__(self, key):
    return key

class CheckManualPages(object):
  def __init__(self, rst_dir):
    self._rst_dir = rst_dir
    self._operations = []
    self._exceptions = []
    self._opnames = dict()

    self._init_exceptions()
    self._init_operations()
    self._find_manpage()

  def _init_exceptions(self):
    """
    list of functions those pages have different names
    or are grouped on one documentation pages
    !this exceptions should actually come from (one of) the
    xml (operations, xmlExamples) files but ...!
    """
    # couldn't come up with a better idea for if = ifthen, ifthenelse
    self._exceptions += ["if"]

    santiago = ["gradx", "grady", "divergence", "diver", "lax", "laplacian"]
    self._exceptions += santiago

    what_is_this = ["c_1_2_b", "c_4_2_b", "c_s_2_b", "c_1_2_o", "c_1_2_n", "c_s_2_o", "c_s_2_n", "c_1_2_s", "c_4_2_s", "c_s_2_d", "c_d_2_s", "c_d_2_n", "c_d_2_o", "c_1_2_d", "c_4_2_d", "c_l_2_d", "c_4_2_l", "c_s_2_l", "c_d_2_l"]
    self._exceptions += what_is_this

    fractions = ["riksfraction", "squarefraction"]
    self._exceptions += fractions

    birds = ["brenner", "ibngauss"]
    self._exceptions += birds

    index = ["indexnominal", "indexboolean", "indexordinal", "indexscalar", "indexdirectional", "indexldd", "indextable", "index"]
    self._exceptions += index

    what_are_these_for = ["_ldddownstreamcell", "test_until", "mapand", "mapor", "_cellfocus", "move", "accu", "drain", "lddcreatend", "distributesimplegauss"]
    self._exceptions += what_are_these_for

    might_be_documented_somewhere = ["diffuse", "ellipseaverage"]
    self._exceptions += might_be_documented_somewhere

    used_but_not_documented = ["muskingum"]
    self._exceptions += used_but_not_documented

    # list with i) rst pages having different operator names
    # ii) several operations on one man page
    self._opnames["*"] = "ster"
    self._opnames["**"] = "sterster"
    self._opnames["+"] = "plus"
    self._opnames["-"] = "min"
    self._opnames["/"] = "slash"
    self._opnames["argorderwithid"] = "argorder"
    self._opnames["argorderwithidarealimited"] = "argorderarealimited"
    self._opnames["argorderwithidaddarealimited"] = "argorderaddarealimited"
    self._opnames["timeinputscalar"] = "timeinput..."
    self._opnames["timeinputdirectional"] = "timeinput..."
    self._opnames["timeinputboolean"] = "timeinput..."
    self._opnames["timeinputldd"] = "timeinput..."
    self._opnames["timeinputnominal"] = "timeinput..."
    self._opnames["timeinputordinal"] = "timeinput..."
    self._opnames["lookupnominal"] = "lookup"
    self._opnames["lookupboolean"] = "lookup"
    self._opnames["lookupordinal"] = "lookup"
    self._opnames["lookupscalar"] = "lookup"
    self._opnames["lookupdirectional"] = "lookup"
    self._opnames["lookupldd"] = "lookup"
    self._opnames["shift0"] = "shift"
    self._opnames["accutraveltimefractionremoved"] = "accutraveltimefraction"
    self._opnames["markwhilesumle"] = "markwhilesum"
    self._opnames["markwhilesumge"] = "markwhilesum"
    self._opnames["lookupstate"] = "dynwave"
    self._opnames["lookuppotential"] = "dynwave"



  def _init_operations(self):
    """ read all operation names from operations.xml """
    parser = ElementTree.XMLParser()
    parser.parser.UseForeignDTD(True)
    parser.entity = AllEntities()
    doc = ElementTree.parse("man_page.xml", parser)
    root = doc.getroot()

    for element in root.iter("Operation"):
      name = element.get("name")
      impl_name = element.get("implName")
      syntax = element.get("syntax")
      tmp = [name, impl_name, syntax]
      self._operations.append(tmp)

  def _op_lookup(self, name):
    try:
      res = self._opnames[name]
    except KeyError:
      res = name
    return res

  def _find_manpage(self):
    """ check if rst page is available """
    for op in self._operations:
      name = op[0]
      impl_name = op[1]
      syntax = op[2]

      # see if function name is documented on another page
      name = self._op_lookup(name)

      rst_filename = "op_{0}.rst".format(name)
      rst_path = os.path.join(self._rst_dir, rst_filename)

      # no documentation page found
      if not os.path.exists(rst_path):
        # operation is neither documented nor handled otherwise
        if name not in self._exceptions:
          # just break the build process
          msg = "\nError: no rst manual page for operation '{0}' found\n".format(name)
          raise SystemExit(msg)

if __name__ == "__main__":
  rst_directory_path = sys.argv[1]
  man_test = CheckManualPages(rst_directory_path)
  sys.exit(0)
