"""
Utilities related to source code.
"""
from __future__ import print_function
import glob
import os.path
import re
import shutil
import string
import sys
import types

# sets module is built-in from 2.6.
if 10 * sys.version_info[0] + sys.version_info[1] < 26:
  import sets

import CMake
import DevEnv
import fileutils
import stringutils
import utils

def setDefaultsEnvSettingsAndSourceDotDevenv():
  # TODO pcr bugzilla #128: doc DEVENCRC or ~/.devenv in .dox files of DEVENV
  # implementation note:
  #  - use _ as start for all vars in this function since we 'source' .devenv
  #    into the current scope and consider all without '_'-start
  #  - to reject a float value for an integer option set the default
  #    explicit to int(), see makeNrOfJobs
  # Defaults (linux, win32 are overwritten afterwards):
  _dotDevenv = {

   # build cmake's fast variant of target
   # issue: the idea was to simply prepend /fast to the target as
   # stated in
   # http://www.cmake.org/Wiki/CMake_FAQ#Is_there_a_way_to_skip_checking_of_dependent_libraries_when_compiling.3F
   # but alas, not all targets support a fast variant
   # therefore we also need to set replaceAllTarget to make cmakeFast usefull
   # with 'make all' which is our default F8 vi binding
   'cmakeFast' : False,

   # replace 'all' by another target
   'replaceAllTarget': 'all',

   # start unittest app from the debugger
   'debugUnittest' : False,

   # boost unittest's --log_level
   'unitTestLogLevel' : 'error',

   # boost unittest's --show_progress=yes/no
   'unitTestShowProgress' : 'yes',

   # boost unittest's --detect_memory_leaks
   #   with patch for 0 value, see below
   'unitTestDetectMemoryLeaks' :int(1),

   # debugger invocation (unittestapp is appended)
   #  valgrind as debugger value also works
   'debugger' : "gdb",

   # number paralel compiles from make
   'makeNrOfJobs' : int(4),

   # call something else instead of
   #  _buildCMakeTarget down in vimbuild.py
   'callInsteadOfCMakeTarget' : "",
   # -k passed to make, do NOT stop on first build error
   'makeKeepGoing' : False,

   # internals not sourced from ~/.devenv
   '_makeTargetSlash' : "/"
  }
  # Defaults different on win32:
  if sys.platform == "win32":
    # invoke visual studio
    _dotDevenv['debugger'] = "devenv /debugexe"

    # value Kor found wise for Dual Core
    _dotDevenv['makeNrOfJobs'] = int(3)

    _dotDevenv['_makeTargetSlash'] = "\\"

    return _dotDevenv

  _home = utils.environmentVariableAsNativePath("HOME")

  if "DEVENVRC" in os.environ:
    _dotDevenvFile = utils.environmentVariableAsNativePath("DEVENVRC")
  else:
    _dotDevenvFile = os.path.join(_home, ".devenvrc")

    if not os.path.exists(_dotDevenvFile):
      _dotDevenvFile = os.path.join(_home, ".devenv")

  if os.path.exists(_dotDevenvFile):
     exec(open(_dotDevenvFile).read())
     for i in locals().keys():
       if i[0] != "_":
         if not i in _dotDevenv.keys():
           print("Warning $HOME/.devenv:",i,"is not a recognized setting")
         elif type(_dotDevenv[i]) != type(locals()[i]):
           print("Warning $HOME/.devenv:",i,"does not have correct type")
         else:
           _dotDevenv[i] = locals()[i]

       if i == "unitTestLogLevel" and _dotDevenv[i] not in \
          ["all","success","test_suite","message","warning","error","cpp_exception","system_error","fatal_error","nothing"]:
           print("Warning $HOME/.devenv: unitTestLogLevel does not have a recognized value")

  return _dotDevenv



_devenvSettings = setDefaultsEnvSettingsAndSourceDotDevenv()

_cModuleExtensions = [
  ".c",
]

_cppModuleExtensions = [
  ".cc",
  ".cpp",
  ".cxx",
]

_stdHeaderNames = [
  "algorithm",
  "cassert",
  "cmath",
  "cstdlib",
  "iostream",
  "limits",
  "map",
  "memory",
  "numeric",
  "set",
  "string",
  "tuple",
  "type_traits",
  "vector",
]

_namespacePattern = "namespace\s+(\w+)\s*{"
_classDeclarationPattern = "class\s+(?P<className>\w+)\s*(?::\s*\w+\s*\w*:*\w+\s*)*(?:/{2}.*)?\s+{"

## Returns the namespace of the C++ code in the current directory.
#
# \param     filename Name of a file to base namespace on.
# \param     contents Sources to base namespace on.
# \return    The namespace guessed or an empty string.
def guessNamespace(
         filename=None,
         contents=None):
  result = ""
  filenamePattern = "(\w+)_\w+\.(?:h|cc)"

  if not result and filename:
    # Base namespace on filename.
    regex = re.compile(filenamePattern)
    match = regex.match(os.path.split(filename)[1])
    if match:
      result = match.group(1)

  if not result and contents:
    # Base namespace on source.
    regex = re.compile(_namespacePattern)
    match = regex.match(contents)
    if match:
      result = match.group(1)

  if not result:
    modules = glob.glob("*_*.cc")

    if modules:
      # Base namespace on filenames of modules.
      regex = re.compile("(\w+)_(?:\w+).cc")
      match = regex.match(modules[0])
      assert match
      result = match.group(1)
    elif os.path.exists("Makefile"):
      # try find in Makefile
      # first idea was having a target namespace echoing the namespace 
      # in Makefile and then do:
      # result = os.popen("make namespace").readlines()[-1][:-1]
      #    [-1] -> last of make output, in case of pre-depencies
      #    [:-1]  slice off newline
      # but get's tricky (infinite recursion) if I am already called 
      #   from Makefile
      # 2n idea grep NAMESPACE=namespace
      namespaceDef = ""
      grepLines = os.popen("fgrep NAMESPACE Makefile").readlines()
      if len(grepLines) > 0:
        namespaceDef = grepLines[-1]
      if (len(namespaceDef) > 0):
        namespaceDef=namespaceDef.replace("NAMESPACE", "")
        if namespaceDef.find(":=") != -1:
          namespaceDef = namespaceDef.replace(":=","")
        else:
          namespaceDef.find("=") != -1
          namespaceDef = namespaceDef.replace("=","")
        result = namespaceDef.strip()
        if len(result.split()) != 1:
          assert len(result.split()) == 0, "namespace must be one word"

  return result



def guessClassName(
         contents):
  result = ""

  if not result:
    # Try destructor, works in header and module.
    regex = re.compile("~(?P<destructorName>\w+)")
    match = regex.search(contents)
    if match:
      result = match.group("destructorName")
      if result.endswith("Private"):
        result = ""

  if not result:
    # Header has class definition.
    regex = re.compile(_classDeclarationPattern)
    match = regex.search(contents)
    if match:
      result = match.group("className")
      if result.endswith("Private"):
        result = ""

  if not result:
    # Module has constructor definition.
    regex = re.compile("(?P<constructorName>\w+)::(?P=constructorName)\(")
    match = regex.search(contents)
    if match:
      result = match.group("constructorName")

  return result



def determineNamesToWrap(
       lines):
  """
  Returns a list of names to wrap in a guarded include statement.

  The lines are inspected to determine the names of headers to include.
  Empty lines or lines with only whitespace are skipped.

  @param lines: One or more lines of C++ code.
  @returns: List of names.
  """

  validPrefix = ["typedef", "enum", "void", "new", "virtual",
                 "static", "const", "public", "protected",
                 "catch", "class", "#include", "#define",
                 "throw", "return", "struct", "if", "while", "do"]
  validName = re.compile("[a-zA-Z][a-zA-Z0-9/:_-]+")

  result = []

  if isinstance(lines, types.StringTypes):
    lines = [lines]

  for line in lines:
    line = line.strip()

    if not len(line) == 0:
      for token in line.split():
        if token in validPrefix:
          continue

        # todo: remove all after a char like (){};&*
        match = validName.search(token)
        if match:
          # name = token[:m.start]
          name = match.group()
        else:
          name = token

        if name in validPrefix:
          continue

        # ns::Class::Method --> ns::Class
        nsSplit = name.split("::", 2)
        if len(nsSplit) > 2:
          name = "%s::%s" % (nsSplit[0], nsSplit[1])

        result.append(name)
        break

  return result



## Case insensitive search for \a filename in \a directoryNames.
#
# \param     directoryNames List of directory names to search in.
# \param     filename Filename to look for.
# \return    Correctly cased filename, if found. Otherwise \a filename
#            is returned unchanced
def _findCasedFilename(
         directoryNames,
         filename):
  for directoryName in directoryNames:
    filenames = glob.glob(os.path.join(directoryName, "*"))
    upperCasedFilename = os.path.join(directoryName, filename).upper()
    upperCasedFilenames = [name.upper() for name in filenames]

    if upperCasedFilename in upperCasedFilenames:
      index = upperCasedFilenames.index(upperCasedFilename)
      filename = filenames[index]

  return filename



## Seaches for the name of a header file for class \a name in namespace \a namespace.
#
# \param     namespace Namespace of class \a name.
# \param     name Class name.
# \return    Name of header file, without directory name prepended,
#            or an empty string if the file could be found.
# \return    Correctly cased header name, without directory name prepended,
#            or an empty string if the file could not be found.
#
# The header file is searched for in the the current directory and in the
# sibling directories.
def findHeader(
         namespace,
         name):
  currentDirectoryName = os.getcwd()
  parentDirectoryName = os.path.split(currentDirectoryName)[0]
  directoriesToSearchIn = [currentDirectoryName]

  for path in os.listdir(parentDirectoryName):
    path = os.path.join(parentDirectoryName, path)
    if os.path.isdir(path) and path != currentDirectoryName:
      directoriesToSearchIn.append(path)

  namespaces = [ None, namespace ]

  for namespace in namespaces:
    # Name of header to look for. Note that the casing might be wrong.
    filename = filenameHeader(namespace, name)
    result = _findCasedFilename(directoriesToSearchIn, filename)

    if os.path.exists(result):
      result = os.path.split(result)[1]
      break
    else:
      result = ""

  return result



def guardIncludeStatement(
       name):
  """
  Returns a guarded include statement based on the name passed in.

  @param name: (Part of the) name of the header to guard.
  @return: Guarded include statement.
  """

  stdTemplate = "#include <yyy>"
  # boostTemplate = "#include <boost/yyy.hpp>"
  thirdPartyTemplate = "#include <yyy.extension>"
  defaultTemplate = "#include \"yyy.h\""

  def wrapStdHeader(
         name):
    if name in ["runtime_error" , "exception" , "bad_alloc"]:
      name = "stdexcept"
    elif name in ["ostream" , "istream" , "cout", "cerr", "cin", "PRINT_VAR"]:
      # NOTE: ostream and istream hebben eigenlijk hun eigen header maar in
      # gcc niet!
      name = "iostream"
    elif name in ["ofstream" , "ifstream"]:
      name = "fstream"
    elif name in ["ostrstream" , "istrstream"]:
      return("#error %s is deprecated use [io]stringstream instead" %
            (name));
    elif name in ["ostringstream" , "istringstream"]:
      name = "sstream"
    elif name in ["auto_ptr"]:
      name = "memory"

    result = stdTemplate
    # result = stringutils.replaceCase("u", result, "XXX", name.replace("/", "_"))
    result = result.replace("yyy", name)
    return result

  def wrapQtHeader(
         name):
    # Find header in Qt include dirs.
    qtRoot = os.environ["QTDIR"]
    directoriesToSearchIn = [
      os.path.join(qtRoot, "include", "QtCore"),
      os.path.join(qtRoot, "include", "QtGui"),
      os.path.join(qtRoot, "include", "QtSql"),
      os.path.join(qtRoot, "include", "QtXml"),
      os.path.join(qtRoot, "include", "QtOpenGL"),
    ]

    name = _findCasedFilename(directoriesToSearchIn, name)
    name = os.path.split(name)[1]

    result = stdTemplate
    # result = stringutils.replaceCase("u", result, "XXX", name)
    result = result.replace("yyy", name)

    return result

  def wrapThirdPartyHeader(
         name,
         extension="h"):
    # These capture not all cases, but are useful.
    nameIsClassName = name.find("::") != -1   # 100% sure.
    nameIsHeaderName = name.find("/") != -1   # 100% sure.
    name = name.replace("::", "/")

    result = thirdPartyTemplate
    # result = stringutils.replaceCase("u", result, "XXX",
    #      name.replace("/", "_").replace("-", "_"))

    if nameIsHeaderName or not nameIsClassName:
      # Assume header has all the cases right already.
      result = result.replace("yyy", name)
    else:
      # Assume the header name is equal to the class name with lower caps.
      result = stringutils.replaceCase("l", result, "yyy", name)

    result = result.replace("extension", extension)

    return result

  def wrapDefaultHeader(
         name):
    result = defaultTemplate
    # result = stringutils.replaceCase("u", result, "XXX", name.replace("/", "_"))
    result = result.replace("yyy", name)

    return result

  result = ""

  name = os.path.splitext(name)[0]

  if name == "PRINT_VAR":
    name = "std::cerr"
  if name == "BOOST_FOREACH":
    name = "boost::foreach"

  if name.find("std::") == 0:
    result = wrapStdHeader(name[5:])
  elif name in _stdHeaderNames:
    result = wrapStdHeader(name)
  elif name.find("boost") == 0 or name.find("xercesc") == 0:
    result = wrapThirdPartyHeader(name, extension="hpp")
  elif name.find("xsd") == 0:
    result = wrapThirdPartyHeader(name, extension="hxx")
  elif   name.find("ogr") == 0 or \
         name.find("gdal") == 0 or \
         name.find("geos") == 0 or \
         name.find("qwt") == 0:
    result = wrapThirdPartyHeader(name, extension="h")
  elif name[0] in ["q", "Q"]:
    result = wrapQtHeader(name)
  else:
    # namespace::className?
    name = name.replace("::","_")

    if "_" in name:
      namespace, name = name.split("_")
    else:
      namespace = guessNamespace()

    headerFilename = findHeader(namespace, name)

    if headerFilename:
      # It is one of ours.
      # if namespace:
      #   name = "%s_%s" % (namespace, name)
      # result = wrapDefaultHeader(name)
      result = wrapDefaultHeader(os.path.splitext(headerFilename)[0])
    elif namespace:
      result = wrapDefaultHeader("%s_%s" % (namespace, name))
    else:
      # It is one of Them.
      result = wrapThirdPartyHeader(name)

  return result



def guardIncludeStatements(
         lines):
  names = determineNamesToWrap(lines)
  result = []
  for name in names:
    result.append(guardIncludeStatement(name))

  return "\n".join(result)

def _cppCopyAndAssignmentCode(className):
  templateCode=""" \
  //! Assignment operator.
  Class&           operator=           (Class const& rhs);

  //! Copy constructor.
                   Class               (Class const& rhs);

//! Copy constructor.
Class::Class(
         Class const& rhs)
  : Base(rhs), // a friendly reminder from your code generator: delete if there is no base
    d_something(rhs.d_something)
{
}

//! Assignment operator.
Class& Class::operator=(
         Class const& rhs)
{
  BaseClass::operator=(rhs); // a friendly reminder from your code generator: delete if there is no base
  if(this != &rhs) {
    d_something = rhs.d_something;
  }

  return *this;
}
"""
  return templateCode.replace("Class",className)


def cppPropertyCode(
         sourceFile,
         lines):
    # purpose:
    #  generate accessor and 'setter' a.k.a properties
    #  for class member definitions read on input
    # use:
    # replace buffer by output command in vim:
    #  eg. !}genprop
    # some test input at end

    contents = file(sourceFile).read().decode("utf-8")
    cl = guessClassName(contents=contents)

    # TODO signed/unsigned prefix with space flexibility
    # these are passed by copy, others by const-ref
    basicTypes = [ 'bool', 'char', 'int', 'size_t', 'long',
                    'float', 'double', 'UINT1', 'INT1',
                    'REAL4', 'REAL8', 'UINT4', 'INT4', 'CSF_VS', 'CSF_CR' ]

    def className(withNs=0):
     if withNs:
       return ns+"::"+cl
     return cl

    class Member:
      def __init__(self,type,name):
        self.d_type=type
        assert(name[0:2]=="d_")
        self.d_name=name[2:]
        self.d_typeLen = max(16,len(self.d_type))
      def d_(self):
        return "d_"+self.d_name
      def set(self):
        return "set"+self.d_name[0].upper()+self.d_name[1:]
      # as passed i/o to the object
      def typePass(self, outNs=0):
        if self.d_type in basicTypes:
          return self.d_type
        t = self.d_type
        # if outNs: # outside of namespace
        #   if t[0] == t[0].upper():
        #     # looks like a class with no ns
        #     # so it the guessed namespace
        #     t = ns+"::"+t
        # keep ptrs intact
        if t[-1] == "*":
          return t
        # obj by const-ref
        return t+" const&"
      def typePassFmt(self):
        s = self.typePass()
        l = max(16,len(s))
        return "%-*s" % (l,s)
      def memberDef(self):
        print("  %-*s %s;" % (self.d_typeLen,self.d_type,self.d_()))
      def getDecl(self):
        print("  %s %-20s() const;" % (self.typePassFmt(),self.d_name))
      def setDecl(self):
        print("  %-17s%-20s(%s %s);" %
          ("void",self.set(),self.typePass(),self.d_name))
      def getDef(self):
        print("//! get value of %s" % self.d_name)
        print("%s %s::%s() const\n{" % (self.typePass(1),className(),self.d_name))
        print("  return %s;\n}\n" % (self.d_()))
      def setDef(self):
        print("//! set value of %s" % self.d_name)
        print("void %s::%s(%s %s)\n{" %
           (className(),self.set(),self.typePass(),self.d_name))
        print("  %s=%s;\n}\n" % (self.d_(),self.d_name))


    # TODO implement generic C++ comment stripping
    #      and iterate over that output
    members=[]
    for line in lines:
      line.strip(line)
      line = line.replace(";","")
      if len(line) == 0:
        continue
      if line[0] == '=':
         print(_cppCopyAndAssignmentCode(className()))
         continue
      type= " ".join(string.split(line)[:-1])
      name= string.split(line)[-1]
      members.append(Member(type,name))

    map((lambda m: m.memberDef()),members)
    map((lambda m: m.setDecl()),members)
    map((lambda m: m.getDecl()),members)
    map((lambda m: m.setDef()),members)
    map((lambda m: m.getDef()),members)

    # test input, excl. from 2-nd # and on
    #  std::vector<com::PathName>  d_inputFiles;  # len(type) > 17
    #  com::PathName               d_outputFile;  # has a ns
    #  unsigned size_t             d_bucketSize;  # not correct,unsigned foobars
    #  Centre                      d_centre;      # prefix with guessed ns
    #  double                      d_halfWidth;   # basic type


def splitName(
         name):
    # Find name of namespace and name of target.
    pos = name.find("::")
    if pos == -1:
      namespace = guessNamespace()
      className = name
    else:
      # Then split name being namespace::CapitalizedClass into
      namespace = name[0:pos]
      className = name[pos + 2:]

    return namespace, className

def filenameHeader(
         namespace,
         name):
  """
  Returns the name of a header file based on our conventions. These are:
    - If a namespace is used the name starts with the namespace folowed
      by an underscore.
    - The name passed in is appended verbatim, without changing its case.
    - The .h extension is appended.
  In other words: {namespace_}name.h

  @param namespace: Namespace used, might be empty.
  @param name: Name of the file without taking the namespace into account.
  @return: Filename for the header.
  """
  assert not os.path.isabs(name)
  assert len(os.path.splitext(name)[1]) == 0

  if namespace:
    headerFilename = namespace + "_" + name + ".h"
  else:
    headerFilename = name + ".h"

  return headerFilename

def filenameModule(
         namespace,
         name):
  """
  Returns the name of a module file based on our conventions. These are:
    - If a namespace is used the name starts with the namespace folowed
      by an underscore.
    - The name passed in is appended verbatim, without changing its case.
    - The .cc extension is appended.
  In other words: {namespace_}name.cc

  @param namespace: Namespace used, might be empty.
  @param name: Name of the file without taking the namespace into account.
  @return: Filename for the module.
  """
  assert not os.path.isabs(name)
  assert len(os.path.splitext(name)[1]) == 0

  if namespace:
    moduleFilename = namespace + "_" + name + ".cc"
  else:
    moduleFilename = name + ".cc"

  return moduleFilename

def _postProcessCPlusPlusStubs(
         namespace,
         header,
         module):
  if namespace:
    header = stringutils.replaceCase("lu", header, "pack", namespace)
    module = stringutils.replaceCase("lu", module, "pack", namespace)
  else:
    header = stringutils.replaceCase("lu", header, "pack_", "")
    header = stringutils.replaceCase("lu", header, " pack", "")
    module = stringutils.replaceCase("lu", module, "pack_", "")
    module = stringutils.replaceCase("lu", module, " pack", "")

  return header, module



def generateGuiTestStub(
         namespace,
         className):
  # Read templates and search and replace stuff and write them out.
  stubHeaderFilename = DevEnv.stubFilename("GuiUnitTest.h")
  stubModuleFilename = DevEnv.stubFilename("GuiUnitTest.cc")
  assert os.path.exists(stubHeaderFilename)
  assert os.path.exists(stubModuleFilename)
  headerFilename = filenameHeader(namespace, className + "Test")
  moduleFilename = filenameModule(namespace, className + "Test")

  header = file(stubHeaderFilename).read()
  module = file(stubModuleFilename).read()

  module = module.replace("pack_classtest.h", headerFilename)

  header, module = _postProcessCPlusPlusStubs(namespace, header, module)

  header = stringutils.replaceCase("u", header, "CLASSTEST", className + "TEST")
  header = header.replace("Class", className)

  module = stringutils.replaceCase("uc", module, "Class", className)

  fileutils.writeNewFile(headerFilename, header)
  fileutils.writeNewFile(moduleFilename, module)



def generateTestStub(
         namespace,
         className,
         plural):
  # Read templates and search and replace stuff and write them out.
  stubHeaderFilename = DevEnv.stubFilename("UnitTest.h")
  stubModuleFilename = DevEnv.stubFilename("UnitTest.cc")
  assert os.path.exists(stubHeaderFilename)
  assert os.path.exists(stubModuleFilename)
  headerFilename = filenameHeader(namespace, className + "Test" +
    ("s" if plural else ""))
  moduleFilename = filenameModule(namespace, className + "Test" +
    ("s" if plural else ""))

  header = file(stubHeaderFilename).read()
  module = file(stubModuleFilename).read()

  module = module.replace("pack_classtest.h", headerFilename)

  header, module = _postProcessCPlusPlusStubs(namespace, header, module)

  header = stringutils.replaceCase("u", header, "TESTCLASS", className + "TEST")
  header = header.replace("Class", className)

  # Prevent replacing CLASS in this case.
  module = module.replace("BOOST_CLASS_TEST_CASE", "BOOST_SSALC_TEST_CASE")
  module = stringutils.replaceCase("uc", module, "Class", className)
  module = module.replace("BOOST_SSALC_TEST_CASE", "BOOST_CLASS_TEST_CASE")

  fileutils.writeNewFile(headerFilename, header)
  fileutils.writeNewFile(moduleFilename, module)

def generateClassStub(
         namespace,
         className):
  # Read templates and search and replace stuff and write them out.
  stubHeaderFilename = DevEnv.stubFilename("[Cc]lass.h")
  stubModuleFilename = DevEnv.stubFilename("[Cc]lass.cc")
  assert os.path.exists(stubHeaderFilename)
  assert os.path.exists(stubModuleFilename)
  headerFilename = filenameHeader(namespace, className)
  moduleFilename = filenameModule(namespace, className)

  header = file(stubHeaderFilename).read()
  module = file(stubModuleFilename).read()

  module = module.replace("pack_class.h", headerFilename)

  header, module = _postProcessCPlusPlusStubs(namespace, header, module)

  header = stringutils.replaceCase("uc", header, "Class", className)
  module = stringutils.replaceCase("uc", module, "Class", className)

  fileutils.writeNewFile(headerFilename, header)
  fileutils.writeNewFile(moduleFilename, module)

def generateStubs(
         namespace,
         className):
  """
  Generates empty source files for a class named namespace::className.

  If the class name ends on Test, then we assume we need to create a stub for
  unittest code.

  If the class name ends on GuiTest, then we assume we need to create a stub
  for Qt Gui unit test code.

  @param namespace: Namespace to put class in, might be empty.
  @param className: Class name to generate stub for.
  @see: L{generateClassStub}, L{generateTestStub}
  """
  pos1 = className.rfind("Tests")
  pos2 = className.rfind("Test")

  if pos2 > 0: # and pos == len(className) - 4:
    if className.rfind("GuiTest") > 0:
      generateGuiTestStub(namespace, className[:pos])
    else:
      if pos1 > 0:
        generateTestStub(namespace, className[:pos1], plural=True)
      else:
        generateTestStub(namespace, className[:pos2], plural=False)
  else:
    generateClassStub(namespace, className)

def _buildVSProject(
         name):
  projects = glob.glob(os.path.join(name, "*.sln"))
  project = None
  options = "/nologo /verbosity:quiet"

  if len(projects) > 0:
    project = projects[0]
  else:
    projects = glob.glob(os.path.join(name, "*.vcproj"))
    options += " /property:Configuration=Debug"
    project = projects[0]

  os.chdir(name)
  status, stdout, stderr = utils.execute(
         "MSBuild.exe %s %s" % (options, project))

  # It appears that all output goes to stdout, no matter what the result status
  # of the build is.
  # Print only lines containing warnings and error messages.
  assert len(stderr) == 0
  for line in stdout.split("\n"):
    if len(line.strip()) > 0:
      if not "__________" in line and \
         not "Project " in line and \
         not "warning MSB4098:" in line and \
         not "Done building project " in line:
         print(line, end="")



def _buildVSObject(
         name):
  print("Building VS objects is not supported yet...")



def _nativeMake():
  keepGoingOption=""
  if _devenvSettings["makeKeepGoing"]:
     keepGoingOption=-k
  result = "make %s -j%d" % (keepGoingOption, _devenvSettings["makeNrOfJobs"])

  return result



def buildProject2(
  name):
  os.chdir(os.path.join(DevEnv.objectsBuildTypeRoot()))

  baseName = DevEnv.projectBaseName(name)
  assert os.path.exists(baseName), baseName
  assert os.path.isdir(baseName), baseName
  os.chdir(baseName)

  return utils.call("%s" % (_nativeMake()))



def rebuildProject(
         generator,
         name):
  result = DevEnv.reconfigureProject(generator, name)

  if result == 0:
    result = buildProject2(name)

  return result



def rebuildProjects(
         generator,
         names):
  result = 0

  for name in names:
    result = rebuildProject(generator, name)

    if result != 0:
      break

  return result



def buildProjects(
         names):
  result = 0

  for name in names:
    result = buildProject2(name)

    if result != 0:
      break

  return result



def objectsDirectoryName(
  pathName):
  result = DevEnv.objectsBuildTypeRoot(pathName)
  assert os.path.exists(result), result
  assert os.path.exists(os.path.join(result, "CMakeCache.txt")), \
    "Configure CMake, See DevEnv Documentation -> Installation"
  result = os.path.join(result, _relativeProjectPath(pathName))
  assert os.path.exists(result), result
  return result



def _rootOfCMakeSourceTree(
         name):
  result = name

  parent = os.path.split(result)[0]

  if os.path.exists(os.path.join(parent, "CMakeLists.txt")):
    result = _rootOfCMakeSourceTree(parent)
  else:
    # In some projects we have a directory set-up such that there is a
    # directory with headers only and no CMakeLists.txt file and a sub-directory
    # with sources (eg: Tests). So, we check the parent of the parent too for
    # a CMakeLists.txt file. This potentially cause a failure in other cases(?).
    parent = os.path.split(parent)[0]

    if os.path.exists(os.path.join(parent, "CMakeLists.txt")):
      result = _rootOfCMakeSourceTree(parent)

  assert os.path.exists(result), result
  return result



def _targetName(
         path):
  return os.path.split(path)[1]



def _testTargetName(
         path):
  # Find test target in CMakeLists.txt file and return its name.
  assert os.path.isdir(path)
  fileName = os.path.join(path, "CMakeLists.txt")
  assert os.path.exists(fileName)
  contents = file(fileName, "r").read()

  # Assumes all unit test executables start with test*.
  expression = "add_executable\s*\(\s*(test[a-z0-9A-Z-]+)"
  pattern = re.compile(expression, re.IGNORECASE)

  match = re.search(pattern, contents)
  if match:
    result = match.group(1)
  else:
    # Default test target name is test<base name of path>.
    result = "test{}".format(os.path.basename(path))

  return result

  # assert match, "Could not find test target, codeutils.py:_testTargetName"

  # return match.group(1)



def _newStyleUnitTestExecutable(
  testTargetName):
  return "run{0}{1}".format(testTargetName[0].upper(), testTargetName[1:])



def _newStyleUnitTesting(
  objectsDirectoryName,
  testTargetName):
  # Return whether run<TestTarget> is one of the available targets.
  status, output, errors = utils.execute("make -C {0} help".format(
    objectsDirectoryName))
  return status == 0 and output.find(
    _newStyleUnitTestExecutable(testTargetName)) != -1



def _runUnitTests(
  path):
  objectsDirectory = objectsDirectoryName(path)
  testTarget = _testTargetName(path)

  if _newStyleUnitTesting(objectsDirectory, testTarget):
    return utils.call("make -C {0} {1}".format(
      objectsDirectory, _newStyleUnitTestExecutable(testTarget)))

  result = _buildCMakeTarget(objectsDirectoryName(path), testTarget)

  if result != 0:
    return result

  testExecutable = os.path.join(DevEnv.objectsBuildTypeRoot(path),
    "bin", testTarget)
  if sys.platform == "win32":
    testExecutable += ".exe"

  # projectName = DevEnv.projectName(path)
  # relativeProjectPath = _relativeProjectPath(path)

  assert os.path.exists(testExecutable), testExecutable
  # if not os.path.exists(testExecutable):
  #   print "%s not found, not running unit tests" % (testExecutable)
  # else:

  testsDirectory = os.path.join(DevEnv.testsRoot(), DevEnv.projectName(path),
         _relativeProjectPath(path))

  # Create entire path.
  if not os.path.exists(testsDirectory):
    os.makedirs(testsDirectory)

    # Ensure empty directory.
    ## shutil.rmtree(testsDirectory)
    ## os.mkdir(testsDirectory)

  os.chdir(testsDirectory)

  prolog = os.path.join(path, "testrun.prolog")
  epilog = os.path.join(path, "testrun.epilog")

  if os.path.exists(prolog):
    status = utils.call("bash %s" % (prolog))
    assert status == 0

  # other usefull options
  #  --build_info=yes  gives stuff we can use for dashboard
  # see http://www.boost.org/doc/libs/1_35_0/libs/test/doc/components/utf/parameters/index.html for all options
  if _devenvSettings["debugUnittest"]:
     cmd = _devenvSettings["debugger"]+" "+testExecutable
  else:
     cmd = testExecutable
     optionMapping = {
       # ~/.devenv        : boost test
       'unitTestLogLevel' : 'log_level',
       'unitTestShowProgress' : 'show_progress',
       'unitTestDetectMemoryLeaks' : 'detect_memory_leaks' }
       # boost unittest's --detect_memory_leaks
       #   docs 1.38.0 and earlier are wrong, the plural leaks is working
     for o in optionMapping.keys():
        cmd += " --%s=%s" % (optionMapping[o], _devenvSettings[o])
  status = utils.call(cmd)

  if os.path.exists(epilog):
    status = utils.call("bash %s" % (epilog))
    assert status == 0

def _relativeProjectPath(
         path):
  assert os.path.isdir(path), path

  while not os.path.exists(os.path.join(path, "CMakeLists.txt")):
    path = os.path.split(path)[0]

  assert os.path.exists(path), path
  assert os.path.exists(os.path.join(path, "CMakeLists.txt")), path

  prefix = os.path.commonprefix([_rootOfCMakeSourceTree(path), path])

  return path.replace(prefix, "")[1:]

def _buildCMakeTarget(
         objectsDirectory,
         target):
  if len(_devenvSettings["callInsteadOfCMakeTarget"]):
    status = utils.call(_devenvSettings["callInsteadOfCMakeTarget"])
    return status

  cwd = os.getcwd()
  os.chdir(objectsDirectory)
  if "MAKEFLAGS" in os.environ:
    # Gnu make controls most makefiles in project directories. On windows we
    # use nmake to build targets for C++ code. When nmake is (indirectly)
    # called from gnu make it complains about the MAKEFLAGS set by gnu make.
    # See it in action by running 'make all' in one of the C++ targets
    # directories.
    del os.environ["MAKEFLAGS"]
  # WARNING, not all cmake generated targets support fast.
  if _devenvSettings["cmakeFast"]:
    target += _devenvSettings['_makeTargetSlash'] + "fast"

  status = CMake.build(target, _devenvSettings["makeNrOfJobs"])
  os.chdir(cwd)
  # return status, stdout, stderr
  return status



def _buildCMakeProject(
         path):
  return _buildCMakeTarget(objectsDirectoryName(path),
         _devenvSettings['replaceAllTarget'])
         # _targetName(path))



def _cleanCMakeProject(
         path):
  return _buildCMakeTarget(objectsDirectoryName(path), "clean")

def _objectExtension():
  result = ".o"
  if sys.platform == "win32":
    result = ".obj"
  return result

def _buildCMakeObject(
         path):
  targetDirectory, sourceName = os.path.split(path)
  objectsDirectory = objectsDirectoryName(targetDirectory)

  target = "%s%s" % (os.path.splitext(sourceName)[0], _objectExtension())

  while not os.path.exists(os.path.join(targetDirectory, "CMakeLists.txt")):
    targetDirectory, prefix = os.path.split(targetDirectory)
    target = os.path.join(prefix, target)

  return _buildCMakeTarget(objectsDirectory, target)

def _buildSconsProject(
         name):
  os.chdir(name)
  options = "-u prjDir=%s" % (name)
  status, stdout, stderr = utils.execute("pythonwrapper scons %s" % (options))

  assert len(stderr) == 0
  print(stdout)



def _buildSconsObject(
         name):
  os.chdir(name)
  options = "-u %s" % (name)
  status, stdout, stderr = utils.execute("pythonwrapper scons %s" % (options))

  assert len(stderr) == 0
  print(stdout)



def _buildMakeProject(
         name):
  options = "-C %s" % (name)
  utils.call("make %s" % (options))
  # status, stdout, stderr = utils.execute("make %s" % (options))
  # if stdout: print stdout
  # if stderr: print stderr



def _buildMakeObject(
         name):
  options = "-C %s" % (os.path.split(name)[0])
  status, stdout, stderr = utils.execute(
         "make %s %s" % (options, os.path.split(name)[1]))
  print(stdout)
  print(stderr)



def _guessProjectType(
         name):
  result = ""

  # First test for cmake files. CMake project types has precedence over other
  # project types.
  if os.path.exists(os.path.join(name, "CMakeLists.txt")) or \
     os.path.exists(os.path.join(name.split(name)[0], "CMakeLists.txt")):
    result = "cmake"
  elif len(glob.glob(os.path.join(name, "*.sln"))) or \
         len(glob.glob(os.path.join(name, "*.vcproj"))):
    # If directory contains a file ending on sln or vcproj than we assume
    # msbuild can be used to build the project.
    result = "vs"
  elif len(glob.glob(os.path.join(name, "SConscript"))):
    result = "scons"
  elif len(glob.glob(os.path.join(name, "Makefile"))) or \
       len(glob.glob(os.path.join(name, "GNUmakefile"))):
    result = "make"

  return result

def runTests(
         path):
  assert os.path.exists(path), path
  assert os.path.isdir(path), path
  assert os.path.isabs(path), path

  type = _guessProjectType(path)
  assert type == "cmake"

  # if _buildCMakeProject(path) == 0:
  _runUnitTests(path)

def buildProject(
         path):
  """
  @todo: Print output of build process as it comes available, not by the time the build is finished.
  """
  assert os.path.exists(path), path
  assert os.path.isdir(path), path
  assert os.path.isabs(path), path

  result = 1
  type = _guessProjectType(path)

  if type == "vs":
    result = _buildVSProject(path)
  elif type == "cmake":
    result = _buildCMakeProject(path)
  elif type == "scons":
    result = _buildSconsProject(path)
  elif type == "make":
    result = _buildMakeProject(path)
  else:
    assert type == ""
    print("I don't now what to do..., add logic to codeutils.py:buildProject")

  return result

def cleanProject(
         path):
  assert os.path.exists(path), path
  assert os.path.isdir(path), path
  assert os.path.isabs(path), path

  result = 1
  type = _guessProjectType(path)

  if type == "cmake":
    result = _cleanCMakeProject(path)
  else:
    assert type == ""
    print("I don't now what to do..., add logic to codeutils.py:cleanProject")

  return result

def buildObject(
         path):
  """
  @todo: Print output of build process as it comes available, not by the time the build is finished.
  """
  assert os.path.exists(path), path
  assert os.path.isfile(path), path
  assert os.path.isabs(path), path

  result = 1
  type = _guessProjectType(os.path.split(path)[0])

  if type == "vs":
    result = _buildVSObject(path)
  elif type == "cmake":
    result = _buildCMakeObject(path)
  elif type == "scons":
    result = _buildSconsObject(path)
  elif type == "make":
    result = _buildMakeObject(path)
  else:
    assert type == ""
    print("I don't now what to do..., add logic to codeutils.py:buildObject")

  return result



def fixOldStylePrototypes(
         source):
  """
  Rewrites old style (K&R) C function prototypes to ANSI C prototypes. It
  does not touch the other code in source. Spacing in the type specification
  is maintained, though tabs and multiple spaces between type tokens and name
  are replaced by single spaces.

  For example::
    char *const function(foo, bar)
      int foo;
      char* bar;
    {
  becomes::
    char *const function(int foo, char* bar)
    {

  @param source: Source code with prototypes to change.
  @return: The input source with fixed prototypes.
  """

  def _rewritePrototype(
         match):
    returnType = match.group("returnType")
    if returnType == "static":
      returnType += " int"
    result = "%s %s(" % (returnType or "int",
         match.group("functionName"))
    argumentNames = [name.strip() for name in
         match.group("argumentNames").split(",")]
    typeNameTuples = [" ".join(tuple.strip().split()) for tuple in
         match.group("typeNameTuples").split(";")[:-1]]
    assert len(argumentNames) == len(typeNameTuples)
    for i in range(len(argumentNames)):
      if i > 0:
        result += ", "
      result += typeNameTuples[i]
    result += ")\n{"
    return result

  # Regex for old style C prototypes.
  # {type} fname ( name, name, name ) type name; type name; type name; {
  # The regex for the name of the function and name for the arguments matches
  # the pointer symbol (*) too. For the result this doesn't matter.
  typePattern = "[\w*]+(?:[ \t]+[\w*]+)*"
  regex = re.compile(r"""
# Optional return type, consisting of one or more tokens. Assume the
# return type is seperated from the function name by one or more spaces
# or tabs (no newlines!).
(?:(?P<returnType>%s)[ \t]+)?

# Function name
(?P<functionName>[\w*]+)\s*

# One or more argument names, between parenthesis.
\(\s*
(?P<argumentNames>\w+(?:\s*,\s*\w+)*)\s*
# \s*
\)\s*

# One or more type name tuples.
(?P<typeNameTuples>%s\s*[\w*]+\s*;(?:\s*%s\s*[\w*]+\s*;)*)\s*
\{
""" % (typePattern, typePattern, typePattern), re.VERBOSE)
  source = regex.sub(_rewritePrototype, source)
  return source



def rename(
    currentName,
    newName,
    filenames):
  currentNamespace, currentName = splitName(currentName)
  newNamespace, newName = splitName(newName)

  print("renaming %s::%s to %s::%s..." % (
         currentNamespace, currentName, newNamespace, newName))

  for filename in filenames:
    # Determine namespace in file.
    contents = file(filename).read().decode("utf-8")
    namespace = guessNamespace(filename=filename, contents=contents)
    className = guessClassName(contents=contents)
    print("%s (%s::%s)" % (filename, namespace, className))

    # Rename NAMESPACE_NAME symbols.
    contents = stringutils.replaceCase("u", contents,
          "%s_%s" % (currentNamespace, currentName),
          "%s_%s" % (newNamespace, newName))

    # Rename NAME symbols.
    contents = stringutils.replaceCase("u", contents, currentName, newName)

    # Rename included header.
    contents = contents.replace(
          "include \"%s_%s.h\"" % (currentNamespace, currentName),
          "include \"%s_%s.h\"" % (newNamespace, newName), 1)

    if namespace == currentNamespace:
      # Code in current file is in the same namespace as the namespace to be
      # renamed.
      if className == currentName:
        # Current file contains the class definition.
        # Rename namespace bla {.
        # Rename namespace bla.
        contents = re.sub(_namespacePattern,
              "namespace %s {" % (newNamespace), contents)
        contents = re.sub("namespace\s+%s" % (currentNamespace),
              "namespace %s" % (newNamespace), contents)

      contents = contents.replace(currentName, newName)
    else:
      # Rename namespace::Name symbols.
      pass

    # print contents
    file(filename, "wb").write(contents.encode("utf-8"))



class FunctionCallMatches(list):
  """
  Class that stores info about the occurences of a function call in one source
  file.
  """
  def __init__(self,
         filename,
         matches):
    self._filename = filename
    self += matches

  def _getFilename(self):
    return self._filename

  filename = property(_getFilename)



class FunctionCallInfo(list):
  """
  Class that stores info about the occurences of a function call in source
  files.
  """
  def __init__(self,
         name):
    self._name = name

  def _getName(self):
    return self._name

  name = property(_getName)

  def __repr__(self):

    result = """\
--------------------------------------------------------------------------------
function %s %d %d
""" % (self.name, len(self), sum(len(match) for match in self))

    for matchesPerFile in self:
      for match in matchesPerFile:
        result += """\
%s %s(%s)
""" % (matchesPerFile.filename, match.group("name"),
    ", ".join([argument.strip() for argument in match.group("arguments").split(",")]))

    return result



def functionCallMatches(
         name,
         source):
  """
  Find and store function call occurences of one function call in one source
  string.
  """
  result = []
  # TODO
  # Improve the argument pattern:
  # cast* space* variable|functioncall
  # - reference
  # - functioncall
  # - string
  # - ...
  argumentPattern = "\S+"
  argumentListPattern = "%s(?:\s*,\s*%s)*" % (argumentPattern, argumentPattern)
  # functionCallPattern = "^(?P<qualification>.*\s*)\\b(?P<name>%s)\s*\(\s*(?P<arguments>(?:%s)?)\s*\)" % (
  functionCallPattern = "^(?P<qualification>.*)\\b(?P<name>%s)\s*\(\s*(?P<arguments>(?:%s)?)\s*\)" % (
         name, argumentListPattern)
  regex = re.compile(functionCallPattern, re.MULTILINE)
  iterator = regex.finditer(source)

  for match in iterator:
    qualification = match.group("qualification").strip()

    # Match is ok if the last character preceding the function name is a
    # non-word character.
    if not len(qualification) or re.compile("\W$").search(qualification):
      # Match is not a declaraction of the function but a real call.
      result.append(match)

  return result



def functionCallInfo(
         functionNames,
         directoryName):
  result = []

  for name in functionNames:
    result.append(FunctionCallInfo(name))

  directories = sets.Set()

  # Loop over all files.
  for root, directoryNames, filenames in os.walk(directoryName):
    for name in filenames:
      extension = os.path.splitext(name)[1]

      # Select C/C++ modules.
      if extension in _cModuleExtensions or extension in _cppModuleExtensions:
        filename = os.path.join(root, name)
        # name = filename.replace(directoryName, "")[1:]
        name = filename.replace(os.sep, "/")
        sys.stdout.write("%s\n" % (name))
        sys.stdout.flush()

        # Loop over all function names to look for.
        for info in result:
          # Store match information for each file the current function name
          # is used in.
          matches = functionCallMatches(info.name, file(filename).read())
          assert isinstance(matches, types.ListType)

          if matches:
            info.append(FunctionCallMatches(name, matches))
            directories.add(
              os.path.normpath(os.path.dirname(name)).replace(os.sep, "/"))

  return result, directories



## Refreshes all sources and targets related to project \a name.
#
# \param name Correctly cased name of project to refresh.
# \return Exit code of running make in project directory.
#
# Refreshing visits the tests, objects and project directories.
def refreshProject(
         name):
  projectDirectory = DevEnv.projectDirectory(name)
  basename = os.path.basename(projectDirectory)
  assert os.path.exists(projectDirectory)
  assert os.path.isdir(projectDirectory)
  assert os.path.normpath(os.path.join(projectDirectory, "..", basename)) == \
         projectDirectory

  # Update project.
  os.chdir(projectDirectory)
  result = utils.call("svn update")
  assert result == 0

  # Update tests directory.
  testsRoot = DevEnv.testsRoot()
  os.chdir(testsRoot)

  if os.path.exists(basename):
    assert os.path.isdir(basename), basename
    shutil.rmtree(basename)

  os.mkdir(basename)

  # Update objects directory.
  objectsBuildTypeRoot = DevEnv.objectsBuildTypeRoot()
  os.chdir(objectsBuildTypeRoot)
  assert os.path.exists(basename)
  assert os.path.isdir(basename)

  os.chdir(basename)
  # result = utils.call("configurecmakeproject.py")
  result = utils.call("%s rebuild_cache" % (_nativeMake()))
  assert result == 0

  # Build everything.
  os.chdir(projectDirectory)
  result = utils.call("make")

  return result



## Calls refreshProject(name) for each project in \a names.
#
# \param names Names of projects to refresh.
# \return Exit code of running make in project directory. The code of the
#         first failing project is returned or 0 if building all projects
#         succeeds.
def refreshProjects(
         names):
  result = 0

  for name in names:
    result = refreshProject(name)

    if result != 0:
      break

  return result



def updateProject(
         name):
  projectDirectory = DevEnv.projectDirectory(name)
  basename = os.path.basename(projectDirectory)
  assert os.path.exists(projectDirectory), projectDirectory
  assert os.path.isdir(projectDirectory), projectDirectory

  # Update project.
  os.chdir(projectDirectory)

  if os.path.exists(".svn"):
    result = utils.call("svn update")
  elif os.path.exists(".git"):
    result = utils.call("git pull")

  return result



def updateProjects(
         names):
  result = 0

  for name in names:
    print(name)
    result = updateProject(name)

    if result != 0:
      break

  return result



## Reports status of all sources of project \a name.
#
# \param name Correctly cased name of project to check.
# \return Exit code of running svn status in project directory.
def statusOfProject(
         name):
  projectDirectory = DevEnv.projectDirectory(name)
  assert os.path.exists(projectDirectory), projectDirectory
  assert os.path.isdir(projectDirectory), projectDirectory

  # Update project.
  os.chdir(projectDirectory)
  sys.stdout.write("$%s\n" % (name.upper()))
  sys.stdout.flush()

  if os.path.exists(".svn"):
    result = utils.call("svn status")
  elif os.path.exists(".git"):
    result = utils.call("git status")

  return result



## Calls statusOfProject(name) for each project in \a names.
#
# \param names Names of projects to report status of.
# \return Exit code of running svn status in project directory. The code of the
#         first failing project is returned or 0 if handling all projects
#         succeeds.
def statusOfProjects(
         names):
  result = 0

  for name in names:
    result = statusOfProject(name)

    if result != 0:
      break

  return result



def stripCcomments(src):
  start = src.find("/*")
  while start != -1:
    newSrc = src[0:start]
    srcTail = src[start:]
    end = srcTail.find("*/")
    assert end != -1
    src = newSrc + srcTail[end+2:]
    start = src.find("/*")
  return src


## create a code file with default matters
# __init__ and __del__ will do some clever stuff on .h and .cc detection
class CppFileGenerator:
  def __init__(self, fileName, generatedFrom=""):
    self.d_fd = open(fileName,'w')
    self.d_fileName = fileName

    moduleName = os.path.splitext(fileName)[0]
    self.d_type  = os.path.splitext(fileName)[1]

    if generatedFrom=="":
      generatedFrom=sys.argv[0]

    self.write('/*!\n\\note\nDo not edit, generated from %s\n*/' % generatedFrom)
    if self.d_type == '.h':
      umn=moduleName.upper()
      self.write('#ifndef INCLUDED_%s\n#define INCLUDED_%s\n\n' % (umn,umn))
    elif self.d_type == '.cc':
      self.write('#ifndef INCLUDED_STDDEFX\n#include "stddefx.h"\n'+\
                 '#define INCLUDED_STDDEFX\n#endif')
  def __del__(self):
    if self.d_type == '.h':
      self.write('\n#endif')
  # close file and remove from filesystem
  # needed with processing errors
  def remove(self):
      self.d_fd.close()
      os.remove(self.d_fileName)
      # ensure __del__ will not write
      self.d_type=''
  def write(self, code):
      self.d_fd.write(code)
      if code[:-1]!= "\n":
       self.d_fd.write("\n")
  def addInclude(self,className):
      self.d_fd.write(guardIncludeStatement(className)+"\n")



