## \file
# This file contains code that uses the DevEnv conventions. The idea
# of collecting them here is that when these conventions change, only this
# file needs to be updated.
#
# Code which should be in here:
# - Code dependent on a DevEnv project-style directory layout.



import glob
import os
import os.path
import shutil
import stat
import sys

import fileutils
import utils



## Returns the directory name of the root of the DevEnv project.
#
# \return    Directory name.
def devEnvRoot():
  return utils.environmentVariableAsNativePath("DEVENV")



## Returns the directory name of the root of a project.
#
# \param     pathName Path name of a file in a DevEnv style project.
# \return    Directory name.
#
# It is assumed that every DevEnv style project has a 'sources'
# sub-directory in the root of the project directory.
#
# If \a pathName is empty, the root of the project the current working directory
# is part of is returned.
def projectRoot(
         pathName=""):
  result = pathName

  if not result:
    result = os.getcwd()

  if result.find("bjects") != -1:
    if result.find("Debug") != -1:
      projectName = os.path.split(result[result.find("Debug") + \
        len("Debug") + 1:])[0]
      result = os.environ[projectName.upper()]

  assert os.path.exists(result), result
  result = os.path.abspath(result)

  # TODO Hack.
  if "OverlaySupportFiles" in result:
    result = utils.nativePath(os.environ["ARCGIS"])
  else:
    # It is assumed that the toplevel directory containing a CMakeLists.txt
    # file is the root of the project. We may want to fine-tune this by
    # searching for the PROJECT/project function in this CMakeLists.txt file.
    parentDirectory = os.path.dirname(result)
    while os.path.exists(parentDirectory) and \
        os.path.exists(os.path.join(parentDirectory, "CMakeLists.txt")):
      result = parentDirectory
      parentDirectory = os.path.dirname(result)
    assert os.path.exists(os.path.join(result, "CMakeLists.txt")), result

  assert os.path.exists(result), result
  assert os.path.isdir(result), result

  return result



##
# \todo Generalize, we could be in a project's sub directory.
def defaultProjectName():
  # If we are one sub directory below $OBJECTS, treat the name of that 
  # subdirectory as the project name. Take branches into account.
  currentDirectory = os.getcwd()
  dirName, baseName = os.path.split(currentDirectory)

  # assert dirName == self._objectsDirectory, "cd to $OBJECTS/project first"
  return baseName.split("-")[0]



def buildType():
  return os.environ["BUILD_TYPE"]



## Returns the directory name of the root of the objects directory.
#
# \param     pathName Path name of a file in a DevEnv style project.
# \return    Directory name.
#
# The root of the objects directory is the directory under which the project
# specific objects directories are put.
#
# If \a pathName is not empty, than the root of the project specific objects
# directory is returned.
def objectsBuildTypeRoot(
         pathName=""):
  assert("OBJECTS" in os.environ)
  result = utils.environmentVariableAsNativePath("OBJECTS")

  if pathName:
    result = os.path.join(result, projectName(pathName))

  assert os.path.exists(result), result
  assert os.path.isdir(result), result

  return result



## Returns the directory name of the root of tests directory.
#
# \return    Directory name.
#
# The root of the tests directory is the directory under which the project
# specific tests directories are put.
def testsRoot():
  assert("TESTS" in os.environ)
  return utils.environmentVariableAsNativePath("TESTS")



## Returns the name of the project \a pathName is part of.
#
# \param     pathName Path name of a file in a DevEnv style project.
# \return    Project name.
# \sa        projectRoot()
#
# If \a pathName is empty, the pathName of the project the current working directory
# is part of is returned.
def projectName(
         pathName=""):
  return os.path.split(projectRoot(pathName))[1]



## Returns the path name of \a pathName in the current project directory or in the DevEnv project directory.
#
# \param     pathName Path name of a file in a DevEnv style project.
# \return    Path name.
#
# The file pointed to by \a pathName is first looked for in the project
# directory the current working directory is part of. If the file is not found
# it is looked for in the DevEnv project directory.
def stubFilename(
         pathName):
  assert pathName
  pathName = os.path.join("[Tt]emplates", "[Ss]ources", pathName)
  result = glob.glob(os.path.join(projectRoot(), "[Ee]nvironment", pathName))

  if not result:
    result = glob.glob(os.path.join(devEnvRoot(), pathName))

  assert result, pathName
  return result[0]



## Creates a new project, with a directory layout and some files.
#
# \param     name Name of the project, correctly cased.
def newProject(
         name):
  assert not os.path.exists(name), "project directory must not exist"

  def writeFile(
         fileName,
         content):
    print("create %s" % (fileName))
    file(fileName, "w").write(content)

  # Nested list of lists.
  directoryNameTree = [
    [ name,
      [ "documentation",
        [ "sources",
          [ "cplusplus",
            []
          ],
        ],
      ],

      [ "environment",
        [ "configuration",
          []
        ],

        [ "templates",
          [ "cmake",
            []
          ],
        ],
      ],

      [
        "sources",
        [ "cplusplus",
          [ "PCRaster%s" % (name),
            []
          ],
        ],
      ],
    ],
  ]

  fileutils.makeDirectories("", directoryNameTree)

  writeFile(os.path.join(name, "Makefile"), """\
all:
\tmake -C sources all
\tmake -C sources runtests
\tmake -C documentation all

clean:
\tmake -C sources clean
\tmake -C documentation clean
""")

  writeFile(os.path.join(name, "documentation", "Makefile"), """\
all:
\tmake -C sources all

clean:
\tmake -C sources clean
""")

  writeFile(os.path.join(name, "documentation", "sources", "Makefile"), """\
all:
\tmake -C cplusplus all

clean:
\tmake -C cplusplus clean
""")

  writeFile(os.path.join(name, "documentation", "sources", "cplusplus",
         "Makefile"), """\
first: all

Doxyfile: $(DEVENV)/Templates/sources/Doxyfile
\tcreatedoxyfile.py $@ PROJECT_NAME=%s INPUT=../../../sources/cplusplus EXCLUDE_PATTERNS=*test.*

all: Doxyfile
\tdoxygen

clean:
\trm -fr html Doxyfile
""" % (name.upper()))

  writeFile(os.path.join(name, "environment", "configuration",
         "bash_profile"), """\
#vim:syntax=bash

export PATH="$PATH:$OBJECTS/$BUILD_TYPE/%s/bin"
export CDPATH="$CDPATH:$%s/sources/cplusplus"

if [ $OSTYPE == "cygwin" ]
then
  export CMAKE_MODULE_PATH="$CMAKE_MODULE_PATH;`cygpath -m $%s/environment/templates/cmake`"
elif [ $OSTYPE == "linux-gnu" ]
then
  export CMAKE_MODULE_PATH="$CMAKE_MODULE_PATH;$%s/environment/templates/cmake"
fi
""" % (name, name.upper(), name.upper(), name.upper()))

  writeFile(os.path.join(name, "sources", "Makefile"), """\
all:
\tmake -C cplusplus all

runtests:
\tmake -C cplusplus runtests

clean:
\tmake -C cplusplus clean
""")

  writeFile(os.path.join(name, "sources", "cplusplus", "Makefile"), """\
all:
\tmake -C PCRaster%s all

runtests:
\tmake -C PCRaster%s runtests

clean:
\tmake -C PCRaster%s clean
""" % (name, name, name))

  writeFile(os.path.join(name, "sources", "cplusplus", "CMakeLists.txt"), """\
PROJECT(%s)

SET(CMAKE_MODULE_PATH $ENV{CMAKE_MODULE_PATH})

INCLUDE(Site)

ADD_SUBDIRECTORY(PCRaster%s)
""" % (name, name))



def projectDirectory(
         name):
  variable = name.upper()
  directory = utils.environmentVariableAsNativePath(variable)

  return directory



## Returns the base name of the project whos name is \a name.
#
# \para      Name of project to return base name of.

# Name might be DevEnv, for example, and project base name might be
# DevEnv-trunk. The environmentvariable $DEVENV is parsed for the base name.
def projectBaseName(
  name):
  return os.path.basename(utils.environmentVariableAsNativePath(name.upper()))



## Configures project \a name.
#
# \param     name Name of project to configure.
#
# This function assumes that the project specific objects directory already
# exists.
def configureProject(
  generator,
  name):
  # None if no such variable is set.
  rootCMakeLists = os.environ.get("%s_ROOT_CMAKELISTS_FILE" % (name.upper()))

  # Having a per project setting is required by DEVENV env
  # KDJ: What's this PCRTREE special stuff?
  if name.upper() == "PCRTREE2PACKAGE":
    name = "PCRTREE2"
    sources = utils.environmentVariableAsNativePath(name.upper())
  elif rootCMakeLists:
    # Project's toplevel CMakeLists.txt file is stored in an a-typical
    # location.
    sources = os.path.split(utils.nativePath(rootCMakeLists))[0]
  elif os.path.exists(os.path.join(utils.environmentVariableAsNativePath(
         name.upper()), "CMakeLists.txt")):
    # Try new style. Cmake file in the root.
    sources = utils.environmentVariableAsNativePath(name.upper())
  elif os.path.exists("%s/Sources/cplusplus" % \
         utils.environmentVariableAsNativePath(name.upper())):
    sources = "%s/Sources/cplusplus" % \
             utils.environmentVariableAsNativePath(name.upper())
  else:
    sources = "%s/sources/cplusplus" % \
         utils.environmentVariableAsNativePath(name.upper())

  assert os.path.exists(sources), sources
  assert os.path.isdir(sources), sources

  objects = "%s/%s" % (objectsBuildTypeRoot(), projectBaseName(name))
  if not os.path.exists(objects):
    os.mkdir(objects)
  assert os.path.isdir(objects), objects

  os.chdir(objects)

  # "-D CMAKE_VERBOSE_MAKEFILE=TRUE "
  #      "-Wdev "
  # "--graphviz=%s.dot "
  command = "cmake -v " \
         "-G %s " \
         "-D CMAKE_BUILD_TYPE=%s " \
         "-D CMAKE_INSTALL_PREFIX=\"%s/install\" " \
         "\"%s\"" % (
         generator, buildType(), objects, sources)

  return utils.call(command)



## Reconfigures project \a name.
#
# \param     name Name of project to reconfigure.
# \sa        configureProject(generator, name)
#
# If a directory for the objects of project \a name does not already exist,
# it is created. If it already exists it is first removed.
def reconfigureProject(
  generator,
  name):

  # At least on Windows, when CMake configures a file and writes it to the
  # binary directory, these files are readonly and Python cannot remove them
  # without changing them to writables. This hook does that.
  def removeReadOnlyFilesToo(
    function,
    path,
    excinfo):
    if function == os.remove:
      assert os.path.isfile(path) and not os.path.islink(path)

      # The remove function couldn't remove a regular file.
      # If the file is readonly, make the file writable and remove it.

      # Check for readonly, if so, make writable, remove and return,
      # otherwise fall through.
      if not os.access(path, os.W_OK):
        os.chmod(path, stat.S_IWRITE)
        os.remove(path)
        return

    # Still here? Dunno how to fix this.
    raise excinfo[0](excinfo[1])

  os.chdir(objectsBuildTypeRoot())
  baseName = projectBaseName(name)

  if os.path.exists(baseName):
    assert os.path.isdir(baseName), baseName
    shutil.rmtree(baseName, False, removeReadOnlyFilesToo)

  os.mkdir(baseName)
  os.chdir(baseName)

  return configureProject(generator, name)



## Reconfigures all project in \a names.
#
# \param     names Names of projects to reconfigure.
# \sa        reconfigureProject(name)
def reconfigureProjects(
         generator,
         names):
  result = 0

  for name in names:
    result = reconfigureProject(generator, name)

    if result != 0:
      break

  return result



