# -*- coding: utf-8 -*-
## \file
# This file contains code that deals with CMake.



import glob
import os
import os.path
import sys

import utils



def defaultBuildTypeName():
  return "Debug"



def isAcceptableBuildTypeName(
       buildType):
  return buildType in ["Debug", "Release"]


def defaultGeneratorName():
  # if sys.platform == "win32":
  #   if os.environ.has_key("VS90COMNTOOLS"):
  #     generator = "\"Visual Studio 9 2008\""
  #   elif os.environ.has_key("VS80COMNTOOLS"):
  #     generator = "\"Visual Studio 8 2005\""
  #   elif os.environ.has_key("VS71COMNTOOLS"):
  #     generator = "\"Visual Studio 7 .NET 2005\""
  #   else:
  #     # Default if no compiler could be found.
  #     generator = "\"NMake Makefiles\""

  #   # Working with VS project files is not streamlined yet. Problem is that
  #   # build types are mixed in a project file, and targets are being build
  #   # in objects/project/bin/config instead of objects/project/bin. This makes
  #   # PATH settings and use of libraries from other projects more difficult.
  #   # Stick with make for now.
  #   generator = "\"NMake Makefiles\""
  # else:
  #   generator = "\"Unix Makefiles\""

  generator = "\"Unix Makefiles\""
  # generator = "\"Visual Studio 9 2008\""

  return generator



# def _buildWithNMake(
#          name,
#          nrJobs):
#   return utils.call("nmake /NOLOGO %s" % (name))



def _buildWithMake(
         name,
         nrJobs):
  return utils.call("make -j%d %s" % (nrJobs, name))



def _buildWithDevEnv(
         name,
         nrJobs):
  if name == "all":
    name = "ALL_BUILD"

  projectFileName = "%s.vcproj" % (name)

  assert os.path.exists(projectFileName), projectFileName

  return utils.call("devenv.com %s /Project %s /build" % (
         projectFileName, name))



def visualStudioProject():
  return len(glob.glob("*.vcproj"))




def nMakeProject():
  return sys.platform == "win32" and len(glob.glob("Makefile"))



def makeProject():
  return len(glob.glob("Makefile")) > 0



def build(
         name,
         nrJobs):
  if visualStudioProject():
    builder = _buildWithDevEnv
  # elif nMakeProject():
  #   builder = _buildWithNMake
  elif makeProject():
    builder = _buildWithMake
  else:
    assert False, "First configure CMake project"

  return builder(name, nrJobs)


