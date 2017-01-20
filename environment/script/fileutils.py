"""
Contains utilities for dealing with files.
"""
import os.path
import stat

def writeNewFile(
         name,
         contents):
  """
  Creates a file and writes the contents to it. An exception will be thrown if a the file already exists.

  The file is opened in binary mode.

  @param name: Name of the file to create.
  @param contents: Contents to write to the file.
  @raise IOError: In case the file already exists.
  """
  if os.path.exists(name):
    raise IOError("File with name %s already exists" % (name))

  file(name, "wb").write(contents)

# Tests if a file can be opened for reading.
def testOpenForReading(pathName):
  if not os.path.exists(pathName):
    raise Exception('File \'%s\': Does not exist' % (pathName))
  mode = os.stat(pathName)[stat.ST_MODE]
  # if not stat.S_ISREG(mode):
  #   raise Exception('File \'%s\': Not a regular file' % (pathName))
  if not (mode & stat.S_IRUSR or mode & stat.S_IRGRP or mode & stat.S_IROTH):
    raise Exception('File \'%s\': No permission to read' % (pathName))



## Makes a tree of directories.
#
# \param root Root directory name.
# \param directoryNameTree Nested list with tree directory names.
# \warning Each path in the tree must end with an empty list.
#
# Here's a sample of a valid layout of the \a directoryNameTree argument.
# \code
# directoryNameTree = [
#   [ "root",
#     [ "leaf1",
#       [ "leaf1_1",
#         [ "leaf1_1_1",
#           []
#         ],
#       ],
#     ],
#
#     [ "leaf2",
#       [ "leaf2_1",
#         []
#       ],
#
#       [ "leaf2_2",
#         [ "leaf2_2_1",
#           []
#         ],
#       ],
#     ],
#   ],
# ]
# \endcode
def makeDirectories(
         root,
         directoryNameTree):

  for subTree in directoryNameTree:
    if len(subTree) > 0:
      directoryName = os.path.join(root, subTree[0])
      print("mkdir %s" % (os.path.join(directoryName)))
      os.mkdir(os.path.join(directoryName))
      makeDirectories(directoryName, subTree[1:])

