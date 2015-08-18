import os.path
import sys
sys.path = \
    [os.path.join(os.path.split(__file__)[0], "../../environment/script")] + \
    sys.path
import fileutils, shellscript

class File2CharArray(shellscript.ShellScript):

  ## Constructor.
  #
  # \param     argv Argument vector.
  def __init__(self,
         argv):
    shellscript.ShellScript.__init__(self, argv)

  def usage(self):
    # TODO
    return """\
Usage: %s TextFile C-identifier
""" % (self.appName())

  def _parseOptions(self):
    argv = self.arguments
    if len(argv) != 2:
      msg = "Wrong number of arguments"
      self.showError(msg)
      sys.exit(1)

    self.d_fileName   = argv[0]
    self.d_identifier = argv[1]

  def arrayLine(self,c,end):
     textVersion=c
     if c == '\n':
       textVersion = "\\n"
     if c == '\\':
       textVersion = "\\ (a slash, added to avoid multi-line comment)"
     return "0x%x %s // %s\n" % (ord(c),end,textVersion);

  def generate(self):

    fileutils.testOpenForReading(self.d_fileName)
    text = open(self.d_fileName, "r").read()
    genFile = self.d_identifier+".h"
    g = open(genFile, "w")
    g.write("static const char %s[] = {\n" % self.d_identifier);
    for c in text:
     g.write(self.arrayLine(c,","));
    g.write(" 0x0 /* terminate */};\n");

  def _run(self):
    # self.parseCmdLine(self.argv())
    self.generate()
    return 0

if __name__ == "__main__": 
  import sys
  sys.exit(File2CharArray(sys.argv).run())

