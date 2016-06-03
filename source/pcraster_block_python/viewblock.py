#!/usr/bin/python2.3
import sys, time
import mayavi, vtk
import shellscript

class ViewerScript(shellscript.ShellScript):

  def __init__(self, argv):
    shellscript.ShellScript.__init__(self, argv)
    self.d_window = mayavi.mayavi()
    self.d_tuples = []

  def usage(self):
    return "Usage: " + self.appName() + " (<block name> <module manager name>)+"

  def parseCommandLine(self):
    if len(self.argv()) == 1 or (len(self.argv()) - 1) % 2 != 0:
      raise shellscript.CommandLineException()

    for i in range(1, len(self.argv()) - 1, 2):
      self.d_tuples.append((self.argv()[i], self.argv()[i + 1]))

  def loadVisualisations(self):
    for tuple in self.d_tuples:
      reader = vtk.vtkXMLImageDataReader()
      reader.SetFileName(tuple[0])
      reader.Update()
      self.d_window.open_vtk_data(reader.GetOutput())
      self.d_window.load_mm(tuple[1])

  def enterEventLoop(self):
    self.d_window.master.wait_window()

  # Results in empty file...
  # def saveAsPNG(self):
  #   self.d_window.renwin.save_png("bla.png")
  #   time.sleep(10)

  def execute(self):
    self.parseCommandLine()
    self.loadVisualisations()
    self.enterEventLoop()
    # self.d_window.Render()
    # self.saveAsPNG()

if __name__ == "__main__":
  script = ViewerScript(sys.argv)
  sys.exit(script.run())



  # def readData(self):
  #   reader = vtk.vtkXMLImageDataReader()
  #   reader.SetFileName(self.d_filename)
  #   reader.Update()
  #   self.d_window.open_vtk_data(reader.GetOutput())

  # def loadFilters(self):
  #   filter = self.d_window.load_filter("Threshold", 0)
  #   filter.fil.SetAttributeModeToUseCellData()
  #   filter.fil.ThresholdByUpper(-1.0)
  #   groter dan 1
  #   # Load the filters.
  #   # f = window.load_filter("WarpScalar", config=0) 
  #   # n = window.load_filter("PolyDataNormals", 0)
  #   # n.fil.SetFeatureAngle (45) # configure the normals.
  #   pass

  # def loadModules(self):
  #   module = self.d_window.load_module("SurfaceMap", 0)
  #   module = self.d_window.load_module("Outline", 0)

  #   # ScalarCutPlane
  #   # Outline
  #   # Legend

  #   # a = window.load_module("Axes", 0)
  #   # a.axes.SetCornerOffset(0.0) # configure the axes module.
  #   # o = window.load_module("Outline", 0)



    # actors = self.d_window.get_render_window().get_renderer().GetActors()
    # actors.InitTraversal()
    # actor = actors.GetNextActor()

    # while actor:
    #   actor.SetVisibility(1)
    #   actor.ApplyProperties()
    #   actor = actors.GetNextActor()

