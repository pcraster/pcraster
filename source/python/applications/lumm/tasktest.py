import os, unittest
import task

class TaskTest(unittest.TestCase):

  def setUp(self):
    # Nothing to do.
    pass

  def tearDown(self):
    # Nothing to do.
    pass

  def testConstructor(self):
    description = "Imports something"
    t = task.Task(description, scriptDirectoryName=".", tempDirectoryName=".", outputDirectoryName=".", configurationItems=[])
    self.assertEqual(t.description(), description)
    # self.assertEqual(t.nrInputs(), 0)
    # self.assertEqual(t.nrOutputs(), 0)






# class CompositeTaskTest(unittest.TestCase):
#   def setUp(self):
#     # Nothing to do.
#     pass
# 
#   def tearDown(self):
#     # Nothing to do.
#     pass
# 
#   def testConstructor(self):
#     description = "Runs two tasks"
#     t = task.CompositeTask(description)
#     self.assertEqual(t.description(), description)
#     self.assertEqual(t.nrInputs(), 0)
#     self.assertEqual(t.nrOutputs(), 0)
# 
#     st1 = task.Task("Sub task 1")
#     st2 = task.Task("Sub task 2")
#     t.append(st1)
#     t.append(st2)
#     self.assertEqual(t.nrTasks(), 2)
#     self.assertEqual(t.nrInputs(), 0)
#     self.assertEqual(t.nrOutputs(), 0)
