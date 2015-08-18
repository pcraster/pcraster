"""
Module for the TaskRunner class implementation.
"""

import os, os.path, utils



class TaskRunner:
  """
  Class for managing a collection of tasks.

  The user of this class can add specialised L{task.Task} objects to a
  TaskRunner object. Calling L{initialise}, L{run} and L{clean} on the
  runner will then forward this call to all layered tasks in turn.
  """

  def __init__(self):
    """
    Default constructor.

    Initialises an empty task list. Default, no log will be created.
    """
    self.__tasks = []
    self.__logFilename = None

  def append(self, task):
    """
    Appends a task to the task list.

    @param task: Task to add.
    @type task: A specialised L{task.Task} object.
    """
    if self.logShouldBeCreated():
      task.setLogFilename(self.logFilename())
    self.__tasks.append(task)

  def tasks(self):
    """
    @return: Task list.
    """
    return self.__tasks

  def nrTasks(self):
    """
    @return: Number of tasks in the task list.
    """
    return len(self.tasks())

  def showInfo(self, label, message=None):
    """
    Shows an informative message.

    The label is shown in a field of 20 characters wide. All messages will be
    outlined immediately to the right of this field (without a double colon
    in between).

    @param label: Label to show before the message.
    @type label: string
    @param message: Message to show.
    @type message: string
    """
    result = "%-20s" % (label)

    if message:
      result += "%s" % (message)

    result += "\n"

    self.writeLog("%s" % (result))

    print result,

  def setLogFilename(self,
         filename):
    """
    Sets the name of the log file to use.

    If called, this function must be called before tasks are added.

    @param filename: Name of the log file.
    @type filename: string
    """
    assert self.nrTasks() == 0

    if os.path.exists(filename):
      os.remove(filename)

    utils.testFileIsWritable(filename)
    self.__logFilename = os.path.abspath(filename)

  def logShouldBeCreated(self):
    """
    @return: Whether a log should be created.
    """
    return bool(self.__logFilename)

  def logFilename(self):
    """
    If called, L{logShouldBeCreated} must return true.

    @return: The name of the log file.
    """
    assert self.logShouldBeCreated()
    return self.__logFilename

  def writeLog(self,
         message):
    """
    Writes a message to the log.

    This function does nothing if L{logShouldBeCreated} returns false.

    @param message: Message to write.
    @type message: string
    """
    if self.logShouldBeCreated():
      file(self.logFilename(), "a").write(message)

  def initialise(self):
    """
    Calls L{task.Task.initialise} on each task in the layered task list.

    @exception Exception: When the function called throws an exception.
    """
    self.showInfo("initialising tasks")

    for task in self.tasks():
      try:
        task.initialise()
      except AssertionError, exception:
        raise
      except Exception, exception:
        utils.raiseException("error while initialising task: %s" %
              (task.description()), exception)

  def run(self):
    """
    Calls L{task.Task.run} on each task in the layered task list.

    @exception Exception: When the function called throws an exception.
    """
    for i in range(0, len(self.tasks())):
      self.showInfo("running task %d/%d" % (i + 1, self.nrTasks()),
         message=self.tasks()[i].description())

      try:
        self.tasks()[i].run()
      except AssertionError, exception:
        raise
      except Exception, exception:
        utils.raiseException("error while running task: %s" %
              (self.tasks()[i].description()), exception)

  def clean(self):
    """
    Calls L{task.Task.clean} on each task in the layered task list.

    @exception Exception: When the function called throws an exception.
    """
    self.showInfo("cleaning tasks")

    for task in self.tasks():
      try:
        task.clean()
      except AssertionError, exception:
        raise
      except Exception, exception:
        utils.raiseException("error while cleaning task: %s" %
              (task.description()), exception)

