import task



class CompositeTask(task.Task):

  def __init__(self, description):
    Task.__init__(self, description)
    self.__tasks = []

  def append(self, task):
    self.__tasks.append(task)

  def nrTasks(self):
    return len(self.tasks())

  def tasks(self):
    return self.__tasks

  def inputs(self):
    result = []
    for task in self.__tasks:
      result += task.inputs()
    return result

  def outputs(self):
    result = []
    for task in self.__tasks:
      result += task.outputs()
    return result

  def run(self):
    for task in self.__tasks:
      task.run()

