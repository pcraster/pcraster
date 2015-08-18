# from compositetask import CompositeTask
from copytask import CopyTask
from renametask import RenameTask

# CompositeTask task("Main task")

# Create bla.txt.

assert os.path.exist("bla.txt")
CopyTask copyTask(input = [Data("bla.txt")], output = [Data("bli.txt")])
copyTask.run()
assert os.path.exist("bla.txt")
assert os.path.exist("bli.txt")

assert os.path.exist("bli.txt")
assert !os.path.exist("blo.txt")
RenameTask renameTask(input = [Data("bli.txt")], output = [Data("blo.txt")])
renameTask.run()
assert !os.path.exist("bli.txt")
assert os.path.exist("blo.txt")


# RenameTask renameTask(output = [Data("blo.txt")])
# renameTask.setAsInput(copyTask)

# Make sure bla.txt exists, bli.txt doesn't and blo.txt does.
# assert os.path.exist("bla.txt")
# assert !os.path.exist("bli.txt")
# assert os.path.exist("blo.txt")
