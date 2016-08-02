import unittest
import os.path
import inspect
import importlib

class ImportTest(unittest.TestCase):


    def _test_functions_in_module(self,
            module,
            function_names):
        function_names_in_module = [pair[0] for pair in
                inspect.getmembers(module,
                    lambda member: inspect.isfunction(member))]
        for function_name in function_names:
            self.assertTrue(function_name in function_names_in_module,
                function_name)

    def test_import_pcraster_multicore(self):
        globals_ = globals()
        locals_ = locals()


        module =importlib.import_module("..multicore", "pcraster.multicore")
        self.assertTrue(inspect.ismodule(module))

        path, name = os.path.split(module.__file__)
        self.assert_(name.find("__init__.py") == 0, name)

        path, name = os.path.split(path)
        self.assertEqual(name, "multicore", name)

        self._test_functions_in_module(module, [
            "slope", "cover", "set_nr_worker_threads"
        ])


if __name__ == '__main__':
    unittest.main()
