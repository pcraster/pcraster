import inspect
import os.path
import unittest


class ImportTest(unittest.TestCase):


    def setUp(self):
        pass

    def tearDown(self):
        pass

    def _test_functions_in_module(self,
            module,
            function_names):
        function_names_in_module = [pair[0] for pair in
                inspect.getmembers(module,
                    lambda member: inspect.isfunction(member))]
        for function_name in function_names:
            self.assertTrue(function_name in function_names_in_module,
                function_name)

    def _test_classes_in_module(self,
            module,
            class_names):
        class_names_in_module = [pair[0] for pair in inspect.getmembers(module,
            lambda member: inspect.isclass(member))]
        for class_name in class_names:
            self.assertTrue(class_name in class_names_in_module, class_name)

    def test_import_pcraster(self):
        globals_ = globals()
        locals_ = locals()

        module = __import__("pcraster", globals_, locals_)
        self.assertTrue(inspect.ismodule(module))

        path, name = os.path.split(module.__file__)
        # name is init.py or init.pyc.
        self.assert_(name.find("__init__.py") == 0, name)

        path, name = os.path.split(path)
        self.assertEqual(name, "pcraster", name)

        self._test_functions_in_module(module, [
            "slope"
        ])
        self._test_classes_in_module(module, [
            # "Aggregator",
            # "Record"
        ])

    def test_import_pcraster_numpy(self):
        globals_ = globals()
        locals_ = locals()

        # # import numpy
        # module = __import__("numpy", globals_, locals_)
        # self.assertTrue("numpy" in module.__file__)
        # self.assertFalse("pcraster" in module.__file__)

        # # import pcraster.numpy
        # module = __import__("pcraster.numpy", globals_, locals_)
        # module = module.numpy
        # self.assertTrue("numpy" in module.__file__)
        # self.assertFalse("pcraster/__init__" in module.__file__)

        # # from pcraster import *
        # module = __import__("pcraster", globals_, locals_, ["*"])
        # self.assertFalse("numpy" in dir(module))
        # # print dir(module)
        # # print module.__file__



        ### # from pcraster import numpy
        ### module = __import__("pcraster", globals_, locals_, ["numpy"])
        ### module = module.numpy
        ### self.assertTrue(inspect.ismodule(module))
        ### self.assertTrue("pcraster/numpy/__init__" in module.__file__,
        ###     module.__file__)

        ### # import pcraster
        ### module = __import__("pcraster", globals_, locals_)

        ### # import numpy
        ### module = __import__("numpy", globals_, locals_)
        ### self.assertFalse("pcraster/numpy" in module.__file__, module.__file__)

        ### # from pcraster import *
        ### module = __import__("pcraster", globals_, locals_, ["*"])
        ### print "numpy" in globals_,  "numpy" in locals_

        ### # import numpy
        ### module = __import__("numpy", globals_, locals_)
        ### self.assertFalse("pcraster/numpy" in module.__file__, module.__file__)

        ### module = __import__("numpy", globals_, locals_, [])
        ### print "numpy" in globals(),  "numpy" in locals()

        ### module = __import__("pcraster", globals(), locals(), [])
        ### print "numpy" in globals(),  "numpy" in locals()

        ### module = __import__("pcraster", globals(), locals(), ["numpy"])
        ### print "numpy" in globals(),  "numpy" in locals()

        ### module = __import__("pcraster", globals(), locals(), ["*"])
        ### print "numpy" in globals(),  "numpy" in locals()

        statements = [
            "import numpy",
            "# import pcraster.numpy",
            "from pcraster import *",
            "assert not \"pcraster/numpy\" in numpy.__file__, numpy.__file__"
        ]
        code = compile("\n".join(statements), "<string>", "exec")
        eval(code)


if __name__ == "__main__":
    unittest.main()
