import pcraster as pcr
from performance.timer_case.operation import Operation, Argument


operation_tuples = [
    ("abs", [pcr.VALUESCALE.Scalar]),
    ("acos", [pcr.VALUESCALE.Scalar]),
    ("add", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("asin", [pcr.VALUESCALE.Scalar]),
    ("atan", [pcr.VALUESCALE.Scalar]),
    ("boolean", [pcr.VALUESCALE.Scalar]),
    ("cos", [pcr.VALUESCALE.Scalar]),
    ("cover", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("defined", [pcr.VALUESCALE.Scalar]),
    ("div", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("equal", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("fac", [pcr.VALUESCALE.Scalar]),
    ("greater", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("greater_equal", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("ifthen", [pcr.VALUESCALE.Boolean, pcr.VALUESCALE.Scalar]),
    ("ifthenelse", [pcr.VALUESCALE.Boolean, pcr.VALUESCALE.Scalar,
        pcr.VALUESCALE.Scalar]),
    ("less", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("less_equal", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("ln", [pcr.VALUESCALE.Scalar]),
    ("log10", [pcr.VALUESCALE.Scalar]),
    ("mapmaximum", [pcr.VALUESCALE.Scalar]),
    ("mapminimum", [pcr.VALUESCALE.Scalar]),
    ("max", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("min", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("mul", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("nominal", [pcr.VALUESCALE.Scalar]),
    ("ordinal", [pcr.VALUESCALE.Scalar]),
    ("power", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("rounddown", [pcr.VALUESCALE.Scalar]),
    ("roundoff", [pcr.VALUESCALE.Scalar]),
    ("roundup", [pcr.VALUESCALE.Scalar]),
    ("scalar", [pcr.VALUESCALE.Nominal]),
    ("sin", [pcr.VALUESCALE.Scalar]),
    ("slope", [pcr.VALUESCALE.Scalar]),
    ("sqr", [pcr.VALUESCALE.Scalar]),
    ("sqrt", [pcr.VALUESCALE.Scalar]),
    ("sub", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("tan", [pcr.VALUESCALE.Scalar]),
    ("unequal", [pcr.VALUESCALE.Scalar, pcr.VALUESCALE.Scalar]),
    ("window4total", [pcr.VALUESCALE.Scalar])
]


operations = [Operation(tpl[0],
    [Argument(value_scale) for value_scale in tpl[1]])
    for tpl in operation_tuples]
