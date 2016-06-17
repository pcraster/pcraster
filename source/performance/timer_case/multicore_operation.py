import pcraster as pcr
from performance.timer_case.operation import Operation, Argument


unary_operations = [
    Operation("abs", [
        Argument(pcr.VALUESCALE.Scalar)]),
    Operation("acos", [
        Argument(pcr.VALUESCALE.Scalar)]),
    Operation("asin", [
        Argument(pcr.VALUESCALE.Scalar)]),
    Operation("atan", [
        Argument(pcr.VALUESCALE.Scalar)]),
    Operation("cos", [
        Argument(pcr.VALUESCALE.Scalar)]),
    # Operation("exp", [
    #     Argument(pcr.VALUESCALE.Scalar)]),
    Operation("slope", [
        Argument(pcr.VALUESCALE.Scalar)]),
    Operation("sqrt", [
        Argument(pcr.VALUESCALE.Scalar)]),
    Operation("window4total", [
        Argument(pcr.VALUESCALE.Scalar)]),
]

binary_operations = [
    Operation("add", [
        Argument(pcr.VALUESCALE.Scalar),
        Argument(pcr.VALUESCALE.Scalar)]),
    Operation("min", [
        Argument(pcr.VALUESCALE.Scalar),
        Argument(pcr.VALUESCALE.Scalar)]),
    Operation("mul", [
        Argument(pcr.VALUESCALE.Scalar),
        Argument(pcr.VALUESCALE.Scalar)]),
    Operation("power", [
        Argument(pcr.VALUESCALE.Scalar),
        Argument(pcr.VALUESCALE.Scalar)]),
]
