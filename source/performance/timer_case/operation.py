class Argument(object):

    def __init__(self,
            value_scale):
        self.value_scale = value_scale


class Operation(object):

    def __init__(self,
            name,
            arguments):
        self.name = name
        self.arguments = arguments
