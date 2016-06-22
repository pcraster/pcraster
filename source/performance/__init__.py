import platform


def determine_database_name():

    distribution = platform.linux_distribution()
    dist_name = distribution[0].lower().replace(" ", "_")
    dist_version = distribution[1].split(".")[0]

    name = "{node}-{machine}-{system}-{dist_name}-{dist_version}.db".format(
        node=platform.node(),
        machine=platform.machine(),
        system=platform.system().lower(),
        dist_name=dist_name,
        dist_version=dist_version,
    )

    return name
