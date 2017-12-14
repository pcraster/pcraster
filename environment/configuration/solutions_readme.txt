Solutions represent hardware and software platforms on which PCRaster
can be built. The contents of each solution shows the folowing:
- Proof that PCRaster can be built on a platform.
- The minimal set of requirements that are needed to build PCRaster.

It is important that each solution is as small/simple as possible. A
complicated solution means that a user using a similar platform has to
jump through hoops to be able to build and use PCRaster. So:
- Try not to use ppa's. Ppa's might trigger the upgrade of all kinds of
  additional packages installed on the user's machine. In case a
  requirement is not available using the platform's package manager,
  use something like Peacock to build it. (If Peacock doesn't support
  building a specific (version of a) package, ask the friendly staff at
  Geoneric to add it, or add support for building it yourself and send
  them a pull request.)

Solutions can be built using the test_solutions.py command. It is located
in the devbase project, which is a submodule of the PCRaster source tree.

Vagrant boxes used in solutions are automatically updated by the
test_solutions.py script.


# Run all solutions, use 4 CPUs and 4Gb RAM.
mkdir -p /tmp/solutions && rm -fr /tmp/solutions/*
$PCRASTER/devbase/script/test_solutions.py --prefix /tmp/solutions --cpus 4 --memory 4000 $PCRASTER/environment/configuration/solutions.json
