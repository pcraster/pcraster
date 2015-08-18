In the build process the manual is build from a node in
$PCRTREE2/sources/cplusplus/libraries/PCRasterModelEngine/CMakeLists.txt
Search for PCRasterManual.

The CMakeLists.txt then calls the Makefile in this folder.

Manual bestaat uit 2 delen:
 - beschrijvende stukken, alleen daar zijn figures.
 - refPages, alleen daar zijn de example maps

De refPages hebben een bepaalde structuur waarvan  we 2 secties willen aanpakken:

1. Synopsis
Te genereren vanuit $PCRTREE2/sources/cplusplus/libraries/PCRasterModelEngine/operation.xml.

2. Examples
Genereren vanuit examples/samples.py en examples/cmdsamples.py
(Zie examples/Makefile).
Moet een pcrcalc and python syntax voorbeeld krijgen.
Ik heb hier al wat werk voor gedaan.

Acties
------
 operation.xml uitbreiden:
  - alle Input en Result elementen moeten verplicht het veld contextName
    hebben voor de naam van het argument en resultaat.
  - operator syntax voor calc en python

Todo:
----
 Cees: $PCRTREE2/data/test/manualExamples/pcrcalcExamples.xml
 is nog de basis voor de unittests van PCRasterModelEngine en PCRasterPython
 dat moet sample.py worden.

Issues:
------
 *  Generation creates a lot of  warnings. Lijken loze warnings.
 * Layout formatting of maps in examples,
    see accutresholdflux
    idea: part of page is 3 column setup, >3 cols should flow to below.
 * figures: Png images need different scales for pdf then for html.
