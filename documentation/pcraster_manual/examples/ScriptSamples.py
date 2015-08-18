sample = newSample("and", "Result")
sample.appendInput("Expr1")
sample.appendInput("Expr2")
sample.code = ExprPcrcalcPython("$Expr1 and $Expr2", "$Expr1 & $Expr2")

sample = newSample("or", "Result")
sample.appendInput("Expr1", useFrom="and")
sample.appendInput("Expr2", useFrom="and")
sample.code = ExprPcrcalcPython("$Expr1 or $Expr2", "$Expr1 | $Expr2")

sample = newSample("xor", "Result")
sample.appendInput("Expr1", useFrom="and")
sample.appendInput("Expr2", useFrom="and")
sample.code = ExprPcrcalcPython("$Expr1 xor $Expr2", "$Expr1 ^ $Expr2")

sample = newSample("not", "Result")
sample.appendInput("Expr1", useFrom="and")
sample.code = ExprPcrcalcPython("not $Expr1", "~ $Expr1 ")

sample = newSample("ster", "Result")
sample.appendInput("Expr1")
sample.appendInput("Expr2")
sample.code = ExprIdentical("""$Expr1 * $Expr2""")

sample = newSample("sterster", "Result")
sample.appendInput("Expr")
sample.appendInput("Power")
sample.code = ExprIdentical("""$Expr ** $Power""")


sample = newSample("minus", "Result")
sample.appendInput("Expr1")
sample.appendInput("Expr2")
sample.code = ExprIdentical("""$Expr1 - $Expr2""")


sample = newSample("plus", "Result")
sample.appendInput("Expr1", useFrom="minus")
sample.appendInput("Expr2", useFrom="minus")
sample.code = ExprIdentical("""$Expr1 + $Expr2""")


sample = newSample("slash", "Result")
sample.appendInput("Expr1")
sample.appendInput("Expr2")
sample.code = ExprIdentical("""$Expr1 / $Expr2""")


sample = newSample("abs", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""abs($Expr)""")


sample = newSample("accu", "Result")
sample.appendInput("Ldd")
sample.appendInput("Material")
sample.code = ExprIdentical("""accuflux($Ldd,$Material)""")


sample = newSample("accu", "Result2")
sample.appendInput("Ldd")
sample.code = ExprIdentical("""accuflux($Ldd,1)""")


sample = newSample("accu", "Result3")
sample.appendInput("Ldd")
sample.appendInput("MaterialMV")
sample.code = ExprIdentical("""accuflux($Ldd,$MaterialMV)""")


sample = newSample("accufraction", "State1")
sample.appendResult("Flux1")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accu")
sample.code = CalcExprPythonScript("""accufractionstate,accufractionflux($Ldd,$Material,0.5)""", """
$State1 = accufractionstate($Ldd,$Material,0.5)
$Flux1 = accufractionflux($Ldd,$Material,0.5)
""")


sample = newSample("accufraction", "State2")
sample.appendResult("Flux2")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material")
sample.appendInput("TransFra")
sample.code = CalcExprPythonScript("""accufractionstate,accufractionflux($Ldd,$Material,$TransFra)""", """
$State2 = accufractionstate($Ldd,$Material,$TransFra)
$Flux2 = accufractionflux($Ldd,$Material,$TransFra)""")


sample = newSample("accucapacity", "State1")
sample.appendResult("Flux1")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accu")
sample.code = CalcExprPythonScript("""accucapacitystate,accucapacityflux($Ldd,$Material,1.5)""","""
$State1 = accucapacitystate($Ldd,$Material,1.5)
$Flux1 = accucapacityflux($Ldd,$Material,1.5)""")


sample = newSample("accucapacity", "State2")
sample.appendResult("Flux2")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accufraction")
sample.appendInput("TransCap")
sample.code = CalcExprPythonScript("""accucapacitystate,accucapacityflux($Ldd,$Material,$TransCap)""","""
$State2 = accucapacitystate($Ldd,$Material,$TransCap)
$Flux2 = accucapacityflux($Ldd,$Material,$TransCap)""")


sample = newSample("accuthreshold", "State1")
sample.appendResult("Flux1")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accu")
sample.code = CalcExprPythonScript("""accuthresholdstate,accuthresholdflux($Ldd,$Material,1.5)""","""
$State1 = accuthresholdstate($Ldd,$Material,1.5)
$Flux1 = accuthresholdflux($Ldd,$Material,1.5)""")


sample = newSample("accuthreshold", "State2")
sample.appendResult("Flux2")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accufraction")
sample.appendInput("TransTH")
sample.code = CalcExprPythonScript("""accuthresholdstate,accuthresholdflux($Ldd,$Material,$TransTH)""","""
$State2 = accuthresholdstate($Ldd,$Material,$TransTH)
$Flux2 = accuthresholdflux($Ldd,$Material,$TransTH)""")


sample = newSample("accutrigger", "State1")
sample.appendResult("Flux1")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accu")
sample.code = CalcExprPythonScript("""accutriggerstate,accutriggerflux($Ldd,$Material,1.5)""","""
$State1=accutriggerstate($Ldd,$Material,1.5)
$Flux1=accutriggerflux($Ldd,$Material,1.5)""")


sample = newSample("accutrigger", "State2")
sample.appendResult("Flux2")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accufraction")
sample.appendInput("TransTH", useFrom="accuthreshold")
sample.code = CalcExprPythonScript("""accutriggerstate,accutriggerflux($Ldd,$Material,$TransTH)""","""
$State2 = accutriggerstate($Ldd,$Material,$TransTH)
$Flux2 = accutriggerflux($Ldd,$Material,$TransTH)""")


sample = newSample("accutrigger", "State3")
sample.appendResult("Flux3")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accufraction")
sample.appendInput("TransTHMV", useFrom="accuthreshold")
sample.code = CalcExprPythonScript("""accutriggerstate,accutriggerflux($Ldd,$Material,$TransTHMV)""","""
$State3 = accutriggerstate($Ldd,$Material,$TransTHMV)
$Flux3 = accutriggerflux($Ldd,$Material,$TransTHMV)""")


sample = newSample("accutraveltime", "State1")
sample.comment = """NOTE this example is a copy of
       $OLDPCRTREE/pcrmanual/manual_updates/AccuTravelTime.zip
       it does miss the .itxt inputs in the examples folder of $OLDPCRTREE
 """
sample.appendResult("Flux1")
sample.appendInput("Ldd")
sample.appendInput("Material")
sample.appendInput("Velocity")
sample.code = CalcExprPythonScript("""accutraveltimestate,accutraveltimeflux($Ldd,$Material,$Velocity)""","""
$State1 = accutraveltimestate($Ldd,$Material,$Velocity)
$Flux1 = accutraveltimeflux($Ldd,$Material,$Velocity)""")

sample = newSample('accutraveltimefractionremoved','Removed1')
sample.comment="""NOTE this example is a copy of
       $OLDPCRTREE/pcrmanual/manual_updates/AccuTravelTime.zip"""
sample.appendInput('Ldd', useFrom='accutraveltime')
sample.appendInput('Material', useFrom='accutraveltime')
sample.appendInput('Velocity', useFrom='accutraveltime')
sample.code = ExprIdentical("accutraveltimefractionremoved($Ldd,$Material,$Velocity, 1)")

sample = newSample('accutraveltimefraction', 'State1_1')
sample.appendResult('Flux1_1')
sample.appendResult('Removed1_1')
sample.appendInput('Ldd1', useFrom='attfTests')
sample.appendInput('Material1_2', useFrom='attfTests')
sample.appendInput('Velocity1_1', useFrom='attfTests')
sample.code =Scripts("""
$State1_1, $Flux1_1 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd1,$Material1_2,$Velocity1_1, 0.5);
$Removed1_1 = accutraveltimefractionremoved($Ldd1,$Material1_2,$Velocity1_1, 0.5);
""","""
$State1_1 = accutraveltimefractionstate($Ldd1,$Material1_2,$Velocity1_1, 0.5);
$Flux1_1 = accutraveltimefractionflux($Ldd1,$Material1_2,$Velocity1_1, 0.5);
$Removed1_1 = accutraveltimefractionremoved($Ldd1,$Material1_2,$Velocity1_1, 0.5);
""")

sample = newSample('accutraveltimefraction', 'State1_2')
sample.appendResult('Flux1_2')
sample.appendResult('Removed1_2')
sample.appendInput('Ldd1', useFrom='attfTests')
sample.appendInput('Material1_2', useFrom='attfTests')
sample.appendInput('Velocity1_2', useFrom='attfTests')
sample.code =Scripts("""
$State1_2, $Flux1_2 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd1,$Material1_2,$Velocity1_2, 0.5);
$Removed1_2 = accutraveltimefractionremoved($Ldd1,$Material1_2,$Velocity1_2, 0.5);
""","""
$State1_2 = accutraveltimefractionstate($Ldd1,$Material1_2,$Velocity1_2, 0.5);
$Flux1_2 = accutraveltimefractionflux($Ldd1,$Material1_2,$Velocity1_2, 0.5);
$Removed1_2 = accutraveltimefractionremoved($Ldd1,$Material1_2,$Velocity1_2, 0.5);
""")

sample = newSample('accutraveltimefraction',
'State1_3')
sample.appendResult('Flux1_3')
sample.appendResult('Removed1_3')
sample.appendInput('Ldd1', useFrom='attfTests')
sample.appendInput('Material1_3', useFrom='attfTests')
sample.appendInput('Velocity1_3', useFrom='attfTests')
sample.appendInput('Fraction1_3', useFrom='attfTests')
sample.code =Scripts("""
$State1_3, $Flux1_3 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd1,$Material1_3,$Velocity1_3, $Fraction1_3);
$Removed1_3 = accutraveltimefractionremoved($Ldd1,$Material1_3,$Velocity1_3, $Fraction1_3);
""","""
$State1_3 = accutraveltimefractionstate($Ldd1,$Material1_3,$Velocity1_3, $Fraction1_3);
$Flux1_3 = accutraveltimefractionflux($Ldd1,$Material1_3,$Velocity1_3, $Fraction1_3);
$Removed1_3 = accutraveltimefractionremoved($Ldd1,$Material1_3,$Velocity1_3, $Fraction1_3);
""")

sample = newSample('accutraveltimefraction',
'State1_4')
sample.appendResult('Flux1_4')
sample.appendResult('Removed1_4')
sample.appendInput('Ldd1', useFrom='attfTests')
sample.appendInput('Material1_3', useFrom='attfTests')
sample.appendInput('Velocity1_4', useFrom='attfTests')
sample.appendInput('Fraction1_3', useFrom='attfTests')
sample.code =Scripts("""
$State1_4, $Flux1_4 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd1,$Material1_3,$Velocity1_4, $Fraction1_3);
$Removed1_4 = accutraveltimefractionremoved($Ldd1,$Material1_3,$Velocity1_4, $Fraction1_3);
""","""
$State1_4 = accutraveltimefractionstate($Ldd1,$Material1_3,$Velocity1_4, $Fraction1_3);
$Flux1_4 = accutraveltimefractionflux($Ldd1,$Material1_3,$Velocity1_4, $Fraction1_3);
$Removed1_4 = accutraveltimefractionremoved($Ldd1,$Material1_3,$Velocity1_4, $Fraction1_3);
""")

sample = newSample('accutraveltimefraction',
'State1_5')
sample.appendResult('Flux1_5')
sample.appendResult('Removed1_5')
sample.appendInput('Ldd1', useFrom='attfTests')
sample.appendInput('Material1_3', useFrom='attfTests')
sample.appendInput('Velocity1_5', useFrom='attfTests')
sample.code =Scripts("""
$State1_5, $Flux1_5 = accutraveltimefractionstate,accutraveltimefractionflux($Ldd1,$Material1_3,$Velocity1_5, 1);
$Removed1_5 = accutraveltimefractionremoved($Ldd1,$Material1_3,$Velocity1_5, 1);
""","""
$State1_5 = accutraveltimefractionstate($Ldd1,$Material1_3,$Velocity1_5, 1);
$Flux1_5 = accutraveltimefractionflux($Ldd1,$Material1_3,$Velocity1_5, 1);
$Removed1_5 = accutraveltimefractionremoved($Ldd1,$Material1_3,$Velocity1_5, 1);
""")

#
# sample = newSample('accutraveltimefraction', 'State1_6')
# sample.comment = "Bugzilla his commented in xml?????"
# sample.appendResult('Flux1_6')
# sample.appendResult('Removed1_6')
# sample.appendInput('Ldd1', useFrom='attfTests')
# sample.appendInput('Material1_3', useFrom='attfTests')
# sample.appendInput('Velocity1_6', useFrom='attfTests')
# sample.code =Scripts("""
# $State1_6, $Flux1_6 =
# accutraveltimefractionstate,accutraveltimefractionflux($Ldd1,$Material1_3,$Velocity1_6, 1);
# $Removed1_6 = accutraveltimefractionremoved($Ldd1,$Material1_3,$Velocity1_6, 1);
# ""","""
# $State1_6 = accutraveltimefractionstate($Ldd1,$Material1_3,$Velocity1_6, 1);
# $Flux1_6 = accutraveltimefractionflux($Ldd1,$Material1_3,$Velocity1_6, 1);
# $Removed1_6 = accutraveltimefractionremoved($Ldd1,$Material1_3,$Velocity1_6, 1);
# """)

sample = newSample('accutraveltimefraction', 'State2_1')
sample.appendResult('Flux2_1')
sample.appendResult('Removed2_1')
sample.appendInput('Ldd2', useFrom='attfTests')
sample.appendInput('Material2_1', useFrom='attfTests')
sample.appendInput('Velocity2_1', useFrom='attfTests')
sample.code =Scripts("""
$State2_1, $Flux2_1 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd2,$Material2_1,$Velocity2_1, 0.5);
$Removed2_1 = accutraveltimefractionremoved($Ldd2,$Material2_1,$Velocity2_1, 0.5)
""","""
$State2_1 = accutraveltimefractionstate($Ldd2,$Material2_1,$Velocity2_1, 0.5);
$Flux2_1 = accutraveltimefractionflux($Ldd2,$Material2_1,$Velocity2_1, 0.5);
$Removed2_1 = accutraveltimefractionremoved($Ldd2,$Material2_1,$Velocity2_1, 0.5)
""")

sample = newSample('accutraveltimefraction',
'State2_2')
sample.appendResult('Flux2_2')
sample.appendResult('Removed2_2')
sample.appendInput('Ldd2', useFrom='attfTests')
sample.appendInput('Material2_2', useFrom='attfTests')
sample.appendInput('Velocity2_2', useFrom='attfTests')
sample.code =Scripts("""
$State2_2, $Flux2_2 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd2,$Material2_2,$Velocity2_2, 0.5);
$Removed2_2 = accutraveltimefractionremoved($Ldd2,$Material2_2,$Velocity2_2, 0.5)
""","""
$State2_2 = accutraveltimefractionstate($Ldd2,$Material2_2,$Velocity2_2, 0.5);
$Flux2_2 = accutraveltimefractionflux($Ldd2,$Material2_2,$Velocity2_2, 0.5);
$Removed2_2 = accutraveltimefractionremoved($Ldd2,$Material2_2,$Velocity2_2, 0.5)
""")

sample = newSample('accutraveltimefraction',
'State2_3')
sample.appendResult('Flux2_3')
sample.appendResult('Removed2_3')
sample.appendInput('Ldd2', useFrom='attfTests')
sample.appendInput('Material2_2', useFrom='attfTests')
sample.appendInput('Velocity2_3', useFrom='attfTests')
sample.appendInput('Fraction2_3', useFrom='attfTests')
sample.code =Scripts("""
$State2_3 = accutraveltimefractionstate($Ldd2,$Material2_2,$Velocity2_3, $Fraction2_3);
$Flux2_3 = accutraveltimefractionflux($Ldd2,$Material2_2,$Velocity2_3, $Fraction2_3);
$Removed2_3 = accutraveltimefractionremoved($Ldd2,$Material2_2,$Velocity2_3, $Fraction2_3);
""","""
$State2_3 = accutraveltimefractionstate($Ldd2,$Material2_2,$Velocity2_3, $Fraction2_3);
$Flux2_3 = accutraveltimefractionflux($Ldd2,$Material2_2,$Velocity2_3, $Fraction2_3);
$Removed2_3 = accutraveltimefractionremoved($Ldd2,$Material2_2,$Velocity2_3, $Fraction2_3);
""")

sample = newSample('accutraveltimefraction',
'State2_4')
sample.appendResult('Flux2_4')
sample.appendResult('Removed2_4')
sample.appendInput('Ldd2', useFrom='attfTests')
sample.appendInput('Material2_2', useFrom='attfTests')
sample.appendInput('Velocity2_4', useFrom='attfTests')
sample.appendInput('Fraction2_3', useFrom='attfTests')
sample.code =Scripts("""
$State2_4, $Flux2_4 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd2,$Material2_2,$Velocity2_4, $Fraction2_3);
$Removed2_4 = accutraveltimefractionremoved($Ldd2,$Material2_2,$Velocity2_4, $Fraction2_3);
""","""
$State2_4 = accutraveltimefractionstate($Ldd2,$Material2_2,$Velocity2_4, $Fraction2_3);
$Flux2_4 = accutraveltimefractionflux($Ldd2,$Material2_2,$Velocity2_4, $Fraction2_3);
$Removed2_4 = accutraveltimefractionremoved($Ldd2,$Material2_2,$Velocity2_4, $Fraction2_3);
""")

sample = newSample('accutraveltimefraction',
'State3_1')
sample.appendResult('Flux3_1')
sample.appendResult('Removed3_1')
sample.appendInput('Ldd3', useFrom='attfTests')
sample.appendInput('Material3_1', useFrom='attfTests')
sample.appendInput('Velocity3_1', useFrom='attfTests')
sample.code =Scripts("""
$State3_1, $Flux3_1 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd3,$Material3_1,$Velocity3_1, 0.5);
$Removed3_1 = accutraveltimefractionremoved($Ldd3,$Material3_1,$Velocity3_1, 0.5);
""","""
$State3_1 = accutraveltimefractionstate($Ldd3,$Material3_1,$Velocity3_1, 0.5);
$Flux3_1 = accutraveltimefractionflux($Ldd3,$Material3_1,$Velocity3_1, 0.5);
$Removed3_1 = accutraveltimefractionremoved($Ldd3,$Material3_1,$Velocity3_1, 0.5);
""")

sample = newSample('accutraveltimefraction',
'State3_2')
sample.appendResult('Flux3_2')
sample.appendResult('Removed3_2')
sample.appendInput('Ldd3', useFrom='attfTests')
sample.appendInput('Material3_1', useFrom='attfTests')
sample.appendInput('Velocity3_2', useFrom='attfTests')
sample.code =Scripts("""
$State3_2, $Flux3_2 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd3,$Material3_1,$Velocity3_2, 0.5);
$Removed3_2 = accutraveltimefractionremoved($Ldd3,$Material3_1,$Velocity3_2, 0.5);
""","""
$State3_2 = accutraveltimefractionstate($Ldd3,$Material3_1,$Velocity3_2, 0.5);
$Flux3_2 = accutraveltimefractionflux($Ldd3,$Material3_1,$Velocity3_2, 0.5);
$Removed3_2 = accutraveltimefractionremoved($Ldd3,$Material3_1,$Velocity3_2, 0.5);
""")

sample = newSample('accutraveltimefraction','State4_1')
sample.appendResult('Flux4_1')
sample.appendResult('Removed4_1')
sample.appendInput('Ldd3', useFrom='attfTests')
sample.appendInput('Material4_1', useFrom='attfTests')
sample.appendInput('Velocity4_1', useFrom='attfTests')
sample.code =Scripts("""
$State4_1, $Flux4_1 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd3,$Material4_1,$Velocity4_1, 0.5);
$Removed4_1 = accutraveltimefractionremoved($Ldd3,$Material4_1,$Velocity4_1, 0.5);
""","""
$State4_1 = accutraveltimefractionstate($Ldd3,$Material4_1,$Velocity4_1, 0.5);
$Flux4_1 = accutraveltimefractionflux($Ldd3,$Material4_1,$Velocity4_1, 0.5);
$Removed4_1 = accutraveltimefractionremoved($Ldd3,$Material4_1,$Velocity4_1, 0.5);
""")

sample = newSample('accutraveltimefraction',
'State4_2')
sample.appendResult('Flux4_2')
sample.appendResult('Removed4_2')
sample.appendInput('Ldd3', useFrom='attfTests')
sample.appendInput('Material4_1', useFrom='attfTests')
sample.appendInput('Velocity4_2', useFrom='attfTests')
sample.code =Scripts("""
$State4_2, $Flux4_2 =
accutraveltimefractionstate,accutraveltimefractionflux($Ldd3,$Material4_1,$Velocity4_2, 0.5);
$Removed4_2 = accutraveltimefractionremoved($Ldd3,$Material4_1,$Velocity4_2, 0.5);
""","""
$State4_2 = accutraveltimefractionstate($Ldd3,$Material4_1,$Velocity4_2, 0.5);
$Flux4_2 = accutraveltimefractionflux($Ldd3,$Material4_1,$Velocity4_2, 0.5);
$Removed4_2 = accutraveltimefractionremoved($Ldd3,$Material4_1,$Velocity4_2, 0.5);
""")

sample = newSample("kinematic", "Qnew")
sample.comment = """invented by Cees, is this a realworld example? """
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accufraction")
sample.code = ExprIdentical("""kinematic($Ldd,$Material,0,1.5,0.6,1,15,10)""")


sample = newSample("kinwave", "State1")
sample.comment = """invented by Cees, is this a realworld example? """
sample.appendResult("Flux1")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accufraction")
sample.code = CalcExprPythonScript("""kinwavestate,kinwaveflux($Ldd,$Material,0,1.5,0.6,5,15,10)""","""
$State1 = kinwavestate($Ldd,$Material,0,1.5,0.6,5,15,10)
$Flux1 = kinwaveflux($Ldd,$Material,0,1.5,0.6,5,15,10)""")


sample = newSample("dynamicwave", "StateChezy")
sample.comment = """invented by Cees, is this a realworld example? """
sample.globalOptions+=["chezy"]
sample.appendResult("FluxChezy")
sample.appendInput("Table", ext="txt", autoCreatedInput=True)
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accufraction")
sample.code = CalcExprPythonScript("""dynwavestate,dynwaveflux(
$Table,
 14, # profileId
$Ldd,
$Material, # oldState
0, # inflow
0, # bottomLevel
0.1, # roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)
""","""
$StateChezy = dynwavestate(
$Table,
14, # profileId
$Ldd,
$Material, # oldState
0, # inflow
0, # bottomLevel
0.1, # roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)
$FluxChezy = dynwaveflux(
$Table,
14, # profileId
$Ldd,
$Material, # oldState
0, # inflow
0, # bottomLevel
0.1, # roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
) """)


sample = newSample("dynamicwave", "State2Chezy")
sample.comment = """Test MV propagation"""
sample.globalOptions+=["chezy"]
sample.appendResult("Flux2Chezy")
sample.appendInput("Table", ext="txt", autoCreatedInput=True)
sample.appendInput("ProfileId2")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("OldState2")
sample.code = CalcExprPythonScript("""dynwavestate,dynwaveflux(
$Table,
$ProfileId2, # with a MV
$Ldd,
$OldState2,
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)
""","""
$State2Chezy=dynwavestate(
$Table,
$ProfileId2, # with a MV
$Ldd,
$OldState2,
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)
$Flux2Chezy=dynwaveflux(
$Table,
$ProfileId2, # with a MV
$Ldd,
$OldState2,
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)""")


sample = newSample("dynamicwave", "State3Chezy")
sample.comment = """Ldd created and reviewed by Willem"""
sample.globalOptions+=["chezy"]
sample.appendResult("Flux3Chezy")
sample.appendInput("Table", ext="txt", autoCreatedInput=True)
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("OldState3")
sample.code = CalcExprPythonScript("""dynwavestate,dynwaveflux(
$Table,
 14,# profileId 
$Ldd,
$OldState3,
0,# inflow 
0,# bottomLevel 
0.1,# roughness 
1,# segmentLength 
10,# nrTimeSlices 
1, # timestepInSecs
0  # constantState
)
""","""
$State3Chezy = dynwavestate(
$Table,
 14,# profileId 
$Ldd,
$OldState3,
0,# inflow 
0,# bottomLevel 
0.1,# roughness 
1,# segmentLength 
10,# nrTimeSlices 
1, # timestepInSecs
0  # constantState
)
$Flux3Chezy = dynwaveflux(
$Table,
 14,# profileId 
$Ldd,
$OldState3,
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)""")


sample = newSample("dynamicwave", "StateManning")
sample.comment = """invented by Cees, is this a realworld example? """
sample.globalOptions+=["manning"]
sample.appendResult("FluxManning")
sample.appendInput("Table", ext="txt", autoCreatedInput=True)
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Material", useFrom="accufraction")
sample.code = CalcExprPythonScript("""dynwavestate,dynwaveflux(
$Table,
 14,# profileId
$Ldd,
$Material, # oldState
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)
""","""
$StateManning = dynwavestate(
$Table,
 14,# profileId
$Ldd,
$Material, # oldState
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)
$FluxManning = dynwaveflux(
$Table,
 14,# profileId
$Ldd,
$Material, # oldState
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)""")


sample = newSample("dynamicwave", "State2Manning")
sample.comment = """MV propagation"""
sample.globalOptions+=["manning"]
sample.appendResult("Flux2Manning")
sample.appendInput("Table", ext="txt", autoCreatedInput=True)
sample.appendInput("ProfileId2")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("OldState2")
sample.code = CalcExprPythonScript("""dynwavestate,dynwaveflux(
$Table,
$ProfileId2, # with a MV
$Ldd,
$OldState2,
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)""","""
$State2Manning = dynwavestate(
$Table,
$ProfileId2, # with a MV
$Ldd,
$OldState2,
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)
$Flux2Manning = dynwaveflux(
$Table,
$ProfileId2, # with a MV
$Ldd,
$OldState2,
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)""")


sample = newSample("dynamicwave", "State3Manning")
sample.comment = """reviewed by Willem"""
sample.globalOptions+=["manning"]
sample.appendResult("Flux3Manning")
sample.appendInput("Table", ext="txt", autoCreatedInput=True)
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("OldState3")
sample.code = CalcExprPythonScript("""dynwavestate,dynwaveflux(
$Table,
 14,# profileId
$Ldd,
$OldState3,
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)
""","""
$State3Manning = dynwavestate(
$Table,
14,# profileId
$Ldd,
$OldState3,
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)
$Flux3Manning = dynwaveflux(
$Table,
14,# profileId
$Ldd,
$OldState3,
0,# inflow
0,# bottomLevel
0.1,# roughness
1,# segmentLength
10,# nrTimeSlices
1, # timestepInSecs
0  # constantState
)""")


sample = newSample("acos", "Result")
sample.globalOptions+=["degrees"]
sample.appendInput("Expr")
sample.code = ExprIdentical("""acos($Expr)""")




sample = newSample("areaarea", "Result")
sample.globalOptions+=["unittrue"]
sample.appendInput("Class")
sample.code = ExprIdentical("""areaarea( $Class)""")




sample = newSample("areadiversity", "Result")
sample.appendInput("Class", useFrom="areaarea")
sample.appendInput("Expr")
sample.code = ExprIdentical("""areadiversity( $Expr, $Class)""")


sample = newSample("areamajority", "Result")
sample.appendInput("Class", useFrom="areaarea")
sample.appendInput("Expr")
sample.code = ExprIdentical("""areamajority( $Expr, $Class)""")


sample = newSample("areamaximum", "Result")
sample.appendInput("Class", useFrom="areaarea")
sample.appendInput("Expr")
sample.code = ExprIdentical("""areamaximum( $Expr, $Class)""")


sample = newSample("areaminimum", "Result")
sample.appendInput("Class", useFrom="areaarea")
sample.appendInput("Expr", useFrom="areamaximum")
sample.code = ExprIdentical("""areaminimum( $Expr, $Class)""")

sample = newSample("areaaverage", "Result")
sample.appendInput("Class", useFrom="areaarea")
sample.appendInput("Expr", useFrom="areamaximum")
sample.code = ExprIdentical("""areaaverage( $Expr, $Class)""")

sample = newSample("areatotal", "Result")
sample.appendInput("Class", useFrom="areaarea")
sample.appendInput("Expr", useFrom="areamaximum")
sample.code = ExprIdentical("""areatotal( $Expr, $Class)""")

sample = newSample("areanormal", "Result")
sample.random =True
sample.appendInput("Class", useFrom="areaarea")
sample.code = ExprIdentical("""areanormal( $Class)""")

sample = newSample("areauniform", "Result")
sample.random =True
sample.appendInput("Class", useFrom="areaarea")
sample.code = ExprIdentical("""areauniform($Class)""")


sample = newSample("asin", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""asin($Expr)""")


sample = newSample("aspect", "Result")
sample.appendInput("Dem", useFrom="slope")
sample.code = ExprIdentical("""aspect($Dem)""")


sample = newSample("atan", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""atan($Expr)""")


sample = newSample("boolean", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""boolean($Expr)""")


sample = newSample("catchment", "Result")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Points")
sample.code = ExprIdentical("""catchment($Ldd,$Points)""")


sample = newSample("cellarea", "Result1")
sample.globalOptions+=["unittrue"]
sample.appendInput("Expr")
sample.setClone(0)
sample.code = ExprIdentical("""cellarea()""")


sample = newSample("cellarea", "Result2")
sample.globalOptions+=["unitcell"]
sample.appendInput("Expr")
sample.setClone(0)
sample.code = ExprIdentical("""cellarea()""")


sample = newSample("celllength", "Result1")
sample.globalOptions+=["unittrue"]
sample.appendInput("Expr", useFrom="cellarea")
sample.setClone(0)
sample.code = ExprIdentical("""celllength()""")


sample = newSample("celllength", "Result2")
sample.globalOptions+=["unitcell"]
sample.appendInput("Expr", useFrom="cellarea")
sample.setClone(0)
sample.code = ExprIdentical("""celllength()""")


sample = newSample("clump", "Result1")
sample.appendInput("Expr")
sample.code = ExprIdentical("""clump( $Expr)""")


sample = newSample("clump", "Result2")
sample.globalOptions+=["nondiagonal"]
sample.appendInput("Expr")
sample.code = ExprIdentical("""clump( $Expr)""")


sample = newSample("cos", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""cos($Expr)""")


sample = newSample("cover", "Result1")
sample.appendInput("Expr1")
sample.code = ExprIdentical("""cover($Expr1,sqrt(9))""")


sample = newSample("cover", "Result2")
sample.appendInput("Expr1")
sample.appendInput("Expr2")
sample.appendInput("Expr3")
sample.code = ExprIdentical("""cover($Expr1,$Expr2,$Expr3)""")


sample = newSample("defined", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""defined($Expr)""")


sample = newSample("lddcreate", "Result1")
sample.appendInput("Dem")
sample.code = ExprIdentical("""lddcreate($Dem,9999999,9999999,9999999,9999999)""")


sample = newSample("lddcreate", "Result2")
sample.globalOptions+=["lddin"]
sample.appendInput("Dem")
sample.code = ExprIdentical("""lddcreate($Dem,9999999,9999999,9999999,9999999)""")


sample = newSample("lddcreate", "Result3")
sample.globalOptions+=["lddin"]
sample.appendInput("Dem")
sample.code = ExprIdentical("""lddcreate($Dem,9999999,5000,9999999,9999999)""")


sample = newSample("lddcreatedem", "Result1")
sample.appendInput("Dem", useFrom="lddcreate")
sample.code = ExprIdentical("""lddcreatedem($Dem,9999999,9999999,9999999,9999999)""")


sample = newSample("lddcreatedem", "Result2")
sample.globalOptions+=["lddcut"]
sample.appendInput("Dem", useFrom="lddcreate")
sample.code = ExprIdentical("""lddcreatedem($Dem,999999,9999999,9999999,9999999)""")


sample = newSample("directional", "Result1")
sample.appendInput("Expr")
sample.code = ExprIdentical("""directional($Expr)""")


sample = newSample("directional", "Result2")
sample.globalOptions+=["degrees"]
sample.appendInput("Expr", useFrom="boolean")
sample.code = ExprIdentical("""directional($Expr)""")


sample = newSample("downstream", "Result")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Expr")
sample.code = ExprIdentical("""downstream( $Ldd, $Expr)""")


sample = newSample("downstreamdist", "Result")
sample.appendInput("Ldd2", useFrom="accu")
sample.code = ExprIdentical("""downstreamdist($Ldd2)""")


sample = newSample("eq", "Result")
sample.appendInput("Expr1")
sample.appendInput("Expr2")
sample.code = ExprIdentical("""$Expr1 == $Expr2""")


sample = newSample("exp", "Result")
sample.appendInput("Power")
sample.code = ExprIdentical("""exp($Power)""")


sample = newSample("ge", "Result")
sample.appendInput("Expr1", useFrom="eq")
sample.appendInput("Expr2", useFrom="eq")
sample.code = ExprIdentical("""$Expr1 >= $Expr2""")


sample = newSample("gt", "Result")
sample.appendInput("Expr1", useFrom="eq")
sample.appendInput("Expr2", useFrom="eq")
sample.code = ExprIdentical("""$Expr1 > $Expr2""")


sample = newSample("idiv", "Result")
sample.appendInput("Expr1", useFrom="slash")
sample.appendInput("Expr2", useFrom="slash")
sample.code = ExprPcrcalcPython("$Expr1 idiv $Expr2", "$Expr1 // $Expr2")


sample = newSample("ifthen", "Result")
sample.appendInput("Cond")
sample.appendInput("Expr1")
sample.code = ExprPcrcalcPython("if($Cond then $Expr1)", "ifthen($Cond, $Expr1)")


sample = newSample("ifthenelse", "Result")
sample.appendInput("Cond", useFrom="ifthen")
sample.appendInput("Expr1", useFrom="ifthen")
sample.appendInput("Expr2")
sample.code = ExprPcrcalcPython("if($Cond,$Expr1,$Expr2)", "ifthenelse($Cond, $Expr1, $Expr2)")


sample = newSample("ldd", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""ldd($Expr)""")


sample = newSample("ldddist", "Result1")
sample.appendInput("Ldd2", useFrom="accu")
sample.appendInput("Points")
sample.code = ExprIdentical("""ldddist($Ldd2,$Points,1)""")


sample = newSample("ldddist", "Result2")
sample.appendInput("Ldd2", useFrom="accu")
sample.appendInput("Points2")
sample.appendInput("FrictMat")
sample.code = ExprIdentical("""ldddist($Ldd2,$Points2,$FrictMat)""")


sample = newSample("le", "Result")
sample.appendInput("Expr1", useFrom="eq")
sample.appendInput("Expr2", useFrom="eq")
sample.code = ExprIdentical("""$Expr1 <= $Expr2""")


sample = newSample("ln", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""ln($Expr)""")


sample = newSample("log10", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""log10($Expr)""")


sample = newSample("lookup", "Result1")
sample.appendInput("Table", ext="txt")
sample.appendInput("Expr1")
sample.code = ExprIdentical("""lookupnominal($Table,$Expr1)""")


sample = newSample("lookup", "Result2")
sample.appendInput("Table2", ext="txt")
sample.appendInput("Expr12")
sample.appendInput("Expr22")
sample.appendInput("Expr32")
sample.code = ExprIdentical("""lookupordinal($Table2,$Expr12,$Expr22,$Expr32)""")


sample = newSample("lookup", "Result3")
sample.comment = """matrixtable is hier fake"""
sample.globalOptions+=["matrixtable"]
sample.appendInput("Table2", ext="txt")
sample.appendInput("Expr12")
sample.appendInput("Expr22")
sample.appendInput("Expr32")
sample.code = ExprIdentical("""lookupordinal($Table2,1,100,$Expr32)""")


sample = newSample("lookup", "Result4")
sample.globalOptions+=["matrixtable"]
sample.appendInput("Table3", ext="txt")
sample.appendInput("Expr13")
sample.appendInput("Expr23")
sample.code = ExprIdentical("""lookupscalar($Table3,$Expr13,$Expr23)""")


sample = newSample("lookuplinear", "Result1")
sample.appendInput("Table", ext="txt")
sample.appendInput("Expr")
sample.code = ExprIdentical("""lookuplinear($Table,$Expr)""")


sample = newSample("lookuplinear", "Result2")
sample.appendInput("Table2", ext="txt")
sample.appendInput("Expr2")
sample.code = ExprIdentical("""lookuplinear($Table2,$Expr2)""")


sample = newSample("lt", "Result")
sample.appendInput("Expr1", useFrom="eq")
sample.appendInput("Expr2", useFrom="eq")
sample.code = ExprIdentical("""$Expr1 < $Expr2""")


sample = newSample("maparea", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""maparea($Expr)""")


sample = newSample("mapmaximum", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""mapmaximum($Expr)""")


sample = newSample("mapminimum", "Result")
sample.appendInput("Expr", useFrom="mapmaximum")
sample.code = ExprIdentical("""mapminimum($Expr)""")


sample = newSample("maptotal", "Result")
sample.appendInput("Expr", useFrom="mapmaximum")
sample.code = ExprIdentical("""maptotal($Expr)""")


sample = newSample("max", "Result1")
sample.appendInput("Expr1")
sample.appendInput("Expr2")
sample.code = ExprIdentical("""max($Expr1,$Expr2)""")


sample = newSample("min", "Result1")
sample.appendInput("Expr1", useFrom="max")
sample.appendInput("Expr2", useFrom="max")
sample.code = ExprIdentical("""min($Expr1,$Expr2)""")


sample = newSample("mod", "Result")
sample.appendInput("Expr1", useFrom="slash")
sample.appendInput("Expr2", useFrom="slash")
sample.code = ExprPcrcalcPython("$Expr1 mod $Expr2", "$Expr1 % $Expr2")


sample = newSample("lddmask", "Result")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Mask")
sample.code = ExprIdentical("""lddmask($Ldd,$Mask)""")


sample = newSample("nodirection", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""nodirection($Expr)""")


sample = newSample("nominal", "Result")
sample.appendInput("Expr", useFrom="rounddown")
sample.code = ExprIdentical("""nominal($Expr)""")


sample = newSample("normalnumber", "Result")
sample.random =True
sample.appendInput("Expr", useFrom="cellarea")
sample.setClone(0)
sample.code = ExprIdentical("""mapnormal()""")


sample = newSample("normalfield", "Result")
sample.random =True
sample.appendInput("Expr", useFrom="uniqueid")
sample.code = ExprIdentical("""normal($Expr)""")



sample = newSample("ne", "Result")
sample.appendInput("Expr1", useFrom="eq")
sample.appendInput("Expr2", useFrom="eq")
sample.code = ExprIdentical("""$Expr1 != $Expr2""")




sample = newSample("order", "Result")
sample.appendInput("Expr", useFrom="succ")
sample.code = ExprIdentical("""order($Expr)""")


sample = newSample("ordinal", "Result")
sample.appendInput("Expr", useFrom="rounddown")
sample.code = ExprIdentical("""ordinal($Expr)""")


sample = newSample("path", "Result")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Points")
sample.code = ExprIdentical("""path($Ldd,$Points)""")


sample = newSample("pit", "Result")
sample.appendInput("Ldd", useFrom="accu")
sample.code = ExprIdentical("""pit($Ldd)""")


sample = newSample("plancurv", "Result")
sample.comment = """afronding ten behoeve van voorbeelden in manual!!!"""
sample.appendInput("Dem", useFrom="slope")
sample.code = ExprIdentical("""roundoff(100*plancurv($Dem))/100""")


sample = newSample("pred", "Result1")
sample.appendInput("Expr", useFrom="succ")
sample.code = ExprIdentical("""pred($Expr)""")


sample = newSample("pred", "Result2")
sample.comment = """hier extra voorbeeld een succ.Expr.imap nemen waaraan legenda is toegekend"""
sample.appendInput("Expr", useFrom="succ")
sample.code = ExprIdentical("""pred($Expr)""")


sample = newSample("profcurv", "Result")
sample.comment = """afronding ten behoeve van voorbeelden in manual!!!"""
sample.appendInput("Dem", useFrom="slope")
sample.code = ExprIdentical("""roundoff(100* profcurv($Dem))/100""")


sample = newSample("lddrepair", "Result")
sample.appendInput("Ldd")
sample.code = ExprIdentical("""lddrepair($Ldd)""")


sample = newSample("rounddown", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""rounddown($Expr)""")


sample = newSample("roundoff", "Result")
sample.appendInput("Expr", useFrom="rounddown")
sample.code = ExprIdentical("""roundoff($Expr)""")


sample = newSample("roundup", "Result")
sample.appendInput("Expr", useFrom="rounddown")
sample.code = ExprIdentical("""roundup($Expr)""")


sample = newSample("scalar", "Result")
sample.appendInput("Expr1", useFrom="and")
sample.setClone(0)
sample.code = ExprIdentical("""scalar(1)""")


sample = newSample("sin", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""sin($Expr)""")


sample = newSample("slope", "Result")
sample.appendInput("Dem")
sample.code = ExprIdentical("""slope($Dem)""")


sample = newSample("slopelength", "Result1")
sample.appendInput("Ldd2", useFrom="accu")
sample.code = ExprIdentical("""slopelength($Ldd2,1)""")


sample = newSample("slopelength", "Result2")
sample.appendInput("Ldd2", useFrom="accu")
sample.appendInput("FrictMat")
sample.code = ExprIdentical("""slopelength($Ldd2,$FrictMat)""")


sample = newSample("spread", "Result1")
sample.appendInput("Points")
sample.code = ExprIdentical("""spread($Points,0,1)""")


sample = newSample("spread", "Result2")
sample.appendInput("Points2")
sample.appendInput("Initial2")
sample.appendInput("FrictMat2")
sample.code = ExprIdentical("""spread($Points2,$Initial2,$FrictMat2)""")


sample = newSample("spreadldd", "Result1")
sample.appendInput("Ldd2", useFrom="accu")
sample.appendInput("Points1")
sample.code = ExprIdentical("""spreadldd($Ldd2, $Points1,0,1)""")


sample = newSample("spreadldd", "Result2")
sample.appendInput("Ldd2", useFrom="accu")
sample.appendInput("Points2")
sample.appendInput("Initial")
sample.appendInput("FrictMat")
sample.code = ExprIdentical("""spreadldd($Ldd2, $Points2,$Initial,$FrictMat)""")


sample = newSample("spreadlddzone", "Result1")
sample.appendInput("Ldd2", useFrom="accu")
sample.appendInput("Points1", useFrom="spreadldd")
sample.code = ExprIdentical("""spreadlddzone($Ldd2, $Points1,0,1)""")


sample = newSample("spreadlddzone", "Result2")
sample.appendInput("Ldd2", useFrom="accu")
sample.appendInput("Points2", useFrom="spreadldd")
sample.appendInput("Initial", useFrom="spreadldd")
sample.appendInput("FrictMat", useFrom="spreadldd")
sample.code = ExprIdentical("""spreadlddzone($Ldd2, $Points2,$Initial,$FrictMat)""")


sample = newSample("spreadzone", "Result1")
sample.appendInput("Points", useFrom="spread")
sample.code = ExprIdentical("""spreadzone($Points,0,1)""")


sample = newSample("spreadzone", "Result2")
sample.appendInput("Points2", useFrom="spread")
sample.appendInput("Initial2", useFrom="spread")
sample.appendInput("FrictMat2", useFrom="spread")
sample.code = ExprIdentical("""spreadzone($Points2,$Initial2,$FrictMat2)""")


sample = newSample("sqr", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""sqr($Expr)""")


sample = newSample("sqrt", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""sqrt($Expr)""")


sample = newSample("subcatchment", "Result")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Points", useFrom="catchment")
sample.code = ExprIdentical("""subcatchment($Ldd,$Points)""")


sample = newSample("succ", "Result1")
sample.comment = """extra voorbeeld succ Result2 hier een succ.Expr.imap nemen waaraan legenda is toegekend"""
sample.appendInput("Expr")
sample.code = ExprIdentical("""succ($Expr)""")


sample = newSample("streamorder", "Result")
sample.appendInput("Ldd", useFrom="accu")
sample.code = ExprIdentical("""streamorder($Ldd)""")


sample = newSample("tan", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""tan($Expr)""")


sample = newSample("uniformnumber", "Result")
sample.random =True
sample.appendInput("Expr", useFrom="cellarea")
sample.setClone(0)
sample.code = ExprIdentical("""mapuniform()""")


sample = newSample("uniformfield", "Result")
sample.random =True
sample.appendInput("Expr", useFrom="uniqueid")
sample.code = ExprIdentical("""uniform($Expr)""")


sample = newSample("uniqueid", "Result")
sample.appendInput("Expr")
sample.code = ExprIdentical("""uniqueid($Expr)""")


sample = newSample("upstream", "Result")
sample.appendInput("Ldd", useFrom="accu")
sample.appendInput("Expr", useFrom="downstream")
sample.code = ExprIdentical("""upstream($Ldd, $Expr)""")


sample = newSample("view", "Result")
sample.appendInput("Dem")
sample.appendInput("Points")
sample.code = ExprIdentical("""view($Dem, $Points)""")


sample = newSample("windowaverage", "Result1")
sample.appendInput("Expr")
sample.code = ExprIdentical("""windowaverage( $Expr, 6)""")


sample = newSample("windowaverage", "Result2")
sample.appendInput("Expr")
sample.appendInput("WinLen2")
sample.code = ExprIdentical("""windowaverage( $Expr, $WinLen2)""")


sample = newSample("shift", "Result")
sample.appendInput("Expr", useFrom="windowaverage")
sample.code = ExprIdentical("""shift($Expr, 1,1)""")


sample = newSample("shift", "Result2")
sample.appendInput("Expr", useFrom="windowaverage")
sample.code = ExprIdentical("""shift($Expr, -1,-1)""")


sample = newSample("shift0", "Result1")
sample.appendInput("Expr", useFrom="windowaverage")
sample.code = ExprIdentical("""shift0($Expr, -1,-1)""")


sample = newSample("windowdiversity", "Result1")
sample.appendInput("Expr")
sample.code = ExprIdentical("""windowdiversity( $Expr, 6)""")


sample = newSample("windowdiversity", "Result2")
sample.appendInput("Expr")
sample.appendInput("WinLen2", useFrom="windowaverage")
sample.code = ExprIdentical("""windowdiversity( $Expr, $WinLen2)""")


sample = newSample("windowhighpass", "Result1")
sample.appendInput("Expr", useFrom="windowaverage")
sample.code = ExprIdentical("""windowhighpass( $Expr, 6)""")


sample = newSample("windowhighpass", "Result2")
sample.appendInput("Expr", useFrom="windowaverage")
sample.appendInput("WinLen2", useFrom="windowaverage")
sample.code = ExprIdentical("""windowhighpass( $Expr, $WinLen2)""")


sample = newSample("windowmajority", "Result1")
sample.appendInput("Expr")
sample.code = ExprIdentical("""windowmajority( $Expr, 6)""")


sample = newSample("windowmajority", "Result2")
sample.appendInput("Expr")
sample.appendInput("WinLen2", useFrom="windowaverage")
sample.code = ExprIdentical("""windowmajority( $Expr, $WinLen2)""")


sample = newSample("windowmaximum", "Result1")
sample.appendInput("Expr", useFrom="windowaverage")
sample.code = ExprIdentical("""windowmaximum($Expr, 6)""")


sample = newSample("windowmaximum", "Result2")
sample.appendInput("Expr", useFrom="windowaverage")
sample.appendInput("WinLen2", useFrom="windowaverage")
sample.code = ExprIdentical("""windowmaximum( $Expr, $WinLen2)""")


sample = newSample("windowminimum", "Result1")
sample.appendInput("Expr", useFrom="windowaverage")
sample.code = ExprIdentical("""windowminimum( $Expr, 6)""")


sample = newSample("windowminimum", "Result2")
sample.appendInput("Expr", useFrom="windowaverage")
sample.appendInput("WinLen2", useFrom="windowaverage")
sample.code = ExprIdentical("""windowminimum($Expr, $WinLen2)""")


sample = newSample("windowtotal", "Result1")
sample.appendInput("Expr", useFrom="windowaverage")
sample.code = ExprIdentical("""windowtotal( $Expr, 6)""")


sample = newSample("windowtotal", "Result2")
sample.appendInput("Expr", useFrom="windowaverage")
sample.appendInput("WinLen2", useFrom="windowaverage")
sample.code = ExprIdentical("""windowtotal( $Expr, $WinLen2)""")


sample = newSample("windowdiversity", "Result3")
sample.appendInput("Expr2")
sample.code = ExprIdentical("""(windowdiversity($Expr2,celllength() * 1.1)) > 1""")


sample = newSample("xcoordinate", "Result")
sample.globalOptions+=["coorlr"]
sample.appendInput("Expr")
sample.code = ExprIdentical("""xcoordinate($Expr)""")



sample = newSample("ycoordinate", "Result")
sample.globalOptions+=["coorcentre"]
sample.appendInput("Expr", useFrom="xcoordinate")
sample.code = ExprIdentical("""ycoordinate($Expr)""")


sample = newSample("argorderarealimited", "Result")
sample.appendInput("Chances11", useFrom="argorderwithidarealimited")
sample.appendInput("Chances12", useFrom="argorderwithidarealimited")
sample.code = ExprIdentical("""argorderarealimited(
 $Chances11,
  2, # limit in pixels
 $Chances12,
  2  # limit in pixels
  )
  """)


sample = newSample("argorderwithidarealimited", "Result")
sample.appendInput("Chances11")
sample.appendInput("Chances12")
sample.code = ExprIdentical("""argorderwithidarealimited(
 $Chances11,
  11,# first id
  2, # limit in pixels
 $Chances12,
  12,# second id
  2  # limit in pixels
  )
  """)


sample = newSample("argorderaddarealimited", "Result")
sample.comment = """add area:  recreation (RIVM notebook page 44)"""
sample.appendInput("CurrentId")
sample.appendInput("Chances1")
sample.appendInput("Chances2")
sample.code = ExprIdentical("""argorderaddarealimited(
 $CurrentId,
 $Chances1,
  1,# add area
 $Chances2,
  4  # limit in pixels
  )
  """)


sample = newSample("argorder", "Result")
sample.appendInput("Chances11", useFrom="argorderwithidarealimited")
sample.appendInput("Chances12", useFrom="argorderwithidarealimited")
sample.code = ExprIdentical("""argorder(
 $Chances11,
 $Chances12)""")


sample = newSample("argorderwithid", "Result")
sample.appendInput("Chances11", useFrom="argorderwithidarealimited")
sample.appendInput("Chances12", useFrom="argorderwithidarealimited")
sample.code = ExprIdentical("""argorderwithid(
 $Chances11,
  11,# first id
 $Chances12,
  12   # second id)
  )
  """)


sample = newSample("areaorder", "Result")
sample.appendInput("Expr")
sample.appendInput("AreaClass")
sample.code = ExprIdentical("""areaorder(
 $Expr,
 $AreaClass) """)


sample = newSample("umin", "Result")
sample.appendInput("Expr", useFrom="abs")
sample.code = ExprIdentical("""-$Expr""")


sample = newSample("uadd", "Result")
sample.appendInput("Expr", useFrom="abs")
sample.code = ExprIdentical("""+$Expr""")


sample = newSample("markwhilesum", "Result1")
sample.appendInput("Expr1")
sample.appendInput("Expr", useFrom="cos")
sample.code = ExprIdentical("""markwhilesumle($Expr1, scalar($Expr), 40)""")


sample = newSample("markwhilesum", "Result2")
sample.appendInput("Expr1")
sample.appendInput("Expr", useFrom="cos")
sample.code = ExprIdentical("""markwhilesumge($Expr1, scalar($Expr), 40)""")


sample = newSample("window4total", "Result1")
sample.appendInput("Expr1", useFrom="max")
sample.code = ExprIdentical("""window4total($Expr1)""")

sample = newSample("muskingum","FlowRate1", resultExt="txt")
sample.appendInput("tssid")
sample.appendInput("Ldd")
sample.appendInput("Inflow",ext="txt")
sample.code = CalcFullScript("""
# example from Applied Hydrology
binding
 Id = muskingum_tssid.map;
 Ldd = muskingum_Ldd.map;
 InTss = muskingum_Inflow.txt;
                      # valid ranges
 In_Timestep = 3600;  # 1 - 3600
 In_K = 2.3;          # 0.5 - 10
 In_x = 0.15;         # 0.1 - 0.9
 In_SegmentLength = 1;# 0.1 - 10
 In_NrIteration = 100;
 # Muskingum routing is stable when 1/2(1-K) <= K/timestepSecs <= 1/2x
 # Python code for test
 #   f1 = 0.5*(1-K)
 #   f2 = K / timestepInSecs
 #   f3 = 0.5 * x
 #   if not ( (f1 <= f2) and (f2 <= f3)):
 #     continue

timer
   1 40 1;
initial

   TimeStepInSecs = In_Timestep;
   K = In_K/TimeStepInSecs;  # [sec]

   x = In_x;

   # EQUAL To value at timestep 1 in InTss
   PrevFlowRate = if(Id eq 1 then 85 else 85)/TimeStepInSecs ;  # m3/sec

   PrevInflow = if(Id eq 1 then 85 else 0)/TimeStepInSecs;      # m3/sec

   # Segmentlength varieren tussen 0.1 en 10
   SegmentLength = In_SegmentLength;

   Iterations = In_NrIteration;

dynamic

   InflowTimestep= timeinputscalar(InTss, Id)/TimeStepInSecs;  # m3/sec

   FlowRate = muskingum(Ldd,PrevFlowRate,PrevInflow,
                        InflowTimestep,K,x,
                        SegmentLength, Iterations,TimeStepInSecs);

   report muskingum_FlowRate1.txt = timeoutput(Id,FlowRate*TimeStepInSecs);

   PrevFlowRate = FlowRate;
   PrevInflow = InflowTimestep;
""")
