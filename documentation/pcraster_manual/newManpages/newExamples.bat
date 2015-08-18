@echo off
REM New examples per January 2009 who need further processing
REM Each function should have a section like below
REM  - the pcrcalc statement should be executable
REM  - reuse existing input data as much as possible
REM  - input table must have .txt extension

REM Name: Shift
REM Shift uses northing,westing and not southing,westing as stated in the manual_updates
REM shift0 will also assign 0 to missing values already in the map
REM If northing or southing are given spatial expression an "ERROR: Unknown exception (programming error)"/"typeinfo::name = not present" occurs and crashes the program
pcrcalc shift0.Result1.omap = shift0(windowaverage.Expr.imap,-1,1)

REM Name: lookuplinear
REM MV = MV
REM Values in expression map outside domain of table search key = MV
REM When search key in table goes up and then down the command uses the first two numbers that contain expression to interpolate
REM TODO: return error message when search key goes up and then down
REM If search key values are in descending order all MV are assigned
pcrcalc lookuplinear.Result1.omap = lookuplinear(lookuplinear.Table.txt,lookuplinear.Expr.imap)
pcrcalc lookuplinear.Result2.omap = lookuplinear(lookuplinear.Table2.txt,lookuplinear.Expr2.imap)

REM Name: markwhilesum
REM MV in expressions = MV in result
REM The Order map can contain: multiple identical values, and and missing counts ie 1,1,2,3,4 without returning an error
REM The Order map can also contain scalar values which seem to be rounded down
REM multiple identical values in the order map seem to be counted from top left to bottom right
pcrcalc markwhilesum.Result1.omap = markwhilesumle(markwhilesum.Expr1.imap,scalar(cos.Expr.imap),40)
pcrcalc markwhilesum.Result2.omap = markwhilesumge(markwhilesum.Expr1.imap,scalar(cos.Expr.imap),40)

REM Name: timeinputsparse
REM Example timeinput\timeinputsparse.mod

REM Name: timeinputmodulo
REM Example: timeinput\timeinputmodulo.mod

REM NAME: lookupmapstack
REM example: timeinput\pcrcalc --clone rain0000.001 lookupmapstack.Result.map = lookupmapstack(rain,6)

REM Name: window4total
pcrcalc window4total.Result1.omap = window4total(max.Expr1.imap)

REM NAME: influencesimplegauss
REM doesn't give neat values for an example, might be better to cut off at the second decimal in the example

REM NAME: accutraveltime
REM examples are already present in ~pcrmanual\manual_updates\AccuTravelTime
REM velocity = 0 seems to make the travel time of cells zero, ie instant travel
REM don't know if I expressed the formula for diffusion correctly

REM NAME: transient, diffuse
REM there are only CC code files in newFunctionsStuff which I can't make much sense of
