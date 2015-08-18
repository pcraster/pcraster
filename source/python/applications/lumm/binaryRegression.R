"read.geoeas" <- function(file)
{
    simplify.names = function(x) {
        # unlist(lapply(strsplit(x, "\\\.", extended=F), function(x) x[length(x)-1]))
        unlist(lapply(strsplit(x, "\\\.", extended=F), function(x) 
            paste(x[(length(x)-1):length(x)],collapse=".")))
    }
    title.line <- scan(file, what = "", n = 1, sep = "\n", quiet = TRUE)
    nvar <- scan(file, what = 1, n = 1, skip = 1, quiet = TRUE)
    vnames <- scan(file, what = "", n = nvar, flush = T, skip = 2, quiet = TRUE)
    #
    #    inlist <- vector("list", length = nvar)
    #    names(inlist) <- vnames
    #    inlist[seq(1, nvar)] <- 0
    #    data <- data.frame(scan(file, what = inlist, skip = nvar + 2))
    data <- matrix(scan(file, what = 1, skip = nvar + 2, quiet = TRUE), ncol = nvar,
        byrow = T, dimnames = list(NULL, vnames))
    data[data == 1e31] <- NA
    data = data.frame(data)
    # EJP: added this line (and function, see top of read.geoeas):
    #names(data) = simplify.names(names(data))
    data
}

## erase things possibly in working data base, for the case
## they are not set in the lumm_init.R file:
dependent.logistic = NULL
dependent.multinomial = NULL
stepwise = NULL
categorical.variables = NULL
fractions.zero = NULL
summaryFile = NULL
lower = NULL
upper = NULL
pdfFile = NULL
vgmFile = NULL
variogramBinWidth = 25.001
variogramCutoff = NULL
maxNumberOfSteps = 1000
#fractionChangedCellsInModelSampleSet = 1

source("binaryRegressionConfig.R")

if (!is.null(summaryFile)) {
    sink(summaryFile, split = TRUE) # split=TRUE: sink to both file and "stdout"
	if (is.null(pdfFile))
		pdfFile = paste(modelSetDirectory, paste(summaryFile, "pdf", sep = "."), 
			sep = .Platform$file.sep)
	if (is.null(vgmFile))
		vgmFile = paste(modelSetDirectory, paste(summaryFile, "_vgm.pdf", sep = ""), 
			sep = .Platform$file.sep)
}

#binaryregression.col  independent.col  influences.col
#modelSetDirectory = "/mnt/hdb2/edzer/ton/lumm/lummoutput-case03/model"
#validationSetDirectory = "/mnt/hdb2/edzer/ton/lumm/lummoutput-case03/validation"

#mod.inf = read.table(
#	paste(modelSetDirectory, influences, sep = .Platform$file.sep),  
#	header = TRUE)
## ignore first three columns: class,column,row:
#mod.inf = mod.inf[-(1:3)]

mod.var = read.geoeas(
	paste(modelSetDirectory, independentVariables, sep = .Platform$file.sep))
## ignore first two columns: x,y
mod.var = mod.var[-(1:2)]

mod.binreg = read.geoeas(
	paste(modelSetDirectory, dependentVariable, sep = .Platform$file.sep))
## ignore first two columns: x,y
mod.xy = mod.binreg[, 1:2, drop=FALSE]
mod.binreg = mod.binreg[, -(1:2), drop=FALSE]
## convert to categorical:
mod.binreg = data.frame(lapply(mod.binreg, as.factor))

mod.landUseChanged = read.geoeas(
	paste(modelSetDirectory, landUseChanged, sep = .Platform$file.sep))
mod.landusechanged = (mod.landUseChanged[[3]] == 1)

## check names & sizes:
#names(mod.inf); dim(mod.inf)
names(mod.var); dim(mod.var)
names(mod.binreg); dim(mod.binreg)
names(mod.landUseChanged); dim(mod.landUseChanged);

## summary of dependent variable:
summary(mod.binreg)
if (length(unique(mod.binreg[[1]])) == 1) {
	cat("THIS VARIABLE IS NOT VARIABLE--QUITTING R PROCESS\n")
	q()
}

## change data type for categorical (factor) variables in *.var:
for (f in categoricalVariables) {
	cat(paste("variable", f, "\n"))
	if (is.null(mod.var[[f]]))
		print(names(f))
	stopifnot(!is.null(mod.var[[f]]))
    print(summary(mod.var[[f]]))
	cat(paste("converting to categorical; category table:\n"))
    mod.var[f] = factor(mod.var[[f]])
    print(summary(mod.var[[f]]))
}

dependent.logistic = names(mod.binreg)
stopifnot(length(dependent.logistic) == 1)
independent = "."

indep.f = paste(independent, collapse = "+")
formula = as.formula(paste(dependent.logistic, indep.f, sep = "~"))
cat("ENTERING LOGISTIC REGRESSION MODULE; STARTING FORMULA: ")
print(formula)
#data = data.frame(mod.binreg[dependent.logistic], mod.inf, mod.var)
data = data.frame(mod.binreg[dependent.logistic], mod.var)
weights = rep(1, nrow(data))
zero.weight = fractionChangedCellsInModelSampleSet / fractionUnchangedCellsInModelSampleSet
weights[!mod.landusechanged] = zero.weight 

cat("WEIGHTED MEAN FOR THIS CLASS IN MODELSET: \n")
print(weighted.mean(as.numeric(mod.binreg[[1]] == 1), weights))

if (is.null(stepwise)) {
    cat("NON-STEPWISE:\n")
    res = glm(formula, data, family = binomial, weights = weights)
	pred = predict(res, data, type = "response")
} else {
    cat(paste("STEPWISE, STEP DIRECTION:", stepwise, "\n"))
    if (is.null(lower))
        lower = as.formula(paste(dependent.logistic, "~1"))
    if (is.null(upper)) {
        if (independent == "." || independent == "1")
            #independent = paste(c(names(mod.inf), names(mod.var)), collapse = "+")
            independent = paste(names(mod.var), collapse = "+")
        upper = as.formula(paste(dependent.logistic, independent, sep = "~"))
    }
    scope = list(lower = lower, upper = upper)
	cat("formula scope (upper/lower):\n")
	print(scope)
    #res.start = glm(lower, data = data, family = binomial, weights = weights)
    if (stepwise == "backward")
		res.start = glm(upper, data = data, family = binomial, weights = weights)
	else
		res.start = glm(lower, data = data, family = binomial, weights = weights)
    res.stepwise = step(res.start, scope = scope, direction = stepwise, 
		# keep = function(x,y) { k = kappa(x) ; print(paste("condition number:",k)) },
		steps = maxNumberOfSteps)
	pred = predict(res.stepwise, data, type = "response")
	res = res.stepwise
}

if (reportSummary) {
    cat("SUMMARY of LOGISTIC REGRESSION MODEL:\n")
    print(summary(res))
}

if (require(ROCR) && !is.null(pdfFile)) {
    x = prediction(pred, mod.binreg[[1]])
    x.perf = performance(x, "tpr", "fpr")
	pdf(pdfFile)
	title = paste("model ROC:", summaryFile)
    plot(x.perf, colorize=F, main = title)
	print(paste("Area under ROC curve:", performance(x, "auc")@y.values[[1]], "\n"))
} else
    print("cannot load package ROCR for drawing ROC curves")


write.csv(
	data.frame(mod.xy, working = residuals(res, type = "working"),
		pearson = residuals(res, type = "pearson"),
		response = residuals(res, type = "response"),
		predicted = predict(res)), 
	file = paste(summaryFile, "residuals.csv", sep = "_")
)

if (require(gstat) && !is.null(vgmFile)) {
	data.vgm = data.frame(mod.xy, res = residuals(res, type = "working"))
	names(data.vgm) = c("x", "y", "res")
	summary(data.vgm)
	# write out working residuals:
	pdf(vgmFile)
	if (is.null(variogramCutoff))
		vgrm = variogram(res ~ 1, ~ x + y, data.vgm, width = variogramBinWidth)
	else
		vgrm = variogram(res ~ 1, ~ x + y, data.vgm, width = variogramBinWidth, 
			cutoff = variogramCutoff)
	# print(vgrm[1:10,])
	print(plot(vgrm, main = summaryFile))
	dev.off()
} else
    print("cannot load package gstat for drawing variogram curves")

## save data:
save.image(paste(summaryFile, ".RData", sep=""), compress = TRUE)

## traceback()
## NOW DO THE VALIDATION; overwrite the former data.frame names to reuse memory:

if (!is.null(summaryFile)) {
	pdfFile = 
		paste(validationSetDirectory, paste(summaryFile, "pdf", sep = "."), 
		sep = .Platform$file.sep)
}

#mod.inf = read.table(
#	paste(validationSetDirectory, influences, sep = .Platform$file.sep),
#	header = TRUE)
## ignore first three columns: class,column,row:
#mod.inf = mod.inf[-(1:3)]

mod.var = read.geoeas(
	paste(validationSetDirectory, independentVariables, sep = .Platform$file.sep))
## ignore first two columns: x,y
mod.var = mod.var[-(1:2)]

mod.binreg = read.geoeas(
	paste(validationSetDirectory, dependentVariable, sep = .Platform$file.sep))
## ignore first two columns: x,y
mod.xy = mod.binreg[, 1:2, drop=FALSE]
mod.binreg = mod.binreg[, -(1:2), drop=FALSE]
## convert to categorical:
mod.binreg = data.frame(lapply(mod.binreg, as.factor))

## check names & sizes:
#names(mod.inf); dim(mod.inf)
names(mod.var); dim(mod.var)
names(mod.binreg); dim(mod.binreg)

## summary of dependent variable:
summary(mod.binreg)
print("UNWEIGHTED MEAN OF DEPENDENT IN VALIDATION SET")
print(mean(as.numeric(mod.binreg[[1]] == 1)))

## change data type for categorical (factor) variables in *.var:
for (f in categoricalVariables) {
	cat(paste("variable", f, "\n"))
    print(summary(mod.var[[f]]))
	cat(paste("converting to categorical; category table:\n"))
    mod.var[f] = factor(mod.var[[f]])
    print(summary(mod.var[[f]]))
}

dependent.logistic = names(mod.binreg)
stopifnot(length(dependent.logistic) == 1)
#data = data.frame(mod.binreg[dependent.logistic], mod.inf, mod.var)
data = data.frame(mod.binreg[dependent.logistic], mod.var)
pred = predict(res, data, type = "response")
print("VALIDATION PREDICTIONS on RESPONSE SCALE SUMMARY:")
summary(pred)

if (require(ROCR) && !is.null(pdfFile)) {
    x = prediction(pred, mod.binreg[[1]])
    x.perf = performance(x, "tpr", "fpr")
	pdf(pdfFile)
	title = paste("validation ROC:", summaryFile)
    plot(x.perf, colorize=F, main = title)
	print(paste("Area under ROC curve:", performance(x, "auc")@y.values[[1]], "\n"))
} else
    print("cannot load package ROCR for drawing ROC curves")
