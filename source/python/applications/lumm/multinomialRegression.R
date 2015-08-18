# todo: standardize predictors
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
#dependent.logistic = NULL
dependent.multinomial = NULL
stepwise = NULL
categorical.variables = NULL
fractions.zero = NULL
summaryFile = NULL
lower = NULL
upper = NULL
pdfFile = NULL
maxNumberOfSteps = 1000

source("multinomialRegressionConfig.R")

if (!is.null(summaryFile)) {
    sink(summaryFile, split = FALSE) # split=TRUE: sink to both file and "stdout"
	if (is.null(pdfFile))
		pdfFile = paste(modelSetDirectory, paste(summaryFile, "pdf", sep = "."), 
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

mod.binreg = read.geoeas(paste(modelSetDirectory, dependentVariable, sep = .Platform$file.sep))
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

## change data type for categorical (factor) variables in *.var:
for (f in categoricalVariables) {
	cat(paste("variable", f, "\n"))
    print(summary(mod.var[[f]]))
	cat(paste("converting to categorical; category table:\n"))
    mod.var[f] = factor(mod.var[[f]])
    print(summary(mod.var[[f]]))
}

dependent.multinom = names(mod.binreg)
## this was the former case, with 1/0 bins transformed into nominal---
# stopifnot(length(dependent.multinom) > 1)
independent = "."

#dummies.cols = matrix(1:ncol(mod.binreg), ncol(mod.binreg), 1)
#mod.binreg = data.frame(lapply(mod.binreg, function(x)as.numeric(as.character(x))))
#f = as.matrix(mod.binreg) %*% dummies.cols
#if (length(unique(f)) > length(dependent.multinom))
#	dependent.multinom = c("zero", dependent.multinom)
#dependent.factor = factor(f, labels = dependent.multinom)
dependent.factor = factor(mod.binreg[[1]])

cat("SUMMARY OF MULTINOMIAL DEPENDENT VARIABLE dep:\n")
print(summary(dependent.factor))

indep.f = paste(independent, collapse = "+")
formula = as.formula(paste("dep", indep.f, sep = "~"))
cat("ENTERING MULTINOMIAL REGRESSION MODULE; FORMULA: ")
print(formula)
#data = data.frame(dep = dependent.factor, mod.inf, mod.var)
data = data.frame(dep = dependent.factor, mod.var)
library(nnet)
weights = rep(1, nrow(data))
#weights[f == 0] = 1/fractionUnchangedCellsInModelSampleSet
weights[f == 0] = fractionChangedCellsInModelSampleSet / fractionUnchangedCellsInModelSampleSet

if (is.null(stepwise)) {
    cat("NON-STEPWISE:\n")
    res = multinom(formula, data, weights = weights)
} else {
    cat(paste("STEPWISE, STEP DIRECTION:", stepwise, "\n"))
    if (is.null(lower))
        lower = as.formula(paste("dep", "~1"))
    if (is.null(upper)) {
        if (independent == "." || independent == "1")
            #independent = paste(c(names(mod.inf), names(mod.var)), collapse = "+")
            independent = paste(names(mod.var), collapse = "+")
        upper = as.formula(paste("dep", independent, sep = "~"))
    }
    scope = list(lower = lower, upper = upper)
	cat("formula scope (upper/lower):\n")
	print(scope)
    #res.start = glm(lower, data = data, family = binomial, weights = weights)
    if (stepwise == "backward")
		res.start = multinom(upper, data = data, weights = weights)
	else
		res.start = multinom(lower, data = data, weights = weights)
    res.stepwise = step(res.start, scope = scope, direction = stepwise,
		steps = maxNumberOfSteps)
	res = res.stepwise
}
#xxx
if (reportSummary) {
    cat("SUMMARY of MULTINOM REGRESSION MODEL:\n")
	print(summary(res))
}

cat("SUMMARY of PREDICTED PROBABILITIES:\n")
pred = predict(res, data, type = "prob")
print(summary(pred))
cat("OBSERVED VS. PREDICTED CLASSES:\n")
print(table(observed = dependent.factor, predicted = predict(res, data, type = "class")))
write.csv(as.data.frame(t(coefficients(res))), file = "multinomcoefficients.csv")
cat("Coefficients written to multinomcoefficients.csv\n")

## NOW DO THE VALIDATION; overwrite the former data.frame names to reuse memory:

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
mod.binreg = mod.binreg[,-(1:2), drop=FALSE]
## convert to categorical:
mod.binreg = data.frame(lapply(mod.binreg, as.factor))

## check names & sizes:
#names(mod.inf); dim(mod.inf)
names(mod.var); dim(mod.var)
names(mod.binreg); dim(mod.binreg)

## summary of dependent variable:
summary(mod.binreg)

## change data type for categorical (factor) variables in *.var:
for (f in categoricalVariables) {
	cat(paste("variable", f, "\n"))
    print(summary(mod.var[[f]]))
	cat(paste("converting to categorical; category table:\n"))
    mod.var[f] = factor(mod.var[[f]])
    print(summary(mod.var[[f]]))
}

#dependent.logistic = names(mod.binreg)
dependent.multinom = names(mod.binreg)
#stopifnot(length(dependent.multinom) > 1)

#dummies.cols = matrix(1:ncol(mod.binreg), ncol(mod.binreg), 1)
#mod.binreg = data.frame(lapply(mod.binreg, function(x)as.numeric(as.character(x))))
#f = as.matrix(mod.binreg) %*% dummies.cols
#if (length(unique(f)) > length(dependent.multinom))
#	dependent.multinom = c("zero", dependent.multinom)
#dependent.factor = factor(f, labels = dependent.multinom)

cat("SUMMARY OF MULTINOMIAL DEPENDENT VARIABLE dep:\n")
dependent.factor = factor(mod.binreg[[1]])
print(summary(dependent.factor))

#data = data.frame(dep = dependent.factor, mod.inf, mod.var)
data = data.frame(dep = dependent.factor, mod.var)
cat("SUMMARY of PREDICTED PROBABILITIES--VALIDATION:\n")
pred = predict(res, data, type = "prob")
print(summary(pred))
cat("OBSERVED VS. PREDICTED CLASSES--VALIDATION:\n")
print(table(observed = dependent.factor, predicted = predict(res, data, type = "class")))
