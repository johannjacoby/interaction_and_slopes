#get example data with predictors X1 and X2 and a dependent variable Y
ds <- as.data.frame(
	read.table(
		"https://raw.github.com/johannjacoby/interaction_and_slopes/master/no_interaction_differing_slopes.dat", 
		header=T, sep="\t"))

#center both predictors
ds$centered.X1 <- scale(ds$X1, center=T, scale=F)
ds$centered.X2 <- scale(ds$X2, center=T, scale=F)

#shift X2 to obtain the simple slope of X1 @ 1SD below the mean of X2
ds$centered.X2.lo <- ds$centered.X2 + sd(ds$X2)
#shift X2 to obtain the simple slope of X1 @ 1SD above the mean of X2
ds$centered.X2.hi <- ds$centered.X2 - sd(ds$X2)

#### yes, it is correct, to get ds$centered.X2.hi you have to <b>subtract</b> 1 SD, 
#### and in order to get ds$centered.X2.lo you have to <b>add</b> 1 SD.



# now estimate the basic regression model to see whether X1 and X2 interact:
model0 <- lm(Y~centered.X1*centered.X2, ds)
# and the two regression models in order to obtain the simple slopes of X1 at X2=1SD below the mean and at X2=1SD above the mean:
model.lo <- lm(Y~centered.X1*centered.X2.lo, ds)
model.hi <- lm(Y~centered.X1*centered.X2.hi, ds)

#show the models estimates
summary(model0); summary(model.lo); summary(model.hi)

# print the interaction and the simple slopes of X1 @ X2=-1SD and X2=+1SD
results0 <- summary(model0)[[4]]; results.lo <- summary(model.lo)[[4]]; results.hi <- summary(model0)[[4]]
cat("\n",
	"Interaction X1 * X2: b=",results0[4],", t=",results0[12],", p=",
					sprintf("%5.4f",results0[16]),ifelse(results0[16] < .05," *",""),"\n",
	"Slope of X1 @ X2 = -1SD: b=",results.lo[2],", t=",results.lo[10],", p=",
					sprintf("%5.4f",results.lo[14]),ifelse(results.lo[14] < .05," *",""),"\n",
	"Slope of X1 @ X2 = +1SD: b=",results.hi[2],", t=",results.hi[10],", p=",
					sprintf("%5.4f",results.hi[14]),ifelse(results.hi[14] < .05," *",""),"\n",
	"absolute diff(p) = |",results.hi[14]," - ",results.lo[14],"| = ", abs(results.hi[14] - results.lo[14]),	"\n",
	"diff(b) = ",results.hi[2]," - ",results.lo[2]," = ",results.hi[2] - results.lo[2],	"\n",	
	sep="")


# simple slope of X1 @ X2 = mean
cat("Slope of X1 @ X2 = mean of X2: b=",results0[2],", t=",results0[10],", p=",sprintf("%5.4f",results0[14]),ifelse(results0[14] < .05," *",""),"\n")


# simple slopes of X1 @ X2 = -2.5 SD and @ X1 = -0.5SD
ds$centered.X2.lo.other <- ds$centered.X2 + 2.5*sd(ds$X2)
ds$centered.X2.hi.other <- ds$centered.X2 + 1.5*sd(ds$X2)
model.lo.other <- lm(Y~centered.X1*centered.X2.lo.other, ds)
model.hi.other <- lm(Y~centered.X1*centered.X2.hi.other, ds)

summary(model.lo.other); summary(model.hi.other)

results.lo.other <- summary(model.lo.other)[[4]]; results.hi.other = summary(model.hi.other)[[4]]
cat(
	"Slope of X1 @ X2 = -2.5SD: b=",results.lo.other[2],", t=",results.lo.other[10],", p=",
					sprintf("%5.4f",results.lo.other[14]),ifelse(results.lo.other[14] < .05," *",""),"\n",
	"Slope of X1 @ X2 = -1.55SD: b=",results.hi.other[2],", t=",results.hi.other[10],", p=",
					sprintf("%5.4f",results.hi.other[14]),ifelse(results.hi.other[14] < .05," *",""),"\n",
	"abs.diff(p) = |",results.hi.other[14]," - ",results.lo.other[14],"| = ",abs(results.hi.other[14] - results.lo.other[14]),	"\n",
	"diff(b) = ",results.hi.other[2]," - ",results.lo.other[2]," = ",results.hi.other[2] - results.lo.other[2],	"\n",	
	sep="")



exampledata.group.means <- as.data.frame(cbind(dv=c(trunc(rnorm(47,2,4)), trunc(rnorm(42,2.4,4))), group=c(rep(1,47), rep(2, 42))))
table( exampledata.group.means$dv	)
#comparing group means against 0
test1 <- t.test(exampledata.group.means[which(exampledata.group.means$group==1),]$dv)
test2 <- t.test(exampledata.group.means[which(exampledata.group.means$group==2),]$dv)
cat(
	"Group 1: t = ",test1$statistic,", p = ", sprintf("%20.10f",test1$p.value),"\n",
	"Group 2: t = ",test2$statistic,", p = ", sprintf("%20.10f",test2$p.value),"\n",
	sep="")

library(gplots)

barplot2(
	tapply(exampledata.group.means$dv, exampledata.group.means$group, mean),
	ylim = c(-1,5),
	names.arg = c("Group 1", "Group 2"),
	plot.ci=TRUE, ci.l=c(test1$conf.int[1], test2$conf.int[1]), ci.u=c(test1$conf.int[2], test2$conf.int[2])
)

#comparing the difference between means against 0
test.both <- t.test(exampledata.group.means$dv, exampledata.group.means$group)
cat(
	"Group comparison : t = ",test.both$statistic,", p ", ifelse(sprintf("%5.4f",test.both$p.value) < .0001, "< .0001", paste(" = ", sprintf("%5.4f",test.both$p.value), sep="")),"\n",
	sep="")
