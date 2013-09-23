ds <- as.data.frame(read.table("https://docs.google.com/uc?export=download&id=0B3pWnSnm2FOpeV9laDFMeU9RUW8"))

ds$centered.X1 <- scale(ds$X1, center=T, scale=F)
ds$centered.X2 <- scale(ds$X2, center=T, scale=F)
ds$centered.X2.lo <- ds$X2 + sd(ds$X2)
ds$centered.X2.hi <- ds$X2 - sd(ds$X2)

model0 <- lm(Y~centered.X1*centered.X2, ds)
model.lo <- lm(Y~centered.X1*centered.X2.lo, ds)
model.hi <- lm(Y~centered.X1*centered.X2.hi, ds)
summary(model0)
summary(model.lo)
summary(model.hi)

results0 <- summary(model0)[[4]]
results.lo <- summary(model.lo)[[4]]
results.hi <- summary(model0)[[4]]

cat("\n",
	"interaction: b=",results0[4],", t=",results0[12],", p=",sprintf("%5.4f",results0[16]),ifelse(results0[16] < .05," *",""),"\n",
	"slope low: b=",results.lo[2],", t=",results.lo[10],", p=",sprintf("%5.4f",results.lo[14]),ifelse(results.lo[14] < .05," *",""),"\n",
	"slope high: b=",results.hi[2],", t=",results.hi[10],", p=",sprintf("%5.4f",results.hi[14]),ifelse(results.hi[14] < .05," *",""),"\n",
	sep="")
