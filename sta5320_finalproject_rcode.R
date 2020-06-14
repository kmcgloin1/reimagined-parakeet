# ----- STA 5320: Final Project -----

# Input variables (based on physicochemical tests):
#  1 - fixed acidity (fa)
#  2 - volatile acidity (va)
#  3 - citric acid (ca)
#  4 - residual sugar (rs)
#  5 - chlorides (chl)
#  6 - free sulfur dioxide (fsd)
#  7 - total sulfur dioxide (tsd)
#  8 - density (den)
#  9 - pH (ph)
#  10 - sulphates (sul)
#  11 - alcohol (alc)
# Output variable (based on sensory data): 
#  12 - quality (score between 0 and 10) (qual)

# set the working directory
setwd("~/Documents/CPP Grad/SPRING 2020/STA 5320/Final Project")

# read in the two data sets
wine.red <- read.csv("winequality-red.csv")
wine.white <- read.csv("winequality-white.csv")

# make sure data makes sense:
head(wine.red)
head(wine.white)

# look at variable names:
names(wine.red)
names(wine.white)

# look at numbers of rows and columns:
dim(wine.red)
dim(wine.white)

# look at types of variables:
sapply(wine.red, class)
sapply(wine.white, class)

summary(wine.red)

# for convenience, make shorter variable names:
names(wine.red) <- c("fa","va","ca","rs","chl","fsd","tsd","den","ph","sul","alc","qual")
names(wine.white) <- names(wine.red)

# ----- EXPLORATORY DATA ANALYSIS -----

# ----- DIFFERENCES IN DISTRIBUTIONS -----

# Which variables have univariate distributions that seem to differ between red and white wines?

# ----- Red Wines Normality -----

qqnorm(wine.red$fa)
qqnorm(wine.red$va)
qqnorm(wine.red$ca)
qqnorm(wine.red$rs)
qqnorm(wine.red$chl)
qqnorm(wine.red$fsd)
qqnorm(wine.red$tsd)
qqnorm(wine.red$den)
qqnorm(wine.red$ph)
qqnorm(wine.red$sul)
qqnorm(wine.red$alc)

shapiro.test(wine.red$fa)
shapiro.test(wine.red$va)
shapiro.test(wine.red$ca)
shapiro.test(wine.red$rs)
shapiro.test(wine.red$chl)
shapiro.test(wine.red$fsd)
shapiro.test(wine.red$tsd)
shapiro.test(wine.red$den)
shapiro.test(wine.red$ph)
shapiro.test(wine.red$sul)
shapiro.test(wine.red$alc)
shapiro.test(wine.red$qual)

# ----- Red Wines Linearity -----

lm1 <- lm(qual ~ fa, data=wine.red) # linear
plot(lm1, 1)
lm2 <- lm(qual ~ va, data=wine.red) # linear
plot(lm2, 1)
lm3 <- lm(qual ~ ca, data=wine.red) # linear
plot(lm3, 1)
lm4 <- lm(qual ~ rs, data=wine.red) # linear
plot(lm4, 1)
lm5 <- lm(qual ~ chl, data=wine.red) # linear
plot(lm5, 1)
lm6 <- lm(qual ~ fsd, data=wine.red) # linear
plot(lm6, 1)
lm7 <- lm(qual ~ tsd, data=wine.red) # linear
plot(lm7, 1)
lm8 <- lm(qual ~ den, data=wine.red) # possibly nonlinear
plot(lm8, 1)

ggplot(lm(qual ~ den, data=wine.red)) + 
  geom_point(aes(x=.fitted, y=.resid)) +
  geom_smooth(aes(x=.fitted, y=.resid), color = "red") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(x='Fitted Values',y='Residuals') +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1))

lm9 <- lm(qual ~ ph, data=wine.red) # linear
plot(lm9, 1)
lm10 <- lm(qual ~ sul, data=wine.red) # nonlinear
plot(lm10, 1)
ggplot(lm(qual ~ sul, data=wine.red)) + 
  geom_point(aes(x=.fitted, y=.resid)) +
  geom_smooth(aes(x=.fitted, y=.resid), color = "red") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(x='Fitted Values',y='Residuals') +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1))
lm11 <- lm(qual ~ alc, data=wine.red) # linear
plot(lm11, 1)

lm12 <- lm(qual ~ fa + va + ca + rs + chl + fsd + tsd + den + ph + sul + alc, data=wine.red)
plot(lm12, 1) # linear

# It appears that for the red wines density and sulphates may be the only predictors that are slightly nonlinear

# ----- White Wines Normality -----

qqnorm(wine.white$fa)
qqnorm(wine.white$va)
qqnorm(wine.white$ca)
qqnorm(wine.white$rs)
qqnorm(wine.white$chl)
qqnorm(wine.white$fsd)
qqnorm(wine.white$tsd)
qqnorm(wine.white$den)
qqnorm(wine.white$ph)
qqnorm(wine.white$sul)
qqnorm(wine.white$alc)
qqnorm(wine.white$qual)

shapiro.test(wine.white$fa)
shapiro.test(wine.white$va)
shapiro.test(wine.white$ca)
shapiro.test(wine.white$rs)
shapiro.test(wine.white$chl)
shapiro.test(wine.white$fsd)
shapiro.test(wine.white$tsd)
shapiro.test(wine.white$den)
shapiro.test(wine.white$ph)
shapiro.test(wine.white$sul)
shapiro.test(wine.white$alc)
shapiro.test(wine.white$qual)

# ----- White Wine Linearity -----

lm1.white <- lm(qual ~ fa, data=wine.white) # linear
plot(lm1.white, 1)
lm2.white <- lm(qual ~ va, data=wine.white) # linear
plot(lm2.white, 1)
lm3.white <- lm(qual ~ ca, data=wine.white) # linear
plot(lm3.white, 1)
lm4.white <- lm(qual ~ rs, data=wine.white) # linear
plot(lm4.white, 1)

lm5.white <- lm(qual ~ chl, data=wine.white) # nonlinear
plot(lm5.white, 1)
ggplot(lm(qual ~ fsd, data=wine.white)) + 
  geom_point(aes(x=.fitted, y=.resid)) +
  geom_smooth(aes(x=.fitted, y=.resid), color = "red") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(x='Fitted Values',y='Residuals') +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1))

lm6.white <- lm(qual ~ fsd, data=wine.white) # nonlinear
plot(lm6.white, 1)


lm7.white <- lm(qual ~ tsd, data=wine.white) # linear
plot(lm7.white, 1)

lm8.white <- lm(qual ~ den, data=wine.white) # nonlinear
plot(lm8.white, 1)

lm9.white <- lm(qual ~ ph, data=wine.white) # linear
plot(lm9.white, 1)
lm10.white <- lm(qual ~ sul, data=wine.white) # linear
plot(lm10.white, 1)
lm11.white <- lm(qual ~ alc, data=wine.white) # linear
plot(lm11.white, 1)

lm12.white <- lm(qual ~ fa + va + ca + rs + chl + fsd + tsd + den + ph + sul + alc, data=wine.white)
plot(lm12.white, 1) # linear

# Of the predictors of the white wines, it appears that chloride, free sulfur dioxide, and density may be nonlinear. 

# The distribution of tsd, den, sul seems to differ between red and white wines

# ----- CORRELATION -----

# Which predictor variables are correlated with Y and which are correlated with each other?

# correlation matrix:
round(cor(wine.red),2)

cor(wine.red$va, wine.red$qual)
cor(wine.red$alc, wine.red$qual)
# For red wines, it appears that va and alc are highly correlated with the quality of the wine.

pairs(wine.red[,11:12],pch=20,col=rgb(0,0,0,.1))

# Among the red wine variables, it appears that fa and ca, fa and den, fa and ph, va and ca, tsd and fsd, ph and ca, and alc and den are highly correlated. 

round(cor(wine.white),2)

pairs(wine.white[,6:12],pch=20,col=rgb(0,0,0,.1))

# For white wines, it appears that alc and den are highly correlated with the quality of the wine. 

# Among the white wine variables, it appears that den and rs, tsd and fsd, tsd and den, and alc and den are highly correlated. 

# It appears the variables of red wine are more correlated than the white wine variables

# subtract the two correlation matrices to see how the correlations
# differ between red and white wines:
round(cor(wine.red)-cor(wine.white),2)

# Great difference in corrlations between red and white wine variables: fa and ca, va and ca, ph and ca, rs and alc, dens and tsd, sul and chl
# fa and ca are more correlated for red wines
# ca and va are more correlated for red wines
# ph and ca are more correlated for red wines
# rs and alc are more correlated for white wines
# dens and tsd are more correlated for white wines
# sul and chl are more correlated for red wines

# scatter plot matrix (of subset of features):
pairs(wine.red[,1:12],pch=20,col=rgb(0,0,0,.1))
pairs(wine.white[,1:12],pch=20,col=rgb(0,0,0,.033))

pairs(wine.red[,4:6],pch=20,col=rgb(0,0,0,.1))
pairs(wine.white[,4:6],pch=20,col=rgb(0,0,0,.033))

pairs(wine.red[,7:11],pch=20,col=rgb(0,0,0,.1))
pairs(wine.white[,6:11],pch=20,col=rgb(0,0,0,.033))

pairs(wine.red[,11:12],pch=20,col=rgb(0,0,0,.1))
pairs(wine.white[,1:12],pch=20,col=rgb(0,0,0,.033))


library(ggcorrplot)
corr.red <- round(cor(wine.red), 2)

red_corr_plot <- ggcorrplot(corr.red, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")) 
  

corr.white <- round(cor(wine.white), 2)

white_corr_plot <- ggcorrplot(corr.white, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")) 

library(ggpubr)
ggarrange(red_corr_plot, white_corr_plot, nrow = 1, ncol = 2)

# ----- OUTLIERS -----

# Are there any outliers in either data set?

# ----- Red Wines -----

plot(wine.red$fa, wine.red$qual) # okay
plot(wine.red$va, wine.red$qual) # maybe
plot(wine.red$ca, wine.red$qual) # maybe
plot(wine.red$rs, wine.red$qual) # maybe/okay
plot(wine.red$chl, wine.red$qual) # maybe
plot(wine.red$fsd, wine.red$qual) # okay
plot(wine.red$tsd, wine.red$qual) # yes, looks to be one
plot(wine.red$den, wine.red$qual) # okay
plot(wine.red$ph, wine.red$qual)  # okay
plot(wine.red$sul, wine.red$qual) # maybe
plot(wine.red$alc, wine.red$qual) # maybe


# do a scatter plot with random subsampling for red wines:
with(wine.red[sample(nrow(wine.red),500),],plot(va,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # this one may be okay; okay by Cook's Distance
with(wine.red[sample(nrow(wine.red),500),],plot(ca,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # okay by Cook's Distance
with(wine.red[sample(nrow(wine.red),500),],plot(chl,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # okay by Cook's Distance
with(wine.red[sample(nrow(wine.red),500),],plot(tsd,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # two outliers by Cook's Distance
with(wine.red[sample(nrow(wine.red),500),],plot(sul,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # two outliers by Cook's Distance
with(wine.red[sample(nrow(wine.red),500),],plot(alc,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # okay by Cook's Distance


library(car) 
slr <- lm(qual ~ sul, data=wine.red)
summary(model1)
plot(wine.red$sul, wine.red$qual, pch=20, xlab="Sulphates", ylab="Quality")

# calculate the Cook's Distance (determine influence):
D <- cooks.distance(slr)

# flag D value that exceed 4/(n-k-1):
high_cook <- which(D > 4/(32-1-1))

# plot the influential observations (as determined by Cook's Distance)
# with red letter C's:
points(wine.red$sul[high_cook],wine.red$qual[high_cook],col="red",pch="C",cex=3)



# ----- White Wines -----

plot(wine.white$fa, wine.white$qual) # maybe
plot(wine.white$va, wine.white$qual) # okay
plot(wine.white$ca, wine.white$qual) # maybe
plot(wine.white$rs, wine.white$qual) # maybe
plot(wine.white$chl, wine.white$qual) # okay
plot(wine.white$fsd, wine.white$qual) # maybe
plot(wine.white$tsd, wine.white$qual) # maybe
plot(wine.white$den, wine.white$qual) # maybe
plot(wine.white$ph, wine.white$qual)  # okay
plot(wine.white$sul, wine.white$qual) # okay
plot(wine.white$alc, wine.white$qual) # okay



# do a scatter plot with random subsampling for white wines:
with(wine.white[sample(nrow(wine.white),500),],plot(fa,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # okay by Cook's Distance
with(wine.white[sample(nrow(wine.white),500),],plot(ca,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # okay by Cook's Distance
with(wine.white[sample(nrow(wine.white),500),],plot(rs,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # okay by Cook's Distance
with(wine.white[sample(nrow(wine.white),500),],plot(fsd,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # one outlier by Cook's Distance
with(wine.white[sample(nrow(wine.white),500),],plot(tsd,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # okay by Cook's Distance
with(wine.white[sample(nrow(wine.white),500),],plot(den,qual,pch=20,col=rgb(0,0,0,.1),cex=2)) # one outlier by Cook's distance 


slr <- lm(qual ~ fsd, data=wine.white)
summary(model1)
plot(wine.white$fsd, wine.white$qual, pch=20, xlab="Free Sulfur Dioxide", ylab="Quality")

# calculate the Cook's Distance (determine influence):
D <- cooks.distance(slr)

# flag D value that exceed 4/(n-k-1):
high_cook <- which(D > 4/(32-1-1))

# plot the influential observations (as determined by Cook's Distance)
# with red letter C's:
points(wine.white$fsd[high_cook],wine.white$qual[high_cook],col="red",pch="C",cex=3)





# ----- MLR -----

model1 <- lm(qual ~ fa + va  + ca + rs + chl + fsd + tsd + den + ph + sul + alc,data=wine.red)
summary(lm(qual ~ fa + va  + ca + rs + chl + fsd + tsd + den + ph + sul + alc,data=wine.red))
# fa, ca, rs, and den are not statistically significant 

model2 <- lm(qual ~ fa + va  + ca + rs + chl + fsd + tsd + den + ph + sul + alc,data=wine.white)
summary(lm(qual ~ fa + va  + ca + rs + chl + fsd + tsd + den + ph + sul + alc,data=wine.white))
# ca, chl, and tsd are not statistically significant 

model3 <- lm(qual ~  va + chl + fsd + tsd + ph + sul + alc,data=wine.red)
summary(lm(qual ~  va + chl + fsd + tsd + ph + sul + alc,data=wine.red))
# all variables are statistically significant 

model4 <- lm(qual ~ fa + va  + rs + fsd + den + ph + sul + alc,data=wine.white)
summary(lm(qual ~ fa + va  + rs + fsd + den + ph + sul + alc,data=wine.white))
# all variables are statistically signficant 

AIC(model1, model3)
AIC(model2, model4)

# ----- PCA -----

# ----- Red Wine -----

# original variable data matrix:
X_red <- cbind(wine.red$fa, wine.red$va, wine.red$ca, wine.red$rs, wine.red$chl, wine.red$fsd, wine.red$tsd, wine.red$den, wine.red$ph, wine.red$sul, wine.red$alc)

means <- apply(X_red, 2, mean)

# for convenience define a standardizing function:
stand <- function(x) (x - mean(x))/sd(x)

# create standardized data matrix:
X.s_red <- apply(X_red,2,stand)

S.s_red <- cov(X.s_red); S.s_red
# note that this is the same as the correlation matrix of the original variables:
S.s_red <- cor(X_red); S.s_red

# find the loadings and variances:
eigen(S.s_red)
pr.out_red <- prcomp(X_red,scale.=TRUE) 

pr.out_red$rotation

biplot(pr.out_red, scale = 0)

# draw scree plot 
plot(seq_len(ncol(X_red)),prcomp(X_red)$sdev^2,type="b",xlab="PC Number",
     ylab="Variance")

# compute variance for each principle component 
pr.var_red <- pr.out_red$sdev^2
pr.var_red

# compute the proportion of variance explained by each principal component
pve_red <- pr.var_red/sum(pr.var_red)
pve_red

# It appears that the first two PC's account for all the variance in the variables. The first PC accounts for 28 percent of the variation and the second PC accounts for 17 percent of the variation. This means that the first two PCs only account for 45.7 percent of the variation of all of the features. (Not so great)

# scree plot of cumulative PVE
plot(cumsum(pve_red), xlab="Principal Component", ylab="Cumulative PVE", ylim=c(0,1), type='b')


# ------ Principal Components Regression for Red Wines -----

x <- model.matrix(qual ~ fa + va + ca + rs + chl + fsd + tsd + den + ph + sul + alc, data=wine.red)[, -1]
y <- wine.red$qual

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

library(pls)
set.seed(2)
pcr.fit <- pcr(qual ~ fa + va + ca + rs + chl + fsd + tsd + den + ph + sul + alc, data=wine.red, scale=TRUE, validation="CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
validationplot(pcr.fit, val.type = "R2")

set.seed(1)
pcr.fit <- pcr(qual ~ fa + va + ca + rs + chl + fsd + tsd + den + ph + sul + alc, data=wine.red, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, x[test,], ncomp=3)
mean((pcr.pred-y.test)^2)

pcr.fit <- pcr(y~x, scale=TRUE, ncomp=4)
summary(pcr.fit)

# ----- White Wine -----

# original variable data matrix:
X_white <- cbind(wine.white$fa, wine.white$va, wine.white$ca, wine.white$rs, wine.white$chl, wine.white$fsd, wine.white$tsd, wine.white$den, wine.white$ph, wine.white$sul, wine.white$alc)

means <- apply(X_white, 2, mean)

# for convenience define a standardizing function:
stand <- function(x) (x - mean(x))/sd(x)

# create standardized data matrix:
X.s_white <- apply(X_white,2,stand)

S.s_white <- cov(X.s_white); S.s_white
# note that this is the same as the correlation matrix of the original variables:
S.s_white <- cor(X_white); S.s_white

# find the loadings and variances:
eigen(S.s_white)
pr.out_white <- prcomp(X_white,scale.=TRUE) 

pr.out_white$rotation

biplot(pr.out_white, scale = 0)

# draw scree plot 
plot(seq_len(ncol(X_white)),prcomp(X_white)$sdev^2,type="b",xlab="PC Number",
     ylab="Variance")

# compute variance for each principle component 
pr.var_white <- pr.out_white$sdev^2
pr.var_white

# compute the proportion of variance explained by each principal component
pve_white <- pr.var_white/sum(pr.var_white)
pve_white

# It appears the first two (maybe three) PC's capture the variance of all eleven predictors. The first PC accounts for 29 percent of the variance in the data and the second PC accounts for 14 percent of the variance in the data. This means that the first two PC account for only 43.6 percent of the variation of all of the features. (Not so great)

# scree plot of cumulative PVE
plot(cumsum(pve_white), xlab="Principal Component", ylab="Cumulative PVE", ylim=c(0,1), type='b')

# ----- PCR for White Wines -----

set.seed(2)
pcr.fit <- pcr(qual ~ fa + va + ca + rs + chl + fsd + tsd + den + ph + sul + alc, data=wine.white, scale=TRUE, validation="CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
validationplot(pcr.fit, val.type = "R2")


# ----- LINEAR SPLINES -----

# ----- Red Wine -----

# ----- Density and Quality -----

pp <- function(x) ifelse(x >= 0,x,0)
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- wine.red$den
xt1 <- x
xt2 <- pp(x-0.993)
xt3 <- pp(x-1)
lm.b <- lm(qual ~ xt1 + xt2 + xt3, data=wine.red)
summary(lm.b)

xs <- seq(0.98, 1.01, .01)

plot(wine.red$den, wine.red$qual, pch= 20, xlab="Density", ylab="Quality")

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-0.993),xt3=pp(xs-1)))
lines(xs,lm.b.pred,col="green",lwd=3)

library(gam)
my_gam <- gam(qual ~ s(den, 4), data=wine.red)
summary(my_gam)

gam.model.pred <- predict(my_gam,newdata=data.frame(den=xs))
lines(xs,gam.model.pred,col="blue",lwd=3)


ggplot(wine.red, aes(x = den, y = qual)) +
  geom_point(size = 0.5) +
  #geom_line(aes(y = lm.b.pred), size = 1, col = "purple") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Sulphates',y='Quality') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

# ----- Sulphates and Quality -----

pp <- function(x) ifelse(x >= 0,x,0)
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- wine.red$sul
xt1 <- x
xt2 <- pp(x-0.8)
xt3 <- pp(x-1.3)
lm.b <- lm(qual ~ xt1 + xt2 + xt3, data=wine.red)
summary(lm.b)

xs <- seq(0.4, 2.1, .01)

plot(wine.red$sul, wine.red$qual, pch=20, xlab="Sulphates", ylab="Quality")

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-0.8),xt3=pp(xs-1.3)))
lines(xs,lm.b.pred,col="blue",lwd=3)

my_gam <- gam(qual ~ s(sul, 4), data=wine.red)
summary(my_gam)

gam.model.pred <- predict(my_gam,newdata=data.frame(sul=xs))
lines(xs,gam.model.pred,col="green",lwd=2)


# ----- White Wine -----

# ----- Chlorides and Quality -----

pp <- function(x) ifelse(x >= 0,x,0)
pp <- function(x) pmax(x,0)

plot(wine.white$chl, wine.white$qual, xlab="Chlorides", ylab="Quality")

# fit the bent line model:
x <- wine.white$chl
xt1 <- x
xt2 <- pp(x-0.08)
xt3 <- pp(x-0.18)
lm.b <- lm(qual ~ xt1 + xt2 + xt3, data=wine.white)
summary(lm.b)

xs <- seq(0, 0.4, .01)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-0.08),xt3=pp(xs-0.18)))
lines(xs,lm.b.pred,col="blue",lwd=3)

my_gam <- gam(qual ~ s(chl, 4), data=wine.white)
summary(my_gam)

gam.model.pred <- predict(my_gam,newdata=data.frame(chl=xs))
lines(xs,gam.model.pred,col="green",lwd=2)

# ----- Density and Quality -----

pp <- function(x) ifelse(x >= 0,x,0)
pp <- function(x) pmax(x,0)

plot(wine.white$den, wine.white$qual, pch=20, xlab="Density", ylab="Quality")

# fit the bent line model:
x <- wine.white$den
xt1 <- x
xt2 <- pp(x-0.99)
xt3 <- pp(x-0.999999)
lm.b <- lm(qual ~ xt1 + xt2 + xt3, data=wine.white)
summary(lm.b)

xs <- seq(0.98, 1.05, .01)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-0.989),xt3=pp(xs-0.999999)))
lines(xs,lm.b.pred,col="blue",lwd=3)

my_gam <- gam(qual ~ s(den,4), data=wine.white)
summary(my_gam)

ys <- predict(my_gam,newdata=data.frame(den=xs))
lines(xs,ys,col="green",lwd=2)


# ----- Free Sulfur Dioxide and Quality -----

pp <- function(x) ifelse(x >= 0,x,0)
pp <- function(x) pmax(x,0)

plot(wine.white$fsd, wine.white$qual, pch=20, xlab="Free Sulfur Dioxide", ylab="Quality")

# fit the bent line model:
x <- wine.white$fsd
xt1 <- x
xt2 <- pp(x-25)
xt3 <- pp(x-90)
lm.b <- lm(qual ~ xt1 + xt2 + xt3, data=wine.white)
summary(lm.b)

xs <- seq(0, 300, .01)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-25),xt3=pp(xs-90)))
lines(xs,lm.b.pred,col="blue",lwd=3)

my_gam <- gam(qual ~ s(fsd, 3), data=wine.white)
summary(my_gam)

ys <- predict(my_gam,newdata=data.frame(fsd=xs))
lines(xs,ys,col="green",lwd=2)



# ----- GAMS -----

library(gam)

# ----- Chloride and Quality -----

plot(wine.red$chl, wine.red$qual)

my_gam <- gam(qual ~ s(chl, 2), data=wine.red)
summary(my_gam)

my_gam <- gam(qual ~ s(chl, 4), data=wine.white)
summary(my_gam)

plot(wine.white$chl, wine.white$qual)

xs <- seq(0, 0.7, .01)
ys <- predict(my_gam,newdata=data.frame(chl=xs))
lines(xs,ys,col="blue",lwd=2)

# ----- Sulphates and Quality -----

plot(wine.red$sul, wine.red$qual)

my_gam <- gam(qual ~ s(sul,4), data=wine.red)
summary(my_gam)

xs <- seq(0, 2, .01)
ys <- predict(my_gam,newdata=data.frame(sul=xs))
lines(xs,ys,col="purple",lwd=2)


plot(wine.white$sul, wine.white$qual)

my_gam <- gam(qual ~ s(sul,4), data=wine.white)
summary(my_gam)

xs <- seq(0, 2, .01)
ys <- predict(my_gam,newdata=data.frame(sul=xs))
lines(xs,ys,col="red",lwd=2)



# ----- LOGISTIC REGRESSION -----

# create a merged dataset by stacking the datasets on top of one another:
wine.all <- rbind(wine.red,wine.white)
dim(wine.all)

# add type variable:
wine.all$typ <- c(rep("red",nrow(wine.red)),rep("white",nrow(wine.white)))
wine.all$typ <- as.factor(wine.all$typ)

# remove qual from data frame
wine.all <- subset(wine.all, select = -c(qual))

# create logistic Y = "qual6":
wine.all$qual6 <- as.numeric(wine.all$qual >= 6)


# ----- ROC PLOT FUNCTION AND AUC COMPUTATION FUNCTION -----

library(DescTools)

make_roc <- function(y,ph,clr="red",new_plot=TRUE,dt=.001) {
  # threshold values at which to calculate sensitivity and false positive rate:
  taus <- seq(0,1,dt)
  # sensitivities for each tau value:
  sens <- numeric(length(taus))
  # false positive rates for each tau value:
  fpr <- numeric(length(taus))
  for (i in seq_along(taus)) {
    # proportion of y=1 observations that have predicted probs geater than tau:
    sens[i] <- mean(ph[y==1] > taus[i])
    # proportion of y=0 observations that have predicted probs geater than tau:
    fpr[i] <- mean(ph[y==0] > taus[i])
  }
  if (new_plot) {
    plot(NA,NA,type="n",xlim=c(0,1),ylim=c(0,1),
         xlab="False Positive Rate",ylab="Sensitivity")
    abline(h=c(0,1),v=c(0,1),a=0,b=1)
  }
  lines(fpr,sens,type="l",col=clr)
}



calc_auc <- function(y,ph,dt=.001) {
  # threshold values at which to calculate sensitivity and false positive rate:
  taus <- seq(0,1,dt)
  # sensitivities for each tau value:
  sens <- numeric(length(taus))
  # false positive rates for each tau value:
  fpr <- numeric(length(taus))
  auc <- 0
  height <- 0
  for (i in seq_along(taus)) {
    # proportion of y=1 observations that have predicted probs geater than tau:
    sens[i] <- mean(ph[y==1] > taus[i])
    # proportion of y=0 observations that have predicted probs geater than tau:
    fpr[i] <- mean(ph[y==0] > taus[i])
  }
  height = (sens[-1]+sens[-length(sens)])/2
  width = -diff(fpr) # = diff(rev(fpr))
  return(sum(height*width))
}

# create training and test data sets

# Set random seed to make results reproducible:
set.seed(17)

# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(wine.all)/2)

# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(wine.all), size = data_set_size)

# Assign the data to the correct sets
training <- wine.all[indexes,]
test <- wine.all[-indexes,]


# ----- LOGISTIC REGRESSION -----

set.seed(17)
# fit logistic model over training data set 
log.model <- glm(qual6 ~ fa + va + ca + rs + chl + fsd + tsd + den + ph + sul + alc + typ, data = training, binomial)
summary(log.model)

# make predictions over the test set
plog <- predict(glm(qual6 ~ fa + va + ca + rs + chl + fsd + tsd + den + ph + sul + alc + typ, data=test, binomial),type="response")

# draw ROC curve and compute AUC for logistic model 
make_roc(test$qual6,plog,"blue")
calc_auc(test$qual6,plog)
# The AUC for our model with all variables in our training set is 0.8034.


# fit logistic model over training data set after removing not statistically significant variables
log.model2 <- glm(qual6 ~ va + rs + fsd + tsd + den + ph + sul + alc + typ, data = training, binomial)
summary(log.model2)

# make predictions over the test set
plog <- predict(glm(qual6 ~ va + rs + fsd + tsd + den + ph + sul + alc + typ, data=test, binomial),type="response")

# draw ROC curve and compute AUC for logistic model 
make_roc(test$qual6,plog,"blue")
calc_auc(test$qual6,plog)
# The AUC for our model with all variables in our training set is 0.8027.



# ----- RANDOM FORESTS -----

library(MASS)
library(tree)

# fit a "regression" tree:
my_tree <- tree(qual6 ~ fa + va + ca + rs + fsd + tsd + ph + sul + alc + typ, data=training)

# print it:
my_tree

# summarize it:
summary(my_tree)

# get predictions:
yhat_tree <- predict(my_tree)

# plot it:
plot(my_tree)
text(my_tree)



library(randomForest)
# Perform training:
random_forest <- randomForest(qual6 ~ fa + va + ca + rs + fsd + tsd + ph + sul + alc + typ, data=training, ntree=300, mtry=3, importance=TRUE)

varImpPlot(random_forest)

# Validation set assessment #1: looking at confusion matrix
y_hat <- predict(random_forest,newdata=test[,-13])

# plot ROC curve and compute the AUC 
make_roc(test$qual6,y_hat,"blue")
calc_auc(test$qual6,y_hat)
# The AUC for our model when using random forests is 0.8715.


