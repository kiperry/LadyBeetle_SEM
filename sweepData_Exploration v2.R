#data exploration. Copied from "data_exploration_5.25.17.R" from bumblebee HM landscape paper

library(car)
library(plyr)
library(usdm) #for vif
library(lme4)
library(AED) #this must be downloaded via github using the following code:
#install.packages("remotes")
#remotes::install_github("romunov/AED")

#import data set
df <- read_csv("Sweeps.2010v2.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y")))
df$Site <- as.factor(df$Site)
df$County <- as.factor(df$County)
df$Plot <- as.factor(df$Plot)
df$Crop <- as.factor(df$Crop)
df$Experiment <- as.factor(df$Experiment)
#complete cases
df2 <- df[complete.cases(df),]
#Outliers 
dotchart(df2$Exotic)
dotchart(df2$Native)
dotchart(df2$Aphids) #transform due to outliers
dotchart(df2$Other.aphid.predators) #transform due to outliers

#####don't do for now because log transforming isn't possible on zeros ------------------
# df2$L.Aphids <- log10(df2$Aphids)
# dotchart(df2$L.Aphids)
# 
# df2$L.preds <- log10(df2$Other.aphid.predators)
# dotchart(df2$L.preds)
#----------------------------------------------------------

#Collinearity: assessed in three ways 
#(1) Pairwise scatterplots

#Some functions needed for the scatterplots that need to be run before creating the scatter plot

##################################################################
panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}
##################################################################

Mypairs <- function(Z) {
  MyVarx <- colnames(Z)
  pairs(Z, labels = MyVarx,
        cex.labels =  2,
        lower.panel = function(x, y, digits=2, prefix="", cex.cor = 7) {
          panel.cor(x, y, digits, prefix, cex.cor)}, 
        upper.panel =  function(x, y) points(x, y, 
                                             pch = 16, cex = 0.8, 
                                             col = gray(0.1)))
  #print(P)
}

#change this to the appropriate data set (potentially with data transformations)
#Z <- cbind(df2$Crop, df2$Exotic, df2$Native, df2$L.Aphids, df2$L.preds) #does not work because of log
#colnames(Z) <- c("Crop","Exotic","Native","Log Aphid","Log Aphid Preds")

Z <- data.frame(df2$Crop, df2$Exotic, df2$Native, df2$Aphids, df2$Other.aphid.predators)
colnames(Z) <- c("Crop","Exotic","Native","Aphid","Aphid Preds")

# pairDF <- data.frame(Cu = log10(HMdata$lCu), Fe = log10(HMdata$lFe), Zn = HMdata$lZn, Ba = HMdata$lBa, Cd = log10(HMdata$lCd+.01), Co = log10(HMdata$lCo+.01), Cr = log10(HMdata$lCr+.01), Li = log10(HMdata$lLi), Ni = log10(HMdata$lNi), Pb = log10(HMdata$lPb+.01), Sb = log10(HMdata$lSb+.01), Si = log10(HMdata$lSi), Sr = log10(HMdata$lSr), Month = as.factor(HMdata$Month), Num.larvae = HMdata$Num.larvae)


Mypairs(Z)

#(2) Correlation coefficients and (3) Variance inflation factors (VIF)

#just the explanatory variables
edf <- Z[,colnames(Z)!="Native"]
edf2 <- edf[,colnames(edf)!="Crop"]


vif(edf2) # uses package usdm

#actual models
mod1 <- glmer(Native~Exotic + Crop + Aphids + Other.aphid.predators + County + (1|Site), family = poisson, data = df2)

#model doesnt converge, so I tried a bunch of things from here: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
#rescale and center continuous parameters
df2scale <- df2
df2Names <- c("Aphids","Other.aphid.predators","Exotic")
df2scale[,df2Names] <- scale(df2scale[,df2Names])

mod1_sc <- update(mod1,data=df2scale)

ss <- getME(mod1_sc,c("theta","fixef"))
m2 <- update(mod1_sc,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

m3 <- update(mod1_sc,start=ss,control=glmerControl(optimizer="nloptwrap",
                                                 optCtrl=list(maxfun=2e5)))

#different optimizer optiions: "bobyqa","Nelder_Mead","nlminbwrap","nmkbw","optimx","nloptwrap" 


mod2 <- glmer.nb(Native~Exotic + Crop + Aphids + Other.aphid.predators + County + (1|Site), data = df2scale)
ss <- getME(mod2,c("theta","fixef"))
m2.2 <- update(mod2,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))



