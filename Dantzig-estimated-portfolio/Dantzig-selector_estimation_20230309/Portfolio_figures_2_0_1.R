# Version 2.0.1

# Last verrsion 2.0.0

# In this file, we 

# 1.draw boxplot of different weights
#

rm(list = ls())

setwd("E:/BaiduSyncdisk/Network_structure_based_portfolio/Dantzig-selector estimation covariance/Dantzig-selector_estimation_20230309")

dir.create("weight_boxplot", showWarnings = FALSE)
dir.create("nega_weight", showWarnings = FALSE)
dir.create("magnitude_nega_weight", showWarnings = FALSE)

# Load Functions and other Files
source('./PackagesNetworkPortfolio.R')
source('./FunctionsNetworkPortfolio.R')

# load raw data
prices<-read.csv("SP500 securities_up_20230308.csv")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y/%m/%d'))

#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
p=dim(return)[2]

# # out-of-sample return
# return_oos <- returnstd[-c(1:493),]

# set label
node.label=colnames(returnstd)
# node.label<-gsub("Equity","",node.label)
# node.label<-gsub("UN","",node.label)
# node.label<-gsub("UW","",node.label)
names(returnstd) = node.label

# generate date timeline 
date<-prices$Dates
date<-as.Date.factor(date[-c(1:503)], format="%Y/%m/%d")
date_weekly<-as.Date.factor(date[(0:187)*7+1], format="%Y-%m-%d")

# quantile
quantl<-seq(0.1,0.9,0.1)

#### Portfolio performance ####
##### Load portfolios #####
load("Portfolios_20230309.RData")
return_minVar <- Portfolio.Scenario$return_minVar
return_minVar_noshort <- Portfolio.Scenario$return_minVar_noshort
return_minVar_Dantzig <- Portfolio.Scenario$return_minVar_Dantzig
return_minVar_glasso <- Portfolio.Scenario$return_minVar_glasso
return_meanVar <- Portfolio.Scenario$return_meanVar
return_meanVar_noshort <- Portfolio.Scenario$return_meanVar_noshort
return_meanVar_Dantzig <- Portfolio.Scenario$return_meanVar_Dantzig
return_meanVar_glasso <- Portfolio.Scenario$return_meanVar_glasso
return_equal <- Portfolio.Scenario$return_equal
return_network_1constraint <- Portfolio.Scenario$return_network_1constraint
return_network_1constraint_Dantzig <- Portfolio.Scenario$return_network_1constraint_Dantzig
return_network_1constraint_glasso <- Portfolio.Scenario$return_network_1constraint_glasso
return_network_1constraint_noshort <- Portfolio.Scenario$return_network_1constraint_noshort
return_network_vary_with_phi <- Portfolio.Scenario$return_network_vary_with_phi
return_network_vary_with_phi_Dantzig <- Portfolio.Scenario$return_network_vary_with_phi_Dantzig
return_network_vary_with_phi_glasso <- Portfolio.Scenario$return_network_vary_with_phi_glasso
return_network_vary_with_phi_noshort <- Portfolio.Scenario$return_network_vary_with_phi_noshort
return_network_datadriven_phistar <- Portfolio.Scenario$return_network_datadriven_phistar
return_network_datadriven_phistar_Dantzig <- Portfolio.Scenario$return_network_datadriven_phistar_Dantzig
return_network_datadriven_phistar_glasso <- Portfolio.Scenario$return_network_datadriven_phistar_glasso
return_network_datadriven_phistar_noshort <- Portfolio.Scenario$return_network_datadriven_phistar_noshort
return_network_2constraint <- Portfolio.Scenario$return_network_2constraint
return_network_2constraint_Dantzig <- Portfolio.Scenario$return_network_2constraint_Dantzig
return_network_2constraint_glasso <- Portfolio.Scenario$return_network_2constraint_glasso
return_network_2constraint_noshort <- Portfolio.Scenario$return_network_2constraint_noshort
return_network_vary_with_phi_2constraint <- Portfolio.Scenario$return_network_vary_with_phi_2constraint
return_network_vary_with_phi_2constraint_Dantzig <- Portfolio.Scenario$return_network_vary_with_phi_2constraint_Dantzig
return_network_vary_with_phi_2constraint_glasso <- Portfolio.Scenario$return_network_vary_with_phi_2constraint_glasso
return_network_vary_with_phi_2constraint_noshort <- Portfolio.Scenario$return_network_vary_with_phi_2constraint_noshort
return_network_datadriven_phistar_2constraint <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint
return_network_datadriven_phistar_2constraint_Dantzig <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint_Dantzig
return_network_datadriven_phistar_2constraint_glasso <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint_glasso
return_network_datadriven_phistar_2constraint_noshort <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint_noshort


cumureturn_minVar <- Portfolio.Scenario$cumureturn_minVar
cumureturn_minVar_noshort <- Portfolio.Scenario$cumureturn_minVar_noshort
cumureturn_minVar_Dantzig <- Portfolio.Scenario$cumureturn_minVar_Dantzig
cumureturn_minVar_glasso <- Portfolio.Scenario$cumureturn_minVar_glasso
cumureturn_meanVar <- Portfolio.Scenario$cumureturn_meanVar
cumureturn_meanVar_noshort <- Portfolio.Scenario$cumureturn_meanVar_noshort
cumureturn_meanVar_Dantzig <- Portfolio.Scenario$cumureturn_meanVar_Dantzig
cumureturn_meanVar_glasso <- Portfolio.Scenario$cumureturn_meanVar_glasso
cumureturn_equal <- Portfolio.Scenario$cumureturn_equal
cumureturn_network_1constraint <- Portfolio.Scenario$cumureturn_network_1constraint
cumureturn_network_1constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_1constraint_Dantzig
cumureturn_network_1constraint_glasso <- Portfolio.Scenario$cumureturn_network_1constraint_glasso
cumureturn_network_1constraint_noshort <- Portfolio.Scenario$cumureturn_network_1constraint_noshort
cumureturn_network_vary_with_phi <- Portfolio.Scenario$cumureturn_network_vary_with_phi
cumureturn_network_vary_with_phi_Dantzig <- Portfolio.Scenario$cumureturn_network_vary_with_phi_Dantzig
cumureturn_network_vary_with_phi_glasso <- Portfolio.Scenario$cumureturn_network_vary_with_phi_glasso
cumureturn_network_vary_with_phi_noshort <- Portfolio.Scenario$cumureturn_network_vary_with_phi_noshort
cumureturn_network_datadriven_phistar <- Portfolio.Scenario$cumureturn_network_datadriven_phistar
cumureturn_network_datadriven_phistar_Dantzig <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_Dantzig
cumureturn_network_datadriven_phistar_glasso <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_glasso
cumureturn_network_datadriven_phistar_noshort <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_noshort
cumureturn_network_2constraint <- Portfolio.Scenario$cumureturn_network_2constraint
cumureturn_network_2constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_2constraint_Dantzig
cumureturn_network_2constraint_glasso <- Portfolio.Scenario$cumureturn_network_2constraint_glasso
cumureturn_network_2constraint_noshort <- Portfolio.Scenario$cumureturn_network_2constraint_noshort
cumureturn_network_vary_with_phi_2constraint <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint
cumureturn_network_vary_with_phi_2constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint_Dantzig
cumureturn_network_vary_with_phi_2constraint_glasso <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint_glasso
cumureturn_network_vary_with_phi_2constraint_noshort <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint_noshort
cumureturn_network_datadriven_phistar_2constraint <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint
cumureturn_network_datadriven_phistar_2constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint_Dantzig
cumureturn_network_datadriven_phistar_2constraint_glasso <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint_glasso
cumureturn_network_datadriven_phistar_2constraint_noshort <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint_noshort


w_minVar <- Portfolio.Scenario$w_minVar
w_minVar_noshort <- Portfolio.Scenario$w_minVar_noshort
w_minVar_Dantzig <- Portfolio.Scenario$w_minVar_Dantzig
w_minVar_glasso <- Portfolio.Scenario$w_minVar_glasso
w_meanVar <- Portfolio.Scenario$w_meanVar
w_meanVar_noshort <- Portfolio.Scenario$w_meanVar_noshort
w_meanVar_Dantzig <- Portfolio.Scenario$w_meanVar_Dantzig
w_meanVar_glasso <- Portfolio.Scenario$w_meanVar_glasso
w_equal <- Portfolio.Scenario$w_equal
w_network_1constraint <- Portfolio.Scenario$w_network_1constraint
w_network_1constraint_Dantzig <- Portfolio.Scenario$w_network_1constraint_Dantzig
w_network_1constraint_noshort <- Portfolio.Scenario$w_network_1constraint_noshort
w_network_1constraint_glasso <- Portfolio.Scenario$w_network_1constraint_glasso
w_network_vary_with_phi <- Portfolio.Scenario$w_network_vary_with_phi
w_network_vary_with_phi_Dantzig <- Portfolio.Scenario$w_network_vary_with_phi_Dantzig
w_network_vary_with_phi_glasso <- Portfolio.Scenario$w_network_vary_with_phi_glasso
w_network_vary_with_phi_noshort <- Portfolio.Scenario$w_network_vary_with_phi_noshort
w_network_datadriven_phistar <- Portfolio.Scenario$w_network_datadriven_phistar
w_network_datadriven_phistar_Dantzig <- Portfolio.Scenario$w_network_datadriven_phistar_Dantzig
w_network_datadriven_phistar_glasso <- Portfolio.Scenario$w_network_datadriven_phistar_glasso
w_network_datadriven_phistar_noshort <- Portfolio.Scenario$w_network_datadriven_phistar_noshort
w_network_2constraint <- Portfolio.Scenario$w_network_2constraint
w_network_2constraint_Dantzig <- Portfolio.Scenario$w_network_2constraint_Dantzig
w_network_2constraint_glasso <- Portfolio.Scenario$w_network_2constraint_glasso
w_network_2constraint_noshort <- Portfolio.Scenario$w_network_2constraint_noshort
w_network_vary_with_phi_2constraint <- Portfolio.Scenario$w_network_vary_with_phi_2constraint
w_network_vary_with_phi_2constraint_Dantzig <- Portfolio.Scenario$w_network_vary_with_phi_2constraint_Dantzig
w_network_vary_with_phi_2constraint_glasso <- Portfolio.Scenario$w_network_vary_with_phi_2constraint_glasso
w_network_vary_with_phi_2constraint_noshort <- Portfolio.Scenario$w_network_vary_with_phi_2constraint_noshort
w_network_datadriven_phistar_2constraint <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint
w_network_datadriven_phistar_2constraint_Dantzig <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint_Dantzig
w_network_datadriven_phistar_2constraint_glasso <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint_glasso
w_network_datadriven_phistar_2constraint_noshort <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint_noshort


n=length(return_equal)
#### Figures output ####
##### P_phi using Plug-in #####
### phi* = 10% - 90% quantile ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_network_vary_with_phi[[1]]),
                  data.frame(cumureturn_network_vary_with_phi[[2]]),
                  data.frame(cumureturn_network_vary_with_phi[[3]]),
                  data.frame(cumureturn_network_vary_with_phi[[4]]),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi[[6]]),
                  data.frame(cumureturn_network_vary_with_phi[[7]]),
                  data.frame(cumureturn_network_vary_with_phi[[8]]),
                  data.frame(cumureturn_network_vary_with_phi[[9]])
),
order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_P_phi_PlugIn",".png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[1:12], ylab="", xlab="")
legend("topleft",legend=c(paste0("P_phi ",quantl[1]*100,"% quantile Plug-in"), 
                          paste0("P_phi ",quantl[2]*100,"% quantile Plug-in"), 
                          paste0("P_phi ",quantl[3]*100,"% quantile Plug-in"), 
                          paste0("P_phi ",quantl[4]*100,"% quantile Plug-in"), 
                          paste0("P_phi ",quantl[5]*100,"% quantile Plug-in"), 
                          paste0("P_phi ",quantl[6]*100,"% quantile Plug-in"), 
                          paste0("P_phi ",quantl[7]*100,"% quantile Plug-in"), 
                          paste0("P_phi ",quantl[8]*100,"% quantile Plug-in"), 
                          paste0("P_phi ",quantl[9]*100,"% quantile Plug-in") ), col=Colors_Ret[1:12], lty=1, cex=0.8)
dev.off()

##### P_phi using Dantzig #####
### phi* = 10% - 90% quantile ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_network_vary_with_phi_Dantzig[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[2]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[4]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[6]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[7]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[8]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[9]])
),
order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_P_phi_Dantzig",".png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[1:12], ylab="", xlab="")
legend("topleft",legend=c(paste0("P_phi ",quantl[1]*100,"% quantile Dantzig"), 
                          paste0("P_phi ",quantl[2]*100,"% quantile Dantzig"), 
                          paste0("P_phi ",quantl[3]*100,"% quantile Dantzig"), 
                          paste0("P_phi ",quantl[4]*100,"% quantile Dantzig"), 
                          paste0("P_phi ",quantl[5]*100,"% quantile Dantzig"), 
                          paste0("P_phi ",quantl[6]*100,"% quantile Dantzig"), 
                          paste0("P_phi ",quantl[7]*100,"% quantile Dantzig"), 
                          paste0("P_phi ",quantl[8]*100,"% quantile Dantzig"), 
                          paste0("P_phi ",quantl[9]*100,"% quantile Dantzig") ), 
       col=Colors_Ret[1:12], lty=1, cex=0.8)
dev.off()

##### P_phi using glasso #####
### phi* = 10% - 90% quantile ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_network_vary_with_phi_glasso[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_glasso[[2]]),
                  data.frame(cumureturn_network_vary_with_phi_glasso[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_glasso[[4]]),
                  data.frame(cumureturn_network_vary_with_phi_glasso[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_glasso[[6]]),
                  data.frame(cumureturn_network_vary_with_phi_glasso[[7]]),
                  data.frame(cumureturn_network_vary_with_phi_glasso[[8]]),
                  data.frame(cumureturn_network_vary_with_phi_glasso[[9]])
),
order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_P_phi_glasso",".png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[1:12], ylab="", xlab="")
legend("topleft",legend=c(paste0("P_phi ",quantl[1]*100,"% quantile glasso"), 
                          paste0("P_phi ",quantl[2]*100,"% quantile glasso"), 
                          paste0("P_phi ",quantl[3]*100,"% quantile glasso"), 
                          paste0("P_phi ",quantl[4]*100,"% quantile glasso"), 
                          paste0("P_phi ",quantl[5]*100,"% quantile glasso"), 
                          paste0("P_phi ",quantl[6]*100,"% quantile glasso"), 
                          paste0("P_phi ",quantl[7]*100,"% quantile glasso"), 
                          paste0("P_phi ",quantl[8]*100,"% quantile glasso"), 
                          paste0("P_phi ",quantl[9]*100,"% quantile glasso") ), 
       col=Colors_Ret[1:12], lty=1, cex=0.8)
dev.off()

##### GMV, MV, EW, best Plug-In, best Dantzig, best glasso #####
### best Plug-In : 60% quantile ###
### best Dantzig : 30% quantile ###
### best glasso : 30% quantile ###
Colors_Ret<-rainbow(6)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_meanVar),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi[[9]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_glasso[[1]]) ),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/GMV_MV_EW_PlugIn_Dantzig_glasso",".png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret, ylab="", xlab="")
legend("topleft",legend=c("GMV", 
                          "MV", 
                          "EW", 
                          "Plug-In", "Dantzig", "glasso"),
       col=Colors_Ret, lty=1, cex=0.8)
dev.off()

##### Boxplot
for (i in 1:9) {
  
  pngname<-paste0(getwd(),"/weight_boxplot/PlugIn_1_constraint_network_",i,"_.png")
  png(file = pngname, width=1000, height=800, bg = "transparent")
  boxplot(w_network_vary_with_phi[[i]])
  dev.off()
}

for (i in 1:9) {
  
  pngname<-paste0(getwd(),"/weight_boxplot/Dantzig_1_constraint_network_",i,"_.png")
  png(file = pngname, width=1000, height=800, bg = "transparent")
  boxplot(w_network_vary_with_phi_Dantzig[[i]])
  dev.off()
}

for (i in 1:9) {
  
  pngname<-paste0(getwd(),"/weight_boxplot/GLASSO_1_constraint_network_",i,"_.png")
  png(file = pngname, width=1000, height=800, bg = "transparent")
  boxplot(w_network_vary_with_phi_glasso[[i]])
  dev.off()
}


pngname<-paste0(getwd(),"/weight_boxplot/Dantzig_1_constraint_network_",1,"_lastday_.png")
png(file = pngname, width=1000, height=800, bg = "transparent")
boxplot(w_network_vary_with_phi_Dantzig[[1]][167,])
dev.off()

pngname<-paste0(getwd(),"/weight_boxplot/GLASSO_1_constraint_network_",1,"_lastday_.png")
png(file = pngname, width=1000, height=800, bg = "transparent")
boxplot(w_network_vary_with_phi_glasso[[1]][167,])
dev.off()

pngname<-paste0(getwd(),"/weight_boxplot/PlugIn_1_constraint_network_",1,"_lastday_.png")
png(file = pngname, width=1000, height=800, bg = "transparent")
boxplot(w_network_vary_with_phi[[1]][167,])
dev.off()

# Negative weights
nega.weights.plugin=c()
for (t in 1:187) {
  nega.weights.plugin[t]=sum(w_network_vary_with_phi[[1]][t,]<0)
}
negative_weights<-zoo(data.frame(nega.weights.plugin),
                      order.by=as.Date(as.character(date_weekly), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/nega_weight/PlugIn_1_constraint_network_",1,"_lastday_.png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(negative_weights)
dev.off()

nega.weights.Dantzig=c()
for (t in 1:187) {
  nega.weights.Dantzig[t]=sum(w_network_vary_with_phi_Dantzig[[1]][t,]<0)
}
negative_weights<-zoo(data.frame(nega.weights.Dantzig),
                      order.by=as.Date(as.character(date_weekly), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/nega_weight/Dantzig_1_constraint_network_",1,"_lastday_.png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(negative_weights)
dev.off()

nega.weights.glasso=c()
for (t in 1:187) {
  nega.weights.glasso[t]=sum(w_network_vary_with_phi_glasso[[1]][t,]<0)
}
negative_weights<-zoo(data.frame(nega.weights.glasso),
                      order.by=as.Date(as.character(date_weekly), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/nega_weight/glasso_1_constraint_network_",1,"_lastday_.png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(negative_weights)
dev.off()

# magnitude of negative weights
magntd.nega.weights=c()
for (t in 1:187) {
  weights=w_network_vary_with_phi[[1]][t,]
  magntd.nega.weights[t]=sum(abs(weights[which(weights<0)]))/sum(abs(weights))
}
magnitude_negative_weights<-zoo(data.frame(magntd.nega.weights),
                                order.by=as.Date(as.character(date_weekly), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/magnitude_nega_weight/PlugIn_1_constraint_network_",1,"_lastday_.png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(magnitude_negative_weights)
dev.off()

magntd.nega.weights=c()
for (t in 1:187) {
  weights=w_network_vary_with_phi_Dantzig[[1]][t,]
  magntd.nega.weights[t]=sum(abs(weights[which(weights<0)]))/sum(abs(weights))
}
magnitude_negative_weights<-zoo(data.frame(magntd.nega.weights),
                                order.by=as.Date(as.character(date_weekly), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/magnitude_nega_weight/Dantzig_1_constraint_network_",1,"_lastday_.png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(magnitude_negative_weights)
dev.off()


magntd.nega.weights=c()
for (t in 1:187) {
  weights=w_network_vary_with_phi_glasso[[1]][t,]
  magntd.nega.weights[t]=sum(abs(weights[which(weights<0)]))/sum(abs(weights))
}
magnitude_negative_weights<-zoo(data.frame(magntd.nega.weights),
                                order.by=as.Date(as.character(date_weekly), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/magnitude_nega_weight/glasso_1_constraint_network_",1,"_lastday_.png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(magnitude_negative_weights)
dev.off()

