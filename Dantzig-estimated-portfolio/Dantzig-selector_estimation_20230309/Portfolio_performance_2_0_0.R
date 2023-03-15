# Version 2.0.0

# Last verrsion 1.0.3

# In this file, 1.we vary quantile from 0.1 to 0.9 and compare them by Plug-in, Dantzig, glasso separately
#              
#               2.compare GMV, MV, EW, best Plug-in, best Dantzig, best glasso

rm(list = ls())

setwd("E:/BaiduSyncdisk/Network_structure_based_portfolio/Dantzig-selector estimation covariance/Dantzig-selector_estimation_20230309")

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
# return_oos <- returnstd[-c(1:500),]

# set label
node.label=colnames(returnstd)
# node.label<-gsub("Equity","",node.label)
# node.label<-gsub("UN","",node.label)
# node.label<-gsub("UW","",node.label)
names(returnstd) = node.label

# generate date timeline 
date<-prices$Dates
date<-as.Date.factor(date[-c(1:503)], format="%Y/%m/%d")

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



#### Annual cumulative returns ####

n=length(return_equal)
xtable(cbind(cumureturn_network_vary_with_phi[[1]][n]/n*252*100,
             cumureturn_network_vary_with_phi[[2]][n]/n*252*100,
             cumureturn_network_vary_with_phi[[3]][n]/n*252*100,
             cumureturn_network_vary_with_phi[[4]][n]/n*252*100,
             cumureturn_network_vary_with_phi[[5]][n]/n*252*100,
             cumureturn_network_vary_with_phi[[6]][n]/n*252*100,
             cumureturn_network_vary_with_phi[[7]][n]/n*252*100,
             cumureturn_network_vary_with_phi[[8]][n]/n*252*100,
             cumureturn_network_vary_with_phi[[9]][n]/n*252*100),digits = 2)

# Dantzig

xtable(cbind(cumureturn_network_vary_with_phi_Dantzig[[1]][n]/n*252*100,
             cumureturn_network_vary_with_phi_Dantzig[[2]][n]/n*252*100,
             cumureturn_network_vary_with_phi_Dantzig[[3]][n]/n*252*100,
             cumureturn_network_vary_with_phi_Dantzig[[4]][n]/n*252*100,
             cumureturn_network_vary_with_phi_Dantzig[[5]][n]/n*252*100,
             cumureturn_network_vary_with_phi_Dantzig[[6]][n]/n*252*100,
             cumureturn_network_vary_with_phi_Dantzig[[7]][n]/n*252*100,
             cumureturn_network_vary_with_phi_Dantzig[[8]][n]/n*252*100,
             cumureturn_network_vary_with_phi_Dantzig[[9]][n]/n*252*100),digits=2)

# glasso

xtable(cbind(cumureturn_network_vary_with_phi_glasso[[1]][n]/n*252*100,
             cumureturn_network_vary_with_phi_glasso[[2]][n]/n*252*100,
             cumureturn_network_vary_with_phi_glasso[[3]][n]/n*252*100,
             cumureturn_network_vary_with_phi_glasso[[4]][n]/n*252*100,
             cumureturn_network_vary_with_phi_glasso[[5]][n]/n*252*100,
             cumureturn_network_vary_with_phi_glasso[[6]][n]/n*252*100,
             cumureturn_network_vary_with_phi_glasso[[7]][n]/n*252*100,
             cumureturn_network_vary_with_phi_glasso[[8]][n]/n*252*100,
             cumureturn_network_vary_with_phi_glasso[[9]][n]/n*252*100),digits = 2)


#### std ####

xtable(cbind(std(return_network_vary_with_phi[[1]])*sqrt(252)*100,
             std(return_network_vary_with_phi[[2]])*sqrt(252)*100,
             std(return_network_vary_with_phi[[3]])*sqrt(252)*100,
             std(return_network_vary_with_phi[[4]])*sqrt(252)*100,
             std(return_network_vary_with_phi[[5]])*sqrt(252)*100,
             std(return_network_vary_with_phi[[6]])*sqrt(252)*100,
             std(return_network_vary_with_phi[[7]])*sqrt(252)*100,
             std(return_network_vary_with_phi[[8]])*sqrt(252)*100,
             std(return_network_vary_with_phi[[9]])*sqrt(252)*100),digits = 2)


# Dantzig

xtable(cbind(std(return_network_vary_with_phi_Dantzig[[1]])*sqrt(252)*100,
             std(return_network_vary_with_phi_Dantzig[[2]])*sqrt(252)*100,
             std(return_network_vary_with_phi_Dantzig[[3]])*sqrt(252)*100,
             std(return_network_vary_with_phi_Dantzig[[4]])*sqrt(252)*100,
             std(return_network_vary_with_phi_Dantzig[[5]])*sqrt(252)*100,
             std(return_network_vary_with_phi_Dantzig[[6]])*sqrt(252)*100,
             std(return_network_vary_with_phi_Dantzig[[7]])*sqrt(252)*100,
             std(return_network_vary_with_phi_Dantzig[[8]])*sqrt(252)*100,
             std(return_network_vary_with_phi_Dantzig[[9]])*sqrt(252)*100),digits = 2)

# glasso

xtable(cbind(std(return_network_vary_with_phi_glasso[[1]])*sqrt(252)*100,
             std(return_network_vary_with_phi_glasso[[2]])*sqrt(252)*100,
             std(return_network_vary_with_phi_glasso[[3]])*sqrt(252)*100,
             std(return_network_vary_with_phi_glasso[[4]])*sqrt(252)*100,
             std(return_network_vary_with_phi_glasso[[5]])*sqrt(252)*100,
             std(return_network_vary_with_phi_glasso[[6]])*sqrt(252)*100,
             std(return_network_vary_with_phi_glasso[[7]])*sqrt(252)*100,
             std(return_network_vary_with_phi_glasso[[8]])*sqrt(252)*100,
             std(return_network_vary_with_phi_glasso[[9]])*sqrt(252)*100),digits = 2)

#### sharpe ratio ####

xtable(cbind(cumureturn_network_vary_with_phi[[1]][n]/n*252/(std(return_network_vary_with_phi[[1]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi[[2]][n]/n*252/(std(return_network_vary_with_phi[[2]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi[[3]][n]/n*252/(std(return_network_vary_with_phi[[3]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi[[4]][n]/n*252/(std(return_network_vary_with_phi[[4]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi[[5]][n]/n*252/(std(return_network_vary_with_phi[[5]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi[[6]][n]/n*252/(std(return_network_vary_with_phi[[6]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi[[7]][n]/n*252/(std(return_network_vary_with_phi[[7]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi[[8]][n]/n*252/(std(return_network_vary_with_phi[[8]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi[[9]][n]/n*252/(std(return_network_vary_with_phi[[9]])*sqrt(252))*100),digits = 2)

# Dantzig

xtable(cbind(cumureturn_network_vary_with_phi_Dantzig[[1]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[1]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_Dantzig[[2]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[2]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_Dantzig[[3]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[3]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_Dantzig[[4]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[4]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_Dantzig[[5]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[5]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_Dantzig[[6]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[6]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_Dantzig[[7]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[7]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_Dantzig[[8]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[8]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_Dantzig[[9]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[9]])*sqrt(252))*100),digits = 2)

# glasso

xtable(cbind(cumureturn_network_vary_with_phi_glasso[[1]][n]/n*252/(std(return_network_vary_with_phi_glasso[[1]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_glasso[[2]][n]/n*252/(std(return_network_vary_with_phi_glasso[[2]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_glasso[[3]][n]/n*252/(std(return_network_vary_with_phi_glasso[[3]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_glasso[[4]][n]/n*252/(std(return_network_vary_with_phi_glasso[[4]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_glasso[[5]][n]/n*252/(std(return_network_vary_with_phi_glasso[[5]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_glasso[[6]][n]/n*252/(std(return_network_vary_with_phi_glasso[[6]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_glasso[[7]][n]/n*252/(std(return_network_vary_with_phi_glasso[[7]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_glasso[[8]][n]/n*252/(std(return_network_vary_with_phi_glasso[[8]])*sqrt(252))*100,
             cumureturn_network_vary_with_phi_glasso[[9]][n]/n*252/(std(return_network_vary_with_phi_glasso[[9]])*sqrt(252))*100),digits = 2)

#### skewness ####

#### kurtosis ####

#### drawdown ####

xtable(cbind(maxDrawdown(return_network_vary_with_phi[[1]]-1),
             maxDrawdown(return_network_vary_with_phi[[2]]-1),
             maxDrawdown(return_network_vary_with_phi[[3]]-1),
             maxDrawdown(return_network_vary_with_phi[[4]]-1),
             maxDrawdown(return_network_vary_with_phi[[5]]-1),
             maxDrawdown(return_network_vary_with_phi[[6]]-1),
             maxDrawdown(return_network_vary_with_phi[[7]]-1),
             maxDrawdown(return_network_vary_with_phi[[8]]-1),
             maxDrawdown(return_network_vary_with_phi[[9]]-1)),digits = 2)
# Dantzig 

xtable(cbind(maxDrawdown(return_network_vary_with_phi_Dantzig[[1]]-1),
             maxDrawdown(return_network_vary_with_phi_Dantzig[[2]]-1),
             maxDrawdown(return_network_vary_with_phi_Dantzig[[3]]-1),
             maxDrawdown(return_network_vary_with_phi_Dantzig[[4]]-1),
             maxDrawdown(return_network_vary_with_phi_Dantzig[[5]]-1),
             maxDrawdown(return_network_vary_with_phi_Dantzig[[6]]-1),
             maxDrawdown(return_network_vary_with_phi_Dantzig[[7]]-1),
             maxDrawdown(return_network_vary_with_phi_Dantzig[[8]]-1),
             maxDrawdown(return_network_vary_with_phi_Dantzig[[9]]-1)),digits = 2)

# glasso

xtable(cbind(maxDrawdown(return_network_vary_with_phi_glasso[[1]]-1),
             maxDrawdown(return_network_vary_with_phi_glasso[[2]]-1),
             maxDrawdown(return_network_vary_with_phi_glasso[[3]]-1),
             maxDrawdown(return_network_vary_with_phi_glasso[[4]]-1),
             maxDrawdown(return_network_vary_with_phi_glasso[[5]]-1),
             maxDrawdown(return_network_vary_with_phi_glasso[[6]]-1),
             maxDrawdown(return_network_vary_with_phi_glasso[[7]]-1),
             maxDrawdown(return_network_vary_with_phi_glasso[[8]]-1),
             maxDrawdown(return_network_vary_with_phi_glasso[[9]]-1)),digits = 2)

#### autocorrelation ####

#### weights ####

#### turnover ####

xtable(cbind(p_turnover(w_network_vary_with_phi[[1]]),
             p_turnover(w_network_vary_with_phi[[2]]),
             p_turnover(w_network_vary_with_phi[[3]]),
             p_turnover(w_network_vary_with_phi[[4]]),
             p_turnover(w_network_vary_with_phi[[5]]),
             p_turnover(w_network_vary_with_phi[[6]]),
             p_turnover(w_network_vary_with_phi[[7]]),
             p_turnover(w_network_vary_with_phi[[8]]),
             p_turnover(w_network_vary_with_phi[[9]])),digits = 2)

# Dantzig

xtable(cbind(p_turnover(w_network_vary_with_phi_Dantzig[[1]]),
             p_turnover(w_network_vary_with_phi_Dantzig[[2]]),
             p_turnover(w_network_vary_with_phi_Dantzig[[3]]),
             p_turnover(w_network_vary_with_phi_Dantzig[[4]]),
             p_turnover(w_network_vary_with_phi_Dantzig[[5]]),
             p_turnover(w_network_vary_with_phi_Dantzig[[6]]),
             p_turnover(w_network_vary_with_phi_Dantzig[[7]]),
             p_turnover(w_network_vary_with_phi_Dantzig[[8]]),
             p_turnover(w_network_vary_with_phi_Dantzig[[9]])),digits = 2)

# glasso

xtable(cbind(p_turnover(w_network_vary_with_phi_glasso[[1]]),
             p_turnover(w_network_vary_with_phi_glasso[[2]]),
             p_turnover(w_network_vary_with_phi_glasso[[3]]),
             p_turnover(w_network_vary_with_phi_glasso[[4]]),
             p_turnover(w_network_vary_with_phi_glasso[[5]]),
             p_turnover(w_network_vary_with_phi_glasso[[6]]),
             p_turnover(w_network_vary_with_phi_glasso[[7]]),
             p_turnover(w_network_vary_with_phi_glasso[[8]]),
             p_turnover(w_network_vary_with_phi_glasso[[9]])),digits = 2)



xtable(rbind(cbind(cumureturn_minVar[n]/n*252*100,
                   cumureturn_meanVar[n]/n*252*100,
                   cumureturn_equal[n]/n*252*100,
                   cumureturn_network_vary_with_phi[[9]][n]/n*252*100,
                   cumureturn_network_vary_with_phi_Dantzig[[1]][n]/n*252*100,
                   cumureturn_network_vary_with_phi_glasso[[1]][n]/n*252*100),
             cbind(std(return_minVar)*sqrt(252)*100,
                   std(return_meanVar)*sqrt(252)*100,
                   std(return_equal)*sqrt(252)*100,
                   std(return_network_vary_with_phi[[9]])*sqrt(252)*100,
                   std(return_network_vary_with_phi_Dantzig[[1]])*sqrt(252)*100,
                   std(return_network_vary_with_phi_glasso[[1]])*sqrt(252)*100),
             cbind(cumureturn_minVar[n]/n*252/(std(return_minVar)*sqrt(252))*100,
                   cumureturn_meanVar[n]/n*252/(std(return_meanVar)*sqrt(252))*100,
                   cumureturn_equal[n]/n*252/(std(return_equal)*sqrt(252))*100,
                   cumureturn_network_vary_with_phi[[9]][n]/n*252/(std(return_network_vary_with_phi[[9]])*sqrt(252))*100,
                   cumureturn_network_vary_with_phi_Dantzig[[1]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[1]])*sqrt(252))*100,
                   cumureturn_network_vary_with_phi_glasso[[1]][n]/n*252/(std(return_network_vary_with_phi_glasso[[1]])*sqrt(252))*100),
             cbind(maxDrawdown(return_minVar-1),
                   maxDrawdown(return_meanVar-1),
                   maxDrawdown(return_equal-1),
                   maxDrawdown(return_network_vary_with_phi[[9]]-1),
                   maxDrawdown(return_network_vary_with_phi_Dantzig[[1]]-1),
                   maxDrawdown(return_network_vary_with_phi_glasso[[1]]-1)),
             cbind(p_turnover(w_minVar),
                   p_turnover(w_meanVar),
                   p_turnover(w_equal),
                   p_turnover(w_network_vary_with_phi[[9]]),
                   p_turnover(w_network_vary_with_phi_Dantzig[[1]]),
                   p_turnover(w_network_vary_with_phi_glasso[[1]]))
             ),digits = 2)
