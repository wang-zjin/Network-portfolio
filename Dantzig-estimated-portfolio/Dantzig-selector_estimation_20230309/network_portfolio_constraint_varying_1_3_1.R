# network_portfolio version 1.3.2

# Comparing version 1.3.1
#
# In this file,  we data "SP500 securities_up_20230308.csv" from 2016-01-04 to 2022-12-30

rm(list = ls())

setwd("E:/BaiduSyncdisk/Network_structure_based_portfolio/Dantzig-selector estimation covariance/Dantzig-selector_estimation_20230309")

# Load Functions and other Files
source('./PackagesNetworkPortfolio.R')
source('./FunctionsNetworkPortfolio.R')

# load data
prices<-read.csv("SP500 securities_up_20230308.csv")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y/%m/%d'))

#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
p=dim(return)[2]

# set label
node.label=colnames(returnstd)
# node.label<-gsub("Equity","",node.label)
# node.label<-gsub("UN","",node.label)
# node.label<-gsub("UW","",node.label)
# node.label<-gsub("UQ","",node.label)
names(returnstd) = node.label

# rolling window
W<-list()
for(t in 0: 187){
  W[[(t+1)]]=returnstd[(1+t*7):(507+t*7),]
}
W_in<-list()
W_out<-list()
for(t in 0: 187){
  W_in[[(t+1)]]=W[[t+1]][c(1:500),]
  W_out[[(t+1)]]=W[[t+1]][c(501:507),]
}
T.windows<-length(W)
# # correlation matrix, Expected return, covariance matrix
# C_in <- list()
# ER_in <- list()
# COV_in <- list()
# EC_in <- list()
# # EC.posi_in <- list()
# # EC.nega_in <- list()
# # BetweennessCentrality<-list()
# # ClosenessCentrality<-list()
# # DegreeCentrality<-list()
# for(t in 1: length(W_in)){
#   C_in[[(t)]] =cor(W_in[[(t)]])
#   ER_in[[(t)]] = colMeans(W_in[[(t)]])
#   COV_in[[(t)]] = cov(W_in[[(t)]])
#   # network_port = network.portfolio(W_in[[(t)]])
#   network_port = network.correlation(W_in[[(t)]])
#   # EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
#   # EC_in[[t]]<-eigen(C_in[[(t)]])$vector[,1]
#   # network_port<-network.abs(W_in[[(t)]])
#   EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
#   max(eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector)
#   min(eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector)
#   boxplot(eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector)
#   # max(eigen_centrality(network_port,directed = FALSE, scale = TRUE)$value)
#   # eigen_centrality(network_port,directed = FALSE, scale = TRUE)$value
#   # BetweennessCentrality[[(t)]] <- betweenness(network_port)
#   # ClosenessCentrality[[(t)]] <- closeness(network_port)
#   # DegreeCentrality[[(t)]] <- DegreeCentrality(network_port)
#   # network_in<-network.posi.naga(W_in[[(t)]])
#   # EC.posi_in[[(t)]] <- eigen_centrality(network_in$positive,directed = FALSE, scale = TRUE)$vector
#   # EC.nega_in[[(t)]] <- eigen_centrality(network_in$negative,directed = FALSE, scale = TRUE)$vector
# }

##### Dantzig selector estimation for eigenvector centrality #####


###### CV for tuning lambda in estimation eigenvector centrality using the first 500 data ######
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)
# block.start=1+(0:(n.block-1))*B
# lmd.i=c()
# for (valid.block in 1:n.block) {
#   valid.ind=NULL
#   for(k in valid.block){
#     valid.ind=c(valid.ind,block.start[k]:(min(block.start[k]+B-1,n)))
#   }
#   n.valid=length(valid.ind)
#   train.ind=setdiff(1:n,valid.ind)
#   n.train=length(train.ind)
#   returnstd.train=returnstd[1:500,][train.ind,]
#   returnstd.valid=returnstd[1:500,][valid.ind,]
#   mu.train=rep(0,p)
#   mu.valid=rep(0,p)
#   A.train=cor(returnstd.train)-diag(1,p,p)
#   A.valid=cor(returnstd.valid)-diag(1,p,p)
#   cov.train=A.train-diag(max(eigen(A.train)$value),p,p)
#   cov.valid=A.valid-diag(max(eigen(A.valid)$value),p,p)
#   lambda.grid=seq(0,1,length=101)[2:101]
#   l.lambda=length(lambda.grid)
#   cv.l.error=NULL
#   cv.l.lmd=NULL
#   for(i in 1:l.lambda){
#     lmd=lambda.grid[i]
#     print(i)
#     # lmd=0.2
#     lin.train=linfun3(cov.train,mu.train,lmd,abs(eigen(cov.valid)$vector[1,1]))
#     # max(lin.train)
#     # max(cov.train%*%lin.train)
#     if(!(all(lin.train==0))){
#       error=sum((cov.valid%*%lin.train-mu.valid)^2)
#       cv.l.error=c(cv.l.error,error)
#       cv.l.lmd=c(cv.l.lmd, lmd)
#     }
#   }
#   lmd.i[valid.block]=min(cv.l.lmd[which(cv.l.error==min(cv.l.error))])
# }
# lmd=mean(lmd.i)
lmd.EC.Dantzig <- 0.682

#   #   EC_DS <- eigenvector centrality estimated by Dantzig selector
# EC_DS<-list()
# a=c()
# for (t in 1: length(W_in)) {
#   print(t)
#   EC_DS[[t]] =linfun3(C_in[[t]]-diag(1,p,p)-diag(max(eigen(C_in[[(t)]])$value),p,p),rep(0,p),lambda=lmd.EC.Dantzig,abs(eigen(C_in[[t]]-diag(1,p,p)-diag(max(eigen(C_in[[(t)]])$value),p,p))$vector[1,1]))
#   EC_DS[[t]]=EC_DS[[t]]/max(EC_DS[[t]])
#   # boxplot(EC_DS[[t]])
#   a[t]=sum(EC_DS[[t]]==0)
# }
# a
# 
# eigenvector_centrality = list("eigenvector_absolute_value"=EC_in,"eigenvector_centrality_Dantzig"=EC_DS,
#                               "zeors_in_EC_DS"=a,"covariance_matrix"=COV_in,"correlation_matrix"=C_in,
#                               "expected_return"=ER_in)
# save(eigenvector_centrality,file = "eigenvector_centrality_20230308.RData")

load("eigenvector_centrality_20230308.RData")
EC_in=eigenvector_centrality$eigenvector_absolute_value
EC_DS=eigenvector_centrality$eigenvector_centrality_Dantzig
ER_in=eigenvector_centrality$expected_return
C_in=eigenvector_centrality$correlation_matrix
COV_in=eigenvector_centrality$covariance_matrix

# # generate timeline "cri" for later portfolio
# date<-prices$Dates
# date<-as.Date.factor(date[-c(1:501)], format="%Y-%m-%d")
# meanlayer1<-list()
# for(t in 1: 167){
#   meanlayer1[[t]]<-date[[t*7]]
# }
# meanlayer1<-as.Date(unlist(meanlayer1))
# psi<-data.frame(meanlayer1)
# psi<-t(psi)
# gloxi<-t(data.frame(psi))  # can not combine
# gloxia<-data.frame(gloxi)            # can not combine
# glox<-as.data.frame(gloxia$meanlayer1) # can not combine
# glox<-data.frame(glox)                 # can not combine
# glox<-as.matrix(glox$gloxia.meanlayer1)
# glox<-glox[-168]
# cri<-as.Date(as.character(glox)) # from "2017-12-13" to "2022-05-27"

#### Dantzig estimation ####

##### CV for tuning lambda #####

# ###### CV for tuning lambda in estimation Sigma^-1 1 using the first 500 data ######

# # Use for recurrence
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)
# block.start=1+(0:(n.block-1))*B
# # valid.block=sort(sample(1:n.block,floor(n.block/4)))
# lmd.i=c()
# for (valid.block in 1:5) {
#   valid.ind=NULL
#   for(k in valid.block){
#     valid.ind=c(valid.ind,block.start[k]:(min(block.start[k]+B-1,n)))
#   }
#   n.valid=length(valid.ind)
#   train.ind=setdiff(1:n,valid.ind)
#   n.train=length(train.ind)
#   returnstd.train=returnstd[1:500,][train.ind,]
#   returnstd.valid=returnstd[1:500,][valid.ind,]
#   mu.train=rep(1,p)
#   mu.valid=rep(1,p)
#   cov.train=cov(returnstd.train)
#   cov.valid=cov(returnstd.valid)
#   lambda.grid=seq(0.1, max(abs(mu.train)),length=101)[2:100]
#   l.lambda=length(lambda.grid)
#   cv.l.error=NULL
#   cv.l=NULL
#   for(i in 1:l.lambda){
#     lmd=lambda.grid[i]
#     print(i)
#     lin.train=linfun1(cov.train,mu.train,lmd)
#     if(!(all(lin.train==0))){
#       error=sum((cov.valid%*%lin.train-mu.valid)^2)
#       cv.l.error=c(cv.l.error,error)
#       cv.l=c(cv.l,lmd)
#     }
#   }
#   lmd.i[valid.block]=min(cv.l[which(cv.l.error==min(cv.l.error))])
# }
# lmd.i
# lmd1=mean(lmd.i)
lmd1=0.4636

###### CV for tuning lambda in estimation Sigma^-1 mu, using the first 500 data ######
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)
# block.start=1+(0:(n.block-1))*B
# # valid.block=sort(sample(1:n.block,floor(n.block/4)))
# lmd.i=c()
# for (valid.block in 1:5) {
#   valid.ind=NULL
#   for(k in valid.block){
#     valid.ind=c(valid.ind,block.start[k]:(min(block.start[k]+B-1,n)))
#   }
#   n.valid=length(valid.ind)
#   train.ind=setdiff(1:n,valid.ind)
#   n.train=length(train.ind)
#   returnstd.train=returnstd[1:500,][train.ind,]
#   returnstd.valid=returnstd[1:500,][valid.ind,]
#   mu.train=colMeans(returnstd.train)
#   mu.valid=colMeans(returnstd.valid)
#   cov.train=cov(returnstd.train)
#   cov.valid=cov(returnstd.valid)
#   lambda.grid=seq(min(max(abs(mu.train))/100,min(abs(mu.train))), 0.01,length=101)[2:100]
#   l.lambda=length(lambda.grid)
#   cv.l.error=NULL
#   cv.l=NULL
#   for(i in 1:l.lambda){
#     lmd=lambda.grid[i]
#     print(i)
#     lin.train=linfun1(cov.train,mu.train,lmd)
#     sum(lin.train==0)
#     if(!(all(lin.train==0))){
#       error=sum((cov.valid%*%lin.train-mu.valid)^2)
#       cv.l.error=c(cv.l.error,error)
#       cv.l=c(cv.l,lmd)
#     }
#   }
#   lmd.i[valid.block]=min(cv.l[which(cv.l.error==min(cv.l.error))])
# }
# lmd.i
# lmd2=mean(lmd.i)
lmd2=0.002142681


# ###### CV for tuning lambda in estimation Sigma^-1 phi, using the first 500 data ######
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)
# block.start=1+(0:(n.block-1))*B
# # valid.block=sort(sample(1:n.block,floor(n.block/4)))
# lmd.i=c()
# for (valid.block in 1:5) {
#   valid.ind=NULL
#   for(k in valid.block){
#     valid.ind=c(valid.ind,block.start[k]:(min(block.start[k]+B-1,n)))
#   }
#   n.valid=length(valid.ind)
#   train.ind=setdiff(1:n,valid.ind)
#   n.train=length(train.ind)
#   returnstd.train=returnstd[1:500,][train.ind,]
#   returnstd.valid=returnstd[1:500,][valid.ind,]
#   cov.train=cov(returnstd.train)
#   cov.valid=cov(returnstd.valid)
#   mu.train=EC_DS[[1]]
#   mu.valid=EC_DS[[1]]
#   # mu.train=linfun3(cor(returnstd.train)-diag(1,p,p)-diag(max(eigen(cor(returnstd.train))$value),p,p),rep(0,p),lambda=lmd.EC.Dantzig,abs(eigen(cor(returnstd.train)-diag(1,p,p)-diag(max(eigen(cor(returnstd.train))$value),p,p))$vector[1,1]))
#   # mu.train=mu.train/max(mu.train)
#   # sum(mu.train==0)
#   # mu.valid=linfun3(cor(returnstd.valid)-diag(1,p,p)-diag(max(eigen(cor(returnstd.valid))$value),p,p),rep(0,p),lambda=lmd.EC.Dantzig,abs(eigen(cor(returnstd.valid)-diag(1,p,p)-diag(max(eigen(cor(returnstd.valid))$value),p,p))$vector[1,1]))
#   # mu.valid=mu.valid/max(mu.valid)
#   # sum(mu.valid==0)
#   lambda.grid=seq(0.1, max(mu.train),length=101)[1:100]
#   l.lambda=length(lambda.grid)
#   cv.l.error=NULL
#   cv.l=NULL
#   for(i in 1:l.lambda){
#     lmd=lambda.grid[i]
#     print(i)
#     # lmd=0.1
#     lin.train=linfun1(cov.train,mu.train,lmd)
#     # sum(lin.train==0)
#     if(!(all(lin.train==0))){
#       error=sum((cov.valid%*%lin.train-mu.valid)^2)
#       cv.l.error=c(cv.l.error,error)
#       cv.l=c(cv.l,lmd)
#     }
#   }
#   lmd.i[valid.block]=min(cv.l[which(cv.l.error==min(cv.l.error))])
# }
# lmd.i
# lmd3=mean(lmd.i)
lmd3=0.4132

##### CV for tuning parameter in glasso #####
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)
# block.start=1+(0:(n.block-1))*B
# # valid.block=sort(sample(1:n.block,floor(n.block/4)))
# rho.i=c()
# for (valid.block in 1:5) {
#   valid.ind=NULL
#   for(k in valid.block){
#     valid.ind=c(valid.ind,block.start[k]:(min(block.start[k]+B-1,n)))
#   }
#   n.valid=length(valid.ind)
#   train.ind=setdiff(1:n,valid.ind)
#   n.train=length(train.ind)
#   returnstd.train=returnstd[1:500,][train.ind,]
#   returnstd.valid=returnstd[1:500,][valid.ind,]
#   mu.train=colMeans(returnstd.train)
#   mu.valid=colMeans(returnstd.valid)
#   cov.train=cov(returnstd.train)
#   cov.valid=cov(returnstd.valid)
#   rho.grid=seq(0,0.8,length=101)[2:101]
#   l.rho=length(rho.grid)
#   cv.rho.error=NULL
#   cv.rho=NULL
#   for (i in 1:l.rho){
#     rho=rho.grid[i]
#     prec.glasso=glasso(cov.train,rho=rho)$wi
#     error=sum((prec.glasso%*%cov.valid-diag(rep(1,p)))^2)
#     cv.rho.error=c(cv.rho.error,error)
#     cv.rho=c(cv.rho,rho)
#     print(i)
#   }
#   rho.i[valid.block]=cv.rho[which(cv.rho.error==min(cv.rho.error))]
# }
# rho=mean(rho.i)
rho<-0.016

#### Dantzig selector estimation for theta1, theta2, theta3 #####
#      theta1 <- Sigma^-1 1
#      theta2 <- Sigma^-1 mu
#      theta3 <- Sigma^-1 phi
# theta1<-list()
# theta2<-list()
# theta3<-list()
# for(t in 1: length(W_in)){
#   print(t)
#   ptm<-proc.time()
#   ## compute global minimum variance portfolio ##
#   theta1[[t]] =linfun1(COV_in[[t]],rep(1,p),lambda=lmd1) # lambda <= 0.1 will lead to be infeasible
#   # print('theta1')
#   theta2[[t]] =linfun1(COV_in[[t]],ER_in[[t]],lambda=lmd2) # lambda <= 0.1 will lead to be infeasible
#   # print('theta2')
#   theta3[[t]] =linfun1(COV_in[[t]],EC_DS[[t]],lambda=lmd3) # lambda <= 0.1 will lead to be infeasible
#   ptm<-proc.time()-ptm
#   print(ptm)
# }
# theta<-list("theta1"=theta1,
#             "theta2"=theta2,
#             "theta3"=theta3)
# save(theta,file="theta_Dantzig_20230308.RData")
load("theta_Dantzig_20230308.RData")
theta1=theta$theta1
theta2=theta$theta2
theta3=theta$theta3


#### Weights of different portfolios in each rolling window ####
##### minimum variance portfolio  #####

###### minimum variance portfolio with Dantzig estimation  ######
w<-list()
cumureturn_minVar_Dantzig<-list()
for(t in 1: length(W_in)){
  w[[t]]=theta1[[t]]/sum(theta1[[t]])
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_minVar_Dantzig[[t]]<-rowSums(aus)
}
return_minVar_Dantzig<-as.matrix(cbind(unlist(cumureturn_minVar_Dantzig))+1)
cumureturn_minVar_Dantzig<-cumprod(return_minVar_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_minVar_Dantzig<-w

###### minimum variance portfolio with plug in  ######
w<-list()
cumureturn_minVar<-list()
for(t in 1: length(W_in)){
  portf_minVar =globalMin.portfolio(ER_in[[(t)]],COV_in[[(t)]])
  w[[(t)]] =portf_minVar$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_minVar[[t]]<-rowSums(aus)
}
return_minVar<-as.matrix(cbind(unlist(cumureturn_minVar))+1)
cumureturn_minVar<-cumprod(return_minVar)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_minVar<-w

##### minimum variance portfolio with glasso #####
w<-list()
cumureturn<-list()
for(t in 1: length(W_in)){
  glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
  w[[t]]=row_sums(glasso.icov)/sum(glasso.icov)
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn[[t]]<-rowSums(aus)
}
return_minVar_glasso<-as.matrix(cbind(unlist(cumureturn))+1)
cumureturn_minVar_glasso<-cumprod(return_minVar_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_minVar_glasso<-w
boxplot(w_minVar_glasso[5,])
# ##### minimum variance portfolio with Dantzig estimation no short #####
# w<-list()
# cumureturn_minVar_Dantzig_noshort<-list()
# for(t in 1: length(W_in)){
#   portf_minVar =globalMin.portfolio(ER_in[[(t)]],COV_in[[(t)]],FALSE)
#   w[[(t)]] =portf_minVar$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_minVar_Dantzig_noshort[[t]]<-rowSums(aus)
# }
# return_minVar_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_minVar_Dantzig_noshort)))
# cumureturn_minVar_Dantzig_noshort<-cumprod(return_minVar_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_minVar_Dantzig_noshort<-w

###### minimum variance portfolio with plug in no short ######
w<-list()
cumureturn_minVar_noshort<-list()
for(t in 1: length(W_in)){
  portf_minVar =globalMin.portfolio(ER_in[[(t)]],COV_in[[(t)]],FALSE)
  w[[(t)]] =portf_minVar$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_minVar_noshort[[t]]<-rowSums(aus)
}
return_minVar_noshort<-as.matrix(cbind(unlist(cumureturn_minVar_noshort))+1)
cumureturn_minVar_noshort<-cumprod(return_minVar_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_minVar_noshort<-w

##### minimum variance portfolio with glasso no short #####
# w<-list()
# cumureturn<-list()
# for(t in 1: length(W_in)){
#   glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
#   w[[t]]=row_sums(glasso.icov)/sum(glasso.icov)
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn[[t]]<-rowSums(aus)
# }
# return_minVar_glasso_noshort<-as.matrix(cbind(unlist(cumureturn)))
# cumureturn_minVar_glasso_noshort<-cumprod(return_minVar_glasso_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_minVar_glasso_noshort<-w

##### mean variance portfolio  #####
###### mean variance portfolio with plug in  ######
w<-list()
cumureturn_meanVar<-list()
for(t in 1: length(W_in)){
  portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]))
  # alpha=(sum(theta2[[t]])*sum(theta1[[t]])*mean(ER_in[[t]])-(sum(theta2[[t]]))^2)/(ER_in[[(t)]]%*%theta2[[t]]*sum(theta1[[t]])-(sum(theta2[[t]]))^2)
  w[[(t)]] =portf_meanVar$weights
  # w[[(t)]] = alpha*theta2[[t]]/sum(theta2[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_meanVar[[t]]<-rowSums(aus)
}
return_meanVar<-as.matrix(cbind(unlist(cumureturn_meanVar))+1)
cumureturn_meanVar<-cumprod(return_meanVar)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_meanVar<-w

###### mean variance portfolio with Dantzig estimation  ######
w<-list()
cumureturn_meanVar_Dantzig<-list()
for(t in 1: length(W_in)){
  # portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]))
  alpha=(sum(theta2[[t]])*sum(theta1[[t]])*mean(ER_in[[t]])-(sum(theta2[[t]]))^2)/(ER_in[[(t)]]%*%theta2[[t]]*sum(theta1[[t]])-(sum(theta2[[t]]))^2)
  w[[(t)]] = alpha*theta2[[t]]/sum(theta2[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_meanVar_Dantzig[[t]]<-rowSums(aus)
}
return_meanVar_Dantzig<-as.matrix(cbind(unlist(cumureturn_meanVar_Dantzig))+1)
cumureturn_meanVar_Dantzig<-cumprod(return_meanVar_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_meanVar_Dantzig<-w

###### mean variance portfolio with glasso ######
w<-list()
cumureturn<-list()
for(t in 1: length(W_in)){
  glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
  alpha=(sum(glasso.icov%*%ER_in[[t]])*sum(glasso.icov)*mean(ER_in[[t]])-(sum(glasso.icov%*%ER_in[[t]]))^2)/(ER_in[[(t)]]%*%glasso.icov%*%ER_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%ER_in[[t]]))^2)
  w[[(t)]] = c(alpha[1]*glasso.icov%*%ER_in[[t]]/sum(glasso.icov%*%ER_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn[[t]]<-rowSums(aus)
}
return_meanVar_glasso<-as.matrix(cbind(unlist(cumureturn))+1)
cumureturn_meanVar_glasso<-cumprod(return_meanVar_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_meanVar_glasso<-w

###### mean variance portfolio with plug in no short ######
w<-list()
cumureturn_meanVar_noshort<-list()
for(t in 1: length(W_in)){
  portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]),shorts = FALSE)
  w[[(t)]] =portf_meanVar$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_meanVar_noshort[[t]]<-rowSums(aus)
}
return_meanVar_noshort<-as.matrix(cbind(unlist(cumureturn_meanVar_noshort))+1)
cumureturn_meanVar_noshort<-cumprod(return_meanVar_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_meanVar_noshort<-w

###### mean variance portfolio with Dantzig no short ######
# w<-list()
# cumureturn_meanVar_noshort<-list()
# for(t in 1: length(W_in)){
#   portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]),shorts = FALSE)
#   w[[(t)]] =portf_meanVar$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_meanVar_noshort[[t]]<-rowSums(aus)
# }
# return_meanVar_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_meanVar_Dantzig_noshort)))
# cumureturn_meanVar_Dantzig_noshort<-cumprod(return_meanVar_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_meanVar_Dantzig_noshort<-w

###### mean varriance portfolio with glasso no short ######
# w<-list()
# cumureturn<-list()
# for(t in 1: length(W_in)){
#   glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
#   alpha=(sum(glasso.icov%*%ER_in[[t]])*sum(glasso.icov)*mean(ER_in[[t]])-(sum(glasso.icov%*%ER_in[[t]]))^2)/(ER_in[[(t)]]%*%glasso.icov%*%ER_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%ER_in[[t]]))^2)
#   w[[(t)]] = c(alpha[1]*glasso.icov%*%ER_in[[t]]/sum(glasso.icov%*%ER_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn[[t]]<-rowSums(aus)
# }
# return_meanVar_glasso_noshort<-as.matrix(cbind(unlist(cumureturn)))
# cumureturn_meanVar_glasso_noshort<-cumprod(return_meanVar_glasso_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_meanVar_glasso_noshort<-w

##### equally weighted portfolio #####
w<-list()
cumureturn_temporal<-list()
centrality_equal_portfolio<-list()
for(t in 1: length(W_in)){
  w[[(t)]] =matrix(1/p,1,p)
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
  centrality_equal_portfolio[[t]]<-as.double(w[[(t)]]%*%EC_in[[(t)]])
}
return_equal<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_equal<-cumprod(return_equal)
# centrality_equal_portfolio<-as.matrix(cbind(unlist(centrality_equal_portfolio)))
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_equal<-w

##### network portfolio  constraint #####
### default setting centrality constraint as mean centrality ###
###### network portfolio fixed constraint as mean centrality with plug in ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.efficient.portfolio(EC_DS[[(t)]], COV_in[[(t)]],mean(EC_DS[[(t)]]))
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_1constraint<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_1constraint<-cumprod(return_network_1constraint)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint<-w

###### network portfolio fixed constraint as mean centrality with Dantzig ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_DS[[(t)]])>mean(EC_DS[[t]])){
    alpha=(sum(theta3[[t]])*sum(theta1[[t]])*mean(EC_DS[[t]])-(sum(theta3[[t]]))^2)/(EC_DS[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2)
    w[[(t)]] = c(alpha)*theta3[[t]]/sum(theta3[[t]])+(1-c(alpha))*theta1[[t]]/sum(theta1[[t]])
  }
  else{
    w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
  }
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_1constraint_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_1constraint_Dantzig<-cumprod(return_network_1constraint_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint_Dantzig<-w

###### network portfolio fixed constraint as mean centrality with glasso ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
  if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_DS[[t]])>mean(EC_DS[[t]])){
    alpha=c((sum(glasso.icov%*%EC_DS[[t]])*sum(glasso.icov)*mean(EC_DS[[t]])-(sum(glasso.icov%*%EC_DS[[t]]))^2)/(EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_DS[[t]]))^2))
    w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_DS[[t]]/sum(glasso.icov%*%EC_DS[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
  }
  else{
    w[[(t)]] = row_sums(glasso.icov)/sum(glasso.icov)
  }
  # alpha=(sum(glasso.icov%*%EC_DS[[t]])*sum(glasso.icov)*mean(EC_DS[[t]])-(sum(glasso.icov%*%EC_DS[[t]]))^2)/(EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_DS[[t]]))^2)
  # w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_DS[[t]]/sum(glasso.icov%*%EC_DS[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_1constraint_glasso<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_1constraint_glasso<-cumprod(return_network_1constraint_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint_glasso<-w

###### network portfolio fixed constraint as mean centrality with plug in no short ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.efficient.portfolio(EC_DS[[(t)]], COV_in[[(t)]],mean(EC_DS[[(t)]]),FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
  # print(t)
}
return_network_1constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_1constraint_noshort<-cumprod(return_network_1constraint_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint_noshort<-w

# ###### network portfolio fixed constraint as mean centrality with Dantzig no short ######
# w<-list()
# cumureturn_temporal<-list()
# for(t in 1: length(W_in)){
#   ## compute global minimum variance portfolio without short ##
#   net.gmin.port = network.efficient.portfolio(EC_DS[[(t)]], COV_in[[(t)]],mean(EC_DS[[(t)]]),FALSE)
#   w[[(t)]] =net.gmin.port$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
#   # print(t)
# }
# return_network_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_Dantzig_noshort<-cumprod(return_network_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_1constraint_Dantzig_noshort<-w

# ###### network portfolio fixed constraint as mean centrality with glasso no short ######
# w<-list()
# cumureturn_temporal<-list()
# for(t in 1: length(W_in)){
#   ## compute global minimum variance portfolio without short ##
#   glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
#   alpha=(sum(glasso.icov%*%EC_DS[[t]])*sum(glasso.icov)*mean(EC_DS[[t]])-(sum(glasso.icov%*%EC_DS[[t]]))^2)/(EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_DS[[t]]))^2)
#   w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_DS[[t]]/sum(glasso.icov%*%EC_DS[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
# }
# return_network_1constraint_glasso_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_1constraint_glasso_noshort<-cumprod(return_network_1constraint_glasso_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_1constraint_glasso_noshort<-w


###### network portfolio varying constraint with plug in ######
cumureturn_network_vary_with_phi<-list()
return_network_vary_with_phi<-list()
quantl<-seq(0.1,0.9,0.1)
w_network_vary_with_phi<-list()
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute 1-constraint network portfolio ##
    net.gmin.port = network.efficient.portfolio(EC_DS[[(t)]], COV_in[[(t)]],quantile(EC_DS[[(t)]],quantl[i]),TRUE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
  cumureturn_network_vary_with_phi[[i]]<-cumprod(return_network_vary_with_phi[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi[[i]]<-w
}

###### network portfolio varying constraint with Dantzig ######
cumureturn_network_vary_with_phi_Dantzig<-list()
return_network_vary_with_phi_Dantzig<-list()
quantl<-seq(0.1,0.9,0.1)
w_network_vary_with_phi_Dantzig<-list()
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    # c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_DS[[(t)]])>mean(EC_DS[[t]])
    if((c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_DS[[(t)]]))>quantile(EC_DS[[(t)]],quantl[i])){
      alpha=c((sum(theta3[[t]])*sum(theta1[[t]])*quantile(EC_DS[[(t)]],quantl[i])-(sum(theta3[[t]]))^2)/(EC_DS[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2))
      w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    }
    else{
      w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
    }
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_Dantzig[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
  cumureturn_network_vary_with_phi_Dantzig[[i]]<-cumprod(return_network_vary_with_phi_Dantzig[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_Dantzig[[i]]<-w
}

###### network portfolio varying constraint with glasso ######
cumureturn_network_vary_with_phi_glasso<-list()
return_network_vary_with_phi_glasso<-list()
quantl<-seq(0.1,0.9,0.1)
w_network_vary_with_phi_glasso<-list()
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
    glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
    if((c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_DS[[t]]))>quantile(EC_DS[[(t)]],quantl[i])){
      alpha=c((sum(glasso.icov%*%EC_DS[[t]])*sum(glasso.icov)*quantile(EC_DS[[(t)]],quantl[i])-(sum(glasso.icov%*%EC_DS[[t]]))^2)/(EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_DS[[t]]))^2))
      w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_DS[[t]]/sum(glasso.icov%*%EC_DS[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
    }
    else{
      w[[(t)]] = row_sums(glasso.icov)/sum(glasso.icov)
    }
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_glasso[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
  cumureturn_network_vary_with_phi_glasso[[i]]<-cumprod(return_network_vary_with_phi_glasso[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_glasso[[i]]<-w
}

###### network portfolio varying constraint with plug in no short ######
cumureturn_network_vary_with_phi_noshort<-list()
return_network_vary_with_phi_noshort<-list()
w_network_vary_with_phi_noshort<-list()
quantl<-seq(0.1,0.9,0.1)
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.efficient.portfolio(EC_DS[[(t)]], COV_in[[(t)]],quantile(EC_DS[[(t)]],quantl[i]),FALSE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
  cumureturn_network_vary_with_phi_noshort[[i]]<-cumprod(return_network_vary_with_phi_noshort[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_noshort[[i]]<-w
}

# ###### network portfolio varying constraint with Dantzig no short ######
# cumureturn_network_vary_with_phi_Dantzig_noshort<-list()
# return_network_vary_with_phi_Dantzig_noshort<-list()
# w_network_vary_with_phi_Dantzig_noshort<-list()
# quantl<-seq(0.1,0.9,0.1)
# for (i in 1:length(quantl)) {
#   w<-list()
#   cumureturn_temporal<-list()
#   for(t in 1: length(W_in)){
#     ## compute global minimum variance portfolio ##
#     net.gmin.port = network.efficient.portfolio(EC_DS[[(t)]], COV_in[[(t)]],quantile(EC_DS[[(t)]],quantl[i]),FALSE)
#     w[[(t)]] =net.gmin.port$weights
#     aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#     cumureturn_temporal[[t]]<-rowSums(aus)
#   }
#   return_network_vary_with_phi_Dantzig_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
#   cumureturn_network_vary_with_phi_Dantzig_noshort[[i]]<-cumprod(return_network_vary_with_phi_noshort[[i]])
#   w<-t(matrix(unlist(w),p,T.windows))
#   colnames(w) = node.label
#   w_network_vary_with_phi_Dantzig_noshort[[i]]<-w
# }

###### network portfolio varying constraint with glasso no short######
# cumureturn_network_vary_with_phi_glasso_noshort<-list()
# return_network_vary_with_phi_glasso_noshort<-list()
# quantl<-seq(0.1,0.9,0.1)
# w_network_vary_with_phi_glasso_noshort<-list()
# for (i in 1:length(quantl)) {
#   w<-list()
#   cumureturn_temporal<-list()
#   for(t in 1: length(W_in)){
#     ## compute global minimum variance portfolio ##glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
#     alpha=(sum(glasso.icov%*%EC_DS[[t]])*sum(glasso.icov)*quantile(EC_DS[[(t)]],quantl[i])-(sum(glasso.icov%*%EC_DS[[t]]))^2)/(EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_DS[[t]]))^2)
#     w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_DS[[t]]/sum(glasso.icov%*%EC_DS[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
#     aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#     cumureturn_temporal[[t]]<-rowSums(aus)
#   }
#   return_network_vary_with_phi_glasso_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
#   cumureturn_network_vary_with_phi_glasso_noshort[[i]]<-cumprod(return_network_vary_with_phi_glasso_noshort[[i]])
#   w<-t(matrix(unlist(w),p,T.windows))
#   colnames(w) = node.label
#   w_network_vary_with_phi_glasso_noshort[[i]]<-w
# }


###### network portfolio data-driven constraint with plug in ######
cumureturn_network_datadriven_phistar<-list()
return_network_datadriven_phistar<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.efficient.portfolio(EC_DS[[(t)]], COV_in[[(t)]],phi_star[t],TRUE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_datadriven_phistar<-cumprod(return_network_datadriven_phistar)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar<-w

###### network portfolio data-driven constraint with Dantzig ######
cumureturn_network_datadriven_phistar_Dantzig<-list()
return_network_datadriven_phistar_Dantzig<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_DS[[(t)]])>phi_star[t]){
    alpha=c((sum(theta3[[t]])*sum(theta1[[t]])*phi_star[t]-(sum(theta3[[t]]))^2)/(EC_DS[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2))
    w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
  }
  else{
    w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
  }
  aus <- as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]] <- rowSums(aus)
}
return_network_datadriven_phistar_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_datadriven_phistar_Dantzig<-cumprod(return_network_datadriven_phistar_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_Dantzig<-w

###### network portfolio data-driven constraint with glasso ######
cumureturn_network_datadriven_phistar_glasso<-list()
return_network_datadriven_phistar_glasso<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
  if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_DS[[t]])>phi_star[t]){
    alpha=c((sum(glasso.icov%*%EC_DS[[t]])*sum(glasso.icov)*phi_star[t]-(sum(glasso.icov%*%EC_DS[[t]]))^2)/(EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_DS[[t]]))^2))
    w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_DS[[t]]/sum(glasso.icov%*%EC_DS[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
  }
  else{
    w[[(t)]] = row_sums(glasso.icov)/sum(glasso.icov)
  }
  aus <- as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]] <- rowSums(aus)
}
return_network_datadriven_phistar_glasso<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_datadriven_phistar_glasso<-cumprod(return_network_datadriven_phistar_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_glasso<-w

###### network portfolio data-driven constraint with plug in no short ######
cumureturn_network_datadriven_phistar_noshort<-list()
return_network_datadriven_phistar_noshort<-list()
cumureturn_temporal<-list()
phi_star<-centrality_equal_portfolio
w<-list()
for(t in 1: length(W_in)){
  ## compute network portfolio data-driven constraint no short ##
  net.gmin.port = network.efficient.portfolio(EC_DS[[(t)]], COV_in[[(t)]],phi_star[t],FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_noshort<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_datadriven_phistar_noshort<-cumprod(return_network_datadriven_phistar_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_noshort<-w

##### network portfolio 2 constraint #####
### default setting centrality constraint as mean centrality ###

###### network portfolio fixed constraint with plug in ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute 2 constraint network portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_DS[[(t)]],ER_in[[(t)]], COV_in[[(t)]],mean(EC_DS[[(t)]]),mean(ER_in[[(t)]]))
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_2constraint<-cumprod(return_network_2constraint)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint<-w

###### network portfolio fixed constraint with Dantzig ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute 2 constraint network portfolio ##
  if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_DS[[(t)]])>mean(EC_DS[[t]])){
    if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
      # 2 constraint case
      M1 <- cbind(rbind(1,mean(EC_DS[[(t)]]),mean(ER_in[[(t)]])),
                  rbind(sum(theta3[[(t)]]),EC_DS[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                  rbind(sum(theta2[[(t)]]),EC_DS[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      M2 <- cbind(rbind(sum(theta1[[(t)]]),EC_DS[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                  rbind(1,mean(EC_DS[[(t)]]),mean(ER_in[[(t)]])),
                  rbind(sum(theta2[[(t)]]),EC_DS[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      M <- cbind(rbind(sum(theta1[[(t)]]),EC_DS[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                 rbind(sum(theta3[[(t)]]),EC_DS[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                 rbind(sum(theta2[[(t)]]),EC_DS[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      gamma1 <- det(M1)/det(M)
      gamma2 <- det(M2)/det(M)
      alpha1 <- c(gamma1*sum(theta3[[t]]))
      alpha2 <- c(gamma2*sum(theta2[[t]]))
      w1 <- (1-alpha1-alpha2)*theta1[[t]]/sum(theta1[[t]])
      w2 <- alpha1*theta3[[t]]/sum(theta3[[t]])
      w3 <- alpha2*theta2[[t]]/sum(theta2[[t]])
      w[[(t)]] = w1 +  w2 + w3
    }
    else{
      # 1 centrality constraint case
      alpha=(sum(theta3[[t]])*sum(theta1[[t]])*mean(EC_DS[[t]])-(sum(theta3[[t]]))^2)/(EC_DS[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2)
      w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    }}
  else{
    if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
      # 1 expected return constraint case 
      alpha=c((sum(theta2[[t]])*sum(theta1[[t]])*mean(ER_in[[t]])-(sum(theta2[[t]]))^2)/(ER_in[[(t)]]%*%theta2[[t]]*sum(theta1[[t]])-(sum(theta2[[t]]))^2))
      w[[(t)]] = alpha*theta2[[t]]/sum(theta2[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    }
    else{
      # global minimum variance case 
      w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
    }
  }
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_2constraint_Dantzig<-cumprod(return_network_2constraint_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint_Dantzig<-w

###### network portfolio fixed constraint with glasso ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute 2 constraint network portfolio ##
  glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
  if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_DS[[(t)]])>mean(EC_DS[[t]])){
    if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
      # 2 constraint case
      M1 <- cbind(rbind(1,mean(EC_DS[[(t)]]),mean(ER_in[[(t)]])),
                  rbind(sum(glasso.icov%*%EC_DS[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]]),
                  rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      M2 <- cbind(rbind(sum(glasso.icov),EC_DS[[(t)]]%*%row_sums(glasso.icov),ER_in[[(t)]]%*%row_sums(glasso.icov)),
                  rbind(1,mean(EC_DS[[(t)]]),mean(ER_in[[(t)]])),
                  rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      M <- cbind(rbind(sum(glasso.icov),EC_DS[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                 rbind(sum(glasso.icov%*%EC_DS[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]]),
                 rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      gamma1 <- det(M1)/det(M)
      gamma2 <- det(M2)/det(M)
      alpha1 <- c(gamma1*sum(glasso.icov%*%EC_DS[[(t)]]))
      alpha2 <- c(gamma2*sum(glasso.icov%*%ER_in[[(t)]]))
      w1 <- (1-alpha1-alpha2)*row_sums(glasso.icov)/sum(glasso.icov)
      w2 <- alpha1*glasso.icov%*%EC_DS[[(t)]]/sum(glasso.icov%*%EC_DS[[(t)]])
      w3 <- alpha2*glasso.icov%*%ER_in[[(t)]]/sum(glasso.icov%*%ER_in[[(t)]])
      w[[(t)]] = c(w1 +  w2 + w3)
    }
    else{
      # 1 centrality constraint case
      alpha=c((sum(glasso.icov%*%EC_DS[[t]])*sum(glasso.icov)*mean(EC_DS[[t]])-(sum(glasso.icov%*%EC_DS[[t]]))^2)/(EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_DS[[t]]))^2))
      w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_DS[[t]]/sum(glasso.icov%*%EC_DS[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
    }}
  else{
    if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
      # 1 expected return constraint case 
      alpha=c((sum(glasso.icov%*%ER_in[[t]])*sum(glasso.icov)*mean(ER_in[[t]])-(sum(glasso.icov%*%ER_in[[t]]))^2)/(ER_in[[(t)]]%*%glasso.icov%*%ER_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%ER_in[[t]]))^2))
      w[[(t)]] = c(alpha[1]*glasso.icov%*%ER_in[[t]]/sum(glasso.icov%*%ER_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
    }
    else{
      # global minimum variance case 
      w[[t]]=row_sums(glasso.icov)/sum(glasso.icov)
    }
  }
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint_glasso<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_2constraint_glasso<-cumprod(return_network_2constraint_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint_glasso<-w

###### network portfolio fixed constraint with plug in no short ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.2constraint.portfolio(EC_DS[[(t)]],ER_in[[t]], COV_in[[(t)]],mean(EC_DS[[(t)]]),mean(ER_in[[t]]),FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_2constraint_noshort<-cumprod(return_network_2constraint_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint_noshort<-w

# ###### network portfolio fixed constraint with Dantzig no short ######
# w<-list()
# cumureturn_temporal<-list()
# for(t in 1: length(W_in)){
#   ## compute global minimum variance portfolio without short ##
#   net.gmin.port = network.2constraint.portfolio(EC_DS[[(t)]],ER_in[[t]], COV_in[[(t)]],mean(EC_DS[[(t)]]),mean(ER_in[[t]]),FALSE)
#   w[[(t)]] =net.gmin.port$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
# }
# return_network_2constraint_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_2constraint_Dantzig_noshort<-cumprod(return_network_2constraint_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_2constraint_Dantzig_noshort<-w


# ###### network portfolio fixed constraint with glasso no short ######
# w<-list()
# cumureturn_temporal<-list()
# for(t in 1: length(W_in)){
#   ## compute 2 constraint network portfolio ##
#   glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
#   
#   M1 <- cbind(rbind(1,mean(EC_DS[[(t)]]),mean(ER_in[[(t)]])),
#               rbind(sum(glasso.icov%*%EC_DS[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]]),
#               rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
#   M2 <- cbind(rbind(sum(glasso.icov),EC_DS[[(t)]]%*%row_sums(glasso.icov),ER_in[[(t)]]%*%row_sums(glasso.icov)),
#               rbind(1,mean(EC_DS[[(t)]]),mean(ER_in[[(t)]])),
#               rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
#   M <- cbind(rbind(sum(glasso.icov),EC_DS[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
#              rbind(sum(glasso.icov%*%EC_DS[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]]),
#              rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
#   gamma1 <- det(M1)/det(M)
#   gamma2 <- det(M2)/det(M)
#   alpha1 <- gamma1*sum(glasso.icov%*%EC_DS[[(t)]])
#   alpha2 <- gamma2*sum(glasso.icov%*%ER_in[[(t)]])
#   w1 <- (1-alpha1-alpha2)*row_sums(glasso.icov)/sum(glasso.icov)
#   w2 <- alpha1*glasso.icov%*%EC_DS[[(t)]]/sum(glasso.icov%*%EC_DS[[(t)]])
#   w3 <- alpha2*glasso.icov%*%ER_in[[(t)]]/sum(glasso.icov%*%ER_in[[(t)]])
#   w[[(t)]] = c(w1 +  w2 + w3)
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
# }
# return_network_2constraint_glasso_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_2constraint_glasso_noshort<-cumprod(return_network_2constraint_glasso_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_2constraint_glasso_noshort<-w


###### network portfolio varying constraint with plug in ######
cumureturn_network_vary_with_phi_2constraint<-list()
return_network_vary_with_phi_2constraint<-list()
w_network_vary_with_phi_2constraint<-list()
quantl<-seq(0.1,0.9,0.1)
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.2constraint.portfolio(EC_DS[[(t)]],ER_in[[t]], COV_in[[(t)]],quantile(EC_DS[[(t)]],quantl[i]),mean(ER_in[[t]]),TRUE)
    # net.gmin.port = network.efficient.portfolio(EC.posi_in[[(t)]], COV_in[[(t)]],phi_star[i],TRUE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
  cumureturn_network_vary_with_phi_2constraint[[i]]<-cumprod(return_network_vary_with_phi_2constraint[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_2constraint[[i]]<-w
}

###### network portfolio varying constraint with Dantzig ######
cumureturn_network_vary_with_phi_2constraint_Dantzig<-list()
return_network_vary_with_phi_2constraint_Dantzig<-list()
w_network_vary_with_phi_2constraint_Dantzig<-list()
quantl<-seq(0.1,0.9,0.1)
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## network portfolio varying constraint with Dantzig estimation ##
    if((c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_DS[[(t)]]))>quantile(EC_DS[[(t)]],quantl[i])){
      if((c(theta1[[t]]/sum(theta1[[t]]))%*%c(ER_in[[(t)]]))<mean(ER_in[[t]])){
        # 2 constraint case
        M1 <- cbind(rbind(1,quantile(EC_DS[[(t)]],quantl[i]),mean(ER_in[[(t)]])),
                    rbind(sum(theta3[[(t)]]),EC_DS[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                    rbind(sum(theta2[[(t)]]),EC_DS[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
        M2 <- cbind(rbind(sum(theta1[[(t)]]),EC_DS[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                    rbind(1,quantile(EC_DS[[(t)]],quantl[i]),mean(ER_in[[(t)]])),
                    rbind(sum(theta2[[(t)]]),EC_DS[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
        M <- cbind(rbind(sum(theta1[[(t)]]),EC_DS[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                   rbind(sum(theta3[[(t)]]),EC_DS[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                   rbind(sum(theta2[[(t)]]),EC_DS[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
        gamma1 <- det(M1)/det(M)
        gamma2 <- det(M2)/det(M)
        alpha1 <- c(gamma1*sum(theta3[[t]]))
        alpha2 <- c(gamma2*sum(theta2[[t]]))
        w1 <- (1-alpha1-alpha2)*theta1[[t]]/sum(theta1[[t]])
        w2 <- alpha1*theta3[[t]]/sum(theta3[[t]])
        w3 <- alpha2*theta2[[t]]/sum(theta2[[t]])
        w[[(t)]] = w1 +  w2 + w3
      }
      else{
        # 1 centrality constraint case
        alpha=c((sum(theta3[[t]])*sum(theta1[[t]])*quantile(EC_DS[[(t)]],quantl[i])-(sum(theta3[[t]]))^2)/(EC_DS[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2))
        w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
      }}
    else{
      if((c(theta1[[t]]/sum(theta1[[t]]))%*%c(ER_in[[(t)]]))<mean(ER_in[[t]])){
        # 1 expected return constraint case 
        alpha=c((sum(theta2[[t]])*sum(theta1[[t]])*mean(ER_in[[t]])-(sum(theta2[[t]]))^2)/(ER_in[[(t)]]%*%theta2[[t]]*sum(theta1[[t]])-(sum(theta2[[t]]))^2))
        w[[(t)]] = alpha*theta2[[t]]/sum(theta2[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
      }
      else{
        # global minimum variance case 
        w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
      }
    }
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint_Dantzig[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
  cumureturn_network_vary_with_phi_2constraint_Dantzig[[i]]<-cumprod(return_network_vary_with_phi_2constraint_Dantzig[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_2constraint_Dantzig[[i]]<-w
}

###### network portfolio varying constraint with glasso ######
cumureturn_network_vary_with_phi_2constraint_glasso<-list()
return_network_vary_with_phi_2constraint_glasso<-list()
w_network_vary_with_phi_2constraint_glasso<-list()
quantl<-seq(0.1,0.9,0.1)
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## network portfolio varying constraint with glasso estimation ##
    glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
    if((c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_DS[[(t)]]))>quantile(EC_DS[[(t)]],quantl[i])){
      if((c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(ER_in[[(t)]]))<mean(ER_in[[t]])){
        # 2 constraint case
        M1 <- cbind(rbind(1,quantile(EC_DS[[(t)]],quantl[i]),mean(ER_in[[(t)]])),
                    rbind(sum(glasso.icov%*%EC_DS[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]]),
                    rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
        M2 <- cbind(rbind(sum(glasso.icov),EC_DS[[(t)]]%*%row_sums(glasso.icov),ER_in[[(t)]]%*%row_sums(glasso.icov)),
                    rbind(1,quantile(EC_DS[[(t)]],quantl[i]),mean(ER_in[[(t)]])),
                    rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
        M <- cbind(rbind(sum(glasso.icov),EC_DS[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                   rbind(sum(glasso.icov%*%EC_DS[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]]),
                   rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
        gamma1 <- det(M1)/det(M)
        gamma2 <- det(M2)/det(M)
        alpha1 <- c(gamma1*sum(glasso.icov%*%EC_DS[[(t)]]))
        alpha2 <- c(gamma2*sum(glasso.icov%*%ER_in[[(t)]]))
        w1 <- (1-alpha1-alpha2)*row_sums(glasso.icov)/sum(glasso.icov)
        w2 <- alpha1*glasso.icov%*%EC_DS[[(t)]]/sum(glasso.icov%*%EC_DS[[(t)]])
        w3 <- alpha2*glasso.icov%*%ER_in[[(t)]]/sum(glasso.icov%*%ER_in[[(t)]])
        w[[(t)]] = c(w1 +  w2 + w3)
      }
      else{
        # 1 centrality constraint case
        alpha=c((sum(glasso.icov%*%EC_DS[[t]])*sum(glasso.icov)*quantile(EC_DS[[(t)]],quantl[i])-(sum(glasso.icov%*%EC_DS[[t]]))^2)/(EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_DS[[t]]))^2))
        w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_DS[[t]]/sum(glasso.icov%*%EC_DS[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
      }}
    else{
      if(((row_sums(glasso.icov)/sum(glasso.icov))%*%ER_in[[(t)]])<mean(ER_in[[t]])){
        # 1 expected return constraint case 
        alpha=c((sum(glasso.icov%*%ER_in[[t]])*sum(glasso.icov)*mean(ER_in[[t]])-(sum(glasso.icov%*%ER_in[[t]]))^2)/(ER_in[[(t)]]%*%glasso.icov%*%ER_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%ER_in[[t]]))^2))
        w[[(t)]] = c(alpha[1]*glasso.icov%*%ER_in[[t]]/sum(glasso.icov%*%ER_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
      }
      else{
        # global minimum variance case 
        w[[t]]=row_sums(glasso.icov)/sum(glasso.icov)
      }
    }
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint_glasso[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
  cumureturn_network_vary_with_phi_2constraint_glasso[[i]]<-cumprod(return_network_vary_with_phi_2constraint_glasso[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_2constraint_glasso[[i]]<-w
}

###### network portfolio varying constraint with plug in no short ######
cumureturn_network_vary_with_phi_2constraint_noshort<-list()
return_network_vary_with_phi_2constraint_noshort<-list()
w_network_vary_with_phi_2constraint_noshort<-list()
quantl<-seq(0.1,0.9,0.1)
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.2constraint.portfolio(EC_DS[[(t)]],ER_in[[t]], COV_in[[(t)]],quantile(EC_DS[[(t)]],quantl[i]),mean(ER_in[[t]]),FALSE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
  cumureturn_network_vary_with_phi_2constraint_noshort[[i]]<-cumprod(return_network_vary_with_phi_2constraint_noshort[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_2constraint_noshort[[i]]<-w
}

###### network portfolio varying constraint with Dantzig no short ######
# cumureturn_network_vary_with_phi_2constraint_Dantzig_noshort<-list()
# return_network_vary_with_phi_2constraint_Dantzig_noshort<-list()
# w_network_vary_with_phi_2constraint_Dantzig_noshort<-list()
# quantl<-seq(0.1,0.9,0.1)
# for (i in 1:length(quantl)) {
#   w<-list()
#   cumureturn_temporal<-list()
#   for(t in 1: length(W_in)){
#     ## compute global minimum variance portfolio ##
#     net.gmin.port = network.2constraint.portfolio(EC_DS[[(t)]],ER_in[[t]], COV_in[[(t)]],quantile(EC_DS[[(t)]],quantl[i]),mean(ER_in[[t]]),FALSE)
#     w[[(t)]] =net.gmin.port$weights
#     aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#     cumureturn_temporal[[t]]<-rowSums(aus)
#   }
#   return_network_vary_with_phi_2constraint_Dantzig_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
#   cumureturn_network_vary_with_phi_2constraint_Dantzig_noshort[[i]]<-cumprod(return_network_vary_with_phi_2constraint_Dantzig_noshort[[i]])
#   w<-t(matrix(unlist(w),p,T.windows))
#   colnames(w) = node.label
#   w_network_vary_with_phi_2constraint_Dantzig_noshort[[i]]<-w
# }

###### network portfolio varying constraint with glasso no short ######

###### network portfolio data-driven constraint with plug in ######
cumureturn_network_datadriven_phistar_2constraint<-list()
return_network_datadriven_phistar_2constraint<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_DS[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER_in[[t]]),TRUE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_datadriven_phistar_2constraint<-cumprod(return_network_datadriven_phistar_2constraint)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_2constraint<-w

###### network portfolio data-driven constraint with Dantzig ######
cumureturn_network_datadriven_phistar_2constraint_Dantzig<-list()
return_network_datadriven_phistar_2constraint_Dantzig<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  if((c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_DS[[(t)]]))>phi_star[t]){
    if((c(theta1[[t]]/sum(theta1[[t]]))%*%c(ER_in[[(t)]]))<phi_star[t]){
      # 2 constraint case
      M1 <- cbind(rbind(1,phi_star[t],mean(ER_in[[(t)]])),
                  rbind(sum(theta3[[(t)]]),EC_DS[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                  rbind(sum(theta2[[(t)]]),EC_DS[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      M2 <- cbind(rbind(sum(theta1[[(t)]]),EC_DS[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                  rbind(1,phi_star[t],mean(ER_in[[(t)]])),
                  rbind(sum(theta2[[(t)]]),EC_DS[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      M <- cbind(rbind(sum(theta1[[(t)]]),EC_DS[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                 rbind(sum(theta3[[(t)]]),EC_DS[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                 rbind(sum(theta2[[(t)]]),EC_DS[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      gamma1 <- det(M1)/det(M)
      gamma2 <- det(M2)/det(M)
      alpha1 <- c(gamma1*sum(theta3[[t]]))
      alpha2 <- c(gamma2*sum(theta2[[t]]))
      w1 <- (1-alpha1-alpha2)*theta1[[t]]/sum(theta1[[t]])
      w2 <- alpha1*theta3[[t]]/sum(theta3[[t]])
      w3 <- alpha2*theta2[[t]]/sum(theta2[[t]])
      w[[(t)]] = w1 +  w2 + w3
    }
    else{
      # 1 centrality constraint case
      alpha=c((sum(theta3[[t]])*sum(theta1[[t]])*phi_star[t]-(sum(theta3[[t]]))^2)/(EC_DS[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2))
      w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    }}
  else{
    if(((theta1[[t]]/sum(theta1[[t]]))%*%ER_in[[(t)]])<mean(ER_in[[t]])){
      # 1 expected return constraint case 
      alpha=c((sum(theta2[[t]])*sum(theta1[[t]])*mean(ER_in[[t]])-(sum(theta2[[t]]))^2)/(ER_in[[(t)]]%*%theta2[[t]]*sum(theta1[[t]])-(sum(theta2[[t]]))^2))
      w[[(t)]] = alpha*theta2[[t]]/sum(theta2[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    }
    else{
      # global minimum variance case 
      w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
    }
  }
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_datadriven_phistar_2constraint_Dantzig<-cumprod(return_network_datadriven_phistar_2constraint_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_2constraint_Dantzig<-w

###### network portfolio data-driven constraint with glasso ######
cumureturn_network_datadriven_phistar_2constraint_glasso<-list()
return_network_datadriven_phistar_2constraint_glasso<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  if((c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_DS[[(t)]]))>phi_star[t]){
    if((c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(ER_in[[(t)]]))<phi_star[t]){
      # 2 constraint case
      M1 <- cbind(rbind(1,phi_star[t],mean(ER_in[[(t)]])),
                  rbind(sum(glasso.icov%*%EC_DS[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]]),
                  rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      M2 <- cbind(rbind(sum(glasso.icov),EC_DS[[(t)]]%*%row_sums(glasso.icov),ER_in[[(t)]]%*%row_sums(glasso.icov)),
                  rbind(1,phi_star[t],mean(ER_in[[(t)]])),
                  rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      M <- cbind(rbind(sum(glasso.icov),EC_DS[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                 rbind(sum(glasso.icov%*%EC_DS[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_DS[[(t)]]),
                 rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_DS[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      gamma1 <- det(M1)/det(M)
      gamma2 <- det(M2)/det(M)
      alpha1 <- c(gamma1*sum(glasso.icov%*%EC_DS[[(t)]]))
      alpha2 <- c(gamma2*sum(glasso.icov%*%ER_in[[(t)]]))
      w1 <- (1-alpha1-alpha2)*row_sums(glasso.icov)/sum(glasso.icov)
      w2 <- alpha1*glasso.icov%*%EC_DS[[(t)]]/sum(glasso.icov%*%EC_DS[[(t)]])
      w3 <- alpha2*glasso.icov%*%ER_in[[(t)]]/sum(glasso.icov%*%ER_in[[(t)]])
      w[[(t)]] = c(w1 +  w2 + w3)
    }
    else{
      # 1 centrality constraint case
      alpha=(sum(glasso.icov%*%EC_DS[[t]])*sum(glasso.icov)*phi_star[t]-(sum(glasso.icov%*%EC_DS[[t]]))^2)/(EC_DS[[(t)]]%*%glasso.icov%*%EC_DS[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_DS[[t]]))^2)
      w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_DS[[t]]/sum(glasso.icov%*%EC_DS[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
    }}
  else{
    if((c(row_sums(glasso.icov)/sum(glasso.icov))%*%ER_in[[(t)]])<mean(ER_in[[t]])){
      # 1 expected return constraint case 
      alpha=(sum(glasso.icov%*%ER_in[[t]])*sum(glasso.icov)*mean(ER_in[[t]])-(sum(glasso.icov%*%ER_in[[t]]))^2)/(ER_in[[(t)]]%*%glasso.icov%*%ER_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%ER_in[[t]]))^2)
      w[[(t)]] = c(alpha[1]*glasso.icov%*%ER_in[[t]]/sum(glasso.icov%*%ER_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
    }
    else{
      # global minimum variance case 
      w[[t]]=row_sums(glasso.icov)/sum(glasso.icov)
    }
  }
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint_glasso<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_datadriven_phistar_2constraint_glasso<-cumprod(return_network_datadriven_phistar_2constraint_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_2constraint_glasso<-w

###### network portfolio data-driven constraint with plug in no short ######
cumureturn_network_datadriven_phistar_2constraint_noshort<-list()
return_network_datadriven_phistar_2constraint_noshort<-list()
cumureturn_temporal<-list()
phi_star<-centrality_equal_portfolio
w<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_DS[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER_in[[t]]),FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal))+1)
cumureturn_network_datadriven_phistar_2constraint_noshort<-cumprod(return_network_datadriven_phistar_2constraint_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_2constraint_noshort<-w

# ###### network portfolio data-driven constraint with Dantzig no short ######
# cumureturn_network_datadriven_phistar_2constraint_Dantzig_noshort<-list()
# return_network_datadriven_phistar_2constraint_Dantzig_noshort<-list()
# cumureturn_temporal<-list()
# phi_star<-centrality_equal_portfolio
# w<-list()
# for(t in 1: length(W_in)){
#   ## compute global minimum variance portfolio ##
#   net.gmin.port = network.2constraint.portfolio(EC_DS[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER_in[[t]]),FALSE)
#   w[[(t)]] =net.gmin.port$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
# }
# return_network_datadriven_phistar_2constraint_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_datadriven_phistar_2constraint_Dantzig_noshort<-cumprod(return_network_datadriven_phistar_2constraint_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_datadriven_phistar_2constraint_Dantzig_noshort<-w

###### network portfolio data-driven constraint with Dantzig no short ######

#### Save portfolios ####
Portfolio.Scenario <- list("return_minVar"=return_minVar,"cumureturn_minVar"=cumureturn_minVar,"w_minVar"=w_minVar,
                           "return_minVar_Dantzig"=return_minVar_Dantzig,"cumureturn_minVar_Dantzig"=cumureturn_minVar_Dantzig,"w_minVar_Dantzig"=w_minVar_Dantzig,
                           "return_minVar_glasso"=return_minVar_glasso,"cumureturn_minVar_glasso"=cumureturn_minVar_glasso,"w_minVar_glasso"=w_minVar_glasso,
                           "return_minVar_noshort"=return_minVar_noshort,"cumureturn_minVar_noshort"=cumureturn_minVar_noshort,"w_minVar_noshort"=w_minVar_noshort,
                           "return_meanVar"=return_meanVar,"cumureturn_meanVar"=cumureturn_meanVar,"w_meanVar"=w_meanVar,
                           "return_meanVar_Dantzig"=return_meanVar_Dantzig,"cumureturn_meanVar_Dantzig"=cumureturn_meanVar_Dantzig,"w_meanVar_Dantzig"=w_meanVar_Dantzig,
                           "return_meanVar_glasso"=return_meanVar_glasso,"cumureturn_meanVar_glasso"=cumureturn_meanVar_glasso,"w_meanVar_glasso"=w_meanVar_glasso,
                           "return_meanVar_noshort"=return_meanVar_noshort,"cumureturn_meanVar_noshort"=cumureturn_meanVar_noshort,"w_meanVar_noshort"=w_meanVar_noshort,
                           "return_equal"=return_equal,"cumureturn_equal"=cumureturn_equal,"w_equal"=w_equal,
                           "return_network_1constraint"=return_network_1constraint,"cumureturn_network_1constraint"=cumureturn_network_1constraint,"w_network_1constraint"=w_network_1constraint,
                           "return_network_1constraint_Dantzig"=return_network_1constraint_Dantzig,"cumureturn_network_1constraint_Dantzig"=cumureturn_network_1constraint_Dantzig,"w_network_1constraint_Dantzig"=w_network_1constraint_Dantzig,
                           "return_network_1constraint_glasso"=return_network_1constraint_glasso,"cumureturn_network_1constraint_glasso"=cumureturn_network_1constraint_glasso,"w_network_1constraint_glasso"=w_network_1constraint_glasso,
                           "return_network_1constraint_noshort"=return_network_1constraint_noshort,"cumureturn_network_1constraint_noshort"=cumureturn_network_1constraint_noshort,"w_network_1constraint_noshort"=w_network_1constraint_noshort,
                           "return_network_vary_with_phi"=return_network_vary_with_phi,"cumureturn_network_vary_with_phi"=cumureturn_network_vary_with_phi,"w_network_vary_with_phi"=w_network_vary_with_phi,
                           "return_network_vary_with_phi_Dantzig"=return_network_vary_with_phi_Dantzig,"cumureturn_network_vary_with_phi_Dantzig"=cumureturn_network_vary_with_phi_Dantzig,"w_network_vary_with_phi_Dantzig"=w_network_vary_with_phi_Dantzig,
                           "return_network_vary_with_phi_glasso"=return_network_vary_with_phi_glasso,"cumureturn_network_vary_with_phi_glasso"=cumureturn_network_vary_with_phi_glasso,"w_network_vary_with_phi_glasso"=w_network_vary_with_phi_glasso,
                           "return_network_vary_with_phi_noshort"=return_network_vary_with_phi_noshort,"cumureturn_network_vary_with_phi_noshort"=cumureturn_network_vary_with_phi_noshort,"w_network_vary_with_phi_noshort"=w_network_vary_with_phi_noshort,
                           "return_network_datadriven_phistar"=return_network_datadriven_phistar,"cumureturn_network_datadriven_phistar"=cumureturn_network_datadriven_phistar,"w_network_datadriven_phistar"=w_network_datadriven_phistar,
                           "return_network_datadriven_phistar_Dantzig"=return_network_datadriven_phistar_Dantzig,"cumureturn_network_datadriven_phistar_Dantzig"=cumureturn_network_datadriven_phistar_Dantzig,"w_network_datadriven_phistar_Dantzig"=w_network_datadriven_phistar_Dantzig,
                           "return_network_datadriven_phistar_glasso"=return_network_datadriven_phistar_glasso,"cumureturn_network_datadriven_phistar_glasso"=cumureturn_network_datadriven_phistar_glasso,"w_network_datadriven_phistar_glasso"=w_network_datadriven_phistar_glasso,
                           "return_network_datadriven_phistar_noshort"=return_network_datadriven_phistar_noshort,"cumureturn_network_datadriven_phistar_noshort"=cumureturn_network_datadriven_phistar_noshort,"w_network_datadriven_phistar_noshort"=w_network_datadriven_phistar_noshort,
                           "return_network_2constraint"=return_network_2constraint,"cumureturn_network_2constraint"=cumureturn_network_2constraint,"w_network_2constraint"=w_network_2constraint,
                           "return_network_2constraint_Dantzig"=return_network_2constraint_Dantzig,"cumureturn_network_2constraint_Dantzig"=cumureturn_network_2constraint_Dantzig,"w_network_2constraint_Dantzig"=w_network_2constraint_Dantzig,
                           "return_network_2constraint_glasso"=return_network_2constraint_glasso,"cumureturn_network_2constraint_glasso"=cumureturn_network_2constraint_glasso,"w_network_2constraint_glasso"=w_network_2constraint_glasso,
                           "return_network_2constraint_noshort"=return_network_2constraint_noshort,"cumureturn_network_2constraint_noshort"=cumureturn_network_2constraint_noshort,"w_network_2constraint_noshort"=w_network_2constraint_noshort,
                           "return_network_vary_with_phi_2constraint"=return_network_vary_with_phi_2constraint,"cumureturn_network_vary_with_phi_2constraint"=cumureturn_network_vary_with_phi_2constraint,"w_network_vary_with_phi_2constraint"=w_network_vary_with_phi_2constraint,
                           "return_network_vary_with_phi_2constraint_Dantzig"=return_network_vary_with_phi_2constraint_Dantzig,"cumureturn_network_vary_with_phi_2constraint_Dantzig"=cumureturn_network_vary_with_phi_2constraint_Dantzig,"w_network_vary_with_phi_2constraint_Dantzig"=w_network_vary_with_phi_2constraint_Dantzig,
                           "return_network_vary_with_phi_2constraint_glasso"=return_network_vary_with_phi_2constraint_glasso,"cumureturn_network_vary_with_phi_2constraint_glasso"=cumureturn_network_vary_with_phi_2constraint_glasso,"w_network_vary_with_phi_2constraint_glasso"=w_network_vary_with_phi_2constraint_glasso,
                           "return_network_vary_with_phi_2constraint_noshort"=return_network_vary_with_phi_2constraint_noshort,"cumureturn_network_vary_with_phi_2constraint_noshort"=cumureturn_network_vary_with_phi_2constraint_noshort,"w_network_vary_with_phi_2constraint_noshort"=w_network_vary_with_phi_2constraint_noshort,
                           "return_network_datadriven_phistar_2constraint"=return_network_datadriven_phistar_2constraint,"cumureturn_network_datadriven_phistar_2constraint"=cumureturn_network_datadriven_phistar_2constraint,"w_network_datadriven_phistar_2constraint"=w_network_datadriven_phistar_2constraint,
                           "return_network_datadriven_phistar_2constraint_Dantzig"=return_network_datadriven_phistar_2constraint_Dantzig,"cumureturn_network_datadriven_phistar_2constraint_Dantzig"=cumureturn_network_datadriven_phistar_2constraint_Dantzig,"w_network_datadriven_phistar_2constraint_Dantzig"=w_network_datadriven_phistar_2constraint_Dantzig,
                           "return_network_datadriven_phistar_2constraint_glasso"=return_network_datadriven_phistar_2constraint_glasso,"cumureturn_network_datadriven_phistar_2constraint_glasso"=cumureturn_network_datadriven_phistar_2constraint_glasso,"w_network_datadriven_phistar_2constraint_glasso"=w_network_datadriven_phistar_2constraint_glasso,
                           "return_network_datadriven_phistar_2constraint_noshort"=return_network_datadriven_phistar_2constraint_noshort,"cumureturn_network_datadriven_phistar_2constraint_noshort"=cumureturn_network_datadriven_phistar_2constraint_noshort,"w_network_datadriven_phistar_2constraint_noshort"=w_network_datadriven_phistar_2constraint_noshort)
save(Portfolio.Scenario,file="Portfolios_20230309.RData")
