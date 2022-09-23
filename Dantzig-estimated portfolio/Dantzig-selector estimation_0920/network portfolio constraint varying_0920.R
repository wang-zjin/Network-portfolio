# In this file, we construct network by 1-correlation matrix

rm(list = ls())

setwd("~/Documents/Code/Network structure based portfolio/Dantzig-selector estimation covariance/Dantzig-selector estimation_0920")

# Load Functions and other Files
source('./PackagesNetworkPortfolio.R')
source('./FunctionsNetworkPortfolio.R')

# load data
prices<-read_excel("SP500 securities.xlsx")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y-%m-%d'))

#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
p=dim(return)[2]

# set label
node.label=colnames(returnstd)
node.label<-gsub("Equity","",node.label)
node.label<-gsub("UN","",node.label)
node.label<-gsub("UW","",node.label)
names(returnstd) = node.label

# rolling window
W<-list()
for(t in 0: 166){
  W[[(t+1)]]=returnstd[(1+t*7):(500+t*7),]
}
W_in<-list()
W_out<-list()
for(t in 0: 166){
  W_in[[(t+1)]]=W[[t+1]][c(1:493),]
  W_out[[(t+1)]]=W[[t+1]][c(494:500),]
}
T.windows<-length(W)
# correlation matrix, Expected return, covariance matrix
C_in <- list()     
ER_in <- list()     
COV_in <- list()     
EC_in <- list()  
EC.posi_in <- list()  
EC.nega_in <- list()  
BetweennessCentrality<-list()
ClosenessCentrality<-list()
DegreeCentrality<-list()
for(t in 1: length(W_in)){
  C_in[[(t)]] =cor(W_in[[(t)]])
  ER_in[[(t)]] = colMeans(W_in[[(t)]])
  COV_in[[(t)]] = cov(W_in[[(t)]])
  network_port = network.portfolio(W_in[[(t)]])
  # network_port = network.correlation(W_in[[(t)]])
  # EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
  # EC_in[[t]]<-eigen(C_in[[(t)]])$vector[,1]
  # network_port<-network.abs(W_in[[(t)]])
  EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
  BetweennessCentrality[[(t)]] <- centr_degree(network_port)
  ClosenessCentrality[[(t)]] <- eigen_centrality(network_port)
  DegreeCentrality[[(t)]] <- eigen_centrality(network_port)
  network_in<-network.posi.naga(W_in[[(t)]])
  EC.posi_in[[(t)]] <- eigen_centrality(network_in$positive,directed = FALSE, scale = TRUE)$vector
  EC.nega_in[[(t)]] <- eigen_centrality(network_in$negative,directed = FALSE, scale = TRUE)$vector
}

# generate timeline "cri" for later portfolio
date<-prices$Dates
date<-as.Date.factor(date[-c(1:501)], format="%Y-%m-%d")
meanlayer1<-list()
for(t in 1: 167){
  meanlayer1[[t]]<-date[[t*7]]
}
meanlayer1<-as.Date(unlist(meanlayer1))
psi<-data.frame(meanlayer1)
psi<-t(psi)
gloxi<-t(data.frame(psi))  # can not combine
gloxia<-data.frame(gloxi)            # can not combine
glox<-as.data.frame(gloxia$meanlayer1) # can not combine
glox<-data.frame(glox)                 # can not combine
glox<-as.matrix(glox$gloxia.meanlayer1)
glox<-glox[-168]
cri<-as.Date(as.character(glox)) # from "2017-12-13" to "2022-05-27"

#### Dantzig estimation ####

##### CV for tuning lambda #####

######CV for tuning lambda in estimation Sigma^-1 1, using the whole sample ######
# n<-dim(returnstd)[1]
# B=250
# n.block=floor(n/B)+1
# block.start=1+(0:(n.block-1))*B
# test.block=sort(sample(1:n.block,floor(n.block/4)))
# test.ind=NULL
# for(k in test.block)
# {
#   test.ind=c(test.ind,block.start[k]:(min(block.start[k]+B-1,n)))
# }
# n.test=length(test.ind)
# train.ind=setdiff(1:n,test.ind)
# n.train=length(train.ind)
# returnstd.train=returnstd[train.ind,]
# returnstd.test=returnstd[test.ind,]
# mu.train=rep(1,p)
# mu.test=rep(1,p)
# cov.train=cov(returnstd.train)
# cov.test=cov(returnstd.test)
# lambda.grid=seq(max(abs(mu.train))/100,max(abs(mu.train)),length=101)[2:101]
# l.lambda=length(lambda.grid)
# cv.l.error=NULL
# for(i in 1:l.lambda){
#   lmd=lambda.grid[i]
#   print(i)
#   lin.train=linfun1(cov.train,mu.train,lmd)
#   error=sum((cov.test%*%lin.train-mu.test)^2)
#   cv.l.error=c(cv.l.error,error)
# }
# plot(lambda.grid,cv.l.error)
# lmd1=min(lambda.grid[which(cv.l.error==min(cv.l.error))])
# lmd1

###### CV for tuning lambda in estimation Sigma^-1 1 using the first 500 data ######
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)+1
# block.start=1+(0:(n.block-1))*B
# test.block=sort(sample(1:n.block,floor(n.block/4)))
# test.ind=NULL
# for(k in test.block){
#   test.ind=c(test.ind,block.start[k]:(min(block.start[k]+B-1,n)))
# }
# n.test=length(test.ind)
# train.ind=setdiff(1:n,test.ind)
# n.train=length(train.ind)
# returnstd.train=returnstd[1:500,][train.ind,]
# returnstd.test=returnstd[1:500,][test.ind,]
# mu.train=rep(1,p)
# mu.test=rep(1,p)
# cov.train=cov(returnstd.train)
# cov.test=cov(returnstd.test)
# lambda.grid=seq(0.25,max(abs(mu.train)),length=101)[2:101]
# l.lambda=length(lambda.grid)
# cv.l.error=NULL
# for(i in 1:l.lambda){
#   lmd=lambda.grid[i]
#   print(i)
#   lin.train=linfun1(cov.train,mu.train,lmd)
#   error=sum((cov.test%*%lin.train-mu.test)^2)
#   cv.l.error=c(cv.l.error,error)
# }
# plot(lambda.grid,cv.l.error)
# lmd1=min(lambda.grid[which(cv.l.error==min(cv.l.error))])
# lmd1 
lmd1 <- 0.3025

###### CV for tuning lambda in estimation Sigma^-1 mu, using the first 500 data ######
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)+1
# block.start=1+(0:(n.block-1))*B
# test.block=sort(sample(1:n.block,floor(n.block/4)))
# test.ind=NULL
# for(k in test.block){
#   test.ind=c(test.ind,block.start[k]:(min(block.start[k]+B-1,n)))
# }
# n.test=length(test.ind)
# train.ind=setdiff(1:n,test.ind)
# n.train=length(train.ind)
# returnstd.train=returnstd[1:500,][train.ind,]
# returnstd.test=returnstd[1:500,][test.ind,]
# mu.train=colMeans(returnstd.train)
# mu.test=colMeans(returnstd.test)
# cov.train=cov(returnstd.train)
# cov.test=cov(returnstd.test)
# lambda.grid=seq(max(abs(mu.train))/100,max(abs(mu.train)),length=101)[2:101]
# l.lambda=length(lambda.grid)
# cv.l.error=NULL
# for(i in 1:l.lambda){
#   lmd=lambda.grid[i]
#   print(i)
#   print(lmd)
#   lin.train=linfun1(cov.train,mu.train,lmd)
#   #	lin.test=linfun(cov.test,mu.test,lmd)
#   error=sum((cov.test%*%lin.train-mu.test)^2)
#   cv.l.error=c(cv.l.error,error)
#   #print(i)
# }
# plot(lambda.grid,cv.l.error)
# lmd2=min(lambda.grid[which(cv.l.error==min(cv.l.error))])
# lmd2 
lmd2 <- 0.001653768

###### CV for tuning lambda in estimation Sigma^-1 phi, using the first 500 data ######
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)+1
# block.start=1+(0:(n.block-1))*B
# test.block=sort(sample(1:n.block,floor(n.block/4)))
# test.ind=NULL
# for(k in test.block){
#   test.ind=c(test.ind,block.start[k]:(min(block.start[k]+B-1,n)))
# }
# n.test=length(test.ind)
# train.ind=setdiff(1:n,test.ind)
# n.train=length(train.ind)
# returnstd.train=returnstd[1:500,][train.ind,]
# returnstd.test=returnstd[1:500,][test.ind,]
# # train centrality
# network.train = network.portfolio(returnstd.train)
# mu.train <- eigen_centrality(network.train,directed = FALSE, scale = TRUE)$vector
# # test centrality
# network.test = network.portfolio(returnstd.test)
# mu.test <- eigen_centrality(network.test,directed = FALSE, scale = TRUE)$vector
# cov.train=cov(returnstd.train)
# cov.test=cov(returnstd.test)
# lambda.grid=seq(0.07,max(abs(mu.train)),length=101)[2:101]
# l.lambda=length(lambda.grid)
# cv.l.error=NULL
# for(i in 1:l.lambda){
#   lmd=lambda.grid[i]
#   print(i)
#   print(lmd)
#   lin.train=linfun1(cov.train,mu.train,lmd)
#   #	lin.test=linfun(cov.test,mu.test,lmd)
#   error=sum((cov.test%*%lin.train-mu.test)^2)
#   cv.l.error=c(cv.l.error,error)
#   #print(i)
# }
# plot(lambda.grid,cv.l.error)
# lmd3=min(lambda.grid[which(cv.l.error==min(cv.l.error))])
# lmd3 
lmd3 <- 0.1816

##### Dantzig selector estimation for theta1, theta2, theta3 #####
#      theta1 <- Sigma^-1 1
#      theta2 <- Sigma^-1 mu
#      theta3 <- Sigma^-1 phi
# theta1<-list()
# theta2<-list()
# theta3<-list()
# # for(t in 6: length(W_in)){
# for(t in 1: 5){
#   print(t)
#   ptm<-proc.time()
#   ## compute global minimum variance portfolio ##
#   theta1[[t]] =linfun1(COV_in[[t]],rep(1,p),lambda=lmd1) # lambda <= 0.1 will lead to be infeasible
#   print('theta1')
#   theta2[[t]] =linfun1(COV_in[[t]],ER_in[[t]],lambda=lmd2) # lambda <= 0.1 will lead to be infeasible
#   print('theta2')
#   theta3[[t]] =linfun1(COV_in[[t]],EC_in[[t]],lambda=lmd3) # lambda <= 0.1 will lead to be infeasible
#   ptm<-proc.time()-ptm
#   print(ptm)
# }
# theta<-list("theta1"=theta1,
#             "theta2"=theta2,
#             "theta3"=theta3)
# save(theta,file="theta_Dantzig.RData")

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
return_minVar_Dantzig<-as.matrix(cbind(unlist(cumureturn_minVar_Dantzig)))
cumureturn_minVar_Dantzig<-cumsum(return_minVar_Dantzig)
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
return_minVar<-as.matrix(cbind(unlist(cumureturn_minVar)))
cumureturn_minVar<-cumsum(return_minVar)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_minVar<-w

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
# cumureturn_minVar_Dantzig_noshort<-cumsum(return_minVar_Dantzig_noshort)
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
return_minVar_noshort<-as.matrix(cbind(unlist(cumureturn_minVar_noshort)))
cumureturn_minVar_noshort<-cumsum(return_minVar_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_minVar_noshort<-w

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
return_meanVar<-as.matrix(cbind(unlist(cumureturn_meanVar)))
cumureturn_meanVar<-cumsum(return_meanVar)
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
return_meanVar_Dantzig<-as.matrix(cbind(unlist(cumureturn_meanVar_Dantzig)))
cumureturn_meanVar_Dantzig<-cumsum(return_meanVar_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_meanVar_Dantzig<-w

###### mean variance portfolio with plug in no short ######
w<-list()
cumureturn_meanVar_noshort<-list()
for(t in 1: length(W_in)){
  portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]),shorts = FALSE)
  w[[(t)]] =portf_meanVar$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_meanVar_noshort[[t]]<-rowSums(aus)
}
return_meanVar_noshort<-as.matrix(cbind(unlist(cumureturn_meanVar_noshort)))
cumureturn_meanVar_noshort<-cumsum(return_meanVar_noshort)
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
# cumureturn_meanVar_Dantzig_noshort<-cumsum(return_meanVar_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_meanVar_Dantzig_noshort<-w

##### equally weighted portfolio #####
w<-list()
cumureturn_temporal<-list()
centrality_equal_portfolio<-list()
for(t in 1: length(W_in)){
  w[[(t)]] =matrix(1/465,1,465)
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
  centrality_equal_portfolio[[t]]<-as.double(w[[(t)]]%*%EC_in[[(t)]])
}
return_equal<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_equal<-cumsum(return_equal)
centrality_equal_portfolio<-as.matrix(cbind(unlist(centrality_equal_portfolio)))
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_equal<-w

##### network portfolio 1 constraint #####
### default setting centrality constraint as mean centrality ###
###### network portfolio fixed constraint as mean centrality with plug in ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],0.8)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_1constraint<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_1constraint<-cumsum(return_network)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint<-w

###### network portfolio fixed constraint as mean centrality with Dantzig ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  # net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],0.8)
  alpha=(sum(theta3[[t]])*sum(theta1[[t]])*mean(EC_in[[t]])-(sum(theta3[[t]]))^2)/(EC_in[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2)
  w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
  # w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_1constraint_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_1constraint_Dantzig<-cumsum(return_network_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint_Dantzig<-w

###### network portfolio fixed constraint as mean centrality with plug in no short ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],mean(EC_in[[(t)]]),FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
  # print(t)
}
return_network_1constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_1constraint_noshort<-cumsum(return_network_1constraint_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint_noshort<-w

# ###### network portfolio fixed constraint as mean centrality with Dantzig no short ######
# w<-list()
# cumureturn_temporal<-list()
# for(t in 1: length(W_in)){
#   ## compute global minimum variance portfolio without short ##
#   net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],mean(EC_in[[(t)]]),FALSE)
#   w[[(t)]] =net.gmin.port$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
#   # print(t)
# }
# return_network_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_Dantzig_noshort<-cumsum(return_network_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_1constraint_Dantzig_noshort<-w

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
    net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),TRUE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi[[i]]<-cumsum(return_network_vary_with_phi[[i]])
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
    alpha=(sum(theta3[[t]])*sum(theta1[[t]])*quantile(EC_in[[(t)]],quantl[i])-(sum(theta3[[t]]))^2)/(EC_in[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2)
    w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_Dantzig[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_Dantzig[[i]]<-cumsum(return_network_vary_with_phi_Dantzig[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_Dantzig[[i]]<-w
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
    net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),FALSE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_noshort[[i]]<-cumsum(return_network_vary_with_phi_noshort[[i]])
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
#     net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),FALSE)
#     w[[(t)]] =net.gmin.port$weights
#     aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#     cumureturn_temporal[[t]]<-rowSums(aus)
#   }
#   return_network_vary_with_phi_Dantzig_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
#   cumureturn_network_vary_with_phi_Dantzig_noshort[[i]]<-cumsum(return_network_vary_with_phi_noshort[[i]])
#   w<-t(matrix(unlist(w),p,T.windows))
#   colnames(w) = node.label
#   w_network_vary_with_phi_Dantzig_noshort[[i]]<-w
# }

###### network portfolio data-driven constraint with plug in ######
cumureturn_network_datadriven_phistar<-list()
return_network_datadriven_phistar<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],phi_star[t],TRUE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar<-cumsum(return_network_datadriven_phistar)
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
  alpha=(sum(theta3[[t]])*sum(theta1[[t]])*phi_star[t]-(sum(theta3[[t]]))^2)/(EC_in[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2)
  w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
  aus <- as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]] <- rowSums(aus)
}
return_network_datadriven_phistar_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_Dantzig<-cumsum(return_network_datadriven_phistar_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_Dantzig<-w

###### network portfolio data-driven constraint no short ######
cumureturn_network_datadriven_phistar_noshort<-list()
return_network_datadriven_phistar_noshort<-list()
cumureturn_temporal<-list()
phi_star<-centrality_equal_portfolio
w<-list()
for(t in 1: length(W_in)){
  ## compute network portfolio data-driven constraint no short ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],phi_star[t],FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_noshort<-cumsum(return_network_datadriven_phistar_noshort)
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
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[(t)]], COV_in[[(t)]],mean(EC_in[[(t)]]),mean(ER_in[[(t)]]))
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_2constraint<-cumsum(return_network_2constraint)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint<-w

###### network portfolio fixed constraint with Dantzig ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute 2 constraint network portfolio ##
  M1 <- cbind(rbind(1,mean(EC_in[[(t)]]),mean(ER_in[[(t)]])),
              rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
              rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
  M2 <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
              rbind(1,mean(EC_in[[(t)]]),mean(ER_in[[(t)]])),
              rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
  M <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
             rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
             rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
  gamma1 <- det(M1)/det(M)
  gamma2 <- det(M2)/det(M)
  alpha1 <- gamma1*sum(theta3[[t]])
  alpha2 <- gamma2*sum(theta2[[t]])
  w1 <- (1-alpha1-alpha2)*theta1[[t]]/sum(theta1[[t]])
  w2 <- alpha1*theta3[[t]]/sum(theta3[[t]])
  w3 <- alpha2*theta2[[t]]/sum(theta2[[t]])
  w[[(t)]] = w1 +  w2 + w3
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_2constraint_Dantzig<-cumsum(return_network_2constraint_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint_Dantzig<-w

###### network portfolio fixed constraint with plug in no short ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],mean(EC_in[[(t)]]),mean(ER_in[[t]]),FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_2constraint_noshort<-cumsum(return_network_2constraint_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint_noshort<-w

# ###### network portfolio fixed constraint with Dantzig no short ######
# w<-list()
# cumureturn_temporal<-list()
# for(t in 1: length(W_in)){
#   ## compute global minimum variance portfolio without short ##
#   net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],mean(EC_in[[(t)]]),mean(ER_in[[t]]),FALSE)
#   w[[(t)]] =net.gmin.port$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
# }
# return_network_2constraint_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_2constraint_Dantzig_noshort<-cumsum(return_network_2constraint_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_2constraint_Dantzig_noshort<-w


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
    net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[t]]),TRUE)
    # net.gmin.port = network.efficient.portfolio(EC.posi_in[[(t)]], COV_in[[(t)]],phi_star[i],TRUE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_2constraint[[i]]<-cumsum(return_network_vary_with_phi_2constraint[[i]])
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
    
    M1 <- cbind(rbind(1,quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[(t)]])),
                rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
    M2 <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                rbind(1,quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[(t)]])),
                rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
    M <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
               rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
               rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
    gamma1 <- det(M1)/det(M)
    gamma2 <- det(M2)/det(M)
    alpha1 <- gamma1*sum(theta3[[t]])
    alpha2 <- gamma2*sum(theta2[[t]])
    w1 <- (1-alpha1-alpha2)*theta1[[t]]/sum(theta1[[t]])
    w2 <- alpha1*theta3[[t]]/sum(theta3[[t]])
    w3 <- alpha2*theta2[[t]]/sum(theta2[[t]])
    w[[(t)]] = w1 +  w2 + w3
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint_Dantzig[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_2constraint_Dantzig[[i]]<-cumsum(return_network_vary_with_phi_2constraint_Dantzig[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_2constraint_Dantzig[[i]]<-w
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
    net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[t]]),FALSE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_2constraint_noshort[[i]]<-cumsum(return_network_vary_with_phi_2constraint_noshort[[i]])
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
#     net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[t]]),FALSE)
#     w[[(t)]] =net.gmin.port$weights
#     aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#     cumureturn_temporal[[t]]<-rowSums(aus)
#   }
#   return_network_vary_with_phi_2constraint_Dantzig_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
#   cumureturn_network_vary_with_phi_2constraint_Dantzig_noshort[[i]]<-cumsum(return_network_vary_with_phi_2constraint_Dantzig_noshort[[i]])
#   w<-t(matrix(unlist(w),p,T.windows))
#   colnames(w) = node.label
#   w_network_vary_with_phi_2constraint_Dantzig_noshort[[i]]<-w
# }

###### network portfolio data-driven constraint with plug in ######
cumureturn_network_datadriven_phistar_2constraint<-list()
return_network_datadriven_phistar_2constraint<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER_in[[t]]),TRUE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_2constraint<-cumsum(return_network_datadriven_phistar_2constraint)
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
  M1 <- cbind(rbind(1,phi_star[t],mean(ER_in[[(t)]])),
              rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
              rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
  M2 <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
              rbind(1,phi_star[t],mean(ER_in[[(t)]])),
              rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
  M <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
             rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
             rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
  gamma1 <- det(M1)/det(M)
  gamma2 <- det(M2)/det(M)
  alpha1 <- gamma1*sum(theta3[[t]])
  alpha2 <- gamma2*sum(theta2[[t]])
  w1 <- (1-alpha1-alpha2)*theta1[[t]]/sum(theta1[[t]])
  w2 <- alpha1*theta3[[t]]/sum(theta3[[t]])
  w3 <- alpha2*theta2[[t]]/sum(theta2[[t]])
  w[[(t)]] = w1 +  w2 + w3
  
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_2constraint_Dantzig<-cumsum(return_network_datadriven_phistar_2constraint_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_2constraint_Dantzig<-w

###### network portfolio data-driven constraint with plug in no short ######
cumureturn_network_datadriven_phistar_2constraint_noshort<-list()
return_network_datadriven_phistar_2constraint_noshort<-list()
cumureturn_temporal<-list()
phi_star<-centrality_equal_portfolio
w<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER_in[[t]]),FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_2constraint_noshort<-cumsum(return_network_datadriven_phistar_2constraint_noshort)
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
#   net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER_in[[t]]),FALSE)
#   w[[(t)]] =net.gmin.port$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
# }
# return_network_datadriven_phistar_2constraint_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_datadriven_phistar_2constraint_Dantzig_noshort<-cumsum(return_network_datadriven_phistar_2constraint_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_datadriven_phistar_2constraint_Dantzig_noshort<-w

#### Save portfolios ####
Portfolio.Scenario <- list("return_minVar",return_minVar,"cumureturn_minVar",cumureturn_minVar,"w_minVar",w_minVar,
                           "return_meanVar",return_meanVar,"cumureturn_meanVar",cumureturn_meanVar,"w_meanVar",w_meanVar,
                           "return_equal",return_equal,"cumureturn_equal",cumureturn_equal,"w_equal",w_equal,
                           "return_minVar_noshort",return_minVar_noshort,"cumureturn_minVar_noshort",cumureturn_minVar_noshort,"w_minVar_noshort",w_minVar_noshort,
                           "return_meanVar_noshort",return_meanVar_noshort,"cumureturn_meanVar_noshort",cumureturn_meanVar_noshort,"w_meanVar_noshort",w_meanVar_noshort,
                           "return_network_1constraint",return_network_1constraint,"cumureturn_network_1constraint",cumureturn_network_1constraint,"w_network_1constraint",w_network_1constraint,
                           "return_network_1constraint_noshort",return_network_1constraint_noshort,"cumureturn_network_1constraint_noshort",cumureturn_network_1constraint_noshort,"w_network_1constraint_noshort",w_network_1constraint_noshort,
                           "return_network_vary_with_phi",return_network_vary_with_phi,"cumureturn_network_vary_with_phi",cumureturn_network_vary_with_phi,"w_network_vary_with_phi",w_network_vary_with_phi,
                           "return_network_vary_with_phi_noshort",return_network_vary_with_phi_noshort,"cumureturn_network_vary_with_phi_noshort",cumureturn_network_vary_with_phi_noshort,"w_network_vary_with_phi_noshort",w_network_vary_with_phi_noshort,
                           "return_network_datadriven_phistar",return_network_datadriven_phistar,"cumureturn_network_datadriven_phistar",cumureturn_network_datadriven_phistar,"w_network_datadriven_phistar",w_network_datadriven_phistar,
                           "return_network_datadriven_phistar_noshort",return_network_datadriven_phistar_noshort,"cumureturn_network_datadriven_phistar_noshort",cumureturn_network_datadriven_phistar_noshort,"w_network_datadriven_phistar_noshort",w_network_datadriven_phistar_noshort,
                           "return_network_2constraint",return_network_2constraint,"cumureturn_network_2constraint",cumureturn_network_2constraint,"w_network_2constraint",w_network_2constraint,
                           "return_network_2constraint_noshort",return_network_2constraint_noshort,"cumureturn_network_2constraint_noshort",cumureturn_network_2constraint_noshort,"w_network_2constraint_noshort",w_network_2constraint_noshort,
                           "return_network_vary_with_phi_2constraint",return_network_vary_with_phi,"cumureturn_network_vary_with_phi_2constraint",cumureturn_network_vary_with_phi_2constraint,"w_network_vary_with_phi_2constraint",w_network_vary_with_phi_2constraint,
                           "return_network_vary_with_phi_2constraint_noshort",return_network_vary_with_phi_2constraint_noshort,"cumureturn_network_vary_with_phi_2constraint_noshort",cumureturn_network_vary_with_phi_2constraint_noshort,"w_network_vary_with_phi_2constraint_noshort",w_network_vary_with_phi_2constraint_noshort,
                           "return_network_datadriven_phistar_2constraint",return_network_datadriven_phistar_2constraint,"cumureturn_network_datadriven_phistar_2constraint",cumureturn_network_datadriven_phistar_2constraint,"w_network_datadriven_phistar_2constraint",w_network_datadriven_phistar_2constraint,
                           "return_network_datadriven_phistar_2constraint_noshort",return_network_datadriven_phistar_2constraint_noshort,"cumureturn_network_datadriven_phistar_2constraint_noshort",cumureturn_network_datadriven_phistar_2constraint_noshort,"w_network_datadriven_phistar_2constraint_noshort",w_network_datadriven_phistar_2constraint_noshort,
                           "return_minVar_Dantzig",return_minVar_Dantzig,"cumureturn_minVar_Dantzig",cumureturn_minVar_Dantzig,"w_minVar_Dantzig",w_minVar_Dantzig,
                           "return_meanVar_Dantzig",return_meanVar_Dantzig,"cumureturn_meanVar_Dantzig",cumureturn_meanVar_Dantzig,"w_meanVar_Dantzig",w_meanVar_Dantzig,
                           "return_network_1constraint_Dantzig",return_network_1constraint_Dantzig,"cumureturn_network_1constraint_Dantzig",cumureturn_network_1constraint_Dantzig,"w_network_1constraint_Dantzig",w_network_1constraint_Dantzig,
                           "return_network_2constraint_Dantzig",return_network_2constraint_Dantzig,"cumureturn_network_2constraint_Dantzig",cumureturn_network_2constraint_Dantzig,"w_network_2constraint_Dantzig",w_network_2constraint_Dantzig)
save(Portfolio.Scenario,file="Portfolios.RData")

#### Figures and animations output ####
##### Figure: cumulative returns of 3 strategies #####
### minVar, minVar no short, equal: (color) 1:3 ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal)
),
order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_3_strategies",".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[1:3], ylab="", xlab="")
# legend("topleft",legend=c("minVar", "minVar no short", "Equal"),
# col=Colors_Ret[1:3], lty=1:2, cex=0.8)
dev.off()
##### Figure: cumulative returns of 5 strategies #####
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7  ###
Colors_Ret<-rainbow(10)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_meanVar),
                  data.frame(cumureturn_meanVar_noshort)
),
order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_5_strategies",".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7)], ylab="", xlab="",title=paste0("Cumulate return with network Dantzig"))
legend("topleft",legend=c("minVar", "minVar no short", "Equal", "meanVar", "meanVar noshort"),
       col=Colors_Ret[c(1:3,4,7)], lty=1, cex=0.8)
dev.off()
##### Animation: cumulative returns with varying constraint of 5 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network varying constraint: (color) 9 ###
### network with phistar 0.8 no short: (color) 5  ###
Colors_Ret<-rainbow(12)
# phi_star<-seq(1,0,-0.05)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
  # col=Colors_Ret[1:5], lty=1, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/pcumulative_return_5_strategies_varying_constraint.gif"))
###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistat 0.8: (color) 9 ###
### network with varying constraint no short: (color) 5  ###
Colors_Ret<-rainbow(12)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
  # col=Colors_Ret[1:5], lty=1:2, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_5_strategies_noshort_varying_constraint.gif"))

##### Animation: cumulative returns with varying constraint of 7 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network varying constraint: (color) 9 ###
### network with phistar 0.8 no short: (color) 5   ###
Colors_Ret<-rainbow(12)
# phi_star<-seq(1,0,-0.05)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="")
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
  # col=Colors_Ret[1:5], lty=1:2, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_7_strategies_varying_constraint.gif"))
###### network noshort varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network with phistat 0.8: (color) 9  ###
### network with varying constraint no short: (color) 5   ###
Colors_Ret<-rainbow(12)
# phi_star<-seq(0.9,0.6,-0.05)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="")
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"), col=Colors_Ret[1:5], lty=1, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_7_strategies_noshort_varying_constraint.gif"))

##### Figures: cumulative returns with varying constraint of 5 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network varying constraint: (color) 9 ###
### network with phistar 0.8 no short: (color) 5   ###
# phi_star<-seq(1,0,-0.05)
Colors_Ret<-rainbow(12)
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/5_strategies_varying_constraint/cumulative_return_with_constraint_quantile",quantl[i]*100,".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal", "network", "network no short"),
         col=Colors_Ret[c(1:3,9,5)], lty=1, cex=0.8)
  dev.off()
}

###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar 0.8: (color) 9 ###
### network varying constraint no short: (color) 5 ###
# phi_star<-seq(0.9,0.6,-0.05)
Colors_Ret<-rainbow(12)
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/5_strategies_noshort_varying_constraint/cumulative_return_with_constraint_quantile",quantl[i]*100,".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal", "network", "network no short"),
         col=Colors_Ret[c(1:3,9,5)], lty=1, cex=0.8)
  dev.off()
}

##### Figures: cumulative returns with varying constraint of 7 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network varying constraint: (color) 9  ###
### network with phistar 0.8 no short: (color) 5   ###
Colors_Ret<-rainbow(12)
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/7_strategies_varying_constraint/cumulative_return_with_constraint_quantile",quantl[i]*100,".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal","meanVar", "meanVar no short",  "network", "network no short"),
         col=Colors_Ret[c(1:3,4,7,9,5)], lty=1, cex=0.8)
  dev.off()
}
###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar 0.8: (color) 9 ###
### network varying constraint no short: (color) 5 ###
# phi_star<-seq(0.9,0.6,-0.05)
Colors_Ret<-rainbow(12)
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/7_strategies_noshort_varying_constraint/cumulative_return_with_constraint_quantile",quantl[i],".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal","meanVar", "meanVar no short", "network", "network no short"),
         col=Colors_Ret[c(1:3,4,7,9,5)], lty=1, cex=0.8)
  dev.off()
}

##### Figure: cumulative returns with 4 network constraints of 5 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar=0.6:0.1:0.9: (color) 9,8,10,11 ###
### network with phistar 0.8 no short: (color) 5  ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
# phi_star<-seq(1,0,-0.05)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi[[3]]),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi[[7]]),
                  data.frame(cumureturn_network_vary_with_phi[[9]]),
                  data.frame(cumureturn_network_noshort)),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_5_strategies_with_constraint_quantile",
                quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,"_",quantl[9]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal",
                          paste("network","quantile=",quantl[3]),paste("network","quantile=",quantl[5]),
                          paste("network","quantile=",quantl[7]),paste("network","quantile=",quantl[9]),
                          "network no short"),
       col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()
###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar 0.8: (color) 9  ###
### network with phistar=0.6:0.1:0.9 no short: (color) 8,10,11,5   ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
# phi_star<-seq(0.9,0.6,-0.05)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[7]])
),
order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_5_strategies_noshort_with_constraint_quantile",
                quantl[1]*100,"_",quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal",paste("network","mean centrality constraint"),
                          paste("network","noshort","quantile=",quantl[1]),paste("network","noshort","quantile=",quantl[3]),
                          paste("network","noshort","quantile=",quantl[5]),paste("network","noshort","quantile=",quantl[7])
),
col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()

##### Figure: cumulative returns with 4 network constraints of 7 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network with phistar=0.6:0.1:0.9: (color) 9,8,10,11  ###
### network with phistar 0.8 no short: (color) 5  ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
# phi_star<-seq(1,0,-0.05)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_meanVar),
                  data.frame(cumureturn_meanVar_noshort),
                  data.frame(cumureturn_network_vary_with_phi[[3]]),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi[[7]]),
                  data.frame(cumureturn_network_vary_with_phi[[9]]),
                  data.frame(cumureturn_network_noshort)),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_7_strategies_with_constraint_quantile",
                quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,"_",quantl[9]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal","meanVar", "meanVar without short",
                          paste("network","quantile=",quantl[3]),paste("network","quantile=",quantl[5]),
                          paste("network","quantile=",quantl[7]),paste("network","quantile=",quantl[9]),
                          "network noshort"),
       col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()
###### network noshort varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network with phistar 0.8: (color) 9 ###
### network with phistar=0.6:0.1:0.9 no short: (color) 8,10,11,5 ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
# phi_star<-seq(0.9,0.6,-0.05)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_meanVar),
                  data.frame(cumureturn_meanVar_noshort),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[7]])
),
order.by=as.Date(as.character(date), format="%Y-%m-%d")
)
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_7_strategies_noshort_with_constraint_quantile",
                quantl[1],"_",quantl[3],"_",quantl[5],"_",quantl[7],".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal","meanVar", "meanVar without short",
                          "network mean centrality constraint",
                          paste("network noshort","quantile=",quantl[1]),paste("network noshort","quantile=",quantl[3]),
                          paste("network noshort","quantile=",quantl[5]),paste("network noshort","quantile=",quantl[7])
),
col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()

#### Portfolio performance ####
n=length(cumureturn_meanVar)
##### (Annual) cumulative returns #####
cumureturn_minVar[n]/n*252
cumureturn_meanVar[n]/n*252
cumureturn_equal[n]/n*252
cumureturn_network_vary_with_phi[[1]][n]/n*252
cumureturn_network[n]/n*252
cumureturn_network_vary_with_phi[[9]][n]/n*252
cumureturn_network_vary_with_phi_2constraint[[1]][n]/n*252
cumureturn_network_2constraint[n]/n*252
cumureturn_network_vary_with_phi_2constraint[[9]][n]/n*252
cumureturn_minVar_noshort[n]/n*252
cumureturn_meanVar_noshort[n]/n*252
cumureturn_network_vary_with_phi_noshort[[1]][n]/n*252
cumureturn_network_noshort[n]/n*252
cumureturn_network_vary_with_phi_noshort[[9]][n]/n*252
cumureturn_network_vary_with_phi_2constraint_noshort[[1]][n]/n*252
cumureturn_network_2constraint_noshort[n]/n*252
cumureturn_network_vary_with_phi_2constraint_noshort[[9]][n]/n*252

n=length(cumureturn_minVar_Dantzig)
cumureturn_minVar_Dantzig[n]/n*252
cumureturn_meanVar_Dantzig[n]/n*252
cumureturn_network_vary_with_phi_Dantzig[[1]][n]/n*252
cumureturn_network_1constraint_Dantzig[n]/n*252
cumureturn_network_vary_with_phi_Dantzig[[9]][n]/n*252
cumureturn_network_vary_with_phi_2constraint_Dantzig[[1]][n]/n*252
cumureturn_network_2constraint_Dantzig[n]/n*252
cumureturn_network_vary_with_phi_2constraint_Dantzig[[9]][n]/n*252

# cumureturn_network_datadriven_phistar[n]/n*252
# cumureturn_network_datadriven_phistar_noshort[n]/n*252
# cumureturn_network_vary_with_phi[[1]][n]/n*252
# cumureturn_network_vary_with_phi[[2]][n]/n*252
# cumureturn_network_vary_with_phi[[3]][n]/n*252
# cumureturn_network_vary_with_phi[[4]][n]/n*252
# cumureturn_network_vary_with_phi[[5]][n]/n*252
# cumureturn_network_vary_with_phi[[6]][n]/n*252
# cumureturn_network_vary_with_phi[[7]][n]/n*252
# cumureturn_network_vary_with_phi[[8]][n]/n*252
# cumureturn_network_vary_with_phi[[9]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[1]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[2]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[3]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[4]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[5]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[6]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[7]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[8]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[9]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[1]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[2]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[3]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[4]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[5]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[6]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[7]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[8]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[9]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[1]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[2]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[3]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[4]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[5]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[6]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[7]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[8]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[9]][n]/n*252

##### std #####
std(return_minVar)*sqrt(252)
std(return_meanVar)*sqrt(252)
std(return_equal)*sqrt(252)
std(return_network_vary_with_phi[[1]])*sqrt(252)
std(return_network)*sqrt(252)
std(return_network_vary_with_phi[[9]])*sqrt(252)
std(return_network_vary_with_phi_2constraint[[1]])*sqrt(252)
std(return_network_2constraint)*sqrt(252)
std(return_network_vary_with_phi_2constraint[[9]])*sqrt(252)
std(return_minVar_noshort)*sqrt(252)
std(return_meanVar_noshort)*sqrt(252)
std(return_network_vary_with_phi_noshort[[1]])*sqrt(252)
std(return_network_noshort)*sqrt(252)
std(return_network_vary_with_phi_noshort[[9]])*sqrt(252)
std(return_network_vary_with_phi_2constraint_noshort[[1]])*sqrt(252)
std(return_network_2constraint_noshort)*sqrt(252)
std(return_network_vary_with_phi_2constraint_noshort[[9]])*sqrt(252)

std(return_minVar_Dantzig)*sqrt(252)
std(return_meanVar_Dantzig)*sqrt(252)
std(return_network_vary_with_phi_Dantzig[[1]])*sqrt(252)
std(return_network_1constraint_Dantzig)*sqrt(252)
std(return_network_vary_with_phi_Dantzig[[9]])*sqrt(252)
std(return_network_vary_with_phi_2constraint_Dantzig[[1]])*sqrt(252)
std(return_network_2constraint_Dantzig)*sqrt(252)
std(return_network_vary_with_phi_2constraint_Dantzig[[9]])*sqrt(252)

# std(return_network_vary_with_phi[[1]])*sqrt(252)
# std(return_network_vary_with_phi[[2]])*sqrt(252)
# std(return_network_vary_with_phi[[3]])*sqrt(252)
# std(return_network_vary_with_phi[[4]])*sqrt(252)
# std(return_network_vary_with_phi[[5]])*sqrt(252)
# std(return_network_vary_with_phi[[6]])*sqrt(252)
# std(return_network_vary_with_phi[[7]])*sqrt(252)
# std(return_network_vary_with_phi[[8]])*sqrt(252)
# std(return_network_vary_with_phi[[9]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[1]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[2]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[3]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[4]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[5]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[6]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[7]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[8]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[9]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[1]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[2]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[3]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[4]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[5]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[6]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[7]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[8]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[9]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[1]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[2]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[3]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[4]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[5]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[6]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[7]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[8]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[9]])*sqrt(252)

# sharpe ratio
SharpeRatio(return_minVar)
SharpeRatio(return_meanVar)
SharpeRatio(return_equal)
SharpeRatio(return_network_vary_with_phi[[1]])
SharpeRatio(return_network)
SharpeRatio(return_network_vary_with_phi[[9]])
SharpeRatio(return_network_vary_with_phi_2constraint[[1]])
SharpeRatio(return_network_2constraint)
SharpeRatio(return_network_vary_with_phi_2constraint[[9]])
SharpeRatio(return_minVar_noshort)
SharpeRatio(return_meanVar_noshort)
SharpeRatio(return_network_vary_with_phi_noshort[[1]])
SharpeRatio(return_network_noshort)
SharpeRatio(return_network_vary_with_phi_noshort[[9]])
SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[1]])
SharpeRatio(return_network_2constraint_noshort)
SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[1]])

SharpeRatio(return_minVar_Dantzig)
SharpeRatio(return_meanVar_Dantzig)
SharpeRatio(return_network_vary_with_phi_Dantzig[[1]])
SharpeRatio(return_network_1constraint_Dantzig)
SharpeRatio(return_network_vary_with_phi_Dantzig[[9]])
SharpeRatio(return_network_vary_with_phi_2constraint_Dantzig[[1]])
SharpeRatio(return_network_2constraint_Dantzig)
SharpeRatio(return_network_vary_with_phi_2constraint_Dantzig[[9]])

# SharpeRatio(return_network_vary_with_phi[[1]])
# SharpeRatio(return_network_vary_with_phi[[2]])
# SharpeRatio(return_network_vary_with_phi[[3]])
# SharpeRatio(return_network_vary_with_phi[[4]])
# SharpeRatio(return_network_vary_with_phi[[5]])
# SharpeRatio(return_network_vary_with_phi[[6]])
# SharpeRatio(return_network_vary_with_phi[[7]])
# SharpeRatio(return_network_vary_with_phi[[8]])
# SharpeRatio(return_network_vary_with_phi[[9]])
# SharpeRatio(return_network_vary_with_phi_noshort[[1]])
# SharpeRatio(return_network_vary_with_phi_noshort[[2]])
# SharpeRatio(return_network_vary_with_phi_noshort[[3]])
# SharpeRatio(return_network_vary_with_phi_noshort[[4]])
# SharpeRatio(return_network_vary_with_phi_noshort[[5]])
# SharpeRatio(return_network_vary_with_phi_noshort[[6]])
# SharpeRatio(return_network_vary_with_phi_noshort[[7]])
# SharpeRatio(return_network_vary_with_phi_noshort[[8]])
# SharpeRatio(return_network_vary_with_phi_noshort[[9]])
# SharpeRatio(return_network_vary_with_phi_2constraint[[1]])
# SharpeRatio(return_network_vary_with_phi_2constraint[[2]])
# SharpeRatio(return_network_vary_with_phi_2constraint[[3]])
# SharpeRatio(return_network_vary_with_phi_2constraint[[4]])
# SharpeRatio(return_network_vary_with_phi_2constraint[[5]])
# SharpeRatio(return_network_vary_with_phi_2constraint[[6]])
# SharpeRatio(return_network_vary_with_phi_2constraint[[7]])
# SharpeRatio(return_network_vary_with_phi_2constraint[[8]])
# SharpeRatio(return_network_vary_with_phi_2constraint[[9]])
# SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[1]])
# SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[2]])
# SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[3]])
# SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[4]])
# SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[5]])
# SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[6]])
# SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[7]])
# SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[8]])
# SharpeRatio(return_network_vary_with_phi_2constraint_noshort[[9]])

##### skewness #####
skewness(return_minVar)
skewness(return_meanVar)
skewness(return_equal)
skewness(return_network_vary_with_phi[[1]])
skewness(return_network)
skewness(return_network_vary_with_phi[[9]])
skewness(return_network_vary_with_phi_2constraint[[1]])
skewness(return_network_2constraint)
skewness(return_network_vary_with_phi_2constraint[[9]])
skewness(return_minVar_noshort)
skewness(return_meanVar_noshort)
skewness(return_network_vary_with_phi_noshort[[1]])
skewness(return_network_noshort)
skewness(return_network_vary_with_phi_noshort[[9]])
skewness(return_network_vary_with_phi_2constraint_noshort[[1]])
skewness(return_network_2constraint_noshort)
skewness(return_network_vary_with_phi_2constraint_noshort[[9]])

skewness(return_minVar_Dantzig)
skewness(return_meanVar_Dantzig)
skewness(return_network_vary_with_phi_Dantzig[[1]])
skewness(return_network_1constraint_Dantzig)
skewness(return_network_vary_with_phi_Dantzig[[9]])
skewness(return_network_vary_with_phi_2constraint_Dantzig[[1]])
skewness(return_network_2constraint_Dantzig)
skewness(return_network_vary_with_phi_2constraint_Dantzig[[9]])

# skewness(return_network_vary_with_phi[[1]])
# skewness(return_network_vary_with_phi[[2]])
# skewness(return_network_vary_with_phi[[3]])
# skewness(return_network_vary_with_phi[[4]])
# skewness(return_network_vary_with_phi[[5]])
# skewness(return_network_vary_with_phi[[6]])
# skewness(return_network_vary_with_phi[[7]])
# skewness(return_network_vary_with_phi[[8]])
# skewness(return_network_vary_with_phi[[9]])
# skewness(return_network_vary_with_phi_noshort[[1]])
# skewness(return_network_vary_with_phi_noshort[[2]])
# skewness(return_network_vary_with_phi_noshort[[3]])
# skewness(return_network_vary_with_phi_noshort[[4]])
# skewness(return_network_vary_with_phi_noshort[[5]])
# skewness(return_network_vary_with_phi_noshort[[6]])
# skewness(return_network_vary_with_phi_noshort[[7]])
# skewness(return_network_vary_with_phi_noshort[[8]])
# skewness(return_network_vary_with_phi_noshort[[9]])
# skewness(return_network_vary_with_phi_2constraint[[1]])
# skewness(return_network_vary_with_phi_2constraint[[2]])
# skewness(return_network_vary_with_phi_2constraint[[3]])
# skewness(return_network_vary_with_phi_2constraint[[4]])
# skewness(return_network_vary_with_phi_2constraint[[5]])
# skewness(return_network_vary_with_phi_2constraint[[6]])
# skewness(return_network_vary_with_phi_2constraint[[7]])
# skewness(return_network_vary_with_phi_2constraint[[8]])
# skewness(return_network_vary_with_phi_2constraint[[9]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[1]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[2]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[3]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[4]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[5]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[6]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[7]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[8]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[9]])

##### kurtosis #####
kurtosis(return_minVar)
kurtosis(return_meanVar)
kurtosis(return_equal)
kurtosis(return_network_vary_with_phi[[1]])
kurtosis(return_network)
kurtosis(return_network_vary_with_phi[[9]])
kurtosis(return_network_vary_with_phi_2constraint[[1]])
kurtosis(return_network_2constraint)
kurtosis(return_network_vary_with_phi_2constraint[[9]])
kurtosis(return_minVar_noshort)
kurtosis(return_meanVar_noshort)
kurtosis(return_network_vary_with_phi_noshort[[1]])
kurtosis(return_network_noshort)
kurtosis(return_network_vary_with_phi_noshort[[9]])
kurtosis(return_network_vary_with_phi_2constraint_noshort[[1]])
kurtosis(return_network_2constraint_noshort)
kurtosis(return_network_vary_with_phi_2constraint_noshort[[9]])

kurtosis(return_minVar_Dantzig)
kurtosis(return_meanVar_Dantzig)
kurtosis(return_network_vary_with_phi_Dantzig[[1]])
kurtosis(return_network_1constraint_Dantzig)
kurtosis(return_network_vary_with_phi_Dantzig[[9]])
kurtosis(return_network_vary_with_phi_2constraint_Dantzig[[1]])
kurtosis(return_network_2constraint_Dantzig)
kurtosis(return_network_vary_with_phi_2constraint_Dantzig[[9]])

# kurtosis(return_network_vary_with_phi[[1]])
# kurtosis(return_network_vary_with_phi[[2]])
# kurtosis(return_network_vary_with_phi[[3]])
# kurtosis(return_network_vary_with_phi[[4]])
# kurtosis(return_network_vary_with_phi[[5]])
# kurtosis(return_network_vary_with_phi[[6]])
# kurtosis(return_network_vary_with_phi[[7]])
# kurtosis(return_network_vary_with_phi[[8]])
# kurtosis(return_network_vary_with_phi[[9]])
# kurtosis(return_network_vary_with_phi_noshort[[1]])
# kurtosis(return_network_vary_with_phi_noshort[[2]])
# kurtosis(return_network_vary_with_phi_noshort[[3]])
# kurtosis(return_network_vary_with_phi_noshort[[4]])
# kurtosis(return_network_vary_with_phi_noshort[[5]])
# kurtosis(return_network_vary_with_phi_noshort[[6]])
# kurtosis(return_network_vary_with_phi_noshort[[7]])
# kurtosis(return_network_vary_with_phi_noshort[[8]])
# kurtosis(return_network_vary_with_phi_noshort[[9]])
# kurtosis(return_network_vary_with_phi_2constraint[[1]])
# kurtosis(return_network_vary_with_phi_2constraint[[2]])
# kurtosis(return_network_vary_with_phi_2constraint[[3]])
# kurtosis(return_network_vary_with_phi_2constraint[[4]])
# kurtosis(return_network_vary_with_phi_2constraint[[5]])
# kurtosis(return_network_vary_with_phi_2constraint[[6]])
# kurtosis(return_network_vary_with_phi_2constraint[[7]])
# kurtosis(return_network_vary_with_phi_2constraint[[8]])
# kurtosis(return_network_vary_with_phi_2constraint[[9]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[1]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[2]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[3]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[4]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[5]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[6]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[7]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[8]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[9]])

##### drawdown #####
maxDrawdown(return_minVar)
maxDrawdown(return_meanVar)
maxDrawdown(return_equal)
maxDrawdown(return_network_vary_with_phi[[1]])
maxDrawdown(return_network)
maxDrawdown(return_network_vary_with_phi[[9]])
maxDrawdown(return_network_vary_with_phi_2constraint[[1]])
maxDrawdown(return_network_2constraint)
maxDrawdown(return_network_vary_with_phi_2constraint[[9]])
maxDrawdown(return_minVar_noshort)
maxDrawdown(return_meanVar_noshort)
maxDrawdown(return_network_vary_with_phi_noshort[[1]])
maxDrawdown(return_network_noshort)
maxDrawdown(return_network_vary_with_phi_noshort[[9]])
maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[1]])
maxDrawdown(return_network_2constraint_noshort)
maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[9]])

maxDrawdown(return_minVar_Dantzig)
maxDrawdown(return_meanVar_Dantzig)
maxDrawdown(return_network_vary_with_phi_Dantzig[[1]])
maxDrawdown(return_network_1constraint_Dantzig)
maxDrawdown(return_network_vary_with_phi_Dantzig[[9]])
maxDrawdown(return_network_vary_with_phi_2constraint_Dantzig[[1]])
maxDrawdown(return_network_2constraint_Dantzig)
maxDrawdown(return_network_vary_with_phi_2constraint_Dantzig[[9]])

# maxDrawdown(return_network_vary_with_phi[[1]])
# maxDrawdown(return_network_vary_with_phi[[2]])
# maxDrawdown(return_network_vary_with_phi[[3]])
# maxDrawdown(return_network_vary_with_phi[[4]])
# maxDrawdown(return_network_vary_with_phi[[5]])
# maxDrawdown(return_network_vary_with_phi[[6]])
# maxDrawdown(return_network_vary_with_phi[[7]])
# maxDrawdown(return_network_vary_with_phi[[8]])
# maxDrawdown(return_network_vary_with_phi[[9]])
# maxDrawdown(return_network_vary_with_phi_noshort[[1]])
# maxDrawdown(return_network_vary_with_phi_noshort[[2]])
# maxDrawdown(return_network_vary_with_phi_noshort[[3]])
# maxDrawdown(return_network_vary_with_phi_noshort[[4]])
# maxDrawdown(return_network_vary_with_phi_noshort[[5]])
# maxDrawdown(return_network_vary_with_phi_noshort[[6]])
# maxDrawdown(return_network_vary_with_phi_noshort[[7]])
# maxDrawdown(return_network_vary_with_phi_noshort[[8]])
# maxDrawdown(return_network_vary_with_phi_noshort[[9]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[1]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[2]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[3]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[4]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[5]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[6]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[7]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[8]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[9]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[1]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[2]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[3]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[4]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[5]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[6]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[7]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[8]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[9]])

##### autocorrelation #####
cor(return_minVar[1:n-1],return_minVar[2:n])
cor(return_meanVar[1:n-1],return_meanVar[2:n])
cor(return_equal[1:n-1],return_equal[2:n])
cor(return_network_vary_with_phi[[1]][1:n-1],return_network_vary_with_phi[[1]][2:n])
cor(return_network[1:n-1],return_network[2:n])
cor(return_network_vary_with_phi[[9]][1:n-1],return_network_vary_with_phi[[9]][2:n])
cor(return_network_vary_with_phi_2constraint[[1]][1:n-1],return_network_vary_with_phi_2constraint[[1]][2:n])
cor(return_network_2constraint[1:n-1],return_network_2constraint[2:n])
cor(return_network_vary_with_phi_2constraint[[9]][1:n-1],return_network_vary_with_phi_2constraint[[9]][2:n])
cor(return_minVar_noshort[1:n-1],return_minVar_noshort[2:n])
cor(return_meanVar_noshort[1:n-1],return_meanVar_noshort[2:n])
cor(return_network_vary_with_phi_noshort[[1]][1:n-1],return_network_vary_with_phi_noshort[[1]][2:n])
cor(return_network_noshort[1:n-1],return_network_noshort[2:n])
cor(return_network_vary_with_phi_noshort[[9]][1:n-1],return_network_vary_with_phi_noshort[[9]][2:n])
cor(return_network_vary_with_phi_2constraint_noshort[[1]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[1]][2:n])
cor(return_network_2constraint_noshort[1:n-1],return_network_2constraint_noshort[2:n])
cor(return_network_vary_with_phi_2constraint_noshort[[9]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[9]][2:n])

cor(return_minVar_Dantzig[1:n-1],return_minVar_Dantzig[2:n])
cor(return_meanVar_Dantzig[1:n-1],return_meanVar_Dantzig[2:n])
cor(return_network_vary_with_phi_Dantzig[[1]][1:n-1],return_network_vary_with_phi_Dantzig[[1]][2:n])
cor(return_network_1constraint_Dantzig[1:n-1],return_network_1constraint_Dantzig[2:n])
cor(return_network_vary_with_phi_Dantzig[[9]][1:n-1],return_network_vary_with_phi_Dantzig[[9]][2:n])
cor(return_network_vary_with_phi_2constraint_Dantzig[[1]][1:n-1],return_network_vary_with_phi_2constraint_Dantzig[[1]][2:n])
cor(return_network_2constraint_Dantzig[1:n-1],return_network_2constraint_Dantzig[2:n])
cor(return_network_vary_with_phi_2constraint_Dantzig[[9]][1:n-1],return_network_vary_with_phi_2constraint_Dantzig[[9]][2:n])


# cor(return_network_vary_with_phi[[1]][1:n-1],return_network_vary_with_phi[[1]][2:n])
# cor(return_network_vary_with_phi[[2]][1:n-1],return_network_vary_with_phi[[2]][2:n])
# cor(return_network_vary_with_phi[[3]][1:n-1],return_network_vary_with_phi[[3]][2:n])
# cor(return_network_vary_with_phi[[4]][1:n-1],return_network_vary_with_phi[[4]][2:n])
# cor(return_network_vary_with_phi[[5]][1:n-1],return_network_vary_with_phi[[5]][2:n])
# cor(return_network_vary_with_phi[[6]][1:n-1],return_network_vary_with_phi[[6]][2:n])
# cor(return_network_vary_with_phi[[7]][1:n-1],return_network_vary_with_phi[[7]][2:n])
# cor(return_network_vary_with_phi[[8]][1:n-1],return_network_vary_with_phi[[8]][2:n])
# cor(return_network_vary_with_phi[[9]][1:n-1],return_network_vary_with_phi[[9]][2:n])
# cor(return_network_vary_with_phi_noshort[[1]][1:n-1],return_network_vary_with_phi_noshort[[1]][2:n])
# cor(return_network_vary_with_phi_noshort[[2]][1:n-1],return_network_vary_with_phi_noshort[[2]][2:n])
# cor(return_network_vary_with_phi_noshort[[3]][1:n-1],return_network_vary_with_phi_noshort[[3]][2:n])
# cor(return_network_vary_with_phi_noshort[[4]][1:n-1],return_network_vary_with_phi_noshort[[4]][2:n])
# cor(return_network_vary_with_phi_noshort[[5]][1:n-1],return_network_vary_with_phi_noshort[[5]][2:n])
# cor(return_network_vary_with_phi_noshort[[6]][1:n-1],return_network_vary_with_phi_noshort[[6]][2:n])
# cor(return_network_vary_with_phi_noshort[[7]][1:n-1],return_network_vary_with_phi_noshort[[7]][2:n])
# cor(return_network_vary_with_phi_noshort[[8]][1:n-1],return_network_vary_with_phi_noshort[[8]][2:n])
# cor(return_network_vary_with_phi_noshort[[9]][1:n-1],return_network_vary_with_phi_noshort[[9]][2:n])
# cor(return_network_vary_with_phi_2constraint[[1]][1:n-1],return_network_vary_with_phi_2constraint[[1]][2:n])
# cor(return_network_vary_with_phi_2constraint[[2]][1:n-1],return_network_vary_with_phi_2constraint[[2]][2:n])
# cor(return_network_vary_with_phi_2constraint[[3]][1:n-1],return_network_vary_with_phi_2constraint[[3]][2:n])
# cor(return_network_vary_with_phi_2constraint[[4]][1:n-1],return_network_vary_with_phi_2constraint[[4]][2:n])
# cor(return_network_vary_with_phi_2constraint[[5]][1:n-1],return_network_vary_with_phi_2constraint[[5]][2:n])
# cor(return_network_vary_with_phi_2constraint[[6]][1:n-1],return_network_vary_with_phi_2constraint[[6]][2:n])
# cor(return_network_vary_with_phi_2constraint[[7]][1:n-1],return_network_vary_with_phi_2constraint[[7]][2:n])
# cor(return_network_vary_with_phi_2constraint[[8]][1:n-1],return_network_vary_with_phi_2constraint[[8]][2:n])
# cor(return_network_vary_with_phi_2constraint[[9]][1:n-1],return_network_vary_with_phi_2constraint[[9]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[1]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[1]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[2]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[2]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[3]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[3]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[4]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[4]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[5]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[5]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[6]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[6]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[7]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[7]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[8]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[8]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[9]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[9]][2:n])

##### weights #####
a<-w_network_2constraint_noshort[1,]
a<-sort(a)
a[(p-9):p]
a
sort(w_network_2constraint_noshort[1,])[(p-9):p]
sort(w_network_2constraint_noshort[2,])[(p-9):p]
sort(w_network_2constraint_noshort[3,])[(p-9):p]
sort(w_network_2constraint_noshort[4,])[(p-9):p]
sort(w_network_2constraint_noshort[5,])[(p-9):p]
sort(w_network_2constraint_noshort[6,])[(p-9):p]

sort(w_network_2constraint_noshort[1,])[(p-9):p]


sort(colSums(w_minVar),decreasing = T)[1:10]
sort(colSums(w_meanVar),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi[[1]]),decreasing = T)[1:10]
sort(colSums(w_network_1constraint),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi[[9]]),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_2constraint[[1]]),decreasing = T)[1:10]
sort(colSums(w_network_2constraint),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_2constraint[[9]]),decreasing = T)[1:10]
sort(colSums(w_minVar_noshort),decreasing = T)[1:10]
sort(colSums(w_meanVar_noshort),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_noshort[[1]]),decreasing = T)[1:10]
sort(colSums(w_network_1constraint_noshort),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_noshort[[9]]),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_2constraint_noshort[[1]]),decreasing = T)[1:10]
sort(colSums(w_network_2constraint_noshort),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_2constraint_noshort[[9]]),decreasing = T)[1:10]

sort(colSums(w_minVar_Dantzig),decreasing = T)[1:10]
sort(colSums(w_meanVar_Dantzig),decreasing = T)[1:10]
sort(colSums(w_network_1constraint_Dantzig),decreasing = T)[1:10]
sort(colSums(w_network_2constraint_Dantzig),decreasing = T)[1:10]

s<-t(apply(w_network_2constraint_noshort, 1, sort))
s<-t(apply(w_network_2constraint_noshort, 1, sort, decreasing=T))


