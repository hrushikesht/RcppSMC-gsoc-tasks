library(RcppSMC)

#set.seed() ensures that simulation can be reproduced
set.seed(7)

#Block Sampling Particle filter
sim_guassian<-simGaussian(len=300)
ans<-blockpfGaussianOpt(sim_guassian$data,lag=10,plot=TRUE,particles = 100)

#Particle Filter Example
sim_linear<-simLineart(len=300)
if(interactive()){
  ans<-pfLineartBS(sim_linear$data,onlinePlot = pfLineartBSOnlinePlot)
}else{
  ans<-pfLineartBS(sim_linear$data,plot=TRUE)
}

#NonLinear Booststrap Particle Filter Example
sim_non_linear<-simNonlin(len=300)
ans<-pfNonlinBS(sim_non_linear$data,plot=TRUE)

