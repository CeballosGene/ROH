rm(list=ls())
results_kb <- function(class)  {
  this_iids_roh <- dat[class,]
  my_list<-c("mean"=mean(this_iids_roh$KB[this_iids_roh$KB>=1500]),
             "sum"=sum(this_iids_roh$KB[this_iids_roh$KB>=1500]),
             "sums"=sum(this_iids_roh$KB[this_iids_roh$KB<1500]),
             "length"=length(this_iids_roh$KB[this_iids_roh$KB>=1500]),
             "cl1"=sum(this_iids_roh$KB[this_iids_roh$KB<=500])-sum(this_iids_roh$KB[this_iids_roh$KB<300]),
             "cl2"=sum(this_iids_roh$KB[this_iids_roh$KB<=1000])-sum(this_iids_roh$KB[this_iids_roh$KB<500]),
             "cl3"=sum(this_iids_roh$KB[this_iids_roh$KB<=2000])-sum(this_iids_roh$KB[this_iids_roh$KB<1000]),
             "cl4"=sum(this_iids_roh$KB[this_iids_roh$KB<=4000])-sum(this_iids_roh$KB[this_iids_roh$KB<2000]),
             "cl5"=sum(this_iids_roh$KB[this_iids_roh$KB<=8000])-sum(this_iids_roh$KB[this_iids_roh$KB<4000]),
             "cl6"=sum(this_iids_roh$KB[this_iids_roh$KB>8000]),
             "Froh"=(sum(this_iids_roh$KB[this_iids_roh$KB>=1500]))/2881033)
  return(my_list)
}
files_list <- list.files("PATH", full.names=TRUE)
dat <- data.frame()
for (i in 1:length(files_list)) {
  dat <- rbind(dat, read.table((files_list[i]),header=TRUE))
}
library(data.table)
dat <- as.data.table(dat)
dat$IID<- sub("^", "SC", dat$IID)
dat$IID<-as.factor(dat$IID)
setkey(dat,"IID")
results <- c()
nLevels <- length(levels(dat$IID))
start <- proc.time()
pb <- txtProgressBar(min = 0, max = nLevels, style = 3)
for (i in 1:nLevels){
  this_iid <- levels(dat$IID)[i]
  results <- rbind(results,results_kb(this_iid))
  setTxtProgressBar(pb,i)
}
close(pb)
proc.time()-start
results<-data.frame(levels(dat$IID),results)
results$IID<-results$levels.dat.IID.
results[results==0] <- NA

write.csv(results, file="PATH")
