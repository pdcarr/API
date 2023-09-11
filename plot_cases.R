############## housekeeping
#options(digits=12)
library(MASS) 
library("pracma")
#####################
n.index <- 6
# name.2014.sheet <- "data/API 2014 Case Control Sheet_CLEANED.csv"
ps.data.name <- "data/PS_10Sep23.csv"
#sheet.2014 <- read.csv(header=TRUE,file=name.2014.sheet,stringsAsFactors=FALSE)
ps.data <- read.csv(header=TRUE,file=ps.data.name,as.is=TRUE,na.strings = "")
z.data <- matrix(data=rep.int(rep.int(0,times=n.index),times=n.index),nrow=n.index,ncol=n.index) # zero matrix

n.data <- length(ps.data$Probability)
case.count <- 0
id.count <- 0 
for (data.index in 1:n.data) {
  if(!is.na(ps.data$Probability[data.index]) && !is.na(ps.data$Strangeness[data.index])) {
    i <- ps.data$Probability[data.index]+1
    j <- ps.data$Strangeness[data.index]+1
    z.data[i,j] <-  z.data[i,j] + 1
    case.count <- case.count + 1
    if(ps.data$Strangeness[data.index] == 0) {
      id.count <- id.count + 1
    }
  }
}

persp(x=seq(0,n.index-1,1),
      y = seq(0,n.index-1,1),
      z= z.data,
      xlab = "Probability",
      ylab = "Strangeness",
      zlab="# of cases",
      nticks=n.index,
      box=TRUE,
      theta=60,phi=30,r=10,expand=1.1,
      col="lavender",shade=0.3,
      ltheta=45,lphi=70,
      main="API Cases as of 10 September 2023")

cat("\nDocumented cases: ",case.count)
cat("\nIdentified cases: ",id.count)
id_frac = id.count/case.count
cat("\nfraction of ID cases = ",id_frac)