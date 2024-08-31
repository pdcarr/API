############## housekeeping
#options(digits=12)
library(MASS) 
library("pracma")
#################### Edit this stuff to control the inputs
earliest_year = 19 # edit this to change the starting year
latest_year = 24  # edit this to change the ending year
ps.data.name <- "data/PS_table.csv"
histogram_color <- "aliceblue"  # fill color for the histogram
perspective_color <- "lavender"
viewpoint_azimuth <- 150 # degrees
viewpoint_elevation <- 30 # degrees
viewpoint_distance <- 2
perspective_shading <- 0.4
perspective_expand <- 0.8
strange_breaks <- 0:5

################ don't edit below this line unless you want to change the logic

n.index <- 6  # number of possible vlaues
search_pattern <- "-\\d{3}" # for searchign case numbers
#####################
search_pattern <- paste(as.character(earliest_year),search_pattern,sep="")
for(yr in (earliest_year+1):latest_year) {
  search_pattern <- paste(as.character(yr),"|",search_pattern,sep="")
}

ps.data <- read.csv(header=TRUE,file=ps.data.name,as.is=TRUE,na.strings = "")
z.data <- matrix(data=rep.int(rep.int(0,times=n.index),times=n.index),nrow=n.index,ncol=n.index) # zero matrix

# comb through the data
n.data <- length(ps.data$Probability)
case.count <- 0
id.count <- 0 
liz.count <-0
for (data.index in 1:n.data) {
  if(!isempty(grep(pattern=search_pattern,ps.data$Case.Number[data.index],perl=TRUE,value=FALSE))) {
    if(!is.na(ps.data$Probability[data.index]) && !is.na(ps.data$Strangeness[data.index])) {
      i <- ps.data$Probability[data.index]+1
      j <- ps.data$Strangeness[data.index]+1
      z.data[i,j] <-  z.data[i,j] + 1
      case.count <- case.count + 1
      if(ps.data$Strangeness[data.index] == 0) {id.count <- id.count + 1}
      if(ps.data$Strangeness[data.index] == 1) {liz.count <- liz.count+1}
    }
  }
}

# perspective plot

persp(x=seq(0,n.index-1,1),
      y = seq(0,n.index-1,1),
      z= z.data,
      xlab = "Case Quality",
      ylab = "Strangeness",
      zlab="# of cases",
      ticktype = "detailed",
      nticks=n.index,
      box=TRUE,
      theta=viewpoint_azimuth,phi=viewpoint_elevation,
      r=viewpoint_distance,expand=perspective_expand,
      col=perspective_color,shade=perspective_shading,
      main=paste("\nDocumented cases since 20",earliest_year,": ",case.count,sep=""))

#histogram

hist(ps.data$Strangeness,
      freq=TRUE,
      xlab="Strangeness Rating",
      breaks=strange_breaks,
     col=histogram_color,main = paste("\nDocumented cases since 20",earliest_year,": ",case.count,sep=""))

# print out the results
cat("\nTotal cases since 20",earliest_year,": ",n.data,sep="")
cat("\nDocumented cases: ",case.count)
cat("\nIdentified cases: ",id.count)
cat("\nlow strangeness count: ",liz.count)
id_frac = id.count/case.count
liz_frac = (id.count + liz.count)/case.count
cat("\nfraction of ID cases = ",id_frac)
cat("\nnon-anomalous fraction: ",liz_frac)