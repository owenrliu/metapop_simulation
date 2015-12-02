#### Channel Islands Fish Density data Exploration ####

####Fish spatial heterogeneity and stability####
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(popbio)
library(doBy)
library(dplyr)

## multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.dat.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## number of unique values
lunique<-function(x) length(unique(x))

W_D<-getwd()
#read in the dat
dat <- read.csv(file=paste(W_D,'/data/fishdensdata.csv',sep=''))

abun.yr <- aggregate(list(count=dat$count),by=list(spp=dat$Species,year=dat$Year), FUN=sum)

abun.yr.site<-aggregate(list(count=dat$count),by=list(spp=dat$Species,year=dat$Year,site=dat$Site), FUN=sum)

## read in species names and match to species ID
tax <- read.csv(file=paste(W_D,'/data/species_id.csv',sep=''))

abun.yr<-merge(abun.yr,tax,by.x='spp',by.y='ID')
abun.yr <- abun.yr[order(abun.yr$spp,abun.yr$year),]

abun.yr.site<-merge(abun.yr.site,tax,by.x='spp',by.y='ID')
abun.yr.site<-abun.yr.site[order(abun.yr.site$spp,abun.yr.site$year,abun.yr.site$site),]

## stability measure, the inverse of CV (mean/SD)
stability <- aggregate(list(stability=abun.yr$count),by=list(spp=abun.yr$spp),FUN=function(x)mean(x)/sd(x))

# an index of spatial heterogeneity
sphet <- function(x) { ## function to calculate the area under the cumulative ranked abundance curve across site for each species/year
  
  het <- sum(sort(x)/max(x))
  return(het)
}

het <- aggregate(list(sphet=abun.yr.site$count),by=list(spp=abun.yr.site$spp,year=abun.yr.site$year),FUN=sphet)
meanhet <- aggregate(list(sphet=het$sphet),by=list(spp=het$spp),FUN=mean,na.rm=T)

inds <- merge(stability,meanhet)
inds <- merge(inds, tax,by.x='spp',by.y='ID')

ggplot(aes(x=sphet,y=stability),data=inds)+geom_point()+geom_smooth(method='lm')