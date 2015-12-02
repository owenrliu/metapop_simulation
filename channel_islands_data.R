#### Channel Islands Benthic Density Comparison ####

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


#### benthic density data ####
# downloaded from http://www.esapubs.org/archive/ecol/E094/245/metadata.php#_Hlk327197452
benthdat <- read.csv(file=paste(W_D,'/data/benthdensdata.csv',sep=''))

# Kelletia and warty sea cucumber data. Species number for kelletia is 9006, for cuc is 11007
kelletia <- subset(benthdat,benthdat$Species==9006)
wartycuc <- subset(benthdat,benthdat$Species==11007)

# site characteristics data
sites <- read.csv(file=paste(W_D,'/data/sitesdata.csv',sep=''))
kelletia<-merge(kelletia,sites,by.x='Site',by.y='Site')
wartycuc<-merge(wartycuc,sites,by.x='Site',by.y='Site')

ggplot()+geom_histogram(aes(x=DensityMean),data=wartycuc)
ggplot()+geom_histogram(aes(x=DensityMean),data=kelletia)

kelletia.by.site <- aggregate(list(density=kelletia$DensityMean),by=list(site=kelletia$Site,island=kelletia$IslandName, lat=kelletia$Latitude__Deg_N_,
                                                                         lon=kelletia$Longitude__Deg_W_,
                                                                         reserve=kelletia$MarineReserve),FUN=mean,na.rm=T)
wartycuc.by.site <- aggregate(list(density=wartycuc$DensityMean),by=list(site=wartycuc$Site,island=wartycuc$IslandName,lat=wartycuc$Latitude__Deg_N_,
                                                                          lon=wartycuc$Longitude__Deg_W_,
                                                                          reserve=wartycuc$MarineReserve),FUN=mean,na.rm=T)
# standard error, NAs removed
se <- function(x) sd(x,na.rm=T)/length(x)
kelletia.site.se <- aggregate(list(se=kelletia$DensityMean),by=list(site=kelletia$Site,island=kelletia$IslandName, lat=kelletia$Latitude__Deg_N_,
                                                                         lon=kelletia$Longitude__Deg_W_,
                                                                         reserve=kelletia$MarineReserve),FUN=se)
kelletia.by.site$se <- kelletia.site.se$se
wartycuc.site.se <- aggregate(list(se=wartycuc$DensityMean),by=list(site=wartycuc$Site,island=wartycuc$IslandName, lat=wartycuc$Latitude__Deg_N_,
                                                                    lon=wartycuc$Longitude__Deg_W_,
                                                                    reserve=wartycuc$MarineReserve),FUN=se)
wartycuc.by.site$se <- wartycuc.site.se$se

# inside vs. outside
kell.in.out <- melt(kelletia.by.site, id.vars='reserve',measure.vars='density')
ggplot()+geom_boxplot(aes(x=reserve,y=value),data=kell.in.out) +
  ggtitle('Kellets Whelk Density') + ylab('Number per square meter')
t.test(x=subset(kell.in.out$value,kell.in.out$reserve=='Inside'),
       y=subset(kell.in.out$value,kell.in.out$reserve=='Outside'))

warty.in.out <- melt(wartycuc.by.site, id.vars='reserve',measure.vars='density')
ggplot()+geom_boxplot(aes(x=reserve,y=value),data=warty.in.out) +
  ggtitle('Warty Sea Cucumber Density') + ylab('Number per square meter')
t.test(x=subset(kell.in.out$value,warty.in.out$reserve=='Inside'),
       y=subset(warty.in.out$value,warty.in.out$reserve=='Outside'))

# Cold vs. Warm region
cold.islands <- c('San Miguel Island','Santa Rosa Island')
kelletia.by.site$env <- character(nrow(kelletia.by.site))
for (i in 1:nrow(kelletia.by.site)) {
  if(kelletia.by.site$island[i]%in%cold.islands) kelletia.by.site$env[i]='cold'
  else kelletia.by.site$env[i]='warm'
}
wartycuc.by.site$env <- character(nrow(wartycuc.by.site))
for (i in 1:nrow(wartycuc.by.site)) {
  if(wartycuc.by.site$island[i]%in%cold.islands) wartycuc.by.site$env[i]='cold'
  else wartycuc.by.site$env[i]='warm'
}

#reserve vs. 'environment'
kell.twoway <- melt(kelletia.by.site, id.vars=c('env','reserve'),measure.vars='density')

#make a grouping variable for plotting
kell.twoway$group <- paste(kell.twoway$env,kell.twoway$reserve)

kell.twoway.plot<- ggplot(aes(x=group,y=value,fill=group),data=kell.twoway)+geom_boxplot() + 
  ggtitle('Kellets Whelk Density') + ylab('Number per square meter') + 
  geom_jitter(position = position_jitter(width = .05), alpha = 0.5)


warty.twoway <- melt(wartycuc.by.site, id.vars=c('env','reserve'),measure.vars='density')

#make a grouping variable for plotting
warty.twoway$group <- paste(warty.twoway$env,warty.twoway$reserve)

warty.twoway.plot <- ggplot(aes(x=group,y=value,fill=group),data=warty.twoway)+geom_boxplot() + 
  ggtitle('Warty Sea Cucumber Density') + ylab('Number per square meter') +
  geom_jitter(position = position_jitter(width = .05), alpha = 0.5)

## Boostrapping the mean, num being the number of bootstrap samples
# takes a vector as input
b.mean <- function(data, num=1000) {
  resamples <- lapply(1:num, function(i) sample(data, replace=T))
  r.mean <- sapply(resamples, mean, na.rm=T)
  std.errors <- sd(r.mean) #standard error of the bootstrapped samples
  cis <- mean(r.mean) + c(1,-1)*std.errors #upper and lower bootstrapped CIs
  return(list(resamples=resamples, means=r.mean,std.errors=std.errors,cis=cis))   
}

#Summary with confidence intervals for groups of cold/inside,cold/outside,warm/inside,warm/outside
# first, figure out the applicable sites
cold.in.sites <- subset(sites$Site,sites$IslandName%in%cold.islands & sites$MarineReserve=='Inside')
cold.out.sites <- subset(sites$Site,sites$IslandName%in%cold.islands & sites$MarineReserve=='Outside')
warm.in.sites <- subset(sites$Site,!(sites$IslandName%in%cold.islands) & sites$MarineReserve=='Inside')
warm.out.sites <- subset(sites$Site,!(sites$IslandName%in%cold.islands) & sites$MarineReserve=='Outside')

# subset the original species data
warty.cold.in <- subset(wartycuc,wartycuc$Site%in%cold.in.sites)
warty.cold.out <- subset(wartycuc,wartycuc$Site%in%cold.out.sites)
warty.warm.in <- subset(wartycuc,wartycuc$Site%in%warm.in.sites)
warty.warm.out<- subset(wartycuc,wartycuc$Site%in%warm.in.sites)

kell.cold.in <- subset(kelletia,kelletia$Site%in%cold.in.sites)
kell.cold.out <- subset(kelletia,kelletia$Site%in%cold.out.sites)
kell.warm.in <- subset(kelletia,kelletia$Site%in%warm.in.sites)
kell.warm.out<- subset(kelletia,kelletia$Site%in%warm.in.sites)

# now, calculate means and bootstrapped CIs
mean.and.cis <- function(data) {
  data.mean <- mean(data,na.rm=T)
  data.bstrap <- b.mean(data,num=1000)
  upper <- data.bstrap$cis[1]
  lower <- data.bstrap$cis[2]
  return(c(data.mean,upper,lower))
}
kci <- mean.and.cis(kell.cold.in$DensityMean)
kco <- mean.and.cis(kell.cold.out$DensityMean)
kwi <- mean.and.cis(kell.warm.in$DensityMean)
kwo <- mean.and.cis(kell.warm.out$DensityMean)
kell.out <- as.data.frame(rbind(kci,kco,kwi,kwo))
kell.out$group <- c('Cold In','Cold Out','Warm In','Warm Out')
colnames(kell.out)<- c('mean','ciupper','cilower','group')

wci <- mean.and.cis(warty.cold.in$DensityMean)
wco <- mean.and.cis(warty.cold.out$DensityMean)
wwi <- mean.and.cis(warty.warm.in$DensityMean)
wwo <- mean.and.cis(warty.warm.out$DensityMean)
warty.out <- as.data.frame(rbind(wci,wco,wwi,wwo))
warty.out$group <- c('Cold In','Cold Out','Warm In','Warm Out')
colnames(warty.out)<- c('mean','ciupper','cilower','group')

kell.out.plot<-ggplot(data=kell.out,aes(x=group,y=mean)) +
  geom_pointrange(aes(ymin=cilower,ymax=ciupper,colour=group),size=1.5,shape=15,stat='identity') +
  ggtitle('Kellets Whelk Mean Density by Site Type') +xlab('Site Type') +ylab('Mean Density') +
  guides(colour=FALSE)

kell.out.plot

warty.out.plot<-ggplot(data=warty.out,aes(x=group,y=mean,colour=group)) +
  geom_pointrange(aes(ymin=cilower,ymax=ciupper),size=1.5,shape=15,stat='identity') +
  ggtitle('Warty Sea Cucumber Mean Density by Site Type')+xlab('Site Type') +ylab('Mean Density (Number per sq. m)')+
  guides(colour=FALSE)
 
warty.out.plot

#### Generalized Function for Any Species in the Dataset ####
# species should be a number from Table 3 at (http://www.esapubs.org/archive/ecol/E094/245/metadata.php#_Hlk327197643)
# assumes site data already loaded, and bethic density full dataset (benthdat, above)
# calls some of the functions defined above
# outputs density plots by site type, with 95% bootstrapped CIs

compare_env_reserve <- function(species) {
  
  # first, subset the original data and merge to sites data
  spp.dat <- subset(benthdat,benthdat$Species==species)
  spp.dat<-merge(spp.dat,sites,by.x='Site',by.y='Site')
  
  # subsets for each environment/management type
  spp.cold.in <- subset(spp.dat,spp.dat$Site%in%cold.in.sites)
  spp.cold.out <- subset(spp.dat,spp.dat$Site%in%cold.out.sites)
  spp.warm.in <- subset(spp.dat,spp.dat$Site%in%warm.in.sites)
  spp.warm.out<- subset(spp.dat,spp.dat$Site%in%warm.in.sites)
  
  # calculate means and CIs for each and merge into output data frame
  sppci <- mean.and.cis(spp.cold.in$DensityMean)
  sppco <- mean.and.cis(spp.cold.out$DensityMean)
  sppwi <- mean.and.cis(spp.warm.in$DensityMean)
  sppwo <- mean.and.cis(spp.warm.out$DensityMean)
  spp.out <- as.data.frame(rbind(sppci,sppco,sppwi,sppwo))
  spp.out$group <- c('Cold In','Cold Out','Warm In','Warm Out')
  colnames(spp.out)<- c('mean','ciupper','cilower','group')
  
  # Return the output data and a plot
  spp.out.plot<- ggplot(data=spp.out,aes(x=group,y=mean)) +
    geom_pointrange(aes(ymin=cilower,ymax=ciupper,colour=group),size=1.5,shape=15,stat='identity') +
    ggtitle('Mean Density by Site Type') +xlab('Site Type') +ylab('Mean Density (Number/square meter)') +
    guides(colour=FALSE)
  
  return(list(out.dat=spp.out, out.plot=spp.out.plot))
}