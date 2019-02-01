






# will need {dplyr} 
library(tidyverse)


#########################################################################################
## generic plotting of pop size throught time by distribution
#########################################################################################

#### Make fake data in data frame
fakedf <- data.frame(day=rep(c(1:40),15), treat=c(rep("a",200),rep("b",200),rep("c",200)), jar=rep(1:15,each=40), count=NA)
fakedf
fakedf$count[fakedf$treat=="a"] <- fakedf$day[fakedf$treat=="a"]*rnorm(200,mean=1.5,sd=0.1)
fakedf$count[fakedf$treat=="b"] <- fakedf$day[fakedf$treat=="b"]*rnorm(200,mean=2,sd=0.1)
fakedf$count[fakedf$treat=="c"] <- fakedf$day[fakedf$treat=="c"]*rnorm(200,mean=1,sd=0.2)
####

#### look at fake data in simple plot
plot(count~day, data=fakedf, col=c(1:3)[unclass(factor(fakedf$treat))], pch=16)
####

#### Summarize data by treatments and day
#### Making distributions of counts for each day and treatment by treatment
fakedf2 <- fakedf %>%
  group_by(treat,day) %>%
  summarise(maxc = max(count),
            minc = min(count),
            meanc = mean(count),
            upperc = quantile(count,probs=c(0.95)),
            lowerc = quantile(count,probs=c(0.05))
            )
head(fakedf2,20)  # look at first 20 rows of fakedata summarized dataframe
####

#### Plot counts by mean, max+min, and 95+5th quantiles with polygons and lines.
plot(meanc~day, data=subset(fakedf2, treat=="a"), type="l", lwd=2, ylim=c(0,max(fakedf2$maxc)), pch=NA)
polygon(c(c(1:40),rev(c(1:40))),c(fakedf2$upperc[fakedf2$treat=="a"],rev(fakedf2$lowerc[fakedf2$treat=="a"])), border=F, col="lightgray")
polygon(c(c(1:40),rev(c(1:40))),c(fakedf2$upperc[fakedf2$treat=="b"],rev(fakedf2$lowerc[fakedf2$treat=="b"])), border=F, col="pink")
polygon(c(c(1:40),rev(c(1:40))),c(fakedf2$upperc[fakedf2$treat=="c"],rev(fakedf2$lowerc[fakedf2$treat=="c"])), border=F, col="green")
points(meanc~day, data=subset(fakedf2, treat=="a"), type="l", lwd=2)
points(meanc~day, data=subset(fakedf2, treat=="b"), type="l", lwd=2, col="red")
points(meanc~day, data=subset(fakedf2, treat=="c"), type="l", lwd=2, col="darkgreen")
points(maxc~day, data=subset(fakedf2, treat=="a"), type="l", lwd=1, lty=2, col="lightgray")
points(minc~day, data=subset(fakedf2, treat=="a"), type="l", lwd=1, lty=2, col="lightgray")
points(maxc~day, data=subset(fakedf2, treat=="b"), type="l", lwd=1, lty=2, col="pink")
points(minc~day, data=subset(fakedf2, treat=="b"), type="l", lwd=1, lty=2, col="pink")
points(maxc~day, data=subset(fakedf2, treat=="c"), type="l", lwd=1, lty=2, col="green")
points(minc~day, data=subset(fakedf2, treat=="c"), type="l", lwd=1, lty=2, col="green")
####
#################################################################################

#################################################################################
## plot and analyze effect of some exposure at time
#################################################################################


#### Make fake data in data frame with similar "trajectories" to start
fakedf3 <- data.frame(day=rep(c(1:40),15), treat=c(rep(0,200),rep(10,200),rep(20,200)), jar=rep(1:15,each=40), count=NA)
fakedf3
fakedf3$count[fakedf3$treat==0] <- fakedf3$day[fakedf3$treat==0]*rnorm(200,mean=1.5,sd=0.1)
fakedf3$count[fakedf3$treat==10] <- fakedf3$day[fakedf3$treat==10]*rnorm(200,mean=1.5,sd=0.1)
fakedf3$count[fakedf3$treat==20] <- fakedf3$day[fakedf3$treat==20]*rnorm(200,mean=1.5,sd=0.2)
####
#### Add "effect" on a given day.
fakedf3 <- fakedf3 %>%
  mutate(counteff = case_when(
    treat==10 & day > 15 ~ count * sample(rnorm(100,0.5,0.1)),
    treat==20 & day > 15 ~ count * sample(rnorm(100,0.25,0.1)),
    TRUE ~ count
  ))
head(fakedf3[250:270,],20)
####

#### look at fake data in simple plot
plot(counteff~day, data=fakedf3, col=c(1:3)[unclass(factor(fakedf3$treat))], pch=16)
####

#### to measure effect
fifteen <- subset(fakedf3, day==15)
seventeen <- subset(fakedf3, day==17)
effectdf <- merge(x=fifteen,y=seventeen,by=c("treat","jar"))
head(effectdf, 20)
effectdf$propchange <- effectdf$counteff.y/effectdf$counteff.x
####

#### solve for "EC50"
library(drc)
drm1 <- drm(propchange~treat, data=effectdf, fct=LL.2(), type="binomial")
plot(drm1, type="all")
ED(drm1, 0.5, type="absolute", interval="delta")
boxplot(propchange~treat, data=effectdf)
####
###################################################################################











