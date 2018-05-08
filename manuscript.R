setwd( '~/Documents/labwork/crowdsource/')
library(RColorBrewer)
library(plot3D)
source('./clean_filter.R')
#clean_filter('20160928/',1)
clean_filter('20161215/',2)

per_user_image<-read.csv('./20161215/per_user_image_filter.csv')
perbox<-read.csv('./20161215/perbox.csv')
#rawperbox_nm<-read.csv('./20161215/20161215-selected/d3ai_tassels_mturk_20161205_qualtrics_survey_responses.csv')
perbox = perbox[,-1]
#par(mfrow=c(1,3))

#Collapse perbox data to per user image and count perfect recall


lower = quantile(per_user_image$question.elapsed,probs=c(0.05,0.95))[1]
upper = quantile(per_user_image$question.elapsed,probs=c(0.05,0.95))[2]
per_user_image_nooutlier = per_user_image[per_user_image$question.elapsed>=lower & per_user_image$question.elapsed<=upper,]

#nooutlier = read.csv('./outlierremoved.csv')
timepred = read.csv('./timepred.csv')

###Generating density plots
plotDensity<-function(perbox,group){
  par(mfrow=c(1,1))
  z1 = as.matrix(table(round(perbox[perbox$type==group,]$recall,2),round(perbox[perbox$type==group,]$precision,2)))
  z1[1,1]<-0
  a1 = prop.table(z1)
  if (group==1) main ="Density: Master MTurk"
  else if (group==2) main = "Density: non-Master MTurk"
  else main="Density: Course Credit"
  image2D(z=a1,xlab="Recall",ylab="Precision",main=main,resfac=0.3,col=brewer.pal(4,'GnBu'),ylim=c(0,1),xlim=c(0,1),breaks=c(0,round(max(a1)/4,5)*1,round(max(a1)/4,5)*2,round(max(a1)/4,5)*3,round(max(a1)/4,5)*4))
}

plotDensity(perbox,1)
plotDensity(perbox,2)
plotDensity(perbox,3)


###Generating violin plots for fmean 

#install.packages('vioplot')
#library(vioplot)
violinPlot(group)
vioplot(per_user_image[per_user_image$type==1,'fmean'],per_user_image[per_user_image$type==2,'fmean'],per_user_image[per_user_image$type==3,'fmean'],
        names = c("Master MTurk","non-Master MTurk","Course Credit"),col='gold')
title("Violin Plot of Mean F-measure per Image per User")

###Generating violin plots for time elapsed and difficulty
library(vioplot)
qe_master_easy = master_nooutlier[master_nooutlier$difficulty==1,'question.elapsed']
qe_master_hard = master_nooutlier[master_nooutlier$difficulty==2,'question.elapsed']
qe_turker_easy = turker_nooutlier[turker_nooutlier$difficulty==1,'question.elapsed']
qe_turker_hard = turker_nooutlier[turker_nooutlier$difficulty==2,'question.elapsed']
qe_sona_easy = sona_nooutlier[sona_nooutlier$difficulty==1,'question.elapsed']
qe_sona_hard = sona_nooutlier[sona_nooutlier$difficulty==2,'question.elapsed']
par(mfrow=c(1,3))
vioplot(qe_master_easy,qe_master_hard,names=c('easy','hard'),col=cols[1])
title('Master MTurk')
vioplot(qe_turker_easy,qe_turker_hard,names=c('easy','hard'),col=cols[2])
title('Non-Master MTurk')
vioplot(qe_sona_easy,qe_sona_hard,names=c('easy','hard'),col=cols[3])
title('Course Credit')

####Generating the time vs time plot
#estimates for the linear regression are from SAS
#only one outlier (15000 seconds removed from SONA group)
plot(per_user_image$question.ordinal,per_user_image$question.elapsed,col="#00000033",
     ylim = c(0,500),pch=20,xlab="Question Ordinal Index", ylab="Time Spent per Question (in seconds)")
#plot(timepred$question_ordinal,timepred$Pred,col="red",pch=16)
cols = brewer.pal(3,"Dark2")
#Master
abline(81.6951,-0.7421,col=cols[1],lwd=3)
#Turker
abline(68.6631,-0.5728,col=cols[2],lwd=3)
#SONA
abline(45.8278,-0.4236,col=cols[3],lwd=3)
legend('topright',col=brewer.pal(3,'Dark2'),legend = c("Master MTurk","non-Master MTurk","Course Credit"),lwd= c(3,3,3))

#5% and 95% outliers removed
plot(per_user_image_nooutlier$question.ordinal,per_user_image_nooutlier$question.elapsed,
     ylim=c(0,120),col="#00000033",pch=20,xlab="Question Ordinal Index", ylab="Time Spent per Question (in seconds)")
cols = brewer.pal(3,"Dark2")
#Master
abline(49.0918,-0.3042,col=cols[1],lwd=3)
#Turker
abline(48.3152,-0.3250,col=cols[2],lwd=3)
#SONA
abline(38.1910,-0.3126,col=cols[3],lwd=3)
legend('topright',col=brewer.pal(3,'Dark2'),legend = c("Master MTurk","non-Master MTurk","Course Credit"),lwd= c(3,3,3))


#Generate accuracy over time plot
#does accuracy change as time goes by
plot(per_user_image$question.ordinal,per_user_image$fmean,ylim=c(0,1),col="#00000033",pch=20,xlab = 'Question Ordinal Index',
     ylab = 'Mean F-value')
cols = brewer.pal(3,"Dark2")
#Master pvalue=0.0457
abline(0.7909,-0.00008,col=cols[1],lwd=3)
#Turker pvalue<0.0001
abline(0.8100,-0.00027,col=cols[2],lwd=3)
#SONA pvalue<0.0001
abline(0.7256,-0.00095,col=cols[3],lwd=3)
legend('bottomleft',col=brewer.pal(3,'Dark2'),legend = c("Master MTurk","non-Master MTurk","Course Credit"),lwd= c(3,3,3))


#Generate accuracy vs time plot
#is accuracy correlated with time spent on that question
lmfit=lm(per_user_image_nooutlier$fmean~per_user_image_nooutlier$question.elapsed)
summary(lmfit)
plot(per_user_image_nooutlier$question.elapsed,per_user_image_nooutlier$fmean,
     xlab = 'Time Spent per Question (in seconds)',ylab='mean F value',
     col="#00000033",pch=20)
#master pvalue<0.0001
abline(0.7869,0.000220,col=cols[1],lwd=3)
#turker pvalue <0.0001
abline(0.7917,0.000226,col=cols[2],lwd=3)
#sona pvalue<0.0001
abline(0.6633,0.000106,col=cols[3],lwd=3)
legend('bottomright',col=brewer.pal(3,'Dark2'),legend = c("Master MTurk","non-Master MTurk","Course Credit"),lwd= c(3,3,3))


#Time over difficult and easy images
summary(lm(question.elapsed~as.factor(difficulty),data=per_user_image_nooutlier))
#significant without random effect, once add in random effect, no longer significant
master_nooutlier = per_user_image_nooutlier[per_user_image_nooutlier$type==1,]
turker_nooutlier = per_user_image_nooutlier[per_user_image_nooutlier$type==2,]
sona_nooutlier = per_user_image_nooutlier[per_user_image_nooutlier$typhie==3,]
#The following is on fmean
height_master = c(mean(master_nooutlier[master_nooutlier$difficulty==1,'fmean']),mean(master_nooutlier[master_nooutlier$difficulty==2,'fmean']))
height_turker = c(mean(turker_nooutlier[turker_nooutlier$difficulty==1,'fmean']),mean(turker_nooutlier[turker_nooutlier$difficulty==2,'fmean']))
height_sona = c(mean(sona_nooutlier[sona_nooutlier$difficulty==1,'fmean']),mean(sona_nooutlier[sona_nooutlier$difficulty==2,'fmean']))
heights = cbind(height_master,height_turker,height_sona)
barplot(heights,beside=T,names=c('Master MTurk','Non-Master MTurk','Course Credit'),ylim=c(0,1),col=cols[1:2],xlab='Difficulty',ylab='Mean F value')
legend('topright',c('easy','hard'),col=cols[1:2],pch=c(15,15))
#The following is on time
height_master = c(mean(master_nooutlier[master_nooutlier$difficulty==1,'question.elapsed']),mean(master_nooutlier[master_nooutlier$difficulty==2,'question.elapsed']))
height_turker = c(mean(turker_nooutlier[turker_nooutlier$difficulty==1,'question.elapsed']),mean(turker_nooutlier[turker_nooutlier$difficulty==2,'question.elapsed']))
height_sona = c(mean(sona_nooutlier[sona_nooutlier$difficulty==1,'question.elapsed']),mean(sona_nooutlier[sona_nooutlier$difficulty==2,'question.elapsed']))
heights = cbind(height_master,height_turker,height_sona)
barplot(heights,beside=T,names=c('Master MTurk','Non-Master MTurk','Course Credit'),col=cols[1:2],xlab='Difficulty',ylab='Mean Question Time (in seconds) ')
legend('topright',c('easy','hard'),col=cols[1:2],pch=c(15,15))


#Binomial (# of perfect recalls) GLM
library(lme4)
glmfit=glm(cbind(recallCount,tasselCount-recallCount)~as.factor(type),data=per_user_image,family=binomial(logit))
glmmfit=glmer(cbind(recallCount,tasselCount-recallCount)~as.factor(type)+(1|user)+(1|image),data=per_user_image,family=binomial(logit))
summary(glmmfit)
anova(glmmfit)
#lme4




#Questions:
#ANOVA normality
#Outlier removed or not (time)
#

####################################################
#####updated 20171012###############################
##Use log-time for question.elapsed
logtime = log(per_user_image$question.elapsed)
per_user_image = cbind(per_user_image,logtime)
hist(per_user_image$logtime, prob=T,xlab='Time (seconds) per Question in Log Scale',main="")
lmfit_time_vs_group = lm(logtime~type,data=per_user_image)

####logtime vs time plot
plot(per_user_image$question.ordinal,per_user_image$logtime,col="#00000033"
     ,pch=20,xlab="Question Ordinal Index", ylab="Time per Question in log scale")
#plot(timepred$question_ordinal,timepred$Pred,col="red",pch=16)
cols = brewer.pal(3,"Dark2")
#Master
abline(3.8522,-0.01043,col=cols[1],lwd=3)
#Turker
abline(3.8457,-0.01073,col=cols[2],lwd=3)
#SONA
abline(3.3837,-0.01181,col=cols[3],lwd=3)
legend('topleft',col=brewer.pal(3,'Dark2'),
       legend = c("Master MTurk","non-Master MTurk","Course Credit"),lwd= c(3,3,3),
       cex=0.5,pt.cex = 0.5)


###Fmean vs logtime
plot(per_user_image$logtime,per_user_image$fmean,
     xlab = 'Time Spent per Question in Log Scale',ylab='Fmean',
     col="#00000033",pch=20)
#master pvalue<0.0001
abline(0.7669,0.006090,col=cols[1],lwd=3)
#turker pvalue <0.0001
abline(0.7579,0.01208,col=cols[2],lwd=3)
#sona pvalue<0.0001
abline(0.5580,0.004452,col=cols[3],lwd=3)
legend('bottomright',col=brewer.pal(3,'Dark2'),
       legend = c("Master MTurk","non-Master MTurk","Course Credit"),
       lwd= c(3,3,3), cex=0.75)

#####READ IN BLUP results
BLUP_fmean = read.csv('./manuscript/image_BLUP_fmean.csv',header=F)
colnames(BLUP_fmean)=c('image','estimate')
hard = rep(NA,nrow(BLUP_fmean))
hard[BLUP_fmean[,2]<0]=1
hard[BLUP_fmean[,2]>=0]=0
BLUP_fmean = cbind(BLUP_fmean,hard)

BLUP_logtime = read.csv('./manuscript/image_BLUP_logtime.csv',header=T)
slow = rep(NA,nrow(BLUP_fmean))
slow[BLUP_logtime[,2]>0]=1
slow[BLUP_logtime[,2]<=0]=0

fit<-lm(BLUP_fmean[,2]~BLUP_logtime[,2])
plot(BLUP_logtime[,2],BLUP_fmean[,2],pch=16,xlab='Image BLUPs for logtime', 
     ylab="Image BLUPs for Fmean",col = c("#af8dc3","#7fbf7b")[new_image_diff[,2]])
abline(coef(fit)[1],coef(fit)[2],col="red")
legend(-0.4,-.05,legend=c("Easy","Hard"),col=c("#af8dc3","#7fbf7b"),pch=16)

image_diff=unique(per_user_image[,c(2,9)])
new_image_diff = image_diff[order(image_diff[,1]),]

plot(per_user_image$difficulty,per_user_image$question.ordinal)
hist(per_user_image[per_user_image$difficulty==1,"question.ordinal"])
hist(per_user_image[per_user_image$difficulty==2,"question.ordinal"])
(per_user_image[per_user_image$image==4,"question.ordinal"])



#####################################################################
#Machine Learning results analysis
ml_sona = read.csv('./ml/sona.csv',quote="'")
ml_turker = read.csv('./ml/turk.csv',quote="'")
ml_master = read.csv('./ml/master.csv',quote="'")

#Delete those that did not complete the survey
sona_users = unique(per_user_image[per_user_image$type==3,"user"])
master_users = unique(per_user_image[per_user_image$type==1,"user"])
turker_users = unique(per_user_image[per_user_image$type==2,"user"])
##Read in raw data
master<-read.csv('~/Documents/labwork/crowdsource/20161215/20161215-selected/d3ai_tassels_mturk_masters_20161205_qualtrics_survey_responses.csv')
turk<-read.csv('~/Documents/labwork/crowdsource/20161215/20161215-selected/d3ai_tassels_mturk_20161205_qualtrics_survey_responses.csv')
sona<-read.csv('~/Documents/labwork/crowdsource/20161215/20161215-selected/d3ai_tassels_sona_20161202_qualtrics_survey_responses.csv')
rawdata = rbind(rbind(master,turk),sona)
sona_ids = levels(rawdata$user.id)[sona_users]
master_ids = levels(rawdata$user.id)[master_users]
turker_ids = levels(rawdata$user.id)[turker_users]
####filter
new_ml_master = ml_master[ml_master[,1] %in% master_ids,]
new_ml_turker = ml_turker[ml_turker[,1] %in% turker_ids,]
new_ml_sona = ml_sona[ml_sona[,1] %in% sona_ids,]

####analysis
hist(new_ml_master$Average.Accuracy,prob=T)
hist(new_ml_turker$Average.Accuracy,prob=T)
hist(new_ml_sona$Average.Accuracy,prob=T)

type = c(rep(1,nrow(new_ml_master)),rep(2,nrow(new_ml_turker)),rep(3,nrow(new_ml_sona)))
ml_data = cbind(rbind(new_ml_master,new_ml_turker,new_ml_sona),type)
ml_fit<-lm(Average.Accuracy~as.factor(type),data=ml_data)
