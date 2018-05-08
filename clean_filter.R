#Cleaning final crowdsourcing data
#None-summary (per-box) data


#experiment 1 was the pilot study
#experiment 2 was the official study

clean_filter<-function(folderToStore, experiment){
  #Import
  if (experiment==2){
    master<-read.csv('./rawdata/20161215-selected/d3ai_tassels_mturk_masters_20161205_qualtrics_survey_responses.csv')
    turk<-read.csv('./rawdata/20161215-selected/d3ai_tassels_mturk_20161205_qualtrics_survey_responses.csv')
    sona<-read.csv('./rawdata/20161215-selected/d3ai_tassels_sona_20161202_qualtrics_survey_responses.csv')
    rawdata = rbind(rbind(master,turk),sona)
  }
  else if (experiment==1){
    rawdata = read.csv('./pilotdata/d3ai_response_data_row_per_user_box_20160928.csv')
  }
  #This filters out all incomplete users
  if (experiment==1){
    data = rawdata[rawdata$user.question.total==40,]
    levels(data$survey.batch)
  }
  else if (experiment==2){
    data = rawdata[rawdata$user.question.total==80,]
    levels(data$survey.batch)
  }

  
  #Functions to calculate F1
  Fmeasure <-function(prec,rec){
    Fvalue = 2*prec*rec/(prec+rec)
    return(Fvalue)
  }
  Fmax<-function(prec,rec){
    #prec and rec have to be vectors of the same length
    if(length(prec)!=length(rec)){
      print("different length")
      return(FALSE)
    }
    else{
      f = Fmeasure(prec,rec)
      id = order(f,decreasing = TRUE)[1]
      return(c(prec[id],rec[id]))
    }
  }
  
  if (experiment==1){
    category = rep(0,nrow(data))
    data1=cbind(data,category)
  }
  else if (experiment==2){
    #read in list of hard and easy images
    easy = read.csv('./easy.txt',header=F)
    hard = read.csv('./hard.txt',header=F)
    easy = cbind(easy,rep(1,length(easy)))
    colnames(easy)=c("image.name","category")
    hard = cbind(hard,rep(2,length(hard)))
    colnames(hard)=c("image.name","category")
    difficulty = rbind(easy,hard)
    #1:easy, 2:hard
    data1 = merge(data,difficulty,by="image.name")
  }
  
  #Renaming images and users
  user = data1$user.id
  levels(user)<-c(1: length(levels(user)))
  image = data1$image.name
  levels(image)<-c(1:length(levels(image)))
  fvalues = Fmeasure(data1$precision,data1$recall)
  type = data1$survey.batch
  if (experiment==1){
    levels(type)<-c(1:2)
    #master:1, sona:3
  }
  else if (experiment==2){
    levels(type)<-c(1:3)
    #master:1, turk:2, sona:3
  }
  
  precision = data1$precision
  recall = data1$recall
  difficulty = data1$category
  #Create a perbox dataset for fvalues
  perbox<-data.frame(cbind(type,user,image,precision,recall,fvalues,difficulty))
  write.csv(perbox,paste0("./",folderToStore,"perbox_filter.csv"))
  #Calculate time per user
  user.time = as.numeric(strptime(data1$user.end.datetime,format="%Y-%m-%d %H:%M:%S" )-strptime(data1$user.start.datetime,format="%Y-%m-%d %H:%M:%S" ))/60
  #hist(user.time)
  #user.time is in minutes
  #question.elapsed in is seconds?
  
  new1<-data.frame(cbind(type,user,image,precision,recall,fvalues,data1$user.question.total,data1$question.ordinal,data1$question.elapsed,user.time,difficulty))
  colnames(new1)[7]<-'user.question.total'
  colnames(new1)[8]<-'question.ordinal'
  colnames(new1)[9]<-'question.elapsed'
  #Create a per image+ user dataset for time, PR and fvalues
  checkIdentity<-function(x){
    #x is a vector of strings
    if (all(x==x[1])) return(x[1])
    else return(stop("not identical"))
  }
  countPerfectRecall<-function(x){
    #x is a vector of recall values from 0 to 1
    c = sum(x>=1)
    return(c)
  }
  countTassels<-function(x){
    #This function just counts the length of the vector
    #x is a vector
    c = length(x)
    return(c)
  }
  #collapse to per user per image
  temp1 = aggregate(cbind(new1$type,new1$question.ordinal,new1$question.elapsed,new1$difficulty),by=list(new1$image,new1$user),checkIdentity)
  colnames(temp1)<-c('image','user','type','question.ordinal','question.elapsed','difficulty')
  temp2 = aggregate(new1$fvalues,by=list(new1$image,new1$user),max,na.rm=T)
  colnames(temp2)<-c('image','user','fmax')
  temp2.1=aggregate(new1$fvalues,by=list(new1$image,new1$user),mean,na.rm=T)
  colnames(temp2.1)<-c('image','user','fmean')
  temp2.2=aggregate(new1$fvalues,by=list(new1$image,new1$user),median,na.rm=T)
  colnames(temp2.2)<-c('image','user','fmedian')
  temp2.3 = merge(temp2.1,temp2.2)
  temp5 = aggregate(new1$recall,by=list(new1$image,new1$user),countPerfectRecall)
  colnames(temp5)<-c('image','user','recallCount')
  temp5.1=aggregate(new1$recall,by=list(new1$image,new1$user),countTassels)
  colnames(temp5.1)<-c('image','user','tasselCount')
  temp5.2 = merge(temp5,temp5.1)
  perUserImage=merge(temp5.2,merge(temp1,merge(temp2,temp2.3)))
  ids1 = which(is.na(perUserImage$fmean))
  ids2 = which(is.infinite(perUserImage$fmean))
  perUserImage = perUserImage[-c(ids1,ids2),]
  write.csv(perUserImage,paste0("./",folderToStore,"per_user_image_filter.csv"))
  #collapse to per user
  temp3 = aggregate(cbind(new1$type,new1$user.time,new1$user.question.total),by=list(new1$user),checkIdentity)
  colnames(temp3)<-c('user','type','user.time','user.question.total')
  temp4 = aggregate(new1$fvalues,by=list(new1$user),max,na.rm=T)
  colnames(temp4)<-c('user','fmax')
  temp4.1 = aggregate(new1$fvalues,by=list(new1$user),mean,na.rm=T)
  colnames(temp4.1)<-c('user','fmean')
  temp4.2 = aggregate(new1$fvalues,by=list(new1$user),median,na.rm=T)
  colnames(temp4.2)<-c('user','fmedian')
  temp4.3=merge(temp4.1,temp4.2)
  perUser = merge(temp3,merge(temp4,temp4.3))
  hist(perUser$user.time)
  write.csv(perUser,paste0("./",folderToStore,"per_user_filter.csv"))
  
  #collapse to per image
  #just care about question.elapsed
  #difficulty not included
  perImage = aggregate(data1$question.elapsed,by=list(data1$image),mean)
  colnames(perImage)<-c('image','meanTimeElapsed')
  write.csv(perImage,paste0("./",folderToStore,"per_image_filter.csv"))
}


