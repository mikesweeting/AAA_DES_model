screen.detected <- function(i,personsInfo,v1other){
  # Screen-detected AAA
  if (("screen" %in% personsInfo$eventHistories[[i]][["screening"]]$events && 
       !("nonvisualization" %in% personsInfo$eventHistories[[i]][["screening"]]$events) &&
       (personsInfo$eventHistories[[i]][["screening"]]$measuredSizes[match("screen", personsInfo$eventHistories
                                                                           [[i]][["screening"]]$events)] >= 
        v1other$aortaDiameterThresholds[1]))){
    num <- 1
    which.min <- min(which(personsInfo$eventHistories[[i]][["screening"]]$events=="screen"))
    firsttime <- personsInfo$eventHistories[[i]][["screening"]]$times[which.min]
  } else {
    num <- 0
    firsttime <- NA
  }
  attr(num, "firsttime") <- firsttime
  return(num)
}

incidentally.detected<-function(i,personsInfo,treatmentGroup,v1other){
  # Number of women with incidentally detected AAAs and no screen detection
  if(treatmentGroup=="noScreening"){
    ## No screening group
    if (("incidentalDetection" %in% personsInfo$eventHistories[[i]][["noScreening"]]$events)){
      num <- 1
      which.min <- min(which(personsInfo$eventHistories[[i]][["noScreening"]]$events=="incidentalDetection"))
      firsttime <- personsInfo$eventHistories[[i]][["noScreening"]]$times[which.min]
    } else {
      num <- 0
      firsttime <- NA
    }
  }
  ## screening group
  if(treatmentGroup=="screening"){
    if (("incidentalDetection" %in% personsInfo$eventHistories[[i]][["screening"]]$events) 
        && !("screen" %in% personsInfo$eventHistories[[i]][["screening"]]$events && 
             !("nonvisualization" %in% personsInfo$eventHistories[[i]][["screening"]]$events) &&
             (personsInfo$eventHistories[[i]][["screening"]]$measuredSizes[match("screen", personsInfo$eventHistories
                                                                                 [[i]][["screening"]]$events)] >= 
              v1other$aortaDiameterThresholds[1]))){
      num <- 1
      which.min <- min(which(personsInfo$eventHistories[[i]][["screening"]]$events=="incidentalDetection"))
      firsttime <- personsInfo$eventHistories[[i]][["screening"]]$times[which.min]
    } else {
      num <- 0
      firsttime <- NA
    }
  }  
  attr(num, "firsttime") <- firsttime
  return(num)
}  

elective.repair <- function(i,personsInfo,treatmentGroup){
  if("electiveSurgeryOpen" %in% personsInfo$eventHistories[[i]][[treatmentGroup]]$events | 
     "electiveSurgeryEvar" %in% personsInfo$eventHistories[[i]][[treatmentGroup]]$events){
    num<-1 
    which.min <- min(which(personsInfo$eventHistories[[i]][[treatmentGroup]]$events %in% c("electiveSurgeryOpen", "electiveSurgeryEvar")))
    firsttime <- personsInfo$eventHistories[[i]][[treatmentGroup]]$times[which.min]
  } else {
    num<-0
    firsttime <- NA
  }
  attr(num, "firsttime") <- firsttime
  return(num)
}

emergency.repair <- function(i,personsInfo,treatmentGroup){
  if("emergencySurgeryOpen" %in% personsInfo$eventHistories[[i]][[treatmentGroup]]$events | 
     "emergencySurgeryEvar" %in% personsInfo$eventHistories[[i]][[treatmentGroup]]$events){
    num<-1 
    which.min <- min(which(personsInfo$eventHistories[[i]][[treatmentGroup]]$events %in% c("emergencySurgeryOpen", "emergencySurgeryEvar")))
    firsttime <- personsInfo$eventHistories[[i]][[treatmentGroup]]$times[which.min]
  } else {
    num<-0
    firsttime <- NA
  }
  attr(num, "firsttime") <- firsttime
  return(num)
}

aaaDeathElective<-function(i,personsInfo,treatmentGroup){
  if("aaaDeath" %in% personsInfo$eventHistories[[i]][[treatmentGroup]]$events & 
     ("electiveSurgeryEvar" %in% personsInfo$eventHistories[[i]][[treatmentGroup]]$events |
      "electiveSurgeryOpen" %in% personsInfo$eventHistories[[i]][[treatmentGroup]]$events)){
    num<-1 
  } else num<-0
  return(num)
}
singleEvent <- function(i,personsInfo,treatmentGroup,event){
  if(event %in% personsInfo$eventHistories[[i]][[treatmentGroup]]$events){
    num <-1
    which.min <- min(which(personsInfo$eventHistories[[i]][[treatmentGroup]]$events == event))
    firsttime <- personsInfo$eventHistories[[i]][[treatmentGroup]]$times[which.min]
  }  else {
    num<-0
    firsttime <- NA
  }
  attr(num, "firsttime") <- firsttime
  return(num)
}

## Main cost-effectiveness table
tab.ce<-function(res=NULL,res.sampled){
  tab<-as.data.frame(t(res$meanQuantities))
  #tab[,"Difference"]<-tab[,2]-tab[,1]
  tab[,"Difference"]<-res.sampled$incrementalMeanQuantities
  tab[,2]<-tab[,3]+tab[,1]
  tab<-rbind(NA,tab[1,],tab[4,],tab[5,],NA,NA,tab[3,],tab[6,])
  tab<-cbind(NA,NA,tab)
  # ICER (discounted)
  tab[11,"Difference"]<-tab["discountedCost","Difference"]/tab["discountedLifeYears","Difference"]
  # ICER (discounted, QA)
  tab[12,"Difference"]<-tab["discountedCost","Difference"]/tab["discountedQalys","Difference"]
  tab[,1]<-c("Life-years","","","","","Costs (GBP)","","","","ICER (GBP per life-year or QALY gained)","","")
  tab[,2]<-c("","Undiscounted","Discounted","Discounted, QA","","","Undiscounted","Discounted","","","Discounted, life-years","Discounted, QA") 
  colnames(tab)<-c("","","Not invited to screening","Invited to screening","Difference")
  return(tab)
}

## Events table
tab.events <- function(personsInfo,personsInfoDifference=NULL,v0,v1other){
  n<-length(personsInfo$eventHistories)
  df<-data.frame("ns"=NA,"s"=NA)
  rownames(df)[1]<-"AAAs detected"
  df["Screen detected","ns"]<-0
  df["Screen detected","s"]<-sum(unlist(lapply(1:n,screen.detected,personsInfo,v1other=v1other)))
  df["Incidentally detected","ns"]<-sum(unlist(lapply(1:n,incidentally.detected,personsInfo,treatmentGroup="noScreening",v1other=v1other)))
  df["Incidentally detected","s"]<-sum(unlist(lapply(1:n,incidentally.detected,personsInfo,treatmentGroup="screening",v1other=v1other)))
  df["AAAs detected",]<-df["Screen detected",]+df["Incidentally detected",]
  df["Consultation for elective AAA repair","ns"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="noScreening",event="consultation")))
  df["Consultation for elective AAA repair","s"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="screening",event="consultation")))
  df["Elective AAA repair","ns"]<-sum(unlist(lapply(1:n,elective.repair,personsInfo,treatmentGroup="noScreening")))
  df["Elective AAA repair","s"]<-sum(unlist(lapply(1:n,elective.repair,personsInfo,treatmentGroup="screening")))
  df["Elective AAA repair contraindicated","ns"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="noScreening",event="contraindicated")))
  df["Elective AAA repair contraindicated","s"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="screening",event="contraindicated")))
  df["AAA rupture","ns"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="noScreening",event="rupture")))
  df["AAA rupture","s"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="screening",event="rupture")))
  df["Emergency AAA repair","ns"]<-sum(unlist(lapply(1:n,emergency.repair,personsInfo,treatmentGroup="noScreening")))
  df["Emergency AAA repair","s"]<-sum(unlist(lapply(1:n,emergency.repair,personsInfo,treatmentGroup="screening")))
  df["AAA-related deaths","ns"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="noScreening",event="aaaDeath")))
  df["AAA-related deaths","s"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="screening",event="aaaDeath")))
  df["Due to elective surgery / long-term complications of elective repair","ns"]<-sum(unlist(lapply(1:n,aaaDeathElective,personsInfo,treatmentGroup="noScreening")))
  df["Due to elective surgery / long-term complications of elective repair","s"]<-sum(unlist(lapply(1:n,aaaDeathElective,personsInfo,treatmentGroup="screening")))
  df["Due to rupture / long-term complications of emergency repair","ns"]<-df["AAA-related deaths","ns"]-df["Due to elective surgery / long-term complications of elective repair","ns"]
  df["Due to rupture / long-term complications of emergency repair","s"]<-df["AAA-related deaths","s"]-df["Due to elective surgery / long-term complications of elective repair","s"]
  df["Non AAA-related deaths","ns"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="noScreening",event="nonAaaDeath")))
  df["Non AAA-related deaths","s"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="screening",event="nonAaaDeath")))
  df["Re-intervention after elective repair","ns"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="noScreening",event="reinterventionAfterElectiveEvar")))+
    sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="noScreening",event="reinterventionAfterElectiveOpen")))
  df["Re-intervention after elective repair","s"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="screening",event="reinterventionAfterElectiveEvar")))+
    sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="screening",event="reinterventionAfterElectiveOpen")))
  df["Re-intervention after emergency repair","ns"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="noScreening",event="reinterventionAfterEmergencyEvar")))+
    sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="noScreening",event="reinterventionAfterEmergencyOpen")))
  df["Re-intervention after emergency repair","s"]<-sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="screening",event="reinterventionAfterEmergencyEvar")))+
    sum(unlist(lapply(1:n,singleEvent,personsInfo,treatmentGroup="screening",event="reinterventionAfterEmergencyOpen")))
  monitor<-eventsandcosts.shiny(personsInfo,event="monitor",v0)
  df["Surveillance measurements","s"]<-monitor[,"screening.n"]
  df["Surveillance measurements","ns"]<-monitor[,"noScreening.n"]
    
  df<-cbind(NA,NA,df)
  if(is.null(personsInfoDifference)){
    df$diff<-df$s-df$ns
    df$perc<-paste0("(",round(100*df$diff/df$ns),"%)")
  } else {
    df$diff<-round(personsInfoDifference$incrementalEvents$Difference)
    df$s<-df$ns+df$diff
    df$perc<-paste0("(",round(100*df$diff/df$ns),"%)")
  }
  colnames(df)<-c("","","Not invited to screening","Invited to screening","Difference","(% of that in non-invited group)")
  df[c(1,4:9,12:15),1]<-rownames(df)[c(1,4:9,12:15)]
  df[c(2:3,10:11),2]<-rownames(df)[c(2:3,10:11)]
  rownames(df)<-NULL
  return(df)
}  




eventsandcosts.shiny<-function(result,events=c("inviteToScreen","screen","requireReinvitation","failToAttendScreen","nonvisualization","monitor",
                                               "dropout","incidentalDetection","consultation","decideOnElectiveSurgery","decideOnReturnToMonitoring",
                                               "contraindicated","monitorFollowingContraindication","electiveSurgeryEvar","electiveSurgeryOpen",
                                               "rupture","emergencySurgeryEvar","emergencySurgeryOpen",
                                               "monitorFollowingEvarSurgery","monitorFollowingOpenSurgery","reinterventionAfterElectiveEvar",
                                               "reinterventionAfterEmergencyEvar","reinterventionAfterEmergencyOpen","aaaDeath","nonAaaDeath","censored"),
          v0){
  eventsandcosts.df<-data.frame(event=NA,screening.n=NA,noScreening.n=NA)
  ordered.events<-events
  
  
  noScreening.events<-unlist(sapply(1:v0$numberOfPersons,function(i){result$eventHistories[[i]]$noScreening$events}))
  screening.events<-unlist(sapply(1:v0$numberOfPersons,function(i){result$eventHistories[[i]]$screening$events}))
  i<-0
  for(event in ordered.events){
    i<-i+1
    if(event %in% screening.events){
      eventsandcosts.df[i,"event"]<-event
      eventsandcosts.df[i,"screening.n"]<-table(screening.events)[names(table(screening.events))==event]
    }
    if(event %in% noScreening.events){
      eventsandcosts.df[i,"event"]<-event
      eventsandcosts.df[i,"noScreening.n"]<-table(noScreening.events)[names(table(noScreening.events))==event]
    }
    
  }
  return(eventsandcosts.df)
}


## Convergence plot
conv.plot<-function(result){
  data<-as.data.frame(result$incrementalCumMean)
  data$x<-1:(dim(data)[1])
  data$ICER<-data$discountedCost/data$discountedQalys
  setToNA<-function(vec,lower=0.01,upper=0.99){
    low<-quantile(vec,lower)
    high<-quantile(vec,upper)
    vec[vec<low]<-NA
    vec[vec>high]<-NA
    vec
  }
  data$lifeYears.new<-setToNA(data$lifeYears)
  data$cost.new<-setToNA(data$cost)
  data$ICER.new<-setToNA(data$ICER)
  a<-ggplot(data=data,aes(x=x))+geom_line(aes(y=lifeYears.new),col="blue")+ylab("Mean incremental life-years")+xlab("Number of patient pairs (simulations)")
  b<-ggplot(data=data,aes(x=x))+geom_line(aes(y=cost.new),col="red")+ylab("Mean incremental costs (GBP)")+xlab("Number of patient pairs (simulations)")
  c<-ggplot(data=data,aes(x=x))+geom_line(aes(y=ICER.new),col="black")+ylab("Incremental cost effectiveness ratio (GBP/QALY)")+xlab("Number of patient pairs (simulations)")
  return(list(LY=a,Cost=b,ICER=c))
}

## Other convergence plots
other.conv.plot<-function(result,event,v1other){
  n<-length(result$eventHistories) 
  if(event=="electiveRepair"){
    data<-data.frame(x=1:n,y=cumsum(unlist(lapply(1:n,elective.repair,result,treatmentGroup="noScreening")))/(1:n))
  } else if(event=="screenDetected") {
    data<-data.frame(x=1:n,y=cumsum(unlist(lapply(1:n,screen.detected,result,v1other=v1other)))/(1:n))
  } else if(event=="incidentallyDetected") {
    data<-data.frame(x=1:n,y=cumsum(unlist(lapply(1:n,incidentally.detected,result,treatmentGroup="noScreening",v1other=v1other)))/(1:n))
  } else if(event=="emergencyRepair") {
    data<-data.frame(x=1:n,y=cumsum(unlist(lapply(1:n,emergency.repair,result,treatmentGroup="noScreening")))/(1:n)) 
  } else {
    data<-data.frame(x=1:n,y=cumsum(unlist(lapply(1:n,singleEvent,result,treatmentGroup="noScreening",event=event)))/(1:n))
  }
  a<-ggplot(data=data,aes(x=x,y=y))+geom_line(col="blue")+ylab("Proportion of not-invited to screening population with event")+xlab("Number of patient pairs (simulations)")
  return(a)
}


# ## Function to create events plot data.frame for any treatment group using standard sampling
# eventsPlotData<-function(personsInfo, events, treatmentGroup) {
#   
#   data<-data.frame(time=NULL,event=NULL)
#   
#   b0<-sapply(personsInfo$eventHistories,function(i){i[[treatmentGroup]]$b0})
#   b1<-sapply(personsInfo$eventHistories,function(i){i[[treatmentGroup]]$b1})
#   for(event in events){
#     # extract event number corresponding to "event" for each individual. NA means the event does not occur
#     eventNumber<-sapply(personsInfo$eventHistories,function(i){match(event,i[[treatmentGroup]]$events)})
#     ## extract event times for those who have the event. NA mean the event does not occur
#     eventTimes<-sapply(1:length(personsInfo$eventHistories),function(i){personsInfo$eventHistories[[i]][[treatmentGroup]]$times[eventNumber[i]]})
#     ## extract AAA sizes at event times
#     eventAAASize<-exp(b0+b1*eventTimes)
#     if(any(!is.na(eventTimes))){
#       data<-rbind(data,data.frame(time=eventTimes[!is.na(eventTimes)],event=event,AAASize=eventAAASize[!is.na(eventTimes)]))
#     }
#   }
#   
#   return(data=data)
# }

## Function to create events plot data.frame for both no screened and screened group (using sampling over the threshold)
eventsPlotData<-function(personsInfo,personsInfoOver,events,threshold){
  
  data<-data.frame(time=NULL,event=NULL,AAASize=NULL,treatmentGroup=NULL)
  
  ## No Screening
  b0.noScreening<-sapply(personsInfo$eventHistories,function(i){i[["noScreening"]]$b0})
  b1.noScreening<-sapply(personsInfo$eventHistories,function(i){i[["noScreening"]]$b1})
  
  ## Screening group (over threshold)
  b0.screening.over<-sapply(personsInfoOver$eventHistories,function(i){i[["screening"]]$b0})
  b1.screening.over<-sapply(personsInfoOver$eventHistories,function(i){i[["screening"]]$b1})
  noScreening.under<-sapply(personsInfo$eventHistories,function(i){i[["noScreening"]]$initialAortaSizeAsMeasured})<threshold
  
  
  for(event in events){
    ## noScreening group
    # extract event number corresponding to "event" for each individual. NA means the event does not occur
    eventNumber<-sapply(personsInfo$eventHistories,function(i){match(event,i[["noScreening"]]$events)})
    ## extract event times for those who have the event. NA mean the event does not occur
    eventTimes<-sapply(1:length(personsInfo$eventHistories),function(i){personsInfo$eventHistories[[i]][["noScreening"]]$times[eventNumber[i]]})
    ## extract AAA sizes at event times
    eventAAASize<-exp(b0.noScreening+b1.noScreening*eventTimes)
    if(any(!is.na(eventTimes))){
      data<-rbind(data,data.frame(time=eventTimes[!is.na(eventTimes)],event=event,
                                  AAASize=eventAAASize[!is.na(eventTimes)],treatmentGroup="Not invited to screening"))
    }
    
    ## screening group
    ## over threshold
    # extract event number corresponding to "event" for each individual. NA means the event does not occur
    eventNumberOver<-sapply(personsInfoOver$eventHistories,function(i){match(event,i[["screening"]]$events)})
    # extract event number corresponding to "event" for each individual. NA means the event does not occur
    eventNumberScreening<-eventNumber
    eventNumberScreening[noScreening.under==F]<-eventNumberOver[noScreening.under==F]
    ## extract event times for those who have the event. NA mean the event does not occur
    eventTimesOver<-sapply(1:length(personsInfoOver$eventHistories),function(i){personsInfoOver$eventHistories[[i]][["screening"]]$times[eventNumberOver[i]]})
    eventTimesScreening<-eventTimes
    eventTimesScreening[noScreening.under==F]<-eventTimesOver[noScreening.under==F]
    ## extract AAA sizes at event times
    eventAAASizeOver<-exp(b0.screening.over+b1.screening.over*eventTimesOver)
    eventAAASizeScreening<-eventAAASize
    eventAAASizeScreening[noScreening.under==F]<-eventAAASizeOver[noScreening.under==F]
    if(any(!is.na(eventTimes))){
      data<-rbind(data,data.frame(time=eventTimesScreening[!is.na(eventTimesScreening)],event=event,
                                  AAASize=eventAAASizeScreening[!is.na(eventTimesScreening)],
                                  treatmentGroup="Invited to screening"))
    }
    
  }
  
  return(data=data)
}

## Function to plot a 2d scatter plot of event against AAA diameter
eventsPlot<-function(data,event,v1other){
  data<-data[data$event==event,]
  
  xlim<-c(0,max(data$time)+5)
  ylim<-c(min(data$AAASize)-1,max(data$AAASize)+1)
  diag.thresh<-v1other$aortaDiameterThreshold[1]
  inter.thresh<-v1other$aortaDiameterThreshold[length(v1other$aortaDiameterThresholds)]
  # ggplot(data,aes(x=time,y=AAASize))+
  #   stat_density2d(aes(alpha=..level..), geom="polygon")+facet_wrap(~treatmentGroup)+
  #   scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))+
  #   geom_point(colour="red",alpha=0.3)+
  #   lims(x = xlim,y = ylim)+
  #   geom_hline(yintercept=c(diag.thresh,inter.thresh),linetype=2)+
  #   xlab("Time since screening (years)")+
  #   ylab("Aorta size (cm)")+guides(alpha=guide_legend(title="Density"))

  ggplot(data,aes(x=time,y=AAASize))+
    stat_density2d(aes(alpha=..level..,fill=..level..),geom="polygon")+facet_wrap(~treatmentGroup)+
    scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))+
    geom_point(colour="red",alpha=0.3)+
    lims(x = xlim,y = ylim)+
    geom_hline(yintercept=c(diag.thresh,inter.thresh),linetype=2)+
    xlab("Time since screening (years)")+
    ylab("Aorta size (cm)")+guides(alpha=guide_legend(title="Density"))+scale_fill_viridis_c()
  
}  


## Time under surveillance 
# (i.e. time from screen detection or incidental detection to elective or emergency surgery or death or censoring)
## IGNORING DROPOUT AND RE-ENTERING SCREENING PROGRAMME 
## ASSUMING SURVEILLANCE TIME IS FROM DETECTION TO DEATH / SURGERY
surveillance.time <- function(i, personsInfo, treatmentGroup, v1other){
  elr.time <- attributes(elective.repair(i, personsInfo, treatmentGroup))$firsttime
  emr.time <- attributes(emergency.repair(i, personsInfo, treatmentGroup))$firsttime
  scr.time <- attributes(screen.detected(i, personsInfo, v1other))$firsttime
  incdet.time <- attributes(incidentally.detected(i, personsInfo, treatmentGroup, v1other))$firsttime
  aaadeath.time <- attributes(singleEvent(i, personsInfo, treatmentGroup, event = "aaaDeath"))$firsttime
  nonaaadeath.time <- attributes(singleEvent(i, personsInfo, treatmentGroup, event = "nonAaaDeath"))$firsttime
  censored.time <- attributes(singleEvent(i, personsInfo, treatmentGroup, event = "censored"))$firsttime
  end.time <- min(elr.time, emr.time, aaadeath.time, nonaaadeath.time, censored.time, na.rm=T)
  if(!is.na(scr.time) | !is.na(incdet.time)){
    start.time <- min(scr.time, incdet.time, na.rm = T)  
  } else {
    start.time <- NA
  }
  surv.time <- end.time - start.time
  return(surv.time)
}

