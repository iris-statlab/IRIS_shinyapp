library(tidyverse)
`%notin%` <- Negate(`%in%`)
library(Kendall)
library(reshape2)

sort_data_by_time <- function(df, time_column) {
  # Check if the column is of type factor
  if (is.factor(df[[time_column]])) {
    # Convert factor to numeric
    df[[time_column]] <- as.numeric(as.character(df[[time_column]]))
  }
  
  # Check if the column is numeric after conversion (handle sequence 1, 2, 3, etc.)
  if (is.numeric(df[[time_column]])) {
    df_ordered <- df[order(df[["subject"]], df[[time_column]]), ]
    
    # Check if the column is of Date type or POSIXct/POSIXlt (or dates in other formats)
  } else if (inherits(df[[time_column]], c("Date", "POSIXct", "POSIXlt", "chron"))) {
    df_ordered <- df[order(df[["subject"]], df[[time_column]]), ]
    
    # If the time is a character and looks like a date, try converting it to Date or POSIXct
  } else if (is.character(df[[time_column]])) {
    # Try to convert to POSIXct first
    df[[time_column]] <- as.POSIXct(df[[time_column]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    # If conversion fails (NA values), try Date conversion
    if (any(is.na(df[[time_column]]))) {
      df[[time_column]] <- as.Date(df[[time_column]], format = "%Y-%m-%d")
    }
    
    # Sort after conversion
    df_ordered <- df[order(df[["subject"]], df[[time_column]]), ]
    
  } else {
    stop("Time column must be numeric, factor, date, or time-based character.")
  }
  
  return(df_ordered)
}

volcano<-function(db,pct){
  db<-as.data.frame(db)
  subject<-unique(db$subject)
  N<-length(subject)
  db<-sort_data_by_time(db, "time")
  db<-db %>%
    group_by(subject) %>%
    mutate(time2 = seq(1, n()))
  db<-as.data.frame(db)
  conf<-c("subject","time","age","sex")
  p<-sum(colnames(db) %in% conf)
  res<-data.frame(subject=0, variable=0, MK_Pval=0, MK_tau=0, spearman_Pval=0, spearman_rho=0, nr_obs=0)
  d.mad_subj<-list()
  i<-1; d.ik<-0
  for (s in subject) {
    #s<-"SENS1"; print(s)
    print(s)
    d.i<-db[db$subject==s,] 
    d.i$time2<-seq(1, nrow(d.i))
    mk.p<-NULL
    mk.tau<-NULL
    sp.cor<-NULL
    cor.rho<-NULL
    nr.obs<-NULL
    d.mad_prot<-list()
    for (j in 1:(ncol(d.i)-p-1)) {
      print(j)
      d.ij<-d.i[,c(1,ncol(d.i),j+p)]
      colnames(d.ij)[1:2]<-c("subject","time")
      
      out.mad<-which(d.ij[,3]>median(d.ij[,3])+2.3*mad(d.ij[,3]) | d.ij[,3]<median(d.ij[,3])-2.3*mad(d.ij[,3]))
      d.mad0<-d.ij[-out.mad,]
      
      if(length(out.mad)!=0 & nrow(d.mad0)>3){
        d.mad<-d.mad0
        colnames(d.mad)<-c("subject","time","y")
        d.mad$variable<-names(d.ij)[3]
        
        mk<-MannKendall(d.mad[,3])
        mk.p[j]<-mk$sl[1]
        mk.tau[j]<-mk$tau[1]
        corp<-cor.test(d.mad[,3], d.mad$time, method = "spearman", use="complete.obs", exact = T)
        sp.cor[j]<-corp$p.value
        cor.rho[j]<-corp$estimate
        nr.obs[j]<-length(d.mad$time)
        
        
      }else{
        d.mad<-d.ij
        colnames(d.mad)<-c("subject","time","y")
        d.mad$variable<-names(d.ij)[3]
        
        mk<-MannKendall(d.mad[,3])
        mk.p[j]<-mk$sl[1]
        mk.tau[j]<-mk$tau[1]
        corp<-cor.test(d.mad[,3], d.mad$time, method = "spearman", use="complete.obs", exact = T)
        sp.cor[j]<-corp$p.value
        cor.rho[j]<-corp$estimate
        nr.obs[j]<-length(d.mad$time)
      }
      
      d.mad_prot[[j]]<-d.mad
      d.ij$res<-ifelse(d.ij[,3]>median(d.ij[,3])+2.3*mad(d.ij[,3]) | 
                         d.ij[,3]<median(d.ij[,3])-2.3*mad(d.ij[,3]),1,0) #1 if outliers
      d.ij$variable<-names(d.ij)[3]
      d.ik<-rbind(d.ik,d.ij[,-3]) # all outliers data
    }
    mk.res<-data.frame(subject=rep(unique(d.i$subject),length(mk.p)),
                       variable=names(d.i)[(p+1):((ncol(d.i))-1)], MK_Pval=mk.p, MK_tau=mk.tau, 
                       spearman_Pval=sp.cor, spearman_rho=cor.rho, nr_obs=nr.obs)
    res<-rbind(res,mk.res)
    d.mad_subj[[i]]<-bind_rows(d.mad_prot)
    i<-i+1
  }
  res<-res[-1,]
  d.mad_all<-bind_rows(d.mad_subj)
  d.out<-d.ik[-1,]
  d.outw<-dcast(d.out, subject+time~variable, value.var = "res")
  d.outw$count.id<-rowSums(d.outw[,-c(1:2)])
  d.outw$pct<-d.outw$count.id/(ncol(d.outw)-3)
  res2<-res
  # for(s in subject){
  #   rs<-res[res$subject==s,]
  #   p.mk<-p.adjust(rs$MK_Pval,method="BH",n=nrow(rs))
  #   p.sp<-p.adjust(rs$spearman_Pval,method="BH",n=nrow(rs))
  #   res[res$subject==s,]$MK_Pval<-p.mk
  #   res[res$subject==s,]$spearman_Pval<-p.sp
  # }
  res$log_p_mk<-(-log10(res$MK_Pval))
  res$signf<-ifelse(res$MK_Pval<=0.05,1,0)
  res$high_tau<-ifelse(abs(res$MK_tau)>0.6999, 1,0)
  
  exc_MK<-subset(res, high_tau==1 & signf==1)
  tab<-table(exc_MK$subject, exc_MK$variable)
  sumtab<-as.data.frame(colSums(tab))
  min.pct<-length(unique(db$subject))*(pct/100)
  length(which(sumtab$`colSums(tab)`>min.pct))
  exc_var1<-rownames(sumtab)[which(sumtab$`colSums(tab)`>min.pct)]
  
  res$log_p_cor<-(-log10(res$spearman_Pval))
  res$signf_cor<-ifelse(res$spearman_Pval<=0.05,1,0)
  res$high_cor<-ifelse(abs(res$spearman_rho)>0.6999, 1,0)
  
  exc_cor<-subset(res, high_cor==1 & signf_cor==1)
  tab<-table(exc_cor$subject, exc_cor$variable)
  sumtab<-as.data.frame(colSums(tab))
  min.pct<-length(unique(db$subject))*(pct/100)
  exc_var2<-rownames(sumtab)[which(sumtab$`colSums(tab)`>min.pct)]
  
  exc_dat<-exc_cor[exc_cor$variable %in% c(exc_var1,exc_var2),]
  exc_dat<-exc_dat %>% group_by(variable) %>% summarise(avg.Pval.corr=mean(spearman_Pval), avg.corr.coeff=mean(abs(spearman_rho)),
                                                 avg.Pval.MK=mean(MK_Pval), avg.MK.tau=mean(abs(MK_tau)))
  colnames(exc_dat)<-c("Variable","p-val Spearman", "Spearman coeff.", "p-val Mann-K", "Mann-K tau")
  return(list(
    res=res,
    exc_var=exc_dat,
    d.out=d.outw
  ))
}


trendsub<-function(db,lim){
  db<-as.data.frame(db)
  colnames(db)<-c("subject","time","y")
  subject<-unique(db$subject)
  db<-sort_data_by_time(db, "time")
  db<-db %>%
    group_by(subject) %>%
    mutate(time2 = seq(1, n()))
  db<-as.data.frame(db)
  
  res<-data.frame(subject=0, MK_Pval=0, MK_tau=0, spearman_Pval=0, spearman_rho=0, nr_obs=0)
  cnt<-0; s.i<-0;j<-1
  
  mk.p<-NULL
  mk.tau<-NULL
  sp.cor<-NULL
  cor.rho<-NULL
  nr.obs<-NULL
  
  for (s in subject) {
    print(s)
    d.i<-db[db$subject==s,] 
    out.mad<-which(d.i[,3]>median(d.i[,3])+lim*mad(d.i[,3]) | d.i[,3]<median(d.i[,3])-lim*mad(d.i[,3]))
    d.mad0<-d.i[-out.mad,]
    
    if(length(out.mad)!=0 & nrow(d.mad0)>3){
      d.mad<-d.i[-out.mad,]
      
      mk<-MannKendall(d.mad[,3])
      mk.p[j]<-mk$sl[1]
      mk.tau[j]<-mk$tau[1]
      corp<-cor.test(d.mad[,3], d.mad$time2, method = "spearman", use="complete.obs", exact = T)
      sp.cor[j]<-corp$p.value
      cor.rho[j]<-corp$estimate
      nr.obs[j]<-length(d.mad$time)
      
      cnt<-cnt+1
      
    }else{
      d.mad<-d.i

      mk<-MannKendall(d.mad[,3])
      mk.p[j]<-mk$sl[1]
      mk.tau[j]<-mk$tau[1]
      corp<-cor.test(d.mad[,3], d.mad$time2, method = "spearman", use="complete.obs", exact = T)
      sp.cor[j]<-corp$p.value
      cor.rho[j]<-corp$estimate
      nr.obs[j]<-length(d.mad$time)
      
      cnt<-cnt
    }
    j<-j+1
    s.i<-c(s.i, s)
  }
  s.i<-s.i[-1]
  
  df2<-db[db$subject %in% s.i,]
  d_mad <- df2 %>% 
    group_by(subject) %>% 
    summarize(mad_up = median(y)+lim*mad(y),
              mad_low = median(y)-lim*mad(y))
  #check if each obs is within the outlier limits
  df<-0
  for (s in unique(df2$subject)) {
    df3<-df2[df2$subject==s,]
    df3$res<-ifelse(df3$y>d_mad[d_mad$subject==s,]$mad_up | 
                      df3$y<d_mad[d_mad$subject==s,]$mad_low,1,0) #1 if outliers
    df<-rbind(df,df3)
  }
  df2<-df[-1,]
  res<-data.frame(subject=subject,
                     MK_Pval=mk.p, MK_tau=mk.tau, 
                     spearman_Pval=sp.cor, spearman_rho=cor.rho, nr_obs=nr.obs)

  res$signf<-ifelse(res$MK_Pval<=0.05,1,0)
  res$high_tau<-ifelse(abs(res$MK_tau)>0.6999, 1,0)
  exc_MK<-subset(res, high_tau==1 & signf==1)
  exc_sub1<-exc_MK$subject
  res$signf_cor<-ifelse(res$spearman_Pval<=0.05,1,0)
  res$high_cor<-ifelse(abs(res$spearman_rho)>0.6999, 1,0)
  exc_cor<-subset(res, high_cor==1 & signf_cor==1)
  exc_sub2<-exc_cor$subject
  
  exc_sub<-c(exc_sub1,exc_sub2)
  exc_dat<-res[res$subject %in% exc_sub,]
  colnames(exc_dat)<-c("Subject","p-val Mann-K","Mann-K tau","p-val Spearman","Spearman coeff.",
                       colnames(exc_dat)[6:10])
  df3<-db[db$subject %in% exc_sub,]
  d_mad3 <- df3 %>% 
    group_by(subject) %>% 
    summarize(mad_up = median(y)+lim*mad(y),
              mad_low = median(y)-lim*mad(y))
  

  return(list(
    df2=df2,
    d_mad=d_mad,
    cnt=cnt,
    res=res,
    exc_sub=exc_sub,
    exc_dat=exc_dat,
    df3=df3,
    d_mad3=d_mad3
  ))
}

varboot<-function(db,lim){
  colnames(db)<-c("subject","time","y")
  subject<-unique(db$subject)
  N<-length(subject)
  db <- sort_data_by_time(db, "time")
  B=1000; i<-1
  varmat<-matrix(nrow=N, ncol=B+1)
  for (s in subject) {
    d.i<-db[db$subject==s,] 
    
    for (j in 1:B) {
      varmat[i,j]<-var(sample(d.i$y, length(d.i$y), replace = T))
    }
    varmat[i,B+1]<-(sd(d.i$y)^2)
    i<-i+1
  }
  colnames(varmat)<-c(paste0("B", seq(1,B)), "var")
  varmat<-as.data.frame(varmat)
  varmat$mean.var<-rowMeans(varmat[,1:B])
  varmat$subject<-subject
  varmat.long<-gather(varmat, boot, var.boot, B1:B1000)
  
  mad.up<-median(varmat$mean.var)+lim*mad(varmat$mean.var)
  mad.low<-median(varmat$mean.var)-lim*mad(varmat$mean.var)
  out.mad<-which(varmat[,"mean.var"]>mad.up | varmat[,"mean.var"]<mad.low)
  out.mad<-varmat[out.mad,]$subject
  exc_sub<-varmat[varmat$subject %in% out.mad,(B+3):(B+2)]
  colnames(exc_sub)<-c("Subject","Boostrapped var.")
  rownames(exc_sub)<-NULL
  return(list(
    varmat.long=varmat.long,
    mad.up=mad.up, mad.low=mad.low,
    out.mad=out.mad, exc_sub=exc_sub
  ))
}
