mse.func<-function(x, true.rr){
  y<-x[,"50%",]  
  mse<-apply(y, 2, function(z) mean( (z -true.rr)^2  ))
  return(mse)
}
##Extract characteristics and generate post-pcv time series with known effect
ts.extract.func <- function(ds2a, outcome.name,covar.names='one', 
                          nsim, ve.irr,decline.length, post.start, date.name='date',
                          pre_months=NULL,
                          post_months=NULL){
  ds2a$date<-ds2a[,date.name]
  ds2a$date<-as.Date(as.character(ds2a$date), tryFormats = c('%m/%d/%Y',"%Y-%m-%d", "%Y/%m/%d")) 
  ds2a$index<-1:nrow(ds2a)
  ds2a$sin12<-sin(2*pi*ds2a$index/12)
  ds2a$cos12<-cos(2*pi*ds2a$index/12)
  ds2a$sin6<-sin(2*pi*ds2a$index/6)
  ds2a$cos6<-cos(2*pi*ds2a$index/6)
  ds2a$date<-as.Date(ds2a$date,tryFormats = c('%m/%d/%Y',"%Y-%m-%d", "%Y/%m/%d"))
  ds2a$one<-1
  ds2a$rand<-rnorm(nrow(ds2a), 0, 1e-1 )
    
  if(is.null(covar.names)){
    covar.names<- 'rand'
  }
    
  for(i in 1:length(covar.names)){
    ds2a[, covar.names[i]]<-scale(log(ds2a[, covar.names[i]]+0.5))
  }
  
   #Restrict observed data to pre-vaccine period
  ds3 <- ds2a[ds2a$date< as.Date(post.start, tryFormats = c("%Y-%m-%d",'%m/%d/%Y', "%Y/%m/%d")),] #Just pre-vaccine period
  
  first.obs.date <- ds3$date[1]
  obs.pre.n <-  round(as.numeric(as.Date(post.start) - first.obs.date)/30.3)
  to.fill.pre.n <- pre_months - obs.pre.n 
  to.fill.pre.n[to.fill.pre.n<0] <- 0
  first.fill.date <- first.obs.date - months(to.fill.pre.n)
  
  last.obs.date <- ds3$date[nrow(ds3)]
  obs.post.n <-  round(as.numeric(last.obs.date - as.Date(post.start) )/30.3) +1
  to.fill.post.n <- post_months - obs.post.n
  
  ds3$obs<-as.factor(ds3$index)
  ds3$t.scale<-ds3$index/max(ds3$index)
  
  time.vars<- c('t.scale','sin12','cos12','sin6','cos6')
  covar.names2<- c(covar.names,time.vars)
  ds3.names<-names(ds3)
  miss.names<-which(ds3.names=='')
  if(length(miss.names)==0){
    miss.names<-ncol(ds3)+1
  }
  
  ds3<-ds3[,-miss.names]
    covars2<- paste(covar.names2, collapse="+")
  form1<-as.formula(paste0(outcome.name, '~', covars2, '+(1|obs)'))
  mod1<-glmer(form1, data=ds3, family='poisson',
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl =
                                       list(maxfun = 2e6)))


    #Combine observed data with filled data for additional pre-time points
    if(to.fill.pre.n>0){
      covars<-ds3[,covar.names2]
      covars<-cbind.data.frame(rep(1, times = nrow(covars)),covars)
      
      index.pre <-  as.numeric(-(to.fill.pre.n-1):0)
      index.pre.t.scale <- index.pre/  max(ds3$index)
      sin12.pre <-sin(2*pi*index.pre/12)
      cos12.pre <-cos(2*pi*index.pre/12)
      sin6.pre <-sin(2*pi*index.pre/6)
      cos6.pre <-cos(2*pi*index.pre/6)
      covars.pre <- matrix(NA, nrow=length(index.pre), ncol=length(covar.names))
      covars.pre[] <-  as.vector(ds3[which(first.obs.date ==ds3$date), covar.names])
      covars.pre <- as.data.frame(covars.pre)
      names(covars.pre) <- covar.names
      pre.append <- cbind.data.frame(rep(1, times = nrow(covars.pre)) ,covars.pre, 't.scale'=index.pre.t.scale,'sin12'=sin12.pre, 'cos12'=cos12.pre, 'sin6'=sin6.pre, 'cos6'=cos6.pre)
      names(pre.append) <- names(covars)
      
      covars.pre.obs <- bind_rows(pre.append,covars)    
    }else{
      covars.pre.obs <- ds3[(nrow(ds3)-pre_months+1):nrow(ds3),covar.names2]
      covars.pre.obs <- cbind.data.frame('Int'=rep(1, times = nrow(covars.pre.obs)) ,covars.pre.obs)
      }
  
    #Combine observed data with filled data for additional post-time points
    max.obs.index <- max(ds3$index)+1
    index.post <-  max.obs.index:(max.obs.index+to.fill.post.n-1)
    index.post.t.scale <- index.post/  max(ds3$index)
    sin12.post <-sin(2*pi*index.post/12)
    cos12.post <-cos(2*pi*index.post/12)
    sin6.post <-sin(2*pi*index.post/6)
    cos6.post <-cos(2*pi*index.post/6)
    covars.post <- matrix(NA, nrow=length(index.post), ncol=length(covar.names))
    covars.post[] <-  as.vector(ds3[which(last.obs.date ==ds3$date), covar.names])
    covars.post <- as.data.frame(covars.post)
    names(covars.post) <- covar.names
    post.append <- cbind.data.frame(rep(1, times = nrow(covars.post)) ,covars.post, 't.scale'=index.post.t.scale,'sin12'=sin12.post, 'cos12'=cos12.post, 'sin6'=sin6.post, 'cos6'=cos6.post)
    names(post.append) <- names(covars.pre.obs)
    
    covars.pre.post.obs <- bind_rows(covars.pre.obs,post.append)    
    
  pred.coefs.reg.mean <-
    mvrnorm(n = nsim,
            mu = fixef(mod1),
            Sigma = vcov(mod1))
  preds.stage1.regmean <-(as.matrix(covars.pre.post.obs) %*% t(pred.coefs.reg.mean))
  int1<-fixef(mod1)[1]
  
  max.post.time<- min(decline.length,(nrow(post.append)))
  additional.fill.post.n <- to.fill.post.n - max.post.time +1
  
  ve.effect<- c( rep(0, nrow(covars.pre.obs)),  
                 seq(from=0, to=log(ve.irr), length.out=max.post.time), 
                 rep(log(ve.irr),length.out=(additional.fill.post.n-1) ))

  re.sd<-as.numeric(sqrt(VarCorr(mod1)[[1]]))
  re.int<-rnorm(n<-length(preds.stage1.regmean), mean=0, sd=re.sd) 
  preds.stage1.regmean<-preds.stage1.regmean + ve.effect +re.int
  
  preds.stage1<-exp(matrix(preds.stage1.regmean, nrow=nrow(preds.stage1.regmean), ncol=ncol(preds.stage1.regmean)))
  preds.stage2<-matrix(rpois(n=length(preds.stage1), lambda=preds.stage1), nrow=nrow(preds.stage1))
  intervention_date <- post.start
  first_date <- intervention_date - months(pre_months)
  time_points <- seq.Date(from=first_date, length.out=nrow(preds.stage2), by='month')
  
  #OBSERVED pre-vaccine time points, subtractin those that are filled
    n.obs.date.use <-pre_months - to.fill.pre.n
    if(n.obs.date.use<0){n.obs.date.use==0}
    first.obs.date.use <- intervention_date - months(n.obs.date.use)
    obs.dates.use <- seq.Date(from=first.obs.date.use, by='month', length.out=n.obs.date.use)

  last.pre.vax.index <- which(time_points ==intervention_date) - 1
  obs.indices.keep <- (last.pre.vax.index-n.obs.date.use+1) : last.pre.vax.index
  preds.stage2[obs.indices.keep,] <-  ds3[obs.indices.keep,outcome.name]
 

  res1<-list('preds.stage2'=preds.stage2, 're.sd'=re.sd, 'int'=int1,
             'covars'=covars.pre.post.obs[,covar.names,drop=F],
             'time_points'=time_points)
  return(res1)
}


######################################
its_func <- function(ds,
                     time_points,
                     covars=x$covars,
                     seasonN = 12 ,
                     intervention_date2,
                     overdispersed=F){
  
  post_period<-as.Date(c(intervention_date2, time_points[length(time_points)]))
  n.pre.time<-which(time_points==post_period[1])-1 #+12 #start vax effect 12 months after intro
  max.post.time<- min(decline.length,(nrow(ds)- n.pre.time))
  
  eval_period <- post_period
  eval_period[1]<- intervention_date2 %m+% months(max.post.time) #declines for decline.lengthm or less
  #ds<-res1$preds.stage2[,1]
  #covars<-res1$covars
  ds <- cbind.data.frame(ds, covars)
  names(ds)[1]<- 'outcome'
  covar.names=names(covars)
  ds$post1 <- 0
  
  
  ds$post1[time_points >= post_period[1] &
             time_points < eval_period[1]] <- 1
  ds$post2 <- 0
  ds$post2[time_points >= eval_period[1]] <- 1
  ds$one=1
  ds$time_index<- 1:nrow(ds)
  ds$spl<- ds$time_index - ds$time_index[time_points == post_period[1]]
  ds$spl[ds$spl<0]<- 0
  ds$spl[time_points >= eval_period[1]]<- ds$spl[time_points == eval_period[1]] #level out
  
  
  if(seasonN==12){
    seas.vars<- c('sin12','cos12','sin6','cos6' )
    ds$time_index<-1:nrow(ds)
    ds$sin12<-sin(2*pi* ds$time_index/12)
    ds$cos12<-cos(2*pi* ds$time_index/12)
    ds$sin6<-sin(2*pi* ds$time_index/6)
    ds$cos6<-cos(2*pi* ds$time_index/6)
  }else if(seasonN==4){
    seas.vars<- c('qtr2','qtr3','qtr4')
    ds$qtr<-quarter(time_points)
    ds$year<-year(time_points)
    ds$qtrdate<- ds$year +ds$qtr/4 - 1/4
    ds<-aggregate(ds, by=list('qtrdate'=ds$qtrdate), FUN=sum)
    ds$post1[ds$post1>0]<-1
    ds$post2[ds$post2>0]<-1
    ds$year<-floor(ds$qtrdate)
    ds$qtr= ds$qtrdate-ds$year +0.25
    ds$qtr2<-0
    ds$qtr3<-0
    ds$qtr4<-0
    
    ds$qtr2[ds$qtr==0.5]<-1
    ds$qtr3[ds$qtr==0.75]<-1
    ds$qtr4[ds$qtr==1.0]<-1
  }
  ds$time_index<-1:nrow(ds)
  ds$time_index <- ds$time_index / max(ds$time_index)
  ds$obs <- as.factor(1:nrow(ds))
  ds$time_post1_int<-ds$time_index*ds$post1
  ds$time_post2_int<-ds$time_index*ds$post2
  covar.vec1<- c(covar.names,seas.vars, 'time_index')
  covar.vec.vax<-c('spl')
  re.names<- '(1|obs)'
  
  #if(overdispersed==T){
  #covar.vec.plus.sep<-paste(c(covar.vec1,covar.vec.vax,re.names), collapse='+')
  #}else{
    covar.vec.plus.sep<-paste(c(covar.vec1,covar.vec.vax), collapse='+')
  #}
    form1<-as.formula(paste0('outcome~'   ,covar.vec.plus.sep,'+ (1|obs)'  ))
    mod1 <-
      glmer(form1,data = ds,family = 'poisson')
    fixed.eff<-fixef(mod1)
    
   #GENERATE PREDICTIONS
  covars3 <-
    as.matrix(cbind(ds[, c(covar.vec1,covar.vec.vax)]))
  covars3 <- cbind.data.frame(rep(1, times = nrow(covars3)), covars3)
  names(covars3)[1] <- "Intercept"
  pred.coefs.reg.mean <-
    mvrnorm(n = 1000,
            mu = fixed.eff,
            Sigma = vcov(mod1))
  rand.eff.sd<-summary(mod1)$varcor[[1]]
  rand.eff<-rnorm( nrow(covars3)*1000,0, rand.eff.sd)
  rand.eff<-matrix(rand.eff, nrow=nrow(covars3))
  preds.stage1.regmean <-
    exp(as.matrix(covars3) %*% t(pred.coefs.reg.mean) 
        +rand.eff )

  #re.sd<-as.numeric(sqrt(VarCorr(mod1)[[1]]))
  #preds.stage1<-rnorm(n<-length(preds.stage1.regmean), mean=preds.stage1.regmean, sd=re.sd)
  #preds.stage1<-exp(matrix(preds.stage1, nrow=nrow(preds.stage1.regmean), ncol=ncol(preds.stage1.regmean)))
  
  #Then for counterfactual, set post-vax effects to 0.
  covars3.cf <-   as.matrix(cbind(ds[, c(covar.vec1)], matrix(0,nrow=nrow(ds),ncol=length(covar.vec.vax) ) ))
  covars3.cf <-
    cbind.data.frame(rep(1, times = nrow(covars3.cf)), covars3.cf)
  preds.stage1.regmean.cf <-    exp(as.matrix(covars3.cf) %*% t(pred.coefs.reg.mean) +rand.eff )
  #preds.stage1.cf<-rnorm(n<-length(preds.stage1.regmean.cf), mean=preds.stage1.regmean.cf, sd=re.sd)
  #preds.stage1.cf<-exp(matrix(preds.stage1.cf, nrow=nrow(preds.stage1.regmean.cf), ncol=ncol(preds.stage1.regmean.cf)))
  
  rr.t <- preds.stage1.regmean / preds.stage1.regmean.cf
  rr.q.t <- t(apply(rr.t, 1, quantile, probs = c(0.025, 0.5, 0.975)))
  
  last.t<-nrow(rr.t) #evaluate at last time point
  preds.stage1.regmean.SUM <-   preds.stage1.regmean[last.t, ]
  preds.stage1.regmean.cf.SUM <-preds.stage1.regmean.cf[last.t, ]
  rr.post <- preds.stage1.regmean.SUM / preds.stage1.regmean.cf.SUM
  rr.q.post <- quantile(rr.post, probs = c(0.025, 0.5, 0.975))
  #matplot(rr.q, type='l', bty='l', lty=c(2,1,2), col='gray')
  #abline(h=1)
  
  # rr.q.last<- rr.q.t[nrow(rr.q.t),]
  #rr.out <- list('rr.q.post' = rr.q.post)
  return(rr.q.post)
}

est.vax.eff<-  function(x, overdisperse1=F,n.season=12, intervention_date){
  ve<-t(apply(x$preds.stage2, 2,its_func ,seasonN=n.season, covars=x$covars, intervention_date2=intervention_date,overdispersed=overdisperse1, time_points=x$time_points) )
  return(ve)
}
