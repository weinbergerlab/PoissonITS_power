##Extract characteristics and generate post-pcv time series with known effect
ts.extract.func<-function(ds2, outcome.name,covar.names='one', 
                          nsim, ve.irr, post.start){
 
  ds2$index<-1:nrow(ds2)
  ds2$sin12<-sin(2*pi*ds2$index/12)
  ds2$cos12<-cos(2*pi*ds2$index/12)
  ds2$sin6<-sin(2*pi*ds2$index/6)
  ds2$cos6<-cos(2*pi*ds2$index/6)
  ds2$date<-as.Date(ds2$date)
  ds2$t.scale<-ds2$index/max(ds2$index)
  ds2$one<-1
  for(i in 1:length(covar.names)){
    ds2[, covar.names[i]]<-scale(log(ds2[, covar.names[i]]+0.5))
  }
  ds3<-ds2[ds2$date< as.Date(post.start),] #Just pre-vaccine period
  ds3$obs<-as.factor(ds3$index)
  time.vars<- c('t.scale','sin12','cos12','sin6','cos6')
  covar.names2<- c(covar.names,time.vars)
  ds3.names<-names(ds3)
  miss.names<-which(ds3.names=='')
  ds3<-ds3[,-miss.names]
    covars2<- paste(covar.names2, collapse="+")
  form1<-as.formula(paste0(outcome.name, '~', covars2, '+(1|obs)'))
  mod1<-glmer(form1, data=ds3, family='poisson',
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl =
                                       list(maxfun = 2e6)))
  covars<-ds2[,covar.names2]
  covars<-cbind.data.frame(rep(1, times = nrow(covars)),covars)
  pred.coefs.reg.mean <-
    mvrnorm(n = nsim,
            mu = fixef(mod1),
            Sigma = vcov(mod1))
  preds.stage1.regmean <-(as.matrix(covars) %*% t(pred.coefs.reg.mean))
  int1<-fixef(mod1)[1]
  
  n.pre.time<-which(ds2$date==post.start)-1 +12 #start vax effect 12 months after intro
  ve.effect<- c( rep(0, n.pre.time),  seq(from=0, to=log(ve.irr), length.out=24))
  ve.effect<- c(ve.effect, rep(log(ve.irr), nrow(ds2)-length(ve.effect) ))
  
  re.sd<-as.numeric(sqrt(VarCorr(mod1)[[1]]))
  re.int<-rnorm(n<-length(preds.stage1.regmean), mean=0, sd=re.sd) 
  preds.stage1.regmean<-preds.stage1.regmean + ve.effect +re.int
  
  preds.stage1<-exp(matrix(preds.stage1.regmean, nrow=nrow(preds.stage1.regmean), ncol=ncol(preds.stage1.regmean)))
  preds.stage2<-matrix(rpois(n=length(preds.stage1), lambda=preds.stage1), nrow=nrow(preds.stage1))
  
 
  res1<-list('preds.stage2'=preds.stage2, 're.sd'=re.sd, 'int'=int1,'covars'=covars[,covar.names,drop=F])
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
  eval_period <- post_period
  eval_period[1]<- intervention_date2 %m+% months(12) #skip first 12 months for eval period
  
  ds <- cbind.data.frame(ds, covars)
  names(ds)[1]<- 'outcome'
  covar.names=names(covars)
  ds$post1 <- 0
  ds$post1[time_points >= post_period[1] &
             time_points < eval_period[1]] <- 1
  ds$post2 <- 0
  ds$post2[time_points >= eval_period[1]] <- 1
  ds$one=1
  
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
  covar.vec.vax<-c('post1', 'post2', 'time_post1_int','time_post2_int')
  re.names<- '(1|obs)'
  
  #if(overdispersed==T){
  #covar.vec.plus.sep<-paste(c(covar.vec1,covar.vec.vax,re.names), collapse='+')
  #}else{
    covar.vec.plus.sep<-paste(c(covar.vec1,covar.vec.vax), collapse='+')
  #}
  form1<-as.formula(paste0('outcome~'   ,covar.vec.plus.sep  ))
  
  #Fit classic ITS model
  if(overdispersed==T){
  mod1 <-
    glm(form1,data = ds,family = quasipoisson(link = log))
    fixed.eff<-coef(mod1)
  }else{
    mod1 <-
      glm(form1,data = ds,family = poisson(link = log))
    fixed.eff<-coef(mod1)
    
  }
  #GENERATE PREDICTIONS
  covars3 <-
    as.matrix(cbind(ds[, c(covar.vec1,covar.vec.vax)]))
  covars3 <- cbind.data.frame(rep(1, times = nrow(covars3)), covars3)
  names(covars3)[1] <- "Intercept"
  pred.coefs.reg.mean <-
    mvrnorm(n = 1000,
            mu = fixed.eff,
            Sigma = vcov(mod1))
  preds.stage1.regmean <-
    exp(as.matrix(covars3) %*% t(pred.coefs.reg.mean))
  #re.sd<-as.numeric(sqrt(VarCorr(mod1)[[1]]))
  #preds.stage1<-rnorm(n<-length(preds.stage1.regmean), mean=preds.stage1.regmean, sd=re.sd)
  #preds.stage1<-exp(matrix(preds.stage1, nrow=nrow(preds.stage1.regmean), ncol=ncol(preds.stage1.regmean)))
  
  #Then for counterfactual, set post-vax effects to 0.
  covars3.cf <-   as.matrix(cbind(ds[, c(covar.vec1)], matrix(0,nrow=nrow(ds),ncol=length(covar.vec.vax) ) ))
  covars3.cf <-
    cbind.data.frame(rep(1, times = nrow(covars3.cf)), covars3.cf)
  preds.stage1.regmean.cf <-    exp(as.matrix(covars3.cf) %*% t(pred.coefs.reg.mean))
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

est.vax.eff<-  function(x, overdisperse1=F,n.season=12, intervention_date,time_points1){
  ve<-t(apply(x$preds.stage2, 2,its_func ,seasonN=n.season, covars=x$covars, intervention_date2=intervention_date,time_points=time_points1,overdispersed=overdisperse1))
  return(ve)
}
