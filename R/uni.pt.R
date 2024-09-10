
uni.pt=function(y,s,alpha=0.05,gamma=0.05,gamma1=NA,gamma2=NA,conf.int=TRUE){
  za=qnorm(1-alpha/2)
  n=length(y)
  pt=mean(y)*(abs(sqrt(n)*mean(y)/s)>za)

  if((is.na(gamma1)==F)&((is.na(gamma2)==F))){
    zg1=qnorm(1-gamma1)
    zg2=qnorm(1-gamma2)

    if(conf.int==FALSE){
      res=data.frame("Sample_mean"=mean(y),"PT"=pt)
      res
    }

    else if(conf.int==TRUE){
      if(pt<(-za*s/sqrt(n))){
        l=pt-zg2*s/sqrt(n)
        u=pt+zg1*s/sqrt(n)
      }
      else if(pt==0){
        l=-(za+zg2)*s/sqrt(n)
        u=(za+zg1)*s/sqrt(n)
      }
      else if(pt>(za*s/sqrt(n))){
        l=pt-zg2*s/sqrt(n)
        u=pt+zg1*s/sqrt(n)
      }

      res=data.frame("Sample_mean"=mean(y),"PT"=pt,"Lower.pivotCI"=l,"Upper.pivotCI"=u)
      res
  }


  }
  else if((is.na(gamma1)==T)&((is.na(gamma2)==T))){
    zg1=qnorm(1-gamma/2)
    zg2=qnorm(1-gamma/2)

    if(conf.int==FALSE){
      res=data.frame("Sample_mean"=mean(y),"PT"=pt,"Lower.pivotCI"=l,"Upper.pivotCI"=u)
      res
    }

    else if(conf.int==TRUE){
      if(pt<(-za*s/sqrt(n))){
        l=pt-zg2*s/sqrt(n)
        u=pt+zg1*s/sqrt(n)
      }
      else if(pt==0){
        l=-(za+zg2)*s/sqrt(n)
        u=(za+zg1)*s/sqrt(n)
      }
      else if(pt>(za*s/sqrt(n))){
        l=pt-zg2*s/sqrt(n)
        u=pt+zg1*s/sqrt(n)
      }

      res=data.frame("Sample_mean"=mean(y),"PT"=pt,"Lower.pivotCI"=l,"Upper.pivotCI"=u)
      res
    }
  }
  else if((is.na(gamma1)==F)&((is.na(gamma2)==T))){
    warning(paste("gamma2 does not exist."))
  }
  else if((is.na(gamma1)==T)&((is.na(gamma2)==F))){
    warning(paste("gamma1 does not exist."))
  }


}
