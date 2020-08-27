#' @author Qiusheng WU
#' @title pltPcorrelation
#'
#' @import ggm
#' @import purrr
#' @import plyr
#' @importFrom stats na.omit var
#' @param pcor_dt  A dataframe of calculate in Partial correlation.
#' @return cal_pcor
#' @export

cal_pcor <- function(pcor_dt=pcor_dt,...){
  

  dt_nm <- names(pcor_dt)
  res_a <- rep(dt_nm,each=length(pcor_dt)-1)
  new_a <- list()
  for (i in 1:length(res_a)) {
    new_a[[i]] <- res_a[i]
  }
  
  pco_b <- list()
  
  for (i in 1:length(dt_nm)) {
    pco_b[[i]] <- (dt_nm)[-i]
  }
  
  dfll <- do.call(rbind,lapply(pco_b, data.frame))
  
  res_b=list()
  for (i in 1:nrow(dfll)) {
    res_b[[i]] <- dfll[i,]
  }
  
  new_nmf <- list()
  for (i in 1:length(pcor_dt)) {
    new_nmf[[i]] <- dt_nm
  }
  
  new_nm <- rep(new_nmf,length(pcor_dt)-1)
  
  grpc <- factor(c(rep("grp",nrow(pcor_dt))))
  fcu <- split(pcor_dt,grpc, drop = FALSE)
  dt <-  rep(fcu,length(new_nm))
  
  ham <- list()
  for (i in 1:length(new_nm)) {
    ham[[i]] <- new_nm[[i]][-which(new_nm[[i]]==new_a[[i]])]
  }
  
  hamn <- list()
  for (i in 1:length(new_nm)) {
    hamn[[i]] <- ham[[i]][-which(ham[[i]]==res_b[[i]])]
  }
  
  my_pcor <- function(new_a=new_a,res_b=res_b,hamn=hamn,dt=dt){
    res <-  pcor(c(new_a,res_b,hamn), var(dt))
    return(res)
  }
  
  r_res <- pmap(list(new_a=new_a,res_b=res_b,hamn=hamn,dt=dt),my_pcor)
  
  names(r_res) <- res_b
  haha <- ldply(r_res)
  haha[,3] <- rep(dt_nm,each=length(pcor_dt)-1)
  names(haha) <- c("var1","Correlation","var2")
  haha$var1 <- factor(haha$var1,levels = unique(haha$var1))
  haha$var2 <- factor(haha$var2,levels = unique(haha$var2))
  
  
  wd_cor <-reshape2::dcast(haha,var1~var2,value.var = 'Correlation')
  row.names(wd_cor) <- wd_cor[,1]
  wd_cor <- wd_cor[,-1]
  wd_cor <- wd_cor[dt_nm,]


  

  
  diag(wd_cor) <- 0###0
  wd_cor[upper.tri(wd_cor)] <- NA
  rname <- as.data.frame(row.names(wd_cor))
  names(rname) <- "rr"
  rrplt <- cbind(rname, wd_cor) %>% as.data.frame() %>% reshape2::melt(id.vars="rr")  %>% as.data.frame()
  names(rrplt)[2:3] <- c("var2","Correlation")
  cor_res <- na.omit(rrplt)
  
  
  ###P
  my_pcorp <- function(new_a=new_a,res_b=res_b,hamn=hamn,dt=dt){
    resp <- pcor.test( pcor(c(new_a,res_b,hamn), var(dt)),
                       length(dt)-1, n=nrow(dt))
    res_p <- resp[[3]]
    return(res_p)
  }
  options(scipen=200000)
  p_res <- pmap(list(new_a=new_a,res_b=res_b,hamn=hamn,dt=dt),my_pcorp)
  names(p_res) <- res_b
  ff_p <- ldply(p_res)
  ff_p[,3] <- rep(dt_nm,each=length(pcor_dt)-1)
  names(ff_p) <- c("var1","p","var2")
  ff_p$var1 <- factor(ff_p$var1,levels = unique(ff_p$var1))
  ff_p$var2 <- factor(ff_p$var2,levels = unique(ff_p$var2))
  
  wd_p <-reshape2::dcast(ff_p,var1~var2,value.var = 'p')
  row.names(wd_p) <- wd_p[,1]
  wd_p <- wd_p[,-1]
   wd_p <- wd_p[dt_nm,]
  diag(wd_p) <- 1
  wd_p[upper.tri(wd_p)] <- 123
  rname <- as.data.frame(row.names(wd_p))
  names(rname) <- "var1"
  ppplt <- cbind(rname, wd_p) %>% as.data.frame() %>% reshape2::melt(id.vars="var1") %>% as.data.frame()
  names(ppplt)[2:3] <- c("var2","p")
  
  p_sig <- as.data.frame(matrix(data = 0,nrow = nrow(ppplt),ncol = 1))
  pp <- as.data.frame(ppplt$p)
  for (i in 1:nrow(ppplt)) {
    if((pp[i,])<=0.001){
      p_sig[i,]="***"
    }else if((pp[i,])>0.001&&(pp[i,])<=0.01){
      p_sig[i,]="**"
    }else if((pp[i,])>0.01&&(pp[i,])<=0.05){
      p_sig[i,]="*"
    }else{
      p_sig[i,]=100
    }
  }
  
  cor_p <- cbind(ppplt,p_sig)
  
  cor_p[cor_p==123] <- NA
  last_p <- na.omit(cor_p)
  last_p[last_p==100] <- ""
  names(last_p)[4] <- "p_sig"
  
  cor_res[,4:5] <- last_p[,3:4]
  last_dt <- as.data.frame(cor_res)
  names(last_dt)[4] <- "p_num"
  
  az <-list()
  for (i in 1:ncol(pcor_dt)) {
    az[[i]] <- data.frame(plot_x=i,plot_y=(1:i))
  }
  xy <- as.data.frame(ldply(az))
  xy$plot_x <- as.numeric(xy$plot_x)
  xy$plot_y <- as.numeric(xy$plot_y)
  
  plt_dt <- as.data.frame(cbind(last_dt,xy))
  plt_dt$rr <- factor(plt_dt$rr,levels = unique(plt_dt$rr))
  plt_dt$var2 <- factor(plt_dt$var2,levels = unique(plt_dt$var2))
  names(plt_dt)[1] <- "var1"
  return (plt_dt)
}
