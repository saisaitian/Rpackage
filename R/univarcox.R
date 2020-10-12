#' Title univarcox
#'
#' @param x variable,character
#' @param time survival time
#' @param status survival status
#' @param data data.frame style contain variable which is factor
#'
#' @return univarcox data.frame style contains results
#' @export
#'
#' @examples univarcox("age")(time="OS",status="EVENT",data=df)
#' univar<-lapply(variable_names, function(x)univarcox(x)(time="OS",status="EVENT",data=df))
#' univartable<-do.call(rbind,lapply(univar,data.frame))
univarcox <- function(x){
  function(time,status,data){
  time <- data[,time]
  status <- data[,status]
  sur<-survival::Surv(time=time, event =status)
  formu<-as.formula(paste0('sur~',x))
  unicox<-survival::coxph(formu,data=data)
  unisum<-summary(unicox)
  HR<-round(unisum$coefficients[,2],3)
  Pvalue<-unisum$coefficients[,5]
  CI95<-paste(round(unisum$conf.int[,c(3)],3),round(unisum$conf.int[,c(4)],3),sep='-')
  univarcox<-data.frame('characteristics'=rownames(unisum$conf.int),
                        'Hazard Ration'=HR,
                        'CI95'=CI95,
                        'pvalue'=Pvalue,
                        'pvalue2'=ifelse(Pvalue < 0.001, "< 0.001", round(Pvalue,3))
                        )
  return(univarcox)
  #return(unisum)
  }

}


