#' Title multicox calculate
#'
#' @param form form a form for mulcox
#' @param data data.frame style contain variable which is factor
#' @param names mulcox variable
#'
#' @return multicox2 data.frame style contains results
#' @export
#'
#' @examples form <- mul.form(c('gender','age'))(time="OS",status="EVENT",data=df)
#'           multicox(form,data=df,names=c("group",'gender','age'))
#'

multicox <- function(form,data,names){
  multicox<-survival::coxph(formula = form,data =data)
  multisum<-summary(multicox)##
  muHR<-round(multisum$coefficients[,2],3)#
  muPvalue<-multisum$coefficients[,5]#
  muCIdown<-round(multisum$conf.int[,3],3)#
  muCIup<-round(multisum$conf.int[,4],3)#
  muCI<-paste0(muCIdown,'-',muCIup)##95%
  multicox2<-data.frame('characteristics'=rownames(multisum$conf.int),
                      'muHazard Ration'=muHR,
                      'muCI95'=muCI,
                      'pvalue'=muPvalue,
                      'mupvalue'=ifelse(muPvalue < 0.001, "< 0.001", round(muPvalue,3)))

  return(multicox2)
}


