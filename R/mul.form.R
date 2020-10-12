#' Title a form for mulcox
#'
#' @param names mulcox variable
#' @param time survival time
#' @param status survival status
#' @param data data.frame style contain variable which is factor
#' @return form a form for mulcox
#' @export
#'
#' @examples mul.form(c('gender','age'))(time="OS",status="EVENT",data=df)

load("data/df.rda")

mul.form <- function(names){
  function(time,status,data){
    time <- data[,time]
    status <- data[,status]
    form<-as.formula(paste0('survival::Surv(time=time, event =status)~',paste0(names,collapse = '+')))
    return(form)
}
}


