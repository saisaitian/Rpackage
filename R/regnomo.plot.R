#' Title regnomo.plot
#'
#' @param variable character variable which is factor
#' @param time survival time
#' @param status survival status
#' @param data data.frame style contain variable which is factor
#'
#' @return
#' @export
#'
#' @examples variable <- names(df)[2:5]
#' form <- mul.form(variable)(time="OS",status="EVENT",data=df)
#' regnomo.plot(variable,t1=1095,t2=1825,form=form,time="OS",status="EVENT",data=df)
regnomo.plot <-function(variable,time="OS",status="EVENT",form,t1,t2,data=df){
  library(rms)
  dd<<-rms::datadist(data)
  options(datadist="dd")
  options(na.action="na.delete")
  cox <- survival::coxph(formula = form , data=data)
  regplot::regplot(cox,
                   failtime = c(t1,t2),
                   prfail = TRUE,
                   showP = T,
                   droplines = F,
                   rank="sd",
                   interval="confidence")

}

