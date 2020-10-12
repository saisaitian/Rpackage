#' Title nomo.plot
#'
#' @param variable character variable which is factor
#' @param t1     3 year (day)
#' @param t2     5 year (day)
#' @param data   data.frame style contain variable which is factor
#'
#' @return plot
#' @export
#'
#' @examples variable=names(df)[2:7]
#' form <- mul.form(variable)(time="OS",status="EVENT",data=df)
#' nomo.plot(variable,t1=1095,t2=1825,form,time="OS",status="EVENT",data=df)


nomo.plot <- function(variable,t1=1095,t2=1825,form,time=time,status=status,data=df){

  dd<<-rms::datadist(data)
  options(datadist='dd')
  options(na.action="na.delete")
  cox<-rms::cph(formula = form,x=T,y=T,surv = T,data = data)
  surv<-rms::Survival(cox)
  surv3<-function(x) surv(t1,x)
  surv5<-function(x) surv(t2,x)

  x<-rms::nomogram(cox,fun = list(surv3,surv5),lp=T,
              funlabel = c('3-year survival Probability','5-year survival Probability'),
              maxscale = 100,fun.at = c(0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1))

  plot(x, lplabel="Linear Predictor",
       xfrac=.35,varname.label=TRUE, varname.label.sep="=", ia.space=.2,
       tck=NA, tcl=-0.20, lmgp=0.3,
       points.label='Points', total.points.label='Total Points',
       total.sep.page=FALSE,
       cap.labels=FALSE,cex.var = 1.05,cex.axis = 1.05,lwd=5,
       label.every = 1,col.grid = gray(c(0.8, 0.95)))

}


