#' Title
#'
#' @param variable character variable which is factor
#' @param time survival time
#' @param status survival status
#' @param data data.frame style contain variable which is factor
#' @param t1 3 year (day)
#' @param t2  5 year (day)
#'
#' @return
#' @export
#'
#' @examples variable <- names(df)[2:4]
#' cali.plot(variable,time="OS",status="EVENT",data=df,t1=1095,t2=1825)
#'
cali.plot <- function(variable,time="OS",status="EVENT",data=df,t1=1095,t2=1825){

  form <- mul.form(variable)(time="OS",status="EVENT",data=df)
  f3 <- rms::cph(form,data=df,x=T,y=T,surv = T,na.action=na.delete,time.inc = t1)
  f5 <- rms::cph(form,data=df,x=T,y=T,surv = T,na.action=na.delete,time.inc = t2)
  cal3<-calibrate(f3, cmethod="KM", method="boot",u=t1,m=50,B=1000)
  cal5<-calibrate(f5, cmethod="KM", method="boot",u=t2,m=50,B=1000)
  pdf("calibration_compare.pdf",width = 8,height = 8)
  plot(cal3,lwd = 2,lty = 0,errbar.col = c("#2166AC"),
       bty = "l",
       xlim = c(0,1),ylim= c(0,1),
       xlab = "Nomogram-prediced OS (%)",ylab = "Observed OS (%)",
       col = c("#2166AC"),
       cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6)
  lines(cal3[,c('mean.predicted',"KM")],
        type = 'b', lwd = 1, col = c("#2166AC"), pch = 16)
  mtext("")

  plot(cal5,lwd = 2,lty = 0,errbar.col = c("#B2182B"),
       xlim = c(0,1),ylim= c(0,1),col = c("#B2182B"),add = T)
  lines(cal5[,c('mean.predicted',"KM")],
        type = 'b', lwd = 1, col = c("#B2182B"), pch = 16)

  abline(0,1, lwd = 2, lty = 3, col = c("#224444"))

  legend("topleft",
         legend = c("3-year","5-year"),
         col =c("#2166AC","#B2182B"),
         lwd = 2,
         cex = 1.2,
         bty = "n")#

  dev.off()

}

