
#' Title variable select from univarcox
#'
#' @param data univarcox data
#' @param p pvalue
#'
#' @return variable
#' @export
#'
#' @examples tmp <- univarcox("age")(time="OS",status="EVENT",data=df)
#'           var.select(tmp,0.05)
var.select <- function(data,p){
  names<-as.character(data$characteristics[data$pvalue < p])
  return(names)
}

