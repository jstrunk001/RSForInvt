
duplicated2=function(x,type=c("all","first","last"),value=F,invert=F){

  if(type[1]=="all") index = duplicated(x) | duplicated(x, fromLast=TRUE)
  if(type[1]=="first") index = duplicated(x)
  if(type[1]=="last") index = duplicated(x, fromLast=TRUE)

  if(invert) index = !index

  if(value) return(x[index])
  if(!value) return(index)

}
dup2=duplicated2

# duplicated2(c(1:15,2:7),value=F)
# dup2(c(1:15,2:7),value=F)
#
# duplicated2(c(1:15,2:7),value=T)
# dup2(c(1:15,2:7),value=T)
