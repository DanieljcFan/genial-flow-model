#导出各行业企业名
setwd("E:/FJC/")
load("report.rda")
Name <- function(input,dat=report){
  name <- input[1]
  id <- grep(input[2],names(dat))
  index <- grep(name, dat[,id])
  tmp <- report[index,]
  output <- unique(tmp[c(2,1,7:9)])
  output <- cbind(行业 = rep(name,nrow(output)), output)
  return(output)
}


field <- read.csv('others/申万行业对应.csv')
field <- apply(field, 2, as.character)
which(nchar(field[,4]) >= 1 )

level <- rep(NA, nrow(field))
level[which(nchar(field[,2]) >= 1)] <- colnames(field)[2]
level[which(nchar(field[,3]) >= 1)] <- colnames(field)[3]
level[which(nchar(field[,4]) >= 1)] <- colnames(field)[4]


input <- cbind(field[,1],level)

output <- apply(input,1,Name)
output <- do.call(rbind, output)
write.csv(output, file = 'others/行业对应企业.csv', row.names = F)
