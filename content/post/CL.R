CL <- function (mat) {
  mat <- as.matrix(mat) 
  share_tech_city <- t(round(t(mat)/rowSums(t(mat)),4))  #regional shares
  share_tech_total <- colSums(t(mat))/sum(t(mat)) # regional share total
  CL <- abs(share_tech_city-share_tech_total)
  CL[is.na(CL)] <- 0
  CL<-0.5*colSums(CL)
  return(CL)
}
library(readxl)
dadosemprego <- read_excel("emprego.xlsx", sheet = "2006")
mat_0 <- as.matrix(dadosemprego[1:79,2:88]) # 2006
cl.ms_0<-round(CL(mat_0),4)
share_tech_city <- t(round(t(mat_0)/rowSums(t(mat_0)),4))  #regional shares
share_tech_total <- colSums(t(mat_0))/sum(t(mat_0)) # regional share total
