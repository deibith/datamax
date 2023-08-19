library(agricolae) #si
library(PMCMRplus) #si
#install.packages("PMCMRplus")
rm(list=ls())

setwd("C:/Users/ASUS/Desktop/La vida/Magister_Inf/1er_trimestre/Optimización estocástica/Segunda_entrega/astr4l-main/")

#Base de datos de resultado
resul <- read.csv(paste0(getwd(), "/BD/Resultados.csv"))

#Valores solucion (ejemplo 1ra corrida)
vals <- substr(resul$solucion[1],2,nchar(resul$solucion[1])-1)
vals <- as.numeric(gsub(" ", "",unlist(strsplit(vals, ","))))

#Une nombres de archivo a tabla
# ldir <-  normalizePath(paste0(getwd(), "/Resultados/graficos"))
# finf <- file.info(dir(path = ldir, full.names = TRUE), extra_cols = FALSE)
# 
# finf0 <- rownames(finf)[order(finf$ctime)]
# 
# tab <- as.data.frame(do.call("rbind", strsplit(finf0, "/")))
# 
# files <- list.files(paste0(getwd(), "/Resultados/graficos"), ".pdf")
# files <- substr(files, 1, nchar(files)-4)
# 
# files <- as.data.frame(do.call("rbind", strsplit(files, "_")))


files <- data.frame(MH=rep(rep(c('PSA','SCA','RSA','WOA','GWO'), each=3),40),
                    Inst = rep(rep(c("F7","F8","F9","F10"), each=15), 10))

resul <- data.frame(files, resul)

#Boxplots
for(i in 1:length(unique(resul$Inst))){
  dat <- resul[which(resul$Inst==unique(resul$Inst)[i]),]#"F8"
  boxplot(fitness~MH, data=dat, main=unique(resul$Inst)[i])  
}


##Pruebas estadisticas U-Mann Whitney

its <- unique(resul$Inst)

for(i in 1:length(its)){
  dat <- resul[which(resul$Inst==unique(resul$Inst)[i]),]#"F8"
  mhs <- unique(dat$MH)
  for(j in 1: length(mhs)){
    val <- round(dat$fitness[which(dat$MH==mhs[j])],3)
    if(sum(val)==mean(val)){
      psh <- NA
    }else{
      sht <- shapiro.test(dat$fitness[which(dat$MH==mhs[j])])
      psh <- sht$p.value
    }
    if(j==1){df <- psh}else{df <- c(df, psh)}
  }
  kw <- kruskal.test(dat$fitness, as.factor(dat$MH))
  NT <- PMCMRplus::kwAllPairsNemenyiTest(fitness ~ as.factor(MH), data = dat, dist="Chisq")
  NT <- data.frame(NT$p.value, Inst=its[i])
  if(i==1){
    DF <- df
    dj <- kw$p.value
    DH <- NT
  }else{
    DF <- data.frame(DF, df)
    dj <- c(dj, kw$p.value)
    DH <- rbind(DH, NT)
  }
}
colnames(DF) <- its
rownames(DF) <- mhs

write.csv(DF, "Normalidad.csv", row.names = T)
write.csv(DH, "Nemenyi.csv", row.names = T)
