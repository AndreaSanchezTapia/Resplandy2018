unzip("./data/GLODAPv2 Merged Master File.csv.zip")
glodap <- read.csv("./data/GLODAPv2 Merged Master File.csv")
head(glodap)
library(dplyr)
files <- list.files("./data/APO", full.names = T)
source <- list.files("./data/APO", full.names = F)
files_list <- list()
files <- files[c(1,4,6)]
for (i in 1:length(files)) {
files_list[[i]] <- read.table(files[i], skip = 13) %>% mutate(source = source[i])
names(files_list[[i]]) <- c("Date" ,      "Value",     "SsnDtr",        "Fit" ,    "FitDtr" , "Num", "Source")
}

datos_APO <- bind_rows(files_list) %>% tidyr::separate(Date, into = c("year", "station_maybe"))
write.csv(datos_APO, "Datos_APO.csv")
head(datos_APO)
library(ggplot2)
datos_APO %>% ggplot(aes(x= year, y = Value))+
                         geom_point()


dw <- read.csv("./data/Data_work.csv")
dw <- dw %>% mutate(diff = APOobserved-Apoff)
head(dw)
dw %>% ggplot(aes(x= Year, y = Value))+
    geom_point()
dim(dw)
dim(datos_APO)

mean_APOobs <- datos_APO %>% group_by(year) %>%
    summarize(mean = mean(Value),
              meanFit = mean(Fit),
              meanFitDTR = mean(FitDtr))
png("./img/res.png")
plot(mean_APOobs$year, mean_APOobs$mean, col ="darkgreen", ylim = c(-350, 100), pch =19)
points(dw$Year,dw$APOobserved, pch =21)
points(dw$Year,dw$APOclimate, col ="orange", pch =19)
points(dw$Year,dw$diff, col ="brown", pch = 19)
#points(mean_APOobs$year, mean_APOobs$meanFit, col = "orange", pch =19)
#points(mean_APOobs$year, mean_APOobs$meanFit, col = "orange", pch =19)
#points(mean_APOobs$year, mean_APOobs$meanFitDTR, col = "red", pch =19)
abline(h=0)
linmod <- lm(dw$APOclimate ~ dw$Year)
abline(linmod, col = "orange")
dev.off()
