#this was about the glodap, download and unzip, the csv is heavy!
#unzip("./data/GLODAPv2 Merged Master File.csv.zip")
glodap <- read.csv("./data/GLODAPv2 Merged Master File.csv")
#ivan worked on this

#APO data
library(dplyr)
library(stringr)
#this reads only the monthly data that is in .txt and the daily is in .csv so if you want to read only daily data use pattern = ".csv$"
files  <- list.files("./data/APO", full.names = T, pattern = ".txt$")
source <- list.files("./data/APO", full.names = F, pattern = ".txt$") %>%
    str_split(".txt", simplify = T) %>% data.frame() %>% dplyr::select(1) %>%
    pull()

#creates an empty list that will be filled with each table

files_list <- list()

files <- files[c(1,4,6)]#they only used data frm these stations
source[c(1, 4, 6),]

#reads the data skiping the 13 first lines of header
for (i in 1:length(files)) {

files_list[[i]] <- read.table(files[i], skip = 13) %>%
    #createsa column with the id of the station (source)
    mutate(source = source[i])
#sets names because we deleted the header (obs. this works for the montlhy files)
names(files_list[[i]]) <- c("Date", "Value", "SsnDtr", "Fit", "FitDtr", "Num",
                            "Source")
}
#joins the dataframes into one
datos_APO <- bind_rows(files_list) %>%
    #splits the first column
    tidyr::separate(Date, into = c("year", "station_maybe"))
#writes
write.csv(datos_APO, "./data/Datos_APO.csv")
#we could do the same for daily or CO" and O2 data
head(datos_APO)

library(ggplot2)
datos_APO %>% ggplot(aes(x = year, y = Value)) +
                         geom_point()
### this isnt summarized
mean_APOobs <- datos_APO %>% group_by(year) %>%
    summarize(mean = mean(Value),
              meanFit = mean(Fit),
              meanFitDTR = mean(FitDtr))

#to compare with the paper data we read the other table
dw <- read.csv("./data/Data_work.csv")
#calculated the difference
dw <- dw %>% mutate(diff = APOobserved - Apoff)
head(dw)
dw %>% ggplot(aes(x= Year, y = diff)) +
    geom_point()
#still the data do not have the same dimensions so we are not truly reproducing the dataset
dim(dw)
dim(datos_APO)

#the plot was therefore done by putting the datasets together but independently from theis dimensions, this was done in base R

#png("./img/res.png")#
#the downloaded data
plot(mean_APOobs$year, mean_APOobs$mean, col = "darkgreen",
     ylim = c(-350, 100), pch = 19)
#the reported data
points(dw$Year, dw$APOobserved, pch = 21)
points(dw$Year,dw$APOclimate, col ="orange", pch =19)
#the difference that we calculated
points(dw$Year,dw$diff, col = "brown", pch = 19)
abline(h=0)
legend("topleft", legend = c("Downloaded", "Reported", "APO climate", "Diff"), pch = c(19, 21, 19, 19), col = c("darkgreen", "black", "orange", "brown"))
linmod <- lm(dw$APOclimate ~ dw$Year)
abline(linmod, col = "orange")
#dev.off()





