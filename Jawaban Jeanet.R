###  INTRODUCTION  ###

morning_greeting<-'Halo, selamat pagi!'
typeof(morning_greeting)

Bandung<-28.5
typeof(Bandung)

integer<-33L
typeof(integer)

itb_di_bandung<-TRUE
typeof(itb_di_bandung)

kompleks<-7i+2
typeof(kompleks)

Angga<-3
Tezar<-4
total_piscok<-Angga+Tezar

Fuadi<-2
sisa_piscok<-total_piscok-Fuadi

28%%6

512%/%31

7^5>8^4
(7^5 > 8^4) || (5*35 > 21*7)


jumlah_MR <- c(15, 20, 11)
jumlah_TI <- c(23, 27, 12)
jumlah_mahasiswa<-jumlah_MR + jumlah_TI
names(jumlah_mahasiswa) <- c("Bandung", "Jakarta", "Surabaya")
jumlah_mahasiswa["Surabaya"]
names(sort(jumlah_mahasiswa, decreasing=TRUE))
Avg <- mean(jumlah_mahasiswa)
names(jumlah_mahasiswa[jumlah_mahasiswa > Avg])

##############################################################################
#READING AND WRITING

Saham <- c("adro", "Antam", "bumi", "PTBA", "medco", "BUMI")
Saham<-gsub('Antam','ANTM',Saham)
Saham <- gsub("medco","medc", Saham)
Saham <- toupper(Saham) #Biar Kapital
Saham <- unique(Saham)
length(Saham) #menghitung jumlah kode Saham
factor(Saham)
factor(Saham, levels=c("ADRO","ANTM","BUMI","PTBA","MEDC","TINS"))

#LATIHAN 2: DATA FRAME

#meng-install package "readxl"
install.packages(readxl)
#load packackage "readxl" untuk digunakan
library(readxl)
#Lakukan import file csv bernama "Dataset Latihan 2 (Data frame)"
data <- read_excel(file.choose())
library(readxl)
data <- read_excel("C:/Users/Owner/OneDrive - Institut Teknologi Bandung/workshop R/Dataset_Data Frame.xlsx")
View(data)

#Lakukan pembuatan vektor-vektor penyusun data "Silver Medal" dan "Bronze Medal"
Country_Code <- c("AUS", "BRA", "CAN", "CHN", "ECU", "GBR", "JPN", "KEN", "KOR", "NZL", "RSA", "RUS", "SWE", "USA")
silver_medal_2021 <- c(7,6,6,32,1,32,14,4,4,6,2,28,6,41)
silver_medal_2016 <- c(34,8,4,30,0,55,13,6,3,25,7,29,23,54)
bronze_medal_2021 <- c(22,8,11,18,0,18,17,2,10,7,0,23,0,33)
bronze_medal_2016 <- c(25,6,61,37,0,26,35,1,10,5,14,34,3,71)

#Lakukan pembuatan vektor "Continent" untuk keperluan soal
Continent <- c("Asia","Europe","Africa","Australia","South America", "North America")

#Periksa struktur data dari dataset "Latihan 2 Dataset" yang telah diimport
(caranya bebas tapi peserta harus tau semua kolom yang dipake dan overall datanya)
head(data)
str(data)
summary(data)

#Lakukan pemisahan data gold medal tahun 2021 dan 2016
data2021 <- data[data$Year==2021,]
data2016 <- data[data$Year==2016,]

#Lakukan pembuatan data silver medal tahun 2021 dan 2016
silver2021 <- data.frame(Country_Code=Country_Code, Silver_Medal=silver_medal_2021)
silver2016 <- data.frame(Country_Code=Country_Code, Silver_Medal=silver_medal_2016)

#Lakukan pembuatan data bronze medal tahun 2021 dan 2016
bronze2021 <- data.frame(Country_Code=Country_Code, Bronze_Medal=bronze_medal_2021)
bronze2016 <- data.frame(Country_Code=Country_Code, Bronze_Medal=bronze_medal_2016)


#Lakukan merging data untuk data gold medal, silver dan bronze tahun 2021
merge2021 <- merge(data2021,silver2021,by.x="Country_Code", by.y="Country_Code")
merge2021 <- merge(merge2021,bronze2021,by.x="Country_Code", by.y="Country_Code")

#Lakukan merging data untuk data gold medal, silver dan bronze tahun 2016
merge2016 <- merge(data2016,silver2016,by.x="Country_Code", by.y="Country_Code")
merge2016 <- merge(merge2016,bronze2016,by.x="Country_Code", by.y="Country_Code")

#Lakukan perhitungan nilai rata-rata gold medal di tahun 2021 untuk tiap continent
mean_gold_2021 <- c(mean(merge2021$Gold_Medal[merge2021$Continent=="Asia"]),
                    mean(merge2021$Gold_Medal[merge2021$Continent=="Europe"]),
                    mean(merge2021$Gold_Medal[merge2021$Continent=="South America"]),
                    mean(merge2021$Gold_Medal[merge2021$Continent=="North America"]),
                    mean(merge2021$Gold_Medal[merge2021$Continent=="Australia"]),
                    mean(merge2021$Gold_Medal[merge2021$Continent=="Africa"]))

#Lakukan perhitungan nilai rata-rata gold medal di tahun 2016 untuk tiap continent
mean_gold_2016 <- c(mean(merge2021$Gold_Medal[merge2021$Continent=="Asia"]),
                    mean(merge2021$Gold_Medal[merge2021$Continent=="Europe"]),
                    mean(merge2021$Gold_Medal[merge2021$Continent=="South America"]),
                    mean(merge2021$Gold_Medal[merge2021$Continent=="North America"]),
                    mean(merge2021$Gold_Medal[merge2021$Continent=="Australia"]),
                    mean(merge2021$Gold_Medal[merge2021$Continent=="Africa"]))

#Lakukan perhitungan nilai rata-rata silver medal di tahun 2021 untuk tiap continent
mean_silver_2021 <- c(mean(merge2021$Silver_Medal[merge2021$Continent=="Asia"]),
                      mean(merge2021$Silver_Medal[merge2021$Continent=="Europe"]),
                      mean(merge2021$Silver_Medal[merge2021$Continent=="South America"]),
                      mean(merge2021$Silver_Medal[merge2021$Continent=="North America"]),
                      mean(merge2021$Silver_Medal[merge2021$Continent=="Australia"]),
                      mean(merge2021$Silver_Medal[merge2021$Continent=="Africa"]))

#Lakukan perhitungan nilai rata-rata silver medal di tahun 2016 untuk tiap continent
mean_silver_2016 <- c(mean(merge2021$Silver_Medal[merge2021$Continent=="Asia"]),
                      mean(merge2021$Silver_Medal[merge2021$Continent=="Europe"]),
                      mean(merge2021$Silver_Medal[merge2021$Continent=="South America"]),
                      mean(merge2021$Silver_Medal[merge2021$Continent=="North America"]),
                      mean(merge2021$Silver_Medal[merge2021$Continent=="Australia"]),
                      mean(merge2021$Silver_Medal[merge2021$Continent=="Africa"]))

#Lakukan perhitungan nilai rata-rata bronze medal di tahun 2021 untuk tiap continent
mean_bronze_2021 <- c(mean(merge2021$Bronze_Medal[merge2021$Continent=="Asia"]),
                      mean(merge2021$Bronze_Medal[merge2021$Continent=="Europe"]),
                      mean(merge2021$Bronze_Medal[merge2021$Continent=="South America"]),
                      mean(merge2021$Bronze_Medal[merge2021$Continent=="North America"]),
                      mean(merge2021$Bronze_Medal[merge2021$Continent=="Australia"]),
                      mean(merge2021$Bronze_Medal[merge2021$Continent=="Africa"]))

#Lakukan perhitungan nilai rata-rata bronze medal di tahun 2016 untuk tiap continent
mean_bronze_2016 <- c(mean(merge2021$Bronze_Medal[merge2021$Continent=="Asia"]),
                      mean(merge2021$Bronze_Medal[merge2021$Continent=="Europe"]),
                      mean(merge2021$Bronze_Medal[merge2021$Continent=="South America"]),
                      mean(merge2021$Bronze_Medal[merge2021$Continent=="North America"]),
                      mean(merge2021$Bronze_Medal[merge2021$Continent=="Australia"]),
                      mean(merge2021$Bronze_Medal[merge2021$Continent=="Africa"]))

#Buat dataframe yang berisi continent, rata-rata gold medal tahun 2021, rata-rata gold medal tahun 2016, rata-rata silver medal tahun 2021, rata-rata silver medal tahun 2016, rata-rata broze medal tahun 2021, dan rata-rata bronze medal tahun 2016
Mean <- data.frame(Continent,mean_gold_2021,mean_gold_2016,mean_silver_2021,mean_silver_2016,mean_bronze_2021,mean_bronze_2016)

#Tampilkan dataframe tersebut
Mean

#Latihan 3 Visualisi Data

#No1
#load package "MASS" untuk digunakan
library("MASS")
#memasukkan package data "road" dari package "MASS" untuk digunakan
roadAccidents <- road
#load package "tidyverse" untuk digunakan
library(tidyverse)
#membuat scatter plot antara data fuel dan deaths
ggplot(data = roadAccidents, aes(x = fuel, y = deaths)) + 
  geom_point() + #untuk menambahkan datapoint/titik observasi 
  geom_smooth(method = "lm") #untuk menambahkan fitted line 

#No2
#import datset "Dataset Latihan 3 (Data visualization)
solarPower <- read.csv(file.choose(), sep=",")
#mengubah format data menjadi date
solarPower$date <- as.Date(solarPower$date, format = "%d/%m/%Y")
#membuat line plot 
ggplot(solarPower, aes(x=date, y=kWh.electricity.day)) +
  geom_line() +
  geom_smooth(
