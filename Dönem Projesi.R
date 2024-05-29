#veri ön inceleme ve paket yükleme
head(imdb_top_1000)
summary(imdb_top_1000)
install.packages("maps")
install.packages("sf")
install.packages("lubridate")
install.packages("zoo")
library(tidyverse)
library(dplyr)
library(maps)
library(ggplot2)
library(sf)
library(lubridate)
library(tidyverse)
library(dplyr)
library(maps)
library(sf)
library(lubridate)
library(zoo)

#ilk grafikte kullanmak üzere tür kolonundaki değerleri virgülden ayırıp yeni satır olarak ekleme
separate_genre_df <- separate_rows(imdb_top_1000, Genre, sep = ", ")

#1 nolu grafik // türe göre dağılım
ggplot(separate_genre_df, aes(x = count, y = Genre) +
  geom_bar(stat = "count", fill = "#F5de50") +
  labs(x = "Film Sayısı",
       y = "Film Türü") +
  theme_bw() +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=15),
        panel.background = element_blank())
        
# ham veride numerik olan yıl değişkenini 2 nolu grafikte kullanmak üzere kategorik değişkene çeviriyorum 
year_cat_imdb <- within(imdb_top_1000, {   
  year.cat <- NA 
  year.cat[2000 <= Released_Year] <- "2000 Sonrası"
  year.cat[1980 <= Released_Year & Released_Year < 2000] <- "1980-2000"
  year.cat[1960 <= Released_Year & Released_Year < 1980] <- "1960-1980"
  year.cat[1940 <= Released_Year & Released_Year < 1960] <- "1940-1960"
  year.cat[Released_Year < 1940] <- "1940 Öncesi"})

#2 nolu grafik // yapım yılına göre dağılım 
ggplot(data = year_cat_imdb, aes(x = year.cat)) +
  geom_bar(stat = "count", fill = "#F5de50") +
  labs(x = "Yapım Yılı",
       y = "Film Sayısı") +
theme_bw() +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=15),
        panel.background = element_blank())    

#3 nolu grafik için hazırlık
film_sureleri_df <- separate_rows(imdb_top_1000, Runtime, sep = " min")
summary(film_sureleri_df)
film_sureleri_df$Runtime <- as.numeric(film_sureleri_df$Runtime)
film_sureleri_df <- subset(film_sureleri_df, Runtime>0)
film_sureleri_df$Released_Year <- as.numeric(film_sureleri_df$Released_Year)


#3 nolu grafik // yapım yılına göre ortalama film süreleri
ggplot(data=film_sureleri_df, aes(x = Released_Year,
               y = Runtime)) +
  geom_point( color = "#F5de50") +
  labs(x = "Yapım Yılı",
       y = "Film Süresi (Dk)") +
   theme_bw() +
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          text = element_text(size=15),
          panel.background = element_blank()) +
  geom_smooth(formula = y ~ x, color = "black", se = FALSE)
  

#5 nolu grafik // Filmlerin IMDB puanları ile Eleştirmen puanlarının karşılaştırması
ggplot(data = separate_genre_df, aes(x = IMDB_Rating, y = Meta_score)) + 
  geom_point(color = "#F5de50") +
  labs(x = "IMDB Puanı",
       y = "Eleştirmen Puanı (Meta Score)") +
  theme_bw() +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=15),
        panel.background = element_blank())

#6 nolu grafik // eleştirmen ve IMDB puanları arasında fark olan yapımların yapım yılını ve hasılatını inceleme
ggplot(data = year_cat_imdb, aes(x = IMDB_Rating, y = Meta_score,  size = Gross, colour = year.cat)) + 
  geom_point() +
  theme_bw() +
  labs(x = "IMDB Puanı",
       y = "Eleştirmen Puanı",
       label = "Yapım Yılı",
       size = "Hasılat") +
  scale_color_manual(values=c("#F5de50","#8B8000", "#999999", "#E69F00", "#000000"))
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=15),
        panel.background = element_blank())

