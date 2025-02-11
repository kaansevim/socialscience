

################ VERİ TEMİZLEME   #########

library(haven)
library(dplyr)

glimpse(instason) #dbl olanlar sayısal dbl+dbl olanlar da sayısal fakat SPSS
#tarafından sayılara birer değer atandığını gösterir. Eğer sayılara değer atandıysa
#topluca bir şekilde hepsi faktöre çevrilebilir. 



#değişken isimlerinin değiştirilmesi
instason1 <- instason %>% 
  rename(cinsiyet=sex, egitim=education, yas=age, is=job)
View(instason1)




#tek tek verileri faktör yapma
instason1$egitim <- as_factor(instason1$egitim)

#birden fazla faktörü aynı anda çevirme
instason1 <- instason1 %>% 
  mutate(across(c(cinsiyet, is, residence), as_factor))

glimpse(instason1)
sapply(instason1, class)  #spss'ten çekilen verilere etiket girildiyse labeled yazar.


#SPSS'den gelen tüm etiketli verileri faktöre çevirmek
instason1 <- instason1 %>% 
  mutate(across(where(is.labelled), as_factor))

glimpse((instason1))


# Sıralı olması gereken faktörleri oluşturma 
instason1$PFA   #Participation friend activity "Kötü" "Normal" "İyi" "Çok iyi"
instason1$FC    #Family communication


# Sıralı hale getirebilmek için önce faktör içinde Türkçe karakter harici yazıları düzenleyelim.

levels(instason1$PFA)
class(instason1$PFA)
table(instason1$PFA)

levels(instason1$PFA) <- c("kotu", "normal", "iyi", "cok iyi")
table(instason1$PFA)

levels(instason1$FC) <- c("kotu", "normal", "iyi")
table(instason1$FC)
# Faktör karakterlerini inglilizce karaktere çevirdikten sonra sıralamalı faktör
#yapabiliriz. (ordered faktör)

instason1$PFA <- factor(instason1$PFA,
                        levels = c("kotu", "normal", "iyi", "cok iyi"),
                        ordered = TRUE)
glimpse(instason1)

## Benzer şekilde FC değişkenini de ordered yapalım.

instason1$FC <- factor(instason1$FC,
                       levels = c("kotu", "normal", "iyi"),
                       ordered = TRUE)





### Ölçek sorularını toplayıp birleştirelim. 
instason1 <- instason1 %>%
  mutate(ps_toplam = rowSums(select(., ps1:ps6), na.rm = TRUE))  #ortalamsını almak için rowMeans seçelilebilir.
View(instason1)




############   İSTATİSTİKLER   ####################

## Ki-Kare Testi
kitest <- table(instason1$cinsiyet, instason1$is)
summary(kitest)





## T-Test
t.test(ps_toplam ~ cinsiyet, data = instason1)




## Anova
anova_model <- aov(ps_toplam~ egitim, data = instason1)
summary(anova_model)


## Korelasyon
cor.test(instason1$ps_toplam, instason1$yas)



## Regresyon
reg_model <- lm(IAS~ps_toplam, data = instason1)
summary(reg_model)









  