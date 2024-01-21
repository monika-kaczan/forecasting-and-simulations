# Biblioteki
library(tidyverse)
library(DataCombine)
library(Metrics)
library(zoo)
library(car)
library(fpp2)
library(tseries)
library(xts)

dane <- dane_kurs_sredni_miesiac
dane

y <- ts(dane$kurs, start = c(2012,1), frequency = 12)
#y_in <- ts(dane[1:101, ]$kurs, start = c(2012,1), frequency = 12)
y_out <- ts(dane[102:125, ]$kurs, start = c(2012,1), frequency = 12)

indeks_czasu <- ts(dane$indeks, start = c(2012, 1), frequency = 12)
prognozy <- data.frame(indeks_czasu, y)

metoda <- rep(NA,40)
rmse <- rep(NA,40)
mae <- rep(NA,40)
mape <- rep(NA,40)
bledy_prognoz_2020m6_2022m5 <- data.frame(metoda, rmse, mae, mape)
#bledy_prognoz_2012m1_2020m5_in <- data.frame(metoda, rmse, mae, mape)


############################
#PROGNOZA NAIWNA "BEZ ZMIAN"
############################

#Prognoza
y_naiwna <- shift(y, -1)
prognozy$y_naiwna <- y_naiwna

#RMSE i MAE
prognozy_2020m6_2022m5 <- prognozy[indeks_czasu > 101 & indeks_czasu < 126, ]

bledy_prognoz_2020m6_2022m5$metoda[1] <- "naiwna" 
bledy_prognoz_2020m6_2022m5$rmse[1] <- rmse(prognozy_2020m6_2022m5$y, prognozy_2020m6_2022m5$y_naiwna) 
bledy_prognoz_2020m6_2022m5$mae[1] <- mae(prognozy_2020m6_2022m5$y, prognozy_2020m6_2022m5$y_naiwna)
bledy_prognoz_2020m6_2022m5$mape[1] <- mape(prognozy_2020m6_2022m5$y, prognozy_2020m6_2022m5$y_naiwna)

residuals_naiwna <- prognozy_2020m6_2022m5$y - prognozy_2020m6_2022m5$y_naiwna

y_naiwna_plot <- ts(prognozy$y_naiwna, start = c(2012,1), frequency = 12)
plot(y, type = "l", col = "black", ylab = "EUR/PLN", main = "Metoda naiwna")
lines(y_naiwna_plot, col = "red", type = "l")

residuals_ma
residuals_naiwna


############################
#SREDNIE RUCHOME - OPTYMALIZACJA OKNA
############################

rzad_ma <- rep(NA,100)
rmse_ma <- rep(NA,100)
mae_ma <- rep(NA,100)
bledy_prognoz_2020m6_2022m5_ma <- data.frame(rzad_ma, rmse_ma, mae_ma)


#Liczenie prognoz i bledow w petli
for (i in 1:100) {
  
  y_ma <- rollmean(y, i, fill=NA, align="right") %>% shift(-1, reminder = FALSE)
  prognozy$y_ma <- y_ma
  
  prognozy_2020m6_2022m5_ma <- prognozy[indeks_czasu > 101 & indeks_czasu < 126, ]
  
  bledy_prognoz_2020m6_2022m5_ma$rzad_ma[i] <- i 
  bledy_prognoz_2020m6_2022m5_ma$rmse_ma[i] <- rmse(prognozy_2020m6_2022m5_ma$y, prognozy_2020m6_2022m5_ma$y_ma) 
  bledy_prognoz_2020m6_2022m5_ma$mae_ma[i] <- mae(prognozy_2020m6_2022m5_ma$y, prognozy_2020m6_2022m5_ma$y_ma)
  
}

plot(bledy_prognoz_2020m6_2022m5_ma$rzad_ma,bledy_prognoz_2020m6_2022m5_ma$rmse, type="l", xlab = "rząd k", ylab = "RMSE")
plot(bledy_prognoz_2020m6_2022m5_ma$rzad_ma,bledy_prognoz_2020m6_2022m5_ma$mae, type="l", xlab = "rząd k", ylab = "MAE")
# Obserwujemy, że błąd prognozy najpierw lekko maleje, a później rośnie wraz z okresem prognozy
# zarówno w przypadku RMSE jak i MAE

bledy_prognoz_2020m6_2022m5_ma$rzad_ma[bledy_prognoz_2020m6_2022m5_ma$rmse_ma == min(bledy_prognoz_2020m6_2022m5_ma$rmse_ma)]
bledy_prognoz_2020m6_2022m5_ma$rzad_ma[bledy_prognoz_2020m6_2022m5_ma$mae_ma == min(bledy_prognoz_2020m6_2022m5_ma$mae_ma)]
# Najniższy błąd prognozy jest obserwowany dla k = 6 (RMSE) i k = 5 (MAE)




test_ma_2020m6_2022m5 <- lm(prognozy_2020m6_2022m5_ma$y ~ prognozy_2020m6_2022m5_ma$y_ma)
linearHypothesis(test_ma_2020m6_2022m5,c("prognozy_2020m6_2022m5_ma$y_ma = 1","(Intercept) = 0"),test="F")
# Odrzucamy H0 że prognoza jest efektywna i nieobciążona

y_ma_final <- rollmean(y, 6, fill=NA, align="right") %>% shift(-1, reminder = FALSE)
y_ma_final_plot <- ts(y_ma_final, start = c(2012,1), frequency = 12)

plot(y, type = "l", col = "black", ylab = "EUR/PLN", main = "Średnia ruchoma dla okna = 6")
lines(y_ma_final_plot, col = "red", type = "l")


# OSTATECZNA METODA
y_ma_final <- rollmean(y, 6, fill=NA, align="right") %>% shift(-1, reminder = FALSE)
prognozy$y_ma_final <- y_ma_final

prognozy_2020m6_2022m5_ma_final <- prognozy[indeks_czasu > 101 & indeks_czasu < 126, ]
prognozy_2020m6_2022m5_ma_final

bledy_prognoz_2020m6_2022m5$metoda[2] <- "ma" 
bledy_prognoz_2020m6_2022m5$rmse[2] <- rmse(prognozy_2020m6_2022m5_ma_final$y, prognozy_2020m6_2022m5_ma_final$y_ma_final) 
bledy_prognoz_2020m6_2022m5$mae[2] <- mae(prognozy_2020m6_2022m5_ma_final$y, prognozy_2020m6_2022m5_ma_final$y_ma_final)
bledy_prognoz_2020m6_2022m5$mape[2] <- mape(prognozy_2020m6_2022m5$y, prognozy_2020m6_2022m5_ma_final$y_ma_final)

residuals_ma <- prognozy_2020m6_2022m5_ma_final$y_ma_final - prognozy_2020m6_2022m5$y
length(prognozy_2020m6_2022m5_ma_final$y)
length(prognozy_2020m6_2022m5_ma_final$y_ma_final)





#####################################################################
#PROSTE WYGLADZANIE WYKLADNICZE - SIMPLE EXPONENTIAL SMOOTHING (SES)
#####################################################################

y_ses_obj <- ses(y, h = 12)
summary(y_ses_obj)
plot(y, type = "l", col = "black", ylab = "EUR/PLN", main = "Proste wygładzanie wykładnicze, alfa = 0.9999")
lines(fitted(y_ses_obj), col = "red", type = "l")


# Smoothing parameter alpha = 0.9999 
# Tak samo jak u naszego artykułu - prognoza upraszcza się do prognozy naiwnej


# Prognozy na ruchomym oknie pętli 100 miesięcy
# Zawsze 100 miesięcy
prognozy_2020m6_2022m5_ses <- prognozy[indeks_czasu > 101 & indeks_czasu < 126, ]

y_ses <- rep(NA,24)
prognozy_2020m6_2022m5_ses$y_ses <- y_ses

for (i in 1:24) {
  
  input_2020m6_2022m5 <- prognozy[indeks_czasu < 101+i & indeks_czasu > 0 + i,]
  y_ses_obj <- ses(input_2020m6_2022m5$y, h=1)
  prognozy_2020m6_2022m5_ses$y_ses[i]<- y_ses_obj$mean[1]
  
}

#RMSE i MAE
bledy_prognoz_2020m6_2022m5$metoda[3] <- "ses" 
bledy_prognoz_2020m6_2022m5$rmse[3] <- rmse(prognozy_2020m6_2022m5_ses$y, prognozy_2020m6_2022m5_ses$y_ses) 
bledy_prognoz_2020m6_2022m5$mae[3] <- mae(prognozy_2020m6_2022m5_ses$y, prognozy_2020m6_2022m5_ses$y_ses)
bledy_prognoz_2020m6_2022m5$mape[3] <- mape(prognozy_2020m6_2022m5_ses$y, prognozy_2020m6_2022m5_ses$y_ses)




#####################################################################
#MODELE ETS (ERROR, TREND, SEASONALITY)
#####################################################################
# Uogólnienie m. Holta-Wintersa

#przyklad dla y_ca
#model="ETS"; i za kazda literke {E,T,S} trzeb podstawic {N,A,M,Z}
#N - none
#A - additive
#M - multiplicative
#Z - automatyczny wybor
#procedura wybierze automatycznie czy trend zwykly czy damped

y_ets <- ets(y, model="ZZZ", opt.crit = "mse")
prognozy$y_ets<- y_ets$fitted
?mape

#RMSE i MAE
prognozy_2020m6_2022m5 <- prognozy[indeks_czasu > 101 & indeks_czasu < 126, ]

bledy_prognoz_2020m6_2022m5$metoda[4] <- "ets" 
bledy_prognoz_2020m6_2022m5$rmse[4] <- rmse(prognozy_2020m6_2022m5$y, prognozy_2020m6_2022m5$y_ets) 
bledy_prognoz_2020m6_2022m5$mae[4] <- mae(prognozy_2020m6_2022m5$y, prognozy_2020m6_2022m5$y_ets)
bledy_prognoz_2020m6_2022m5$mape[4] <- mape(prognozy_2020m6_2022m5$y, prognozy_2020m6_2022m5$y_ets)

plot(y, type = "l", col = "black", ylab = "EUR/PLN", main = "Model ETS: błąd multiplikatywny, brak trendu i sezonowości")
lines(y_ets$fitted, col = "red", type = "l")
abline(v = 2020.5)
# ŁADNIEJSZY WYKRES




#####################################################################
#MODELE ARIMA
#####################################################################

# Model
y_arima_obj <- auto.arima(y)
summary(y_arima_obj)

plot(y, type = "l", col = "black", ylab = "EUR/PLN", main = "Model ARIMA(3, 1, 0)")
lines(y_arima_obj$fitted, col = "red", type = "l")


#### Z RUCHOMYM OKNEM tj. zawsze mamy 8 ostatnich ####
#horyzont prognozy
k <- 1

#okno weryfikacji trafnosci prognoz = 24
prognozy_2020m6_2022m5_arima <- prognozy[indeks_czasu > 101 & indeks_czasu < 126, ]

for (i in 1:24) {
  
  # Na jakim oknie prognozujemy
  input_2020m6_2022m5 <- prognozy[indeks_czasu < 101+i & indeks_czasu > 0 + i,]
  
  #DYNAMIKA M/M
  #full auto
  y_arma_obj <- auto.arima(input_2020m6_2022m5$y) %>% forecast(h=k)
  prognozy_2020m6_2022m5_arima$y_sca_mm_arma1[i] <- y_arma_obj$mean[k]

}

bledy_prognoz_2020m6_2022m5$metoda[5] <- "arima"
bledy_prognoz_2020m6_2022m5$rmse[5] <- rmse(prognozy_2020m6_2022m5$y, prognozy_2020m6_2022m5_arima$y_sca_mm_arma1) 
bledy_prognoz_2020m6_2022m5$mae[5] <- mae(prognozy_2020m6_2022m5$y, prognozy_2020m6_2022m5_arima$y_sca_mm_arma1) 
bledy_prognoz_2020m6_2022m5$mape[5] <- mape(prognozy_2020m6_2022m5$y, prognozy_2020m6_2022m5_arima$y_sca_mm_arma1)

residuals_arima <- prognozy_2020m6_2022m5$y - prognozy_2020m6_2022m5_arima$y_sca_mm_arma1

dm.test(residuals_ma, residuals_naiwna, alternative = c("two.sided"), h = 1, power = 2)


actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
mape(actual, predicted)
