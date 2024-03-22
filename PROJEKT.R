library(forecast)
library(tseries)
library(lmtest)
library(randtests)
library(whitestrap)
install.packages("readxl")
library(readxl)

# 1.Wczytanie danych o nazwie Roczne Wskaźniki Makroekonomiczne

dane <- read_excel("roczne_wskazniki_makroekonomiczne_ludnosc.xlsx", sheet = 1)
head(dane) #wyświetlenie pierwszych 6 wierszy.

wiek<-ts(dane$Mężczyźni, start = c(1989,1), end = c(2022, 1), freq = 1)
wiek
plot.ts(wiek, xlab="Rok", ylab="wiek", xaxt = "n", type="p",  pch=20)
lines(wiek, lty=3)
axis(side=1,at=c(1989:2022), las=2)
#Wykres punktowy oraz linia trendu zostały stworzone na podstawie szeregu czasowego przedstawiającego zmiany w wartościach w latach 1989-2022.Oś X na wykresie przedstawia lata od 1989 do 2022.

# 2.Ustalenie zbioru uczącego i testowego

length(wiek)
nTestowy <- 5
(nUczacy <- length(wiek) - nTestowy)
(uczacy <- window(wiek, start = c(1989, 1), end = c(1989, nUczacy), freq = 1))
(testowy <- window(wiek, start = c(1989, nUczacy + 1), end = c(1989, nUczacy + nTestowy), freq = 1))

#Zbiór uczący (uczacy) zawiera dane od początku szeregu czasowego do końca 2017 roku.
#Zbiór testowy (testowy) zawiera dane od początku 2018 roku do końca 2022 roku.
#Liczba punktów danych w zbiorze uczącym wynosi 29, a w zbiorze testowym wynosi 5.

# 3.Analiza graficzna 

plot.ts(wiek, xlab="Rok", ylab="wiek", xaxt = "n", type="p",  pch=20)
lines(wiek, lty=3)
axis(side=1,at=c(1989:2022), las=2)

plot.ts(uczacy, xlab="Rok", ylab="wiek", xaxt = "n", type="p",  pch=20)
lines(uczacy, lty=3)
axis(side=1,at=c(1989:2017), las=2)

#Wykres przedstawia serię czasową "wiek" dla zbioru uczącego.
#Punkty na wykresie reprezentują wartości "wiek" w poszczególnych latach.
#Linie przerywane łączą te punkty, co pomaga zobaczyć ogólny trend.
#Etykiety osi x przedstawiają lata od 1989 do 2017, obrócone o 90 stopni dla lepszej czytelności.

# 4.Wybór (wstępny) postaci funkcji trendu
# Wybieramy trend liniowy

# 5. Oszacowanie parametrów

wiek.trend <- tslm(uczacy ~ trend)
wyniki<-summary(wiek.trend)
wyniki$coefficients[,1]

#Z założenia odnośnie tego,że trend jest liniowy ->Model regresji sugeruje, że w 1989 roku średni "wiek" wynosił około 65.89 lat.
#Trend regresji (wzrost średniego wieku) wynosi około 0.28 lat rocznie.

# 6. Diagnostyka modelu
# 6.1 Badanie istotności parametrów strukturalnych:
wyniki$coefficients # Istoność parametrów
wyniki$fstatistic # Statystyka F
#Wartości p dla obu współczynników (Intercept i trend) są bardzo małe, co oznacza, że oba są statystycznie istotne.
#Statystyka F-wartość bardzo duża (1460.502) wskazuje, że co najmniej jeden z parametrów modelu jest istotny.
#Podsumowując, istotność statystyczna parametrów modelu sugeruje, że zarówno początkowy poziom w 1989 roku, jak i roczny wzrost są istotne dla modelu regresji. Model jest również ogólnie statystycznie istotny.

# 6.2 Dopasowania modelu do danych empirycznych
(R.kw<-wyniki$r.squared) # Współczynnik determinacji
(R.kw<-wyniki$adj.r.squared) # Skorygowany współczynnik determinacji
(Fi.kw<-1-R.kw) # Współczynnik zbieżności 
(s<-wyniki$sigma) # Odchylenie resztowe
(s.kw<-wyniki$sigma^2) # Wariancja resztowa
(V.s<-s/mean(uczacy)) # Współczynnik zmienności resztowej (losowej)
#Model wykazuje wysoki współczynnik determinacji, co oznacza, że znaczna część wariancji zmiennej zależnej jest wyjaśniona przez model.
#Współczynnik zbieżności wskazuje, że istnieje pewien stopień niezgodności między modelem a danymi. 
#Reszty są stosunkowo stabilne, a współczynnik zmienności resztowej jest niski, co świadczy o dobrej jakości dopasowania modelu do danych empirycznych.

# 6.3 Weryfikacja własności struktury stochastycznej

reszty<-wyniki$residuals
plot(reszty)
abline(h=0)

#Losowość składnika losowego
runs.test(reszty) #Test losowości  
runs.test(reszty, plot=TRUE)

# Normalność reszt:
hist(reszty, density=5, breaks=5, prob=TRUE, ylim=c(0,0.035)) 
curve(dnorm(x, mean=0, sd=s), col="darkblue", lwd=2, add=TRUE, yaxt="n")

shapiro.test(reszty) #shapiro.test(): Shapiro-Wilk Normality Test (stats)
jarque.bera.test(reszty) #jarque.bera.test(): Jarque-Bera test for normality (tseries)

#Podsumowując, wyniki wskazują, że reszty nie wykazują silnych dowodów na odrzucenie założenia o normalności.

#Jednorodność wariancji

# Test White'a: 
white_test(wiek.trend)
#Wartość p jest większa niż tradycyjny poziom istotności 0.05, co sugeruje, że nie ma silnych dowodów na odrzucenie hipotezy zerowej o homoskedastyczności.
#W takim przypadku możemy przyjąć, że nie ma istotnych dowodów na występowanie heteroskedastyczności w resztach modelu.

# Badanie autokorelacji składnika losowego  

Acf(reszty)
Pacf(reszty)

# Test Durbina-Watsona (paczka: lmtest):
dwtest(wiek.trend)

# Test Ljunga-Boxa
Box.test(reszty, lag = 1, type =  "Ljung-Box")
#Wartość poniżej 0.05 sugeruje, że istnieje istotna autokorelacja.
# Test Boxa-Pierce'a
Box.test(reszty, lag = 1, type = "Box-Pierce")
#Podobnie jak test Ljunga-Boxa, wartość poniżej 0.05 sugeruje istotną autokorelację.
#Podsumowując, wyniki tych testów wskazują na potencjalną obecność autokorelacji w resztach modelu. 

#Testowanie postaci funkcyjnej modelu

#Test RESET (Ramsey,a) .
resettest(wiek.trend)
#p-value wynosi 0.6112, co jest większe niż 0.05, co sugeruje, że nie ma istotnej potrzeby uwzględnienia dodatkowych nieliniowych składników w modelu.
#Test liniowości:
#Harvey-Collier test
harvtest(wiek.trend)
#p-value wynosi 0.4862, co jest znacznie większe niż 0.05, co oznacza, że nieliniowość nie jest statystycznie istotna.
#Podsumowując, oba testy sugerują, że model może być dobrze dopasowany do danych bez konieczności uwzględniania dodatkowych nieliniowych efektów.

# 7. Wyznaczenie prognoz na okres testowy 

wiek.prog.test <- forecast(wiek.trend , h = nTestowy, level = c(80, 95))
wiek.prog.test 
plot(wiek.prog.test )
#punktowa prognoza wieku na kolejne lata.


# 8. Wyznaczenie mierników dokładności prognoz ex post na zbiorze testowym.

m.expost<-accuracy(wiek.prog.test, testowy)
m.expost

plot(wiek.prog.test, type="b" , pch=20)
lines(wiek.trend$fitted.values, lwd = 1, col=4)
points(testowy, pch=20,col=1)
#Ogólnie rzecz biorąc, wyniki dla zestawu treningowego są obiecujące, ale dla zestawu testowego widać tendencję do większych błędów, co sugeruje, że model może być mniej dokładny.

# 10. Oszacowanie parametrów wybranego  modelu w oparciu o wszystkie dost?pne dane 

wybrany.model <- tslm(wiek ~ trend)

# 11. Wyznaczenie prognoz na okresy poza pr?b?
wiek.prog <- forecast(wybrany.model , h = 3, level = c(80, 95))
wiek.prog
plot(wiek.prog, type="b" , pch=20)
lines(wybrany.model$fitted.values, lwd = 1, col=4)
#Prognozy:
#Dla roku 2023 przewidywany wiek wynosi 74.89.
#Dla roku 2024 przewidywany wiek wynosi 75.14.
#Dla roku 2025 przewidywany wiek wynosi 75.38.
#Przedzialy ufności:
#Dla każdego prognozowanego roku przedstawione są przedzialy ufności na poziomie 80% i 95%.

# Ocena miernikami ex anete dok?adno?ci tych prognoz 

X<-model.matrix(~wiek)
war.resztowa<-(summary(wybrany.model)$sigma)^2
war.resztowa

length(wiek)

(c.35<-t(as.matrix(c(1,35))))
(c.36<-t(as.matrix(c(1,36))))
(c.37<-t(as.matrix(c(1,37))))

exante.35<-sqrt(war.resztowa*(1+c.35%*%(solve(t(X)%*%X))%*%t(c.35)))
exante.35
exante.36<-sqrt(war.resztowa*(1+c.36%*%(solve(t(X)%*%X))%*%t(c.36)))
exante.36
exante.37<-sqrt(war.resztowa*(1+c.37%*%(solve(t(X)%*%X))%*%t(c.37)))
exante.37

exante<-c(exante.35, exante.36, exante.37)

(V.exante<-exante/mean(wiek))
#Wariancja resztowa:
#Obliczona wariancja resztowa wynosi 0.5929288.
#Prognozy ex ante:
#Dla okresu 35: Prognoza przedokresowa wynosi 2.022505.
#Dla okresu 36: Prognoza przedokresowa wynosi 1.97427.
#Dla okresu 37: Prognoza przedokresowa wynosi 1.926253.
#Współczynnik zmienności resztowej (losowej):
#Współczynnik zmienności resztowej dla każdej z prognoz przedokresowych wynosi odpowiednio 0.02864857, 0.02796532, 0.02728517.
#Otrzymane wartości sugerują stosunkowo niską zmienność resztową w kontekście przewidywania wieku dla kolejnych okresów.

# ---------------------------- Model Holta --------------------------

fit1.holt <- holt(uczacy, alpha=0.9, beta=0.2, initial="simple", h=5) 
fit1.holt
fit2.holt <- holt(uczacy, alpha=0.4, beta=0.4, h=5) 
fit2.holt

# Wyniki:
summary(fit1.holt)
summary(fit2.holt)
fit1.holt$model$state #b to jest ct we wzorze a l to y z daszkiem 
fitted(fit1.holt)
fit1.holt$mean
#Model fit1.holt:
#Parametry modelu:
#alpha (poziom pochłaniania) wynosi 0.9.
#beta (poziom trendu) wynosi 0.2.
#Początkowe wartości stanu to: poziom (l) = 66.8, trend (b) = -0.6.
#Błąd modelu:
#RMSE wynosi 0.4097.
#Mape wynosi 0.4356.
#MAE wynosi 0.3011.
#ACF1 wynosi 0.3363.
#Prognozy:


#Model fit2.holt:
#Parametry modelu:
#alpha wynosi 0.4.
#beta wynosi 0.4.
#Początkowe wartości stanu to: poziom (l) = 66.205, trend (b) = -0.1807.
#Błąd modelu:
#RMSE wynosi 0.3905.
#Mape wynosi 0.4691.
#MAE wynosi 0.3259.
#ACF1 wynosi 0.2867.

fit3.holt<-holt(uczacy, h=5) 
summary(fit3.holt)
sink()
plot(fit1.holt , type="o", ylab="", xlab="", 
     fcol="white", plot.conf=FALSE)
lines(fitted(fit1.holt), col="blue") 
lines(fitted(fit2.holt), col="red")
lines(fitted(fit3.holt), col="green")
lines(fit1.holt$mean, col="blue", type="o") 
lines(fit2.holt$mean, col="red", type="o")
lines(fit3.holt$mean, col="green", type="o")
legend("topleft", lty=1, col=c("black","blue","red","green"), 
       c("Data","Model 1","Model 2","Model 3"))

round(accuracy(fit1.holt$mean,testowy), 3)
round(accuracy(fit2.holt$mean,testowy), 3)
round(accuracy(fit3.holt$mean,testowy), 3)

#Najniższą wartość MAPE uzyskaliśmy dla modelu fit2.holt (1,59), który okazał się najskuteczniejszy i wykorzystaliśmy go do oszacowania prognozy. 

# Prognozy 
h<-5 #Horyzont prognozy
prog.holt.test <- (forecast(fit2.holt, h=h))$mean
prog.holt.test

ex.post.testowy<-accuracy(prog.holt.test, testowy)
ex.post.testowy

h2<-3 #Horyzont prognozy
prog.holt <- (forecast(prog.holt.test, h=h2))$mean
prog.holt
#Prognozy na horyzoncie 3 lat (2023-2025) uzyskane z Modelu:
#2023: 74.45
#2024: 74.50
#2025: 74.55

###############################################################################
library(forecast)
library(TSA)
library(zoo)
library(tseries)
library(lmtest)
library(randtests)
library(whitestrap)
library(readxl)

# Podstaw dane
ts_dane <- ts(dane$Mężczyźni, start = c(1989, 1), freq = 1)

# Wykres dla podstawionych danych
plot.ts(ts_dane, xlab = "Rok", ylab = "Wiek [lata z≥]", xaxt = "n", type = "p", pch = 20)
lines(ts_dane, lty = 3)
axis(side = 1, at = c(1989:2022), las = 2)

# Ustalenie zbioru uczącego i testowego
length(ts_dane)
nTestowy.2 <- 3
uczacy <- window(ts_dane, end = c(2019, 1))
testowy.2 <- window(ts_dane, start = c(2020, 1), end = c(2022, 1))
y <- uczacy

# Obliczanie średniej ruchomej
srednia.ruchoma.scent.1 <- ma(y, order = 1)
srednia.ruchoma.scent.12 <- ma(y, order = 12)
plot(y, type = "b", pch = 20)
lines(srednia.ruchoma.scent.1, type = "b", col = 2)
lines(srednia.ruchoma.scent.12, type = "b", col = 3)
#Kod ten generuje wykres punktowy dla wektora danych y-dwie linie reprezentujące średnie ruchome o różnych rzędach (1 i 12).
# Poprawienie średniej ruchomej
srednia.ruchoma <- rollmean(y, k = 12, align = "center", fill = NA)
lines(srednia.ruchoma, type = "b", col = 4)

srednia.ruchoma.right <- rollmean(y, k = 12, align = "right", fill = NA)
srednia.ruchoma.left <- rollmean(y, k = 12, align = "left", fill = NA)
srednia.ruchoma.center <- rollmean(y, k = 12, align = "center", fill = NA)
plot(y, type = "b")
lines(srednia.ruchoma.right, type = "b", col = 2)
lines(srednia.ruchoma.left, type = "b", col = 3)
lines(srednia.ruchoma.center, type = "b", col = 4)
#różne warianty średniej ruchomej,okno przesuwa się w różnych kierunkach w stosunku do danych

h <- nTestowy.2

# Metody prognozowania
meanf(y, h)
#Otrzymujemy prognozę punktu (Point Forecast) oraz przedziały ufności o poziomach 80% (Lo 80, Hi 80) i 95% (Lo 95, Hi 95).
#Wszystkie te wartości są stałe na przyszłe okresy (2020, 2021, 2022)
#Oznacza, że prognozy są identyczne dla każdego okresu.
naive(y, h)
#przyszłe wartości będą takie same jak ostatnia obserwacja
rwf(y, h)  # Alternatywnie
#Zakłada, że przyszłe wartości będą równe ostatniej obserwacji.

# ale:
rwf(y, h, drift = TRUE)
#W tym przypadku prognozy punktowe są dostosowane o dryft, co można zauważyć, patrząc na różnice między prognozami dla różnych lat.


# Modele Fit
fit.meanf <- meanf(y, h = h)
fit.naive <- naive(y, h = h)
fit.rwf_drift <- rwf(y, h = h, drift = TRUE)

fit.meanf$mean
plot(fit.meanf)

fit.naive$mean
plot(fit.naive)

fit.rwf_drift$mean
plot(fit.rwf_drift)
#Dopasowanie trzech różnych modeli prognozowania do danych
#Przedstawia wyniki w formie prognozowanych wartości i przedziałów ufności na wykresach
#Prognozy
z <-testowy.2
z
h.2 <- 3 #planowane są prognozy na kolejne 3 okresy.
#szereg czasowy z danymi od 2020 do 2022.
# Metody prognozowania
prog.meanf<- meanf(z, h.2)
prog.meanf
#2023: Przewidywana średnia wartość wynosi 72.6, a przedziały ufności (80% i 95%) to odpowiednio od 70.86 do 74.34 i od 68.63 do 76.57.
#2024: Podobnie jak w 2023 roku, prognozowana średnia wartość wynosi 72.6, a przedziały ufności są podobne.
#2025: Prognoza dla 2025 roku jest identyczna, co wynika z tego, że metoda oparta na średniej arytmetycznej utrzymuje stałe prognozy.
prog.naive <-naive(z, h.2)
prog.naive
#2023: Przewidywana średnia wartość wynosi 73.4, a przedziały ufności (80% i 95%) to odpowiednio od 71.78 do 75.02 i od 70.92 do 75.88.
#2024: Prognozowana wartość wzrasta do 73.4, a przedziały ufności zmieniają się, odzwierciedlając zmienność prognozy.
#2025: Kolejna prognoza wzrasta do 73.4, a przedziały ufności nadal odzwierciedlają zmienność.
prog.rwf <-rwf(z, h.2)  # Alternatywnie
prog.rwf
#2023: Przewidywana średnia wartość wynosi 73.4, a przedziały ufności (80% i 95%) to odpowiednio od 71.78 do 75.02 i od 70.92 do 75.88.
#2024: Podobnie jak w 2023 roku, prognozowana średnia wartość wynosi 73.4, a przedziały ufności są podobne.
#2025: Prognoza dla 2025 roku jest identyczna, co wynika z tego, że metoda "random walk" utrzymuje stałe prognozy.

# ale:
prog.rwf.2 <-rwf(z, h.2, drift = TRUE)
prog.rwf.2
#2023: Przewidywana średnia wartość wynosi 73.8, a przedziały ufności (80% i 95%) to odpowiednio od 71.14 do 76.46 i od 69.73 do 77.87.
#2024: Prognozowana wartość rośnie do 74.2, a przedziały ufności również się zmieniają.
#2025: Kolejna prognoza wzrasta do 74.6, a przedziały ufności nadal odzwierciedlają zmienność prognozy, uwzględniając dryft.
# Modele Fit
prog.fit.meanf <- meanf(z, h = h.2)
prog.fit.meanf
#2023: Przewidywana średnia wartość wynosi 72.6, co jest zgodne z wcześniejszymi prognozami dla tej metody.
#2024: Podobnie jak w 2023 roku, prognozowana średnia wartość wynosi 72.6.
#2025: Prognoza dla 2025 roku utrzymuje się na poziomie 72.6, co wynika z charakterystyki metody średniej arytmetycznej utrzymującej stałe prognozy.
prog.fit.naive<- naive(z, h = h.2)
prog.fit.naive
#2023: Przewidywana średnia wartość wynosi 73.4, co jest zgodne z wcześniejszymi prognozami dla tej metody.
#2024: Prognozowana wartość wzrasta do 73.4, co również jest zgodne z wcześniejszymi prognozami.
#2025: Kolejna prognoza wzrasta do 73.4, co jest zgodne z wcześniejszymi prognozami.
prog.fit.rwf_drift<- rwf(z, h = h.2, drift = TRUE)
prog.fit.rwf_drift
#2023: Przewidywana średnia wartość wynosi 73.8, co jest zgodne z wcześniejszymi prognozami dla tej metody.
#2024: Prognozowana wartość rośnie do 74.2, co jest zgodne z wcześniejszymi prognozami.
#2025: Kolejna prognoza wzrasta do 74.6, co jest zgodne z wcześniejszymi prognozami.
prog.fit.meanf$mean
plot(prog.fit.meanf)

prog.fit.naive$mean
plot(prog.fit.naive)

prog.fit.rwf_drift$mean
plot(prog.fit.rwf_drift)
#Podsumowując, każdy z modeli prognozowania stosuje unikalne strategie w przewidywaniu przyszłych wartości.
#Model oparty na średniej arytmetycznej i metoda naiwna prezentują prognozy utrzymane na stałym poziomie.
#Model "random walk" z uwzględnieniem dryftu uwypukla tendencję wzrostową w danych, co wpływa na prognozowane wartości w kolejnych okresach.