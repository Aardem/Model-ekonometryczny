library(readr)
library(psych)
library(corrplot)
library(dplyr)
library(lmtest)
library(ggplot2)
library(car)
library(strucchange)
library(grid)
library(gridExtra)
library(DescTools)

setwd("E:/Studia/Sem. 1 II/Metody ekonometryczne/Projekt")
dane <- read_delim("IiE20192020dataset5.csv", 
                                  ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                  trim_ws = TRUE)

data.frame(round(describe(dane)[,c(3:5,11:12)],2),"coef_var"=round(apply(dane,2,function(x){sd(x, na.rm = TRUE)/abs(mean(x, na.rm = TRUE))}),2),"sum_of_NA"=apply(dane,2,function(x){sum(is.na(x))}))

#transformacja zmiennej zero-jedynkowej
dane$X7A<-ifelse(dane$X7=="A",1,0)
dane$X7B<-ifelse(dane$X7=="B",1,0)
dane$X7C<-ifelse(dane$X7=="C",1,0)
dane$X7<-NULL

corrplot(cor(na.omit(dane),method="pearson"),"number")


w1<-ggplot(dane, aes(X1, Y)) +
  geom_point()+
  geom_smooth(method="lm")

w2<-ggplot(dane, aes(X2, Y)) +
  geom_point()+
  geom_smooth(method="lm")

w3<-ggplot(dane, aes(X3, Y)) +
  geom_point()+
  geom_smooth(method="lm")

w4<-ggplot(dane, aes(X4, Y)) +
  geom_point()+
  geom_smooth(method="lm")

w5<-ggplot(dane, aes(X5, Y)) +
  geom_point()+
  geom_smooth(method="lm")

w6<-ggplot(dane, aes(X6, Y)) +
  geom_point()+
  geom_smooth(method="lm")

w7A<-ggplot(dane, aes(X7A, Y)) +
  geom_point()+
  geom_smooth(method="lm")

w7B<-ggplot(dane, aes(X7B, Y)) +
  geom_point()+
  geom_smooth(method="lm")

w7C<-ggplot(dane, aes(X7C, Y)) +
  geom_point()+
  geom_smooth(method="lm")

grid.arrange(w1, w2, w3, w4, w5, w6,w7A, w7B, w7C,top=textGrob("Wykresy zale¿noœci", gp=gpar(fontsize=12)))

dane_df<-data.frame("wartosc"=c(dane$Y,dane$X1,dane$X2,dane$X3,dane$X4,dane$X5,dane$X6),
                    "zmienna"=c(rep("Y",nrow(dane)),rep("X1",nrow(dane)),rep("X2",nrow(dane)),rep("X3",nrow(dane)),rep("X4",nrow(dane)),rep("X5",nrow(dane)),rep("X6",nrow(dane))))
w0<-dane%>%
  ggplot(aes(x = "Y", y = Y)) +
  geom_boxplot(fill="grey")+
  labs(x="",y="")
w1<-dane%>%
  ggplot(aes(x = "X1", y = X1)) +
  geom_boxplot()+
  labs(x="",y="")
w2<-dane%>%
  ggplot(aes(x = "X2", y = X2)) +
  geom_boxplot()+
  labs(x="",y="")
w3<-dane%>%
  ggplot(aes(x = "X3", y = X3)) +
  geom_boxplot()+
  labs(x="",y="")
w4<-dane%>%
  ggplot(aes(x = "X4", y = X4)) +
  geom_boxplot()+
  labs(x="",y="")
w5<-dane%>%
  ggplot(aes(x = "X5", y = X5)) +
  geom_boxplot()+
  labs(x="",y="")
w6<-dane%>%
  ggplot(aes(x = "X6", y = X6)) +
  geom_boxplot()+
  labs(x="",y="")

grid.arrange(w1, w2, w3, w4, w5, w6,w0,top=textGrob("Wykresy pude³kowe", gp=gpar(fontsize=12)))

bth <-function(x) 2 * IQR(x,na.rm=TRUE) / (length(x)^(1/3))
w0<-ggplot(dane, aes(Y)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  stat_function(fun =  function(x) dnorm(x, mean = mean(dane$Y,na.rm=TRUE), sd = sd(dane$Y,na.rm=TRUE))*bth(dane$Y)*length(dane$Y),col="red",size=1)+
  labs(y="")
w1<-ggplot(dane, aes(X1)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  labs(y="")+
  stat_function(fun =  function(x) dnorm(x, mean = mean(dane$X1,na.rm=TRUE), sd = sd(dane$X1,na.rm=TRUE))*bth(dane$X1)*length(dane$X1),col="red",size=1)
w2<-ggplot(dane, aes(X2)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  labs(y="")+
  stat_function(fun =  function(x) dnorm(x, mean = mean(dane$X2,na.rm=TRUE), sd = sd(dane$X2,na.rm=TRUE))*bth(dane$X2)*length(dane$X2),col="red",size=1)
w3<-ggplot(dane, aes(X3)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  labs(y="")+
  stat_function(fun =  function(x) dnorm(x, mean = mean(dane$X3,na.rm=TRUE), sd = sd(dane$X3,na.rm=TRUE))*bth(dane$X3)*length(dane$X3),col="red",size=1)
w4<-ggplot(dane, aes(X4)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  labs(y="")+
  stat_function(fun =  function(x) dnorm(x, mean = mean(dane$X4,na.rm=TRUE), sd = sd(dane$X4,na.rm=TRUE))*bth(dane$X4)*length(dane$X4),col="red",size=1)
w5<-ggplot(dane, aes(X5)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  labs(y="")+
  stat_function(fun =  function(x) dnorm(x, mean = mean(dane$X5,na.rm=TRUE), sd = sd(dane$X5,na.rm=TRUE))*bth(dane$X5)*length(dane$X5),col="red",size=1)
w6<-ggplot(dane, aes(X6)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  labs(y="")+
  stat_function(fun =  function(x) dnorm(x, mean = mean(dane$X6,na.rm=TRUE), sd = sd(dane$X6,na.rm=TRUE))*bth(dane$X6)*length(dane$X6),col="red",size=1)
w7A<-ggplot(dane, aes(X7A)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  labs(y="")
w7B<-ggplot(dane, aes(X7B)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  labs(y="")
w7C<-ggplot(dane, aes(X7C)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  labs(y="")
grid.arrange(w1, w2, w3, w4, w5, w6, w7A,w7B,w7C,w0,top=textGrob("Histogramy", gp=gpar(fontsize=12)))

#usuwanie outlierow  
outliersY <- boxplot(dane$Y, plot=FALSE)$out
outliersX1 <- boxplot(dane$X1, plot=FALSE)$out
outliersX2 <- boxplot(dane$X2, plot=FALSE)$out
outliersX3 <- boxplot(dane$X3, plot=FALSE)$out
outliersX4 <- boxplot(dane$X4, plot=FALSE)$out
outliersX5 <- boxplot(dane$X5, plot=FALSE)$out
outliersX6 <- boxplot(dane$X6, plot=FALSE)$out
dane <- dane[-which(dane$Y %in% outliersY),]
dane <- dane[-which(dane$X1 %in% outliersX1),]
dane <- dane[-which(dane$X2 %in% outliersX2),]
dane <- dane[-which(dane$X3 %in% outliersX3),]
dane <- dane[-which(dane$X4 %in% outliersX4),]
dane <- dane[-which(dane$X5 %in% outliersX5),]
dane <- dane[-which(dane$X6 %in% outliersX6),]

set.seed(293454)
index<-sample(1:nrow(dane),0.75*nrow(dane))
dane_train_unscaled<-dane[index,]
dane_test_unscaled<-dane[-index,]

#statystyki i korelacja
data.frame(round(describe(dane_train_unscaled)[,c(3:5,11:12)],2),"coef_var"=round(apply(dane_train_unscaled,2,function(x){sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)}),2),"sum_of_NA"=apply(dane_train_unscaled,2,function(x){sum(is.na(x))}))
data.frame(round(describe(dane_test_unscaled)[,c(3:5,11:12)],2),"coef_var"=round(apply(dane_test_unscaled,2,function(x){sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)}),2),"sum_of_NA"=apply(dane_test_unscaled,2,function(x){sum(is.na(x))}))

corrplot(cor(na.omit(dane_train_unscaled),method="pearson"),"number")
corrplot(cor(na.omit(dane_test_unscaled),method="pearson"),"number")

#skalowanie
dane_scaled<-data.frame(scale(dane[,-c(8,9,10)]),dane[,c(8,9,10)])

#podzial na zbior test i tening
dane_train<-dane_scaled[index,]
dane_test<-dane_scaled[-index,]

#usuwanie NA
dane_train<-na.omit(dane_train)
dane_test<-na.omit(dane_test)

#dobor zmiennych
hellwig <- function( y, x, method="pearson")
{
  requireNamespace("utils")
  x <- as.data.frame(x)
  cm <- stats::cor(x, method=method) # correlation matrix among indeps
  cd <- stats::cor(x, y, method=method) # correlations with dependent
  # list of combination vectors
  k <- sapply( seq(1, length(x)), function(i)
    utils::combn(length(x), i, simplify=FALSE) )
  k <- do.call("c", k)
  # function calculating individual capacities
  hfun <- function(v)
  {
    sapply(v, function(i) cd[i]^2 / sum(abs(cm[v,i])) )
  }
  h <- sapply(k, hfun)
  data.frame( k = sapply( k, paste, collapse=","),
              h = round(sapply(h, sum),3),
              stringsAsFactors=FALSE)
}

h<-hellwig(dane_train$Y,dane_train[,c(-1)])
(arrange(h,desc(h)))[c(1:10),]


model1<-lm(Y~X1+X2+X3+X4+X5+X6+X7A+X7B,dane_train)
summary(model1)
shapiro.test(model1$residuals)
resettest(model1)

model2<-lm(Y~I(X1^2)+X2+X3+I(X4^2)+X5+X7A+X7B,dane_train)
summary(model2)
shapiro.test(model2$residuals)
resettest(model2)

model3<-lm(Y~I(X1*X7B)+X2+X3+X4+X5+X7A,dane_train)
summary(model3)
shapiro.test(model3$residuals)
resettest(model3)

h<-hellwig(dane_train$Y,dane_train[,-c(1,6,7,8,9)])
(arrange(h,desc(h)))[c(1:10),]

#model_interactions<-lm(Y~(X1+X2+X3+X4+X5+X6+X7A+X7B)^2,dane_train)
#anova(model_interactions)

#ostateczny model
model<-lm(Y~X1+X2+X3+X4+X5,dane_train)
summary(model)
shapiro.test(model$residuals)
resettest(model)

#zalozenia MNK:

#1.Sk³adnik losowy ma wielowymiarowy rozk³ad normalny
shapiro.test(model$residuals)

#2.Zmienne objaœniaj¹ce Xi s¹ wielkoœciami nielosowymi o ustalonych elementach - spelnione

#3.Rz¹d macierzy X równy jest liczbie szacowanych parametrów - spelnione???

#4.Liczebnoœæ próby jest wiêksza ni¿ liczba szacowanych parametrów - spelnione

#5.Nie wystêpuje zjawisko wspó³liniowoœci pomiêdzy zmiennymi objaœniaj¹cymi
#Wartosc vif:
vif(model)

#6.Wartoœæ oczekiwana sk³adnika losowego jest równa zero
mean(model$residuals)

#7.Sk³adnik losowy ma sta³¹ skoñczon¹ wariancjê
bptest(model)
  
#8.Nie wystêpuje zjawisko autokorelacji sk³adnika losowego
dwtest(model)
  
#9.Szacowany model ekonometryczny jest liniowy wzglêdem parametrów
resettest(model)

#10.Musi wystepowac odpowiednio wysoki stopieñ dopasowania modelu (dopasowanie modelu do danych empirycznych) - skorzystamy ze statystyki F dla wspolczynnika determinacji.

#11.Stabilnosc - test Chowa
sctest(model,type = "Chow")

#12.Koincydencja
cor<-cor(na.omit(dane_train))[1,2:6]
coef<-model$coefficients[-1]
koin<-cor*coef>0
koincydencja_df<-data.frame("wspolczynniki_modelu"=cor,"korelacja_ze_zm._bwght"=coef,"koincydencja"=koin)

#13.Test Walda-Wolfowitza - test Serii
RunsTest(dane_train$Y)
RunsTest(dane_train$X1)
RunsTest(dane_train$X2)
RunsTest(dane_train$X3)
RunsTest(dane_train$X4)
RunsTest(dane_train$X5)

#14. Laczna istotnosc
waldtest(model)

#prognoza

#Dla podanych wartosci zmiennych objasniajacych, prognoza przyjmuje nastepujaca wartosc:

pred<-predict(model, newdata = dane_test, interval = 'prediction')[,c(1)]

#Przedzia³ ufnosci prognozy to:

pred_interval<-predict(model, dane_test, interval = 'prediction')[,c(2,3)]

diff<-(dane_test$Y-pred)
diffs<-abs(dane_test$Y-pred)
diff

pred_df<-data.frame("obserwacja"=c(1:length(pred),1:length(pred)),"Y"=c(dane_test$Y,pred),"wartosc"=c(rep("rzeczywista",length(pred)),rep("prognostyczna",length(pred))))
pred_df<-data.frame("obserwacja"=1:length(dane_test$Y),"Y"=dane_test$Y,"Y_prognoza"=pred,"lewy"=pred_interval[,1],"prawy"=pred_interval[,2])

pred_df <- pred_df[order(pred_df$Y_prognoza),]
pred_df$obserwacja <- sort(pred_df$obserwacja)

ggplot(data = pred_df, aes(x = obserwacja, y = Y)) + 
  geom_point(color='blue',size=3) +
  geom_point(color='red',data = pred_df, aes(x=obserwacja, y=Y_prognoza),size=3)+
  geom_line(color='red',data = pred_df, aes(x=obserwacja, y=lewy),size=1,linetype="dashed")+
  geom_line(color='red',data = pred_df, aes(x=obserwacja, y=prawy),size=1,linetype="dashed")


#Sredni blad predykcji EX POST wynosi:

pred.error_rel<-mean(diff)
pred.error_rel

#Stosowanie tego rodzaju bledu niewskazane jest gdy wartosci bledow prognozy maja wartosci dodatnie oraz ujemne. Sredni blad predykcji jest wtedy zanizony - bledy dodatnie zmniejszja bledy ujemne. 

#Sredni bezwzgledny blad predykcji EX POST wynosi:

pred.error_abs<-mean(diffs)
pred.error_abs

#Œredniokwadratowy b³¹d predykcji EX POST wynosi:

pred.error_meansqr<-mean(diffs^2)
pred.error_meansqr

#srednia bledu predykcji
sd_error<-sd(diff)


#œredni bezwzglêdny procentowy b³¹d predykcji EX POST wynosi:

pred.error_absper<-pred.error_abs/mean(abs(pred))*100
pred.error_absper

