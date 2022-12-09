
# Time Series Project

# ��s�ؼ�: �H�ɶ��ǦC���R����F���ɮ��F���O�_�y���֤߳q���v�U��?

# ��s����: 1990�~1�� ~ 2017�~12��

# ��ƨӷ�: FRED

setwd("D:/Chang-web/data")
library(readr)
corecpi <- read_csv("CPILFENS_1990-2017.csv", show_col_types = F)
colnames(corecpi) <- c("date", "cpi")
# ��l��Ʈɶ��ǦC��
cpi <- ts(corecpi$cpi, start=c(1990,1),end=c(2017,12),frequency=12)
plot(cpi, xlab="year", ylab="cpi index", main = "Monthly CPI index", lwd = 2) 

# ���F���ɶ��ƦC�󬰥�í�A�]���O�n�[��֤߳q���v�A�ҥH�� CPI �� log�A�A�}�l�B�z�öi����R�C
corelogcpi <- data.frame("date" = corecpi$date, "logcpi" = log(corecpi$cpi))
logcpi <- ts(corelogcpi$logcpi, start=c(1990,1),end=c(2017,12),frequency=12)
# ����ƫ᪺�ɶ��ǦC�ϡA���ɶ��Ͷ�
plot(logcpi, xlab="year", ylab="logcpi index", main = "Monthly CPI index", lwd = 2);abline(reg = lm(logcpi ~ time(logcpi)), col = "blue", lwd = 2) # time trend

# ���ΰV�m�M���ն�: ���μзǬ��p�Ƿ|�b2006�~�᪺���פɮ�2015�~12��A�]������~���A�H2016�~1�묰�����I�C
## �V�m��1990�~1�� ~ 2015�~12��(�@312�����)
## ���ն�2016�~1�� ~ 2017�~12��(�@24�����)
#### ���O1: �]��2019�~�᪺�̱��A�g�ټƾ��ܰʥi������L�~�ͦ]���z�Z�A�|�v�T�w�����G�A�]�����������ɮ��ɶ��I���������i�����R�C
#### ���O2: ���ն�����u�ݭn��5 ~ 10���A���]���F����g�٪��v�T���ɶ������A�ҥH�W�[���ն��϶���2�~(24��)�C

corecpi_train <- corelogcpi[-c(313:336), ]
corecpi_test <- corelogcpi[313:336, ]
str(corecpi_train)

logcpi <- ts(corecpi_train$logcpi, start=c(1990,1),end=c(2015,12), frequency = 12)
library(astsa)
logcpiacf2 <- acf2(logcpi, max.lag = 100) # acf: slow decay


# ���R�y�{:

## ��k�@�B�t���B�z:
### a. ���ɶ��ͶաA���@���t��
d1logcpi <- diff(logcpi, 1) 
ts.plot(d1logcpi, main = "first difference of logcpi")
### ���ǦC���M�S���Ҽ{�u�`�ʮt���A�]���w�g�O�w�A�A�ҥH�ڭ��٬O���t�A�ҫ�
library(tseries)
adf.test(d1logcpi) # stationary
### b. �@���t����A�u�`�ʳ���acf�w�C����A�A���@��lag = 12���u�`�ʮt��
d1logcpiacf <- acf2(d1logcpi,  max.lag = 180)
d12d1logcpi <- diff(d1logcpi, 12) 
ts.plot(d12d1logcpi, main = "first and seasonal difference of logcpi")
d12d1logcpiacf <- acf2(d12d1logcpi,  max.lag = 150) # �S�������ͶաA�����A���t��
adf.test(d12d1logcpi) # stationary


## ��k�G�B�H�ɶ�t�@���ܼƤ��j�k�B�z: ���h��CPI�W�����ͶաA�ϥήɶ�t�@���j�k�ܼơA�o�쪺fitted values�p��ݮt�A�[��ݮt�ɶ��ǦC�ϡA�A���A�����t���C
t <- 1:312
reg1 <- lm(logcpi ~ 1 + t, data = corecpi_train)
res1 <- reg1$residuals
ts.plot(res1, main = "residual time series of regression on time")
res1acf <- acf2(res1, max.lag = 100)
### res1��acf���w�C����A�����@���t��
d1res1 <- diff(res1, 1) 
ts.plot(d1res1, main = c(paste("residual time series of regression on time"),
                         paste("(first order difference)")))
d1res1acf <- acf2(d1res1, max.lag = 180)
### �@���t����A�u�`�ʳ����w�C����A�A���@��lag = 12���u�`�ʮt��
d12d1res1 <- diff(d1res1, 12) 
ts.plot(d12d1res1, main = c(paste("residual time series of regression on time"),
                         paste("(first and seasonal difference)")))
d12d1res1acf <- acf2(d12d1res1, max.lag = 150) # �S�������ͶաA�����A���t��
adf.test(d12d1res1) # stationary
### �ڭ̵o�{ d12d1res1 �M d12d1logcpi �� acf, pacf �ۦP�A���]���O�q���P�覡�o�쪺�ǦC�A�B����p��PMSE���覡�]�۲��A�]�����M�t�A�ҫ�order����ۦP�A��������ر��ΡC


## ��k�T�B�h�ܶq�j�k�B�z: �ϥ� IPI �M UE �@���j�k�ܼơA�[��o��ݮt���ɶ��ǦC�ϡA�A���A�����t���C
### ���n�ܼƤ@: �u�~�Ͳ����� industry production index
ipi <- read_csv("IPI_1990-2017.csv", show_col_types = F)
## train and test set
ipi_train <- ipi[-c(313:336), ]
### ���n�ܼƤG: ���~�v unemployment rate
ue <-  read_csv("UE_1990-2017.csv", show_col_types = F)
### train and test set
ue_train <- ue[-c(313:336), ]
traindata <- data.frame(corecpi_train, IPI = ipi_train$IPB50001N, UE = ue_train$UNRATENSA) 
str(traindata)  # �������O�ƭȫ��ܼ�
### regression
### a. ���j�k�ݮt�ǦC�D�w�A�A���]��ACF�S���w�C����A�ڭ̹��չ��ؼҡC
reg2 <- lm(logcpi ~ IPI + UE, data = traindata)
res2 <- reg2$residuals
ts.plot(res2, main = "residual time series of multiple linear regression")
res2acf <- acf2(res2, max.lag = 100) 
adf.test(res2) # not stationary

### �j�k�ݮt�ǦC�D�w�A�A�B�ɶ��ǦC���٦��ǤW�ɪ��ͶաA�Ҽ{�@���t��
d1res2 <- diff(res2, 1) 
ts.plot(d1res2, main = c(paste("residual time series of multiple linear regression"),
                         paste("(first order difference)")))
d1res2acf <- acf2(d1res2, max.lag = 180) 
adf.test(d1res2)
### �@���t�����w�A�ǦC�A���Oacf�bseasonal���w�C����{�H�A�Ҽ{�A�@���u�`�ʮt��
d12d1res2 <- diff(d1res2, 12) 
ts.plot(d12d1res2, main = c(paste("residual time series of multiple linear regression"),
                         paste("(first and seasonal difference)")))
### b. ���j�k�ݮt�ǦC�S�����㪺�ͶաA�����A�t��
d12d1res2acf <- acf2(d12d1res2, max.lag = 150) 
adf.test(d12d1res2) # stationary


# ���O��|�اǦC(��k�@:a.b.; ��k�T:a.b.)�إ� SARIMA �ҫ�: �e�Xacf�Mpacf�A�M�worder

## ��k�@�B�G���ɶ��ƦC�ϳ��b 0 ����_���A�ҥH�t�A�ҫ��Ҥ��Ҽ{���p�`�ƶ�
## ��k�@:
### a.
d1logcpiacf2 <- acf2(d1logcpi, max.lag = 150)
### seasonal part: acf and pacf tail off => seasonal ARMA(1,1)
### non-seasonal part: acf tails off; pacf �b�e��� seasonal lag �������@����� => AR(1)
fit00 <- stats::arima(logcpi, order=c(1,1,0), seasonal=list(order=c(1,0,1), period=12), method = "CSS-ML")
fit00 # �Y�Ƴ������
### seasonal part: acf and pacf tail off => seasonal ARMA(1,1)
### non-seasonal part: acf �bseasonal ���k�U���@����� => MA(1)
fit01 <- stats::arima(logcpi, order=c(0,1,1), seasonal=list(order=c(1,0,1), period=12), method = "CSS-ML")
fit01 # �Y�Ƴ������
### seasonal part: acf and pacf tail off => seasonal ARMA(1,1)
### non-seasonal part: acf and pacf tail off => ARMA(1,1)
fit02 <- stats::arima(logcpi, order=c(1,1,1), seasonal=list(order=c(1,0,1), period=12), method = "CSS-ML")
fit02 # �Y�Ƴ������
### seasonal part: acf tails off; pacf lag = 2 => seasonal AR(2)
### non-seasonal part: acf tails off;  pacf �b�e��� seasonal lag �������@����� => AR(1)
fit10 <- stats::arima(logcpi, order=c(1,1,0), seasonal=list(order=c(2,0,0), period=12), method = "CSS-ML")
fit10 # �Y�Ƴ������
### seasonal part: acf tails off; pacf lag = 2 => seasonal AR(2)
### non-seasonal part: acf �bseasonal ���k�U���@����� => MA(1)
fit11 <- stats::arima(logcpi, order=c(0,1,1), seasonal=list(order=c(2,0,0), period=12), method = "CSS-ML")
fit11 # �Y�Ƴ������
### seasonal part: acf tails off; pacf lag = 2 => seasonal AR(2)
### non-seasonal part:  acf and pacf tail off => ARMA(1,1)
fit12 <- stats::arima(logcpi, order=c(1,1,1), seasonal=list(order=c(2,0,0), period=12), method = "CSS-ML")
fit12 # �Y�Ƴ������

### b.
d12d1logcpiacf2 <- acf2(d12d1logcpi, max.lag = 150)
### seasonal part: acf cuts off at lag = 1; pacf tails off => seasonal MA(1)
### non-seasonal part: acf �bseasonal lag = 1 ����������; pacf tails off => MA(2)
fit20 <- stats::arima(logcpi, order=c(0,1,2), seasonal=list(order=c(0,1,1), period=12), method = "CSS-ML")
fit20 # ma2 �Y�Ƥ���ۡA�ݭn�ץ�
fit21 <- stats::arima(logcpi, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12), method = "CSS-ML")
fit21 # �Y�Ƴ������
### seasonal part: acf cuts off at lag = 1; pacf tails off => seasonal MA(1)
### non-seasonal part: acf and pacf tail off => ARMA(1,1)
fit22 <- stats::arima(logcpi, order=c(1,1,1), seasonal=list(order=c(0,1,1), period=12), method = "CSS-ML")
fit22 # �Y�Ƴ������
### seasonal part: acf and pacf tail off => seasonal ARMA(1,1)
### non-seasonal part: acf �bseasonal lag = 1 ����������; pacf tails off => MA(2)
fit30 <- stats::arima(logcpi, order=c(0,1,2), seasonal=list(order=c(1,1,1), period=12), method = "CSS-ML")
fit30 # ma2, sar1 �Y�Ƥ���ۡA�ݭn�ץ��A�ץ��ᵲ�G�� fit21
### seasonal part: acf and pacf tail off => seasonal ARMA(1,1)
### non-seasonal part: acf and pacf tail off => ARMA(1,1)
fit31 <- stats::arima(logcpi, order=c(1,1,1), seasonal=list(order=c(1,1,1), period=12), method = "CSS-ML")
fit31 # sar1 �Y�Ƥ���ۡA�ݭn�ץ�, �ץ��ᬰ fit22


## ��k�G: (���order���z�ѻP��k�@a.�ۦP�A�t�O�b��ϥθ�Ƭ��j�k�᪺�ݮt)
d12d1res1acf2 <- acf2(d12d1res1, max.lag = 150)
### seasonal part: acf cuts off at lag = 1; pacf tails off => seasonal MA(1)
### non-seasonal part: acf �bseasonal lag = 1 ����������; pacf tails off => MA(2)
fit40 <- stats::arima(res1, order=c(0,1,2), seasonal=list(order=c(0,1,1), period=12), method = "CSS-ML")
fit40 # ma2 �Y�Ƥ���ۡA�ݭn�ץ�
fit41 <- stats::arima(res1, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12), method = "CSS-ML")
fit41 # �Y�Ƴ������
### seasonal part: acf cuts off at lag = 1; pacf tails off => seasonal MA(1)
### non-seasonal part: acf and pacf tail off => ARMA(1,1)
fit42 <- stats::arima(res1, order=c(1,1,1), seasonal=list(order=c(0,1,1), period=12), method = "CSS-ML")
fit42 # �Y�Ƴ������
### seasonal part: acf and pacf tail off => seasonal ARMA(1,1)
### non-seasonal part: acf �bseasonal lag = 1 ����������; pacf tails off => MA(2)
fit50 <- stats::arima(res1, order=c(0,1,2), seasonal=list(order=c(1,1,1), period=12), method = "CSS-ML")
fit50 # ma2, sar1 �Y�Ƥ���ۡA�ݭn�ץ��A�ץ��ᵲ�G�� fit41
### seasonal part: acf and pacf tail off => seasonal ARMA(1,1)
### non-seasonal part: acf and pacf tail off => ARMA(1,1)
fit51 <- stats::arima(res1, order=c(1,1,1), seasonal=list(order=c(1,1,1), period=12), method = "CSS-ML")
fit51 # sar1 �Y�Ƥ���ۡA�ݭn�ץ�, �ץ��ᬰ fit42



## ��k�T:
## ��k�Ta.���ɶ��ƦC�Ϥ��b 0 ����_���A�ҥH�t�A�ҫ��Ҽ{���p�`�ƶ�
### a.
res2acf2 <- acf2(res2, max.lag = 150)
### seasonal part: acf tails off; pacf cut off at lag = 1 => seasonal AR(1)
### nonseasonal part: acf and pacf tail off => ARMA(1,1)
### �]���S���b 0 ����_���A�ҥH���p�`�ƶ�
fit60 <- stats::arima(res2, order=c(1,0,1), seasonal=list(order=c(1,0,0), period=12), method = "CSS-ML")
fit60  # ma �Y�Ƥ����
fit61 <- stats::arima(res2, order=c(1,0,0), seasonal=list(order=c(1,0,0), period=12), method = "CSS-ML")
fit61 # �Y�Ƴ������
### �Ѯv��ĳ:�����tAR(4)+Seasonal order
### seasonal part: acf tails off; pacf cut off at lag = 1 => seasonal AR(1)
### nonseasonal part: acf tail off; pacf cut off at lag = 5 => AR(5)
fit62 <- stats::arima(res2, order=c(5,0,0), seasonal=list(order=c(1,0,0), period=12), method = "CSS-ML")
fit62 # ar �Y�Ƥ���ۡA�R��ᬰ fit61

## ��k�Tb.���ɶ��ƦC�Ϧb 0 ����_���A�ҥH�t�A�ҫ����Ҽ{���p�`�ƶ�
### b.
d12d1res2acf <- acf2(d12d1res2, max.lag = 150) 
### seasonal part: acf cut off at lag = 1; pacf tails off => seasonal MA(1)
### nonseasonal part:  acf �bseasonal �k�Υ������@����۪�lag => MA(1)
fit70 <- stats::arima(res2, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12), method = "CSS-ML")
fit70 # �Y�Ƴ������
### seasonal part: acf cut off at lag = 1; pacf tails off => seasonal MA(1)
### nonseasonal part:  acf tails off; pacf �bseasonal lag = 0,1 �k����� => AR(1)
fit71 <- stats::arima(res2, order=c(1,1,0), seasonal=list(order=c(0,1,1), period=12), method = "CSS-ML")
fit71 # �Y�Ƴ������
### seasonal part: pacf cut off at lag = 2 => seasonal AR(2)
### nonseasonal part:  acf �bseasonal �k�Υ������@����۪�lag => MA(1)
fit80 <- stats::arima(res2, order=c(0,1,1), seasonal=list(order=c(2,1,0), period=12), method = "CSS-ML")
fit80 # �Y�Ƴ������
### seasonal part: pacf cut off at lag = 2 => seasonal AR(2)
### nonseasonal part:  acf tails off; pacf �bseasonal lag = 0,1 �k����� => AR(1)
fit81 <- stats::arima(res2, order=c(1,1,0), seasonal=list(order=c(2,1,0), period=12), method = "CSS-ML")
fit81 # �Y�Ƴ������

# ���O�C�X�|�اǦC�U���Կ�ҫ��A�HAIC���зǡA�b�T�ؤ�k�U�U�ۿ�X�̨μҫ�: fit12; fit42; fit71
## ��k�@: fit12
### a.
fit00$aic # -3373.177
fit01$aic # -3369
fit02$aic # -3354.22
fit10$aic # -3351.049
fit11$aic # -3348.36
fit12$aic # -3373.339
### b.
fit21$aic # -3259.66
fit22$aic # -3285.293


## ��k�G: fit42
fit41$aic # -3260.736
fit42$aic # -3285.47

## ��k�T: fit71
### a.
fit61$aic # -1930.805
### b.
fit70$aic # -1951.541
fit71$aic # -1952.119
fit80$aic # -1950.68
fit81$aic # -1951.195


# �T�{�T�ؼҫ������q�L�ݮt�˩w�C
## ���O1: �ݮt�˩w�n�ΦѮv�W�Ҫ� Ljung--Box test statistic �o�� p-value�A�ΰj��e�X�ϧ��[��O�_�ֿnlag���G�����W�L0.05�C
## ���O2: �i�H�A�� SARIMA ����X���G QQplot, residuals acf �T�{�ҫ��q�L�ݮt�˩w�C

## ��k�@: fit12 �S���q�L�ݮt�˩w
library(TSA)
stdresfit12 <- rstandard(fit12)  
ts.plot(stdresfit12, ylab="", main="standardized residuals(fit12)")
stdresfit12acf2 <- acf2(stdresfit12, max.lag = 150) 
B_text_p_value = c(0,0)
for(hh in 1:30){
  B_text_p_value[hh] = Box.test(stdresfit12, lag=hh, type="Ljung-Box")$p.value
}
plot(1:30, B_text_p_value[1:30], type="p", 
     main="p values for Ljung-Box statistic (fit12)", 
     xlab="lag", ylab="p value", ylim=c(0,1));abline(h=0.05, lty=2, col=4) 

### �ץ��ҫ�: 
#### �[�� fit12 �зǤƴݮt��acf & pacf
#### seasonal part: acf tail off and pacf cut off at lag = 2. => add seasonal AR(2)
mfit120 <- stats::arima(logcpi, order=c(1,1,1), seasonal=list(order=c(4,0,0), period=12), method = "CSS-ML")
mfit120 # sar3 �Y�Ƥ���ۡA�ݭn�ץ�
# �ץ��ҫ��� mfit121
mfit121 <- stats::arima(logcpi, order=c(1,1,1), seasonal=list(order=c(3,0,0), period=12), method = "CSS-ML")
mfit121 # �Y�Ƴ������
mfit121$aic # -3378.061 ��p�� AIC

### mfit121 ���q�L�ݮt�˩w
stdresmfit121 <- rstandard(mfit121)  
ts.plot(stdresmfit121, ylab="", main="standardized residuals(mfit121)")
stdresmfit121acf2 <- acf2(stdresmfit121, max.lag = 150) # close to white noise
B_text_p_value = c(0,0)
for(hh in 1:30){
  B_text_p_value[hh] = Box.test(stdresmfit121, lag=hh, type="Ljung-Box")$p.value
}
plot(1:30, B_text_p_value[1:30], type="p", 
     main="p values for Ljung-Box statistic (mfit121)", 
     xlab="lag", ylab="p value", ylim=c(0,1));abline(h=0.05, lty=2, col=4) 

## check
sarima(logcpi,1,1,1,3,0,0,12)

## ��k�G: fit42 ���q�L�ݮt�˩w
stdresfit42 <- rstandard(fit42)  
ts.plot(stdresfit42, ylab="", main="standardized residuals(fit42)")
stdresfit42acf2 <- acf2(stdresfit42, max.lag = 100) # close to white noise
B_text_p_value = c(0,0)
for(hh in 1:100){
  B_text_p_value[hh] = Box.test(stdresfit42, lag=hh, type="Ljung-Box")$p.value
}
plot(1:100, B_text_p_value[1:100], type="p", 
     main="p values for Ljung-Box statistic (fit42)", 
     xlab="lag", ylab="p value", ylim=c(0,1));abline(h=0.05, lty=2, col=4)

## check
sarima(res1, 1,1,1,0,1,1,12) 

## ��k�T: fit71 �S���q�L�ݮt�˩w
stdresfit71 <- rstandard(fit71)  
ts.plot(stdresfit71, ylab="", main="standardized residuals(fit71)")
stdresfit71acf2 <- acf2(stdresfit71, max.lag = 100) # close to white noise
B_text_p_value = c(0,0)
for(hh in 1:100){
  B_text_p_value[hh] = Box.test(stdresfit71, lag=hh, type="Ljung-Box")$p.value
}
plot(1:100, B_text_p_value[1:100], type="p", 
     main="p values for Ljung-Box statistic (fit71)", 
     xlab="lag", ylab="p value", ylim=c(0,1));abline(h=0.05, lty=2, col=4) 
### �Ѯv��ĳ:�ݮt�˩w���G�w�g�ܦn�A�����A�ץ��ҫ�

### �ץ��ҫ�: 
#### �[�� fit71 �зǤƴݮt��acf & pacf
#### non-seasonal part: acf and pacf tail off => ARMA(1,1)
mfit71 <- stats::arima(res2, order=c(2,1,1), seasonal=list(order=c(0,1,1), period=12), method = "CSS-ML")
mfit71 # �Y�Ƴ������
mfit71$aic # -1951.004 AIC ���y�L�j�@��
#### �Ѯv��ĳ:�i�HAR(1)��MA(1) => �Y�Ʀ�����ۡA�ĪG�S�� ARMA(1,1) �n

### mfit71 ���q�L�ݮt�˩w
stdresmfit71 <- rstandard(mfit71)  
ts.plot(stdresmfit71, ylab="", main="standardized residuals(mfit71)")
stdresmfit71acf2 <- acf2(stdresmfit71, max.lag = 100) # close to white noise
B_text_p_value = c(0,0)
for(hh in 1:100){
  B_text_p_value[hh] = Box.test(stdresmfit71, lag=hh, type="Ljung-Box")$p.value
}
plot(1:100, B_text_p_value[1:100], type="p", 
     main="p values for Ljung-Box statistic (mfit71)", 
     xlab="lag", ylab="p value", ylim=c(0,1));abline(h=0.05, lty=2, col=4) 

## check
sarima(res2, 2,1,1,0,1,1,12) 

## �̫�b�T�ؤ覡���q�L�ݮt�˩w���ҫ����O��: mfit121, fit42, fit71, mfit71


# �w��
library(forecast)
## 24-steps ahead predictions and confidence intervals
forcastm121 <- forecast(mfit121, level=c(95), h=2*12)
forcast42 <- forecast(fit42, level=c(95), h=2*12)
forcast71 <- forecast(fit71, level=c(95), h=2*12)
forcastm71 <- forecast(mfit71, level=c(95), h=2*12)
forcastm121
forcast42
forcast71
forcastm71

## ��ı�Ƨe�{�w�����G
## Fan charts
plot(forcastm121, main = "Forecasts from SARIMA(1,1,1)*(3,0,0)[12]")

tscorelogcpi <- ts(corelogcpi$logcpi, start=c(1990,1),end=c(2017,12),frequency=12)
plot(tscorelogcpi, xlab="year", ylab="logcpi index", 
     main = "Forecasts from SARIMA(1,1,1)*(3,0,0)[12]", 
     lwd = 1, lty = 1, ylim = c(4.85, 5.7), col = "black")
lines(forcastm121$mean, type = "l", col = "blue", lwd = 2)
lines(forcastm121$lower, col = "orange", lwd = 2, lty = 1)
lines(forcastm121$upper, col = "orange", lwd = 2, lty = 1)
legend("topleft", 
       c("95% prediction interval", "prediction", "core_cpi% in dataset"),
       lty = 1, lwd = c(2,2,1), 
       col = c("orange", "blue", "black"))


### fan chart of residuals
plot(forcast42, main = "Forecasts of residuals from SARIMA(1,1,1)*(0,1,1)[12]")
### �q�j�k�ڭ̦��Y�ƪ����p�ȡA�N�o�ǫY�ƩM�ҹ������ն��������ܼƬۭ��å[�`�A�A�N�o�Ǽƭȥ[�W�w���ݮt�A���G�����ն����t�A��(fitted value)�C
beta0_hat <- reg1$coefficients[[1]]
beta1_hat <- reg1$coefficients[[2]]
prederror1 <- forcast42$mean
t <- 313:336
fittedreg1 <- beta0_hat + t*beta1_hat + prederror1
lbd_fit42 <- beta0_hat + t*beta1_hat + forcast42$lower
ubd_fit42 <- beta0_hat + t*beta1_hat + forcast42$upper 
fittedreg1 <- ts(fittedreg1, start = c(2016,1), end = c(2017,12), frequency = 12)
lbd_fit42 <- ts(lbd_fit42, start = c(2016,1), end = c(2017,12), frequency = 12)
ubd_fit42 <- ts(ubd_fit42, start = c(2016,1), end = c(2017,12), frequency = 12)
plot(tscorelogcpi, xlab="year", ylab="logcpi index", 
     main = "Forecasts from SARIMA(1,1,1)*(0,1,1)[12]", 
     lwd = 1, lty = 1, ylim = c(4.85, 5.7))
lines(fittedreg1, type = "l", col = "blue", lwd = 2)
lines(lbd_fit42, col = "orange", lwd = 2, lty = 1)
lines(ubd_fit42, col = "orange", lwd = 2, lty = 1)
legend("topleft", 
       c("95% prediction interval", "prediction", "core_cpi% in dataset"),
       lty = 1, lwd = c(2,2,1), 
       col = c("orange", "blue", "black"))


### fan chart of residuals
plot(forcast71, main = "Forecasts of residuals from SARIMA(1,1,0)*(0,1,1)[12]")
plot(forcastm71, main = "Forecasts of residuals from SARIMA(2,1,1)*(0,1,1)[12]")

beta0_hat <- reg2$coefficients[[1]]
beta1_hat <- reg2$coefficients[[2]]
beta2_hat <- reg2$coefficients[[3]]
ipi_test <- ipi[313:336, 2]
ue_test <- ue[313:336, 2]
prederror2_fit71 <- forcast71$mean
prederror2_fitm71 <- forcastm71$mean
fittedreg2_fit71 <- c() 
fittedreg2_mfit71 <- c()
lbd_fit71 <- c()
ubd_fit71 <- c()
lbd_mfit71 <- c()
ubd_mfit71 <- c()
for(i in 1:24){
  
  fittedreg2_fit71[i] <- beta0_hat + beta1_hat * ipi_test$IPB50001N[i] + beta2_hat * ue_test$UNRATENSA[i] + prederror2_fit71[i]
  fittedreg2_mfit71[i] <- beta0_hat + beta1_hat * ipi_test$IPB50001N[i] + beta2_hat * ue_test$UNRATENSA[i] + prederror2_fitm71[i]
  lbd_fit71[i] <- beta0_hat + beta1_hat * ipi_test$IPB50001N[i] + beta2_hat * ue_test$UNRATENSA[i] + forcast71$lower[i]
  ubd_fit71[i] <- beta0_hat + beta1_hat * ipi_test$IPB50001N[i] + beta2_hat * ue_test$UNRATENSA[i] + forcast71$upper[i]
  lbd_mfit71[i] <- beta0_hat + beta1_hat * ipi_test$IPB50001N[i] + beta2_hat * ue_test$UNRATENSA[i] + forcastm71$lower[i]
  ubd_mfit71[i] <- beta0_hat + beta1_hat * ipi_test$IPB50001N[i] + beta2_hat * ue_test$UNRATENSA[i] + forcastm71$upper[i]

}
fittedreg2_fit71 <- ts(fittedreg2_fit71, start = c(2016,1), end = c(2017,12), frequency = 12)
fittedreg2_mfit71 <- ts(fittedreg2_mfit71, start = c(2016,1), end = c(2017,12), frequency = 12)
lbd_fit71 <- ts(lbd_fit71, start = c(2016,1), end = c(2017,12), frequency = 12)
ubd_fit71 <- ts(ubd_fit71, start = c(2016,1), end = c(2017,12), frequency = 12)
lbd_mfit71 <- ts(lbd_mfit71, start = c(2016,1), end = c(2017,12), frequency = 12)
ubd_mfit71 <- ts(ubd_mfit71, start = c(2016,1), end = c(2017,12), frequency = 12)

plot(tscorelogcpi, xlab="year", ylab="logcpi index", 
     main = "Forecasts from SARIMA(1,1,0)*(0,1,1)[12]",
     lwd = 1, lty = 1, ylim = c(4.85, 5.7))
lines(fittedreg2_fit71, type = "l", col = "blue", lwd = 2)
lines(lbd_fit71, col = "orange", lwd = 2, lty = 1)
lines(ubd_fit71, col = "orange", lwd = 2, lty = 1)
legend("topleft", 
       c("95% prediction interval", "prediction", "core_cpi% in dataset"),
       lty = 1, lwd = c(2,2,1), 
       col = c("orange", "blue", "black"))

plot(tscorelogcpi, xlab="year", ylab="logcpi index", 
     main = "Forecasts from SARIMA(2,1,1)*(0,1,1)[12]",
     lwd = 1, lty = 1, ylim = c(4.85, 5.7))
lines(fittedreg2_mfit71, type = "l", col = "blue", lwd = 2)
lines(lbd_mfit71, col = "orange", lwd = 2, lty = 1)
lines(ubd_mfit71, col = "orange", lwd = 2, lty = 1)
legend("topleft", 
       c("95% prediction interval", "prediction", "core_cpi% in dataset"),
       lty = 1, lwd = c(2,2,1), 
       col = c("orange", "blue", "black"))

## �[����ն�������
#### �ѤU�ϡA �����@�B�G���ҫ��A��w���P��ڭȧ�{���ܱ���Amfit71(SARIMA(2,1,1)*(0,1,1)[12]) ���w�����γ̤��z�Q�C
#### �q��k�T�ҫ��ץ����G�A�p�G�b�ݮt�˩w�l�D��������۪�lag�A�i��|�ɭP�w���󤣺�T�C(��k�T����ȰQ�� fit71 �w������)
#### �t�~�A�i�H�[���b�e15���Afit42���w�������ǽT�A�M�Ӧb20����A�ϦӬO mfit121(SARIMA(1,1,1)*(3,0,0)[12]) �������ڭȡC
#### �q�ϥi�ݥX mfit121, fit42 ���w�����G�A���b�~��(��12���M24��)���U�����{�H�A�����G�M���ն�����ڭȤ@�P�C
plot(forcastm121$mean,
     ylim = c(5.495, 5.56), xlim = c(2016, 2018), xaxt="n",
     type = "l", col = "darkorange", lwd = 2, xlab = "",
     main = "Forcasts from models and observed data in 2016-2017", ylab = "core_cpi%")
axis(1, at=seq(2016, 2018, by=0.5), labels = c("2016 Jan", "2016 June", "2017 Jan", "2017 June", "2018 Jan"))
lines(fittedreg1, type = "l", col = "green", lwd = 2)
lines(fittedreg2_fit71, type = "l", col = "purple", lwd = 2)
lines(fittedreg2_mfit71, type = "l", col = "blue", lwd = 2)
observed <- ts(corecpi_test$logcpi, start = c(2016,1), end = c(2017,12), frequency = 12)
lines(observed,type = "l", col = "black", lwd = 2)
points(forcastm121$mean, col = "darkorange", pch = 16)
points(fittedreg1, col = "green", pch = 16)
points(fittedreg2_fit71, col = "purple", pch = 16)
points(fittedreg2_mfit71, col = "blue", pch = 16)
points(observed, col = "black", pch = 16)
legend("bottomright", 
       c("SARIMA(1,1,1)*(3,0,0)[12]", "SARIMA(1,1,1)*(0,1,1)[12]", "SARIMA(1,1,0)*(0,1,1)[12]","SARIMA(2,1,1)*(0,1,1)[12]", "core_cpi% in test set"),
       lty = 1, lwd = 2,pch = 16, 
       col = c("darkorange", "green","purple", "blue", "black"))


## �z�L���PMSE�j�p��X�̨μҫ�
### ������ն���کM�w�����t�Z
#### ��k�@
pred_corecpi <- data.frame(forcastm121$mean)
error_mfit121 <- c()
errorsq_mfit121 <- c()
for(i in 1:24){
  
  error_mfit121[i] <- corecpi_test[i,2] - pred_corecpi[i,1]
  errorsq_mfit121[i] <- error_mfit121[i]^2
  PMSE_mfit121 <- mean(errorsq_mfit121)
  
}


### �ڭ̱N���ն�����ڸ�ƻP��t�A�Ȭ۴�A�p�⥭��M�A�������A�i�H�o�� PMSE�C��k�G�B�T��PMSE�ҬO�z�L���y�{�p��o�X�����G�C
#### ��k�G
error_fit42 <- c()
errorsq_fit42 <- c()
for(i in 1:24){
  
  error_fit42[i] <- corecpi_test[i,2] - fittedreg1[i]
  errorsq_fit42[i] <- error_fit42[i]^2
  PMSE_fit42 <- mean(errorsq_fit42)
}

#### ��k�T
error_fit71 <- c()
errorsq_fit71 <- c()
for(i in 1:24){
  
  error_fit71[i] <- corecpi_test[i,2] - fittedreg2_fit71[i]
  errorsq_fit71[i] <- error_fit71[i]^2
  PMSE_fit71 <- mean(errorsq_fit71)
  
}

## �H����e�{
result <- round(data.frame(errorsq_mfit121, errorsq_fit42, errorsq_fit71), 6)
k <- c()
for(i in 1:24){
  k[i] <- paste(i,"-step")
}
rownames(result) <- k
colnames(result) <- c("mfit121", "fit42", "fit71")

## fit42 ���̤p�� PMSE�A�䦸�O mfit121�A�̫�h�O fit71�C
## �]�� fit42(�H�ɶ�t�������ܼƪ��j�k���t���ҫ�) �����}�n���w���~��A�H���ҫ��@���������i�q�L�ݮt�˩w���̨ΰt�A�ҫ��C
## PMSE
PMSE <- round(c(PMSE_mfit121, PMSE_fit42, PMSE_fit71), 6)
result <- data.frame(rbind(result, PMSE))
rownames(result)[25] <- "PMSE"
result


# ����
## �Q�� fit42(SARIMA(1,1,1)*(0,1,1)[12]) ���w�����ΡG
### 1. 
#### �N��ڭ�(observed_data)�B�t�A��(fitted values)�B��t(square_error)�B�w���϶�(prediction interval) �H����e�{�C
#### prediction intervals
#### ��ڭȳ����bprediction interval���C
df <- data.frame("observed_data" = corecpi_test$logcpi, 
           "fitted_value" = fittedreg1,
           "square_error" = round(errorsq_fit42,6),
           "95%_lower_bound" = lbd_fit42,
           "95%_upper_bound" = ubd_fit42
           )
df

### 2. 
#### �z�L�p��e������t�Z�A�i�H�o�{�b2016�~�P2017�~���A�����t�V���ܤƶq�A�D�n��]�O�����p�Ƿ|�b2015(0.25% �V> 0.50%)�M2016(0.50% �V> 0.75%)�~�������ť��ɮ��C
#### �]����w���̨ΰt�A�ҫ�(fit42)���ϬM�X�ɮ��F����I��A�֤߳q���v�b���ӤU�����{�H�A���ܤɮ��F���b����ƥi�������������q�����ĪG�A�ŦX�������i���w�����G�C
diff <- c()
for(i in 2:24){
  diff[i] <- round(fittedreg1[i] - fittedreg1[i-1], 6)
}
diff

### 3. �i��i����V
#### ��k�G�ήɶ��ܼƮ����ͶաA���ܼƥi�H������ƪ�99%���ܲ��C
#### ��k�G�ήɶ��ܼƮ����ͶաA�վ��P�w�Y�� (adjusted R squared) ��ܦ��ܼƥi�H������ƪ�99%���ܲ��C��k�T�A�ڭ̧�H�u�~�Ͳ����ƩM���~�v�@�������ܼƮ����ͶաA�ĪG�O�̮t���A���ܹ�󦹸�ơA�����L�����ܼƪ����G�èS���u��ɶ��ܼơA�վ��P�w�Y�Ƭ�95%�A�T�겤�C���k�G99%�����G�C
#### �b�����s����ĳ�D�A���i�H���զh�ܶq�j�k���覡�A�����`�N�ҿ�������ܼƪ����n�ʡC

## ����
# �{���X�P��X���G:
# https://chang-web.github.io/Time-Series-Analysis/Time-Series-Final-Report.html

# ��ƨӷ�:
# https://fred.stlouisfed.org/series/CPILFENS
# https://fred.stlouisfed.org/series/UNRATENSA
# https://fred.stlouisfed.org/series/IPB50001N

# �ѦҸ��:
# http://www.math.chalmers.se/Stat/Grundutb/GU/MSA220/S16/Lecture9-2015.pdf
# https://www.investopedia.com/ask/answers/12/inflation-interest-rate-relationship.asp
# https://money.cnn.com/2015/12/16/news/economy/federal-reserve-interest-rate-hike/index.html
# https://en.wikipedia.org/wiki/History_of_Federal_Open_Market_Committee_actions#December_2015_historic_interest_rate_hike

