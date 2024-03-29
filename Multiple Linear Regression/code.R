
## 一、主題
# 預期壽命的預測

## 二、資料來源: kaggle; WHO; United Nation

## 三、文獻回顧
# 2019-2022年間，共有四篇文獻使用這筆資料作分析。
# 2019, 2021 年的文獻有提到開發和已開發國家在預期壽命的預測是不相同的，但都沒有做這項分析。
# 文獻有提及多變數迴歸的方法，但因為要和機器學習比較預測能力，比較少著墨在變數選擇和模型診斷。
# 只有一篇文獻按洲別分類預測，和多變數迴歸都有較好的預測結果(和多項式迴歸比較)

## 四、動機與目標
# 如果不考慮年度，將所有資料直接放入模型預測不太適當，這次報告先以前一年2013的資料預測2014年的預期壽命。
# 將開發中和已開發國家分別預測，同時加入六大洲的虛擬變數，比較全部國家一起預測的模型，以PMSE為預測能力指標。
# 關心:預測能力的變化、洲變數的影響、模型重要變數選擇差異和係數詮釋。

## 五、資料前處理
# import datasets
setwd("D:/Chang-web/data")
library(readxl)
continents <- read_excel("continents.xlsx")
life <- read_excel("life expectancy.xlsx")

# merge datasets by country name
# add continents columns
# left join
library(tidyverse)
library(dplyr)
continents <- rename(continents, Country = country)
data <- merge(x = life, y = continents, by = "Country", all.x = TRUE) |> relocate(continent, .after = Year)


# 遺失值的處理
# 文獻:平均值或0補值、刪除整欄變數
# 報告處理的方式:count total missing values in each column
# 並沒有某一欄出現異常多的NA，又因為建模前才要篩選變數，所以刪除NA前，不考慮刪減變數
sapply(data, function(x) sum(is.na(x)))
data <- na.omit(data)

# 報告使用的資料集 datafull
# 鎖定 2013, 2014 年，對2013建模，對2014預測
# 找出2013~2014年都有資料的國家: 127 個
# 總資料筆數: 254
# 總變數: 23
# original number of countries: 129
y <- data |> filter(Year >= 2013) |> summarise(Year, Country) |> group_by(Country) |> tally()
y <- merge(x = y, y = continents, by = "Country", all.x = TRUE)
y |> filter(n >= 2) |> group_by(continent) |> tally() # 這些國家分布於六大洲
z <- y |> filter(n >= 2) |> dplyr::select(Country, n)
# status: 1 = developed; 0 = developing
datafull <- merge(x = z, y = data, by = "Country", all.x = TRUE) |>
            filter(Year == 2013 | Year == 2014) |> dplyr::select(-n) |>
            mutate(status = if_else(Status == "Developed", 1, 0)) |> 
            relocate(status, .after = continent) |> dplyr::select(-Status)
# 避免格式錯誤:重新命名欄位
datafull <- dplyr::rename(datafull, Life_expectancy = `Life expectancy`)
datafull <- dplyr::rename(datafull, Adult_Mortality = `Adult Mortality`)
datafull <- dplyr::rename(datafull, Hepatitis_B = `Hepatitis B`)
datafull <- dplyr::rename(datafull, under_five_deaths = `under-five deaths`)
datafull <- dplyr::rename(datafull, Total_expenditure = `Total expenditure`)
datafull <- dplyr::rename(datafull, HIV_AIDS = `HIV/AIDS`)
datafull <- dplyr::rename(datafull, thinness_1_19_years = `thinness  1-19 years`)
datafull <- dplyr::rename(datafull, Income_composition_of_resources = `Income composition of resources`)
dat2013 <- datafull |> filter(Year == "2013") 
dat2014 <- datafull |> filter(Year == "2014")

# 2013 資料簡介
ls(dat2013) # 23 variables
str(dat2013)
summary(dat2013)
# 已開發國家幾乎都是歐洲，開發中國家以非洲居多
table(dat2013$status, dat2013$continent)


# 從洲間和國家發展狀態觀察，預期壽命都有些差異
# 文獻討論過洲的差異，這次報告討論開發程度的差異
# 開發和已開發國家最大的預期壽命幾乎相同，但已開發在最小、平均、中位數都多出超過10歲，標準差也比較小，盒狀圖可以看出類似的結果。
dat2013 |> group_by(continent) |> summarise(min_life = min(Life_expectancy),
                                            mean_life = mean(Life_expectancy),
                                            max_life = max(Life_expectancy),
                                            median_life = median(Life_expectancy),
                                            standard_diviation = sd(Life_expectancy))
dat2013 |> group_by(status) |> summarise(min_life = min(Life_expectancy),
                                         mean_life = mean(Life_expectancy),
                                         max_life = max(Life_expectancy),
                                         median_life = median(Life_expectancy),
                                         standard_diviation = sd(Life_expectancy))

par(mfrow = c(1,1))
boxplot(dat2013$Life_expectancy ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Life_expectancy")


# 初步從盒狀圖、相關係數矩陣圖刪除可能會產生共線性問題的變數
# remove columns having close meaning
# 1. GDP: remove "percentage expenditure"
# 2. thinness 1-19 years: remove "thinness 5-9 years"
# 3. under five deaths: remove "infant deaths"
library(corrplot)
corrplot(cor(datafull[,-c(1:4)]), method = "color", tl.srt = 45, type = "lower", order = "hclust")

# 社會面變數:成年自殺數(每千人)、15歲以上喝一公升酒的酒精濃度、人口數、就學年數
par(mfrow = c(2,2))
boxplot(dat2013$Adult_Mortality ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Adult_Mortality")
boxplot(dat2013$Alcohol ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Alcohol")
boxplot(dat2013$Population ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Population")
boxplot(dat2013$Schooling ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Schooling")

# 經濟面變數:
# percentage expenditure: 健康層面的花費佔GDP的百分比
# total expenditure: 政府在健康支出佔總政府支出的百分比
# income composition of resources: 資源的所得組成百分比
par(mfrow = c(2,2))
boxplot(dat2013$GDP ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "GDP")
boxplot(dat2013$`percentage expenditure` ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "`percentage expenditure`")
boxplot(dat2013$Total_expenditure ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Total_expenditure")
boxplot(dat2013$Income_composition_of_resources ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Income_composition_of_resources")

# 健康面變數: BMI，Hepatitis_B(一歲幼童的B型肝炎免疫力%)、Polio(poliomyelitis; 一歲幼童的脊髓灰質炎免疫力%)、Diphtheria(一歲幼童的白喉免疫力%)
# AIDS(0-4歲每千人因為愛滋病死亡的人數)、Measles(每千人患麻疹的通報數)、thinness(過瘦的人口百分比)、infant death(每千人五歲以下幼童死亡率)
par(mfrow = c(2,5))
boxplot(dat2013$BMI ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "BMI")
boxplot(dat2013$Hepatitis_B ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Hepatitis_B")
boxplot(dat2013$Polio ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Polio")
boxplot(dat2013$Diphtheria ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Diphtheria")
boxplot(dat2013$HIV_AIDS ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "HIV_AIDS")
boxplot(dat2013$Measles ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "Measles")
boxplot(dat2013$thinness_1_19_years ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "thinness_1_19_years")
boxplot(dat2013$`thinness 5-9 years` ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "`thinness 5-9 years`")
boxplot(dat2013$under_five_deaths ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "under_five_deaths")
boxplot(dat2013$`infant deaths` ~ dat2013$status, 
        xlab = c(paste("status"), paste("1: developed; 0: developing")), ylab = "infant deaths")

datafull <- datafull |> dplyr::select(-c(`percentage expenditure`, `thinness 5-9 years`, `infant deaths`))
dat2013 <- datafull |> filter(Year == "2013") # 最後報告用資料的變數為20個
dat2014 <- datafull |> filter(Year == "2014") 


## 六、資料分析方法、流程與結果 multiple regression 
# method 1: full model
# split data to developed and developing 
dat2013ed <- dat2013 |> filter(status == 1)
dat2013ing <- dat2013 |> filter(status == 0)
dat2014ed <- dat2014 |> filter(status == 1)
dat2014ing <- dat2014 |> filter(status == 0)

# full model
library(fastDummies)
dat2013dm <- dat2013 |> dummy_cols(select_columns = "continent") 
dat2013dm <- dplyr::rename(dat2013dm, continent_North_America = `continent_North America`)
dat2013dm <- dplyr::rename(dat2013dm, continent_South_America = `continent_South America`)

## let Africa as the base continent
# 顯著的變數:Income_composition_of_resources(+), HIV_AIDS(-), Alcohol(-), Total_expenditure(+), Population(+), 
# 洲的變數完全不顯著
# R-squared: 0.86
reg <- lm(Life_expectancy ~. -Year -Country -continent -continent_Africa , data = dat2013dm)
summary(reg)

# model diagnosis
# from test
# check normality: shapiro wilk test
shapiro.test(reg$residuals) # not normal
# check heteroskedastic
library(lmtest)
bptest(reg) # no  heteroscedasticity

# from plots
par(mfrow = c(2,2))
plot(reg)
# check influential point by cook's distance
par(mfrow = c(1,1))
plot(reg, which = 4) 
# 印度在五歲以下死亡人數和過瘦人口比例都是最高
dat2013dm$Country[53] # india
library(olsrr)
# DFFITS Plot
# cutoff: 2*sqrt(22/127) = 0.832
ols_plot_dffits(reg)

# leverage points (hii > 2*22/127=0.35): 
dat2013dm$Country[which(lm.influence(reg)$hat > 0.35)]
dat2013dm$continent[which(lm.influence(reg)$hat > 0.35)] # 分布於六大洲

# outlier detection
# studentized residual plot
# handyplots: threshold = +-3 (common)
par(mfrow = c(1,1))
library(handyplots)
library(MASS)
resplot(reg, highlight.outliers = TRUE, residuals = c("student"))
# Niger, Sierra Leone 
dat2013dm$Country[which(studres(reg) >= 3 | studres(reg) <= -3)] 
dat2013dm$continent[which(studres(reg) >= 3 | studres(reg) <= -3)] # 都是非洲國家

# remove outlier and influential point
# 顯著的變數: Income_composition_of_resources(+), HIV_AIDS(-), Total_expenditure(+), Adult_Mortality(-), Alcohol(-), Population(+), continent_North_America(+)
# continent_North_America 是正向顯著的變數，代表北美洲國家人民的預期壽命平均會比非洲人民多2歲。
tdat2013dm <- dat2013dm |> filter(Country != "Niger") |> filter(Country != "Sierra Leone") |> filter(Country != "India")
treg <- lm(Life_expectancy ~. -Year -Country -continent -continent_Africa , data = tdat2013dm)
summary(treg)

par(mfrow = c(2,2))
plot(treg)

# model diagnosis
## from test
### check normality: shapiro wilk test
shapiro.test(treg$residuals) # normal
### check heteroskedastic
bptest(treg) # no heteroscedasticity

# model selection
## reg 和 treg 在 stepwise 選到的變數相同
# 所有洲的變數都被刪除了
#ols_step_forward_p(reg)
#ols_step_backward_p(reg)
ols_step_both_p(reg)

#ols_step_forward_p(treg)
#ols_step_backward_p(treg)
ols_step_both_p(treg)

# adj. R_squared: 0.87
regstep <- lm(Life_expectancy ~ Income_composition_of_resources + Adult_Mortality + 
              HIV_AIDS + Total_expenditure + Population + under_five_deaths, data = dat2013dm)
summary(regstep)
# adj. R_squared: 0.89
# 有刪除outlier, leverage point 的模型，有更顯著的係數，係數變動方向合理。
tregstep <- lm(Life_expectancy ~ Income_composition_of_resources + Adult_Mortality + 
              HIV_AIDS + Total_expenditure + Population + under_five_deaths, data = tdat2013dm)
summary(tregstep)


# check multicollinearity
# no multicollinearity
library(olsrr)
ols_vif_tol(tregstep)


# the best full model : tregstep
# prediction in 2014
tdat2014 <- dat2014 |> filter(Country != "Niger") |> filter(Country != "Sierra Leone") |> filter(Country != "India")
b <- matrix(tregstep$coefficients, ncol = 1)
a <- rep(1,124)
mat2014 <- tdat2014 |> dplyr::select(Income_composition_of_resources, Adult_Mortality, 
                                          HIV_AIDS, Total_expenditure, Population, under_five_deaths) |>
                      mutate(a) |> relocate(a, .before = Income_composition_of_resources) |> as.matrix()
predictfull <- as.numeric(mat2014 %*% b) 
squared_error <- (predictfull - tdat2014$Life_expectancy)^2
dat2014_full <- tdat2014 |> dplyr::select(Country, continent, status, Life_expectancy) |> mutate(predictfull, squared_error)
pmse_full <- mean(squared_error)
pmse_full
# 平均預期壽命:70.8032 (觀測值: 70.95403)
mean(predictfull) 
mean(dat2014_full$Life_expectancy)

# 預測差距超過2歲的國家，有50個，其中只有8個是已開發國家(都在歐洲)。
par(mfrow = c(1,1))
barplot(abs(predictfull - tdat2014$Life_expectancy), ylim = c(0,12), ylab = "squared error", xlab = "countries", 
        main = c(paste( "Squared error in 2014"), paste("for all countries (stepwise)")))
mtext("PMSE = 8.53", side=3, col = "red", lwd = 2)
dat2014_full |> filter(squared_error > 4) |> group_by(continent, status) |> count()

# method 2: split models
# developed
# 不考慮洲變數: only one country is not Europe
# adj. R-squared = 0.78 
# 係數幾乎都不顯著，只有 population 顯著
dat2013edd <- dat2013ed |> dplyr::select(-c(Year, Country, continent, status, HIV_AIDS))
reged <- lm(Life_expectancy ~., data = dat2013edd)
summary(reged)

# from plots
par(mfrow = c(2,2))
plot(reged)

# model diagnosis: 沒有非常極端的點，又因為樣本數很有限，不考慮刪除觀測值
# from test
# check normality: shapiro wilk test
shapiro.test(reged$residuals) # normal
# check heteroskedastic
bptest(reged) # no  heteroscedasticity

# check influential point by cook's distance
par(mfrow = c(1,1))
plot(reged, which = 4) 
dat2013ed$Country[which(cooks.distance(reged) > 1)]
# DFFITS Plot
# cutoff: 2*sqrt(15/19) = 1.78
ols_plot_dffits(reged)

# leverage points (hii > 2*15/19=1.58)
# no leverage points
dat2013ed$Country[which(lm.influence(reged)$hat > 1.58)]
dat2013ed$continent[which(lm.influence(reged)$hat > 1.58)] 

# outlier detection
# studentized residual plot
# handyplots: threshold = +-3 (common)
# Belgium, Netherlands, Poland, Sweden
par(mfrow = c(1,1))
resplot(reged, highlight.outliers = TRUE, residuals = c("standard"))
dat2013ed$Country[which(studres(reged) >= 3 | studres(reged) <= -3)] 

# 資料筆數少，變數多，先選變數
# 從相關係數矩陣圖，看不出需要刪減的變數
corrplot(cor(dat2013edd), method = "color", tl.srt = 45, type = "lower", order = "hclust")
# variable selection
# 三種方式結果都不一致，且backward排除的係數是其他方法保留的係數，都先建迴歸模型觀察。
ols_step_forward_p(reged)
ols_step_backward_p(reged)
ols_step_both_p(reged)
# 觀察 R-squared:backward 模型表現最好，留下的變數也最多
summary(lm(Life_expectancy ~ thinness_1_19_years + Income_composition_of_resources + Diphtheria + Population + GDP, data = dat2013edd))
summary(lm(Life_expectancy ~. -Alcohol - thinness_1_19_years -Diphtheria, data = dat2013edd))
summary(lm(Life_expectancy ~ thinness_1_19_years + Income_composition_of_resources , data = dat2013edd))

# 顯著的變數: Polio(+), Total_expenditure(+), Population(+), Adult_Mortality(+), BMI(-), under_five_deaths(+)
# note: Adult_Mortality; under_five_deaths 的係數變動方向不太合理
# R-squared:0.8681 
treged <- lm(Life_expectancy ~. -Alcohol - thinness_1_19_years -Diphtheria, data = dat2013edd)
summary(treged)
par(mfrow = c(2,2))
plot(treged)

# model diagnosis from test
# check normality: shapiro wilk test
shapiro.test(treged$residuals) # normal
# check heteroskedastic
bptest(treged)

# check multicollinearity
# 可能有共線性問題，但因為這些變數在模型是顯著的，先預測觀察 PMSE 再考慮回來調整
ols_vif_tol(treged)

# the best model for developed countries : treged
# prediction in 2014
b <- matrix(treged$coefficients, ncol = 1)
a <- rep(1,19)
mat2014ed <- dat2013edd |> dplyr::select(-c(Life_expectancy, Alcohol, thinness_1_19_years, Diphtheria)) |>
  mutate(a) |> relocate(a, .before = Adult_Mortality) |> as.matrix()
predicted <- as.numeric(mat2014ed %*% b) 
squared_error <- (predicted - dat2013edd$Life_expectancy)^2
dat2014_ed <- dat2013ed |> dplyr::select(Country, continent, status, Life_expectancy) |> mutate(predicted, squared_error)
pmse_ed <- mean(squared_error) 
pmse_ed
# 平均預期壽命:80.3 (觀測值: 81.02)
mean(predicted)
mean(dat2014ed$Life_expectancy)

# 預測差距超過2歲的國家，只有義大利，義大利是 outlier。
par(mfrow = c(1,1))
barplot(abs(predicted - dat2013edd$Life_expectancy), ylim = c(0,12), ylab = "squared error", xlab = "countries", 
        main = c(paste( "Squared error in 2014"), paste("for developed countries (backward)")))
mtext("PMSE = 0.83", side=3, col = "red", lwd = 2)
dat2014_ed |> filter(squared_error > 4) |> dplyr::select(Country)


# developing
# 考慮continent間的差異
dat2013dming <- dat2013ing |> dummy_cols(select_columns = "continent") 
dat2013dming <- dplyr::rename(dat2013dming, continent_North_America = `continent_North America`)
dat2013dming <- dplyr::rename(dat2013dming, continent_South_America = `continent_South America`)

# let Africa as the base continent
# R-squared: 0.8383
# 顯著的變數: Income_composition_of_resources(+), HIV_AIDS(-), Alcohol(-), Total_expenditure(+),  Population(+)
# 洲變數的影響都不顯著
# 係數變動方向合理
reging <- lm(Life_expectancy ~. -Year -Country -continent -continent_Africa -status, data = dat2013dming)
summary(reging)

# model diagnosis
# from test
# check normality: shapiro wilk test
shapiro.test(reging$residuals) # normal
# check heteroskedastic
bptest(reging) # no  heteroscedasticity

# from plots
par(mfrow = c(2,2))
plot(reging)
# check influential point by cook's distance
par(mfrow = c(1,1))
plot(reging, which = 4) 
dat2013dming$Country[46] # india 
# DFFITS Plot
# cutoff: 2*sqrt(21/108) = 0.88
ols_plot_dffits(reging)

# leverage points (hii > 2*21/108=0.39)
# Canada, India, Lesotho, Montenegro, Nigeria
dat2013dming$Country[which(lm.influence(reging)$hat > 0.39)]
dat2013dming$continent[which(lm.influence(reging)$hat > 0.39)] # 分布於四大洲

# outlier detection
# studentized residual plot
# handyplots: threshold = +-3 (common)
resplot(reging, highlight.outliers = TRUE, residuals = c("student"))
# Niger, Sierra Leone
dat2013dming$Country[which(studres(reging) >= 3 | studres(reging) <= -3)] # 在 full model 也是 outliers
dat2013dming$continent[which(studres(reging) >= 3 | studres(reging) <= -3)] 

# remove outlier and influential point
# R-squared: 0.8804
# 顯著的變數: Income_composition_of_resources(+), Total_expenditure(+), HIV_AIDS(-), Population(+), Adult_Mortality(-), Alcohol(-), under_five_deaths(-)  
# 洲變數影響變顯著: continent_North_America(+), continent_South_America(+)
# 係數變動方向合理
tdat2013dming <- dat2013dming |> filter(Country != "Niger") |> filter(Country != "Sierra Leone") |> filter(Country != "India")
treging <- lm(Life_expectancy ~. -Year -Country -continent -continent_Africa -status, data = tdat2013dming)
summary(treging)
par(mfrow = c(2,2))
plot(treging)

# model diagnosis
# from test
# check normality: shapiro wilk test
shapiro.test(treging$residuals) # normal
# check heteroskedastic
bptest(treging) # no heteroscedasticity

# model selection
# 三種方式選到的變數不同
# 因為 backward 刪除的變數和 forward 留下的幾乎相對，所以被刪除的變數不考慮放入模型，選擇forward
# 以 forward 和 stepwise 為候選模型，發現兩者雖然變數個數相差很多，但模型表現類似，表示 stepwise 是比較精簡的模型；
# 然而，forward 多的變數仍然有顯著的，所以決定兩個都預測，比較 PMSE 。
# stepwise 在修正後模型選到的變數和 full model 的候選模型完全相同!!
ols_step_forward_p(reging) 
ols_step_backward_p(reging)
ols_step_both_p(reging)

ols_step_forward_p(treging)
ols_step_backward_p(treging)
ols_step_both_p(treging)

# R-squared: 0.8827
# 顯著的變數: Total_expenditure(+), HIV_AIDS(-), Income_composition_of_resources(+), Population(+), Adult_Mortality(-), Alcohol(-)
# 洲變數影響有顯著: continent_North_America(+)
# 係數變動方向合理
reging_f <- lm(Life_expectancy ~ Income_composition_of_resources + HIV_AIDS + Total_expenditure + 
                 Adult_Mortality + Population + under_five_deaths + 
                 GDP + Alcohol + Diphtheria + continent_North_America + continent_South_America + continent_Europe , data = tdat2013dming)
summary(reging_f)
par(mfrow = c(2,2))
plot(reging_f) 

# R-squared: 0.8767
# 顯著的變數: Total_expenditure(+), HIV_AIDS(-), Income_composition_of_resources(+), Population(+), Adult_Mortality(-), under_five_deaths(-)
# 係數變動方向合理
reging_s <- lm(Life_expectancy ~ Income_composition_of_resources + HIV_AIDS + Total_expenditure + 
                 Adult_Mortality + Population + under_five_deaths, data = tdat2013dming)
summary(reging_s)
par(mfrow = c(2,2))
plot(reging_s)

# check multicollinearity
# no multicollinearity
ols_vif_tol(reging_f) # 變數很多但沒有共線性問題
ols_vif_tol(reging_s) # 沒有共線性問題


# the best models for developing countries: reging_f and reging_s
# prediction in 2014
dat2014dming <- dat2014ing |> dummy_cols(select_columns = "continent") 
dat2014dming <- dplyr::rename(dat2014dming, continent_North_America = `continent_North America`)
dat2014dming <- dplyr::rename(dat2014dming, continent_South_America = `continent_South America`)
tdat2014dming <- dat2014dming |> filter(Country != "Niger") |> filter(Country != "Sierra Leone") |> filter(Country != "India")
a <- rep(1,105)
## prediction in 2014 (reging_f)
b <- matrix(reging_f$coefficients, ncol = 1)
mat2014ing_f <- tdat2014dming |> dplyr::select(Income_composition_of_resources, HIV_AIDS, Total_expenditure, 
                                               Adult_Mortality, Population, under_five_deaths, GDP, 
                                               Alcohol, Diphtheria, continent_North_America, continent_South_America, continent_Europe ) |>
  mutate(a) |> relocate(a, .before = Income_composition_of_resources) |> as.matrix()
predicting_f <- as.numeric(mat2014ing_f %*% b) 
squared_error_f <- (predicting_f - tdat2014dming$Life_expectancy)^2
dat2014_ing_f <- tdat2014dming |> dplyr::select(Country, continent, status, Life_expectancy) |> mutate(predicting_f, squared_error_f)
pmse_ing_f <- mean(squared_error_f) # 6.8
pmse_ing_f
# 平均預期壽命: 69.168 (觀測值: 68.8546)
mean(predicting_f)
mean(dat2014dming$Life_expectancy)

par(mfrow = c(1,1))
barplot(abs(predicting_f - tdat2014dming$Life_expectancy),  ylim = c(0,12), ylab = "squared error", xlab = "countries", 
        main = c(paste( "Squared error in 2014"), paste("for developing countries (forward)")))
mtext("PMSE = 6.81", side=3, col = "red", lwd = 2)
dat2014_ing_f |> filter(squared_error_f > 4) |> group_by(continent) |> count()

# prediction in 2014 (reging_s)
b <- matrix(reging_s$coefficients, ncol = 1)
mat2014ing_s <- tdat2014dming |> dplyr::select(Income_composition_of_resources, HIV_AIDS, Total_expenditure, 
                                               Adult_Mortality, Population, under_five_deaths) |>
  mutate(a) |> relocate(a, .before = Income_composition_of_resources) |> as.matrix()
predicting_s <- as.numeric(mat2014ing_s %*% b) 
squared_error_s <- (predicting_s - tdat2014dming$Life_expectancy)^2
dat2014_ing_s <- tdat2014dming |> dplyr::select(Country, continent, status, Life_expectancy) |> mutate(predicting_s, squared_error_s)
pmse_ing_s <- mean(squared_error_s)
pmse_ing_s
# PMSE 雖然比 forward 大，但是在平均預期壽命的預測卻比較準確
# 平均預期壽命: 69.11297 (觀測值: 68.8546)
mean(predicting_s)

par(mfrow = c(1,1))
barplot(abs(predicting_s - tdat2014dming$Life_expectancy),  ylim = c(0,12), ylab = "squared error", xlab = "countries", 
        main = c(paste( "Squared error in 2014"), paste("for developing countries (stepwise)")))
mtext("PMSE = 7.26", side=3, col = "red", lwd = 2)
dat2014_ing_s |> filter(squared_error_s > 4) |> group_by(continent) |> count()


## 七、結論與討論
# 1. 影響預期壽命在各種模型的重要變數意義:
# 分開預測的結果，PMSE都有比原本模型好一些，特別是已開發國家；開發中國家模型中，有加入洲相關變數對PMSE有些微改善，
# 在多變數迴歸模型，洲的變數對預測影響不大。
# 因為資料的已開發國家數比開發中國家少很多，所以full model 的預測結果，包含所選變數，會和 developing model 很相似。
# 資料的已開發國家多位於歐洲，從變數可以看出這地區主要影響預期壽命的因素和開發中國家是不太相同的，例如沒有 AIDS 但是有 BMI。

# 2. 變數係數的大小和顯著性詮釋:
# 模型重要變數係數正負向影響解釋，只有已開發國模型的部分變數不太合理 (比較表)
# 已開發國家模型有兩個變數的變動方向不太合理，雖然預測結果很好，但還需要從這些國家的背景歸納出合理的解釋。

# 3. 和文獻結果比較
# 預測準度:
# 圖
par(mfrow = c(2,2))
barplot(abs(predictfull - tdat2014$Life_expectancy), ylim = c(0,12), ylab = "squared error", xlab = "countries", 
        main = c(paste( "Squared error in 2014"), paste("for all countries (stepwise)")))
mtext("PMSE = 8.53", side=3, col = "red", lwd = 2)
barplot(abs(predicted - dat2013edd$Life_expectancy), ylim = c(0,12), ylab = "squared error", xlab = "countries", 
        main = c(paste( "Squared error in 2014"), paste("for developed countries (backward)")))
mtext("PMSE = 0.83", side=3, col = "red", lwd = 2)
barplot(abs(predicting_f - tdat2014dming$Life_expectancy),  ylim = c(0,12), ylab = "squared error", xlab = "countries", 
        main = c(paste( "Squared error in 2014"), paste("for developing countries (forward)")))
mtext("PMSE = 6.81", side=3, col = "red", lwd = 2)
barplot(abs(predicting_s - tdat2014dming$Life_expectancy),  ylim = c(0,12), ylab = "squared error", xlab = "countries", 
        main = c(paste( "Squared error in 2014"), paste("for developing countries (stepwise)")))
mtext("PMSE = 7.26", side=3, col = "red", lwd = 2)
# 表
# R-squared, PMSE 在文獻、報告比較表


## 八、可能改進的方向
# 1. 考慮長期的趨勢
# 開發中以肯亞為例，已開發以法國為例
## developing
Kenya <- data |> filter(Country == "Kenya")
par(mfrow = c(2,3))
plot(Kenya$Year, Kenya$Alcohol, pch = 16)
plot(Kenya$Year, Kenya$`Adult Mortality`, pch = 16)
plot(Kenya$Year, Kenya$under_five_deaths, pch = 16)
plot(Kenya$Year, Kenya$Population, pch = 16)
plot(Kenya$Year, Kenya$HIV_AIDS, pch = 16)
plot(Kenya$Year, Kenya$Income_composition_of_resources, pch = 16)      

## developed
France <- data |> filter(Country == "France")
par(mfrow = c(2,3))
plot(France$Year, France$Alcohol, pch = 16)
plot(France$Year, France$`Adult Mortality`, pch = 16)
plot(France$Year, France$GDP, pch = 16)
plot(France$Year, France$`Total expenditure`, pch = 16)
plot(France$Year, France$Polio, pch = 16)
plot(France$Year, France$Hepatitis_B, pch = 16)    

# 2. 現有資料缺乏性別資訊，日後應該要分別預測不同性別的預期壽命

