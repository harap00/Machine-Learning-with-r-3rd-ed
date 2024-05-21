# 2 データを管理し、理解する

## 2.1 Rのデータ構造

### 2.1.1 ベクトル

install.packages('tidyverse')
library('tidyverse')

subject_name <- c('John Doe', 'Jane Doe', 'Steve Graves')
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

temperature[2]
temperature[2:3]
temperature[-2]
temperature[c(TRUE, TRUE, FALSE)]


### 2.1.2 因子

gender <- factor(c('MALE', 'FEMALE', 'MALE'))
gender

blood <- factor(c('O', 'AB', 'A'), levels = c('A', 'B', 'AB', 'O'))
blood

symptoms <- factor(c('SEVERE', 'MILD', 'MODERATE'), 
                   levels = c('MILD', 'MODERATE', 'SEVERE'),
                   ordered = TRUE)
symptoms

symptoms > 'MODERATE'


### 2.1.3 リスト

subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]

subject1 <- list(fullname = subject_name[1],
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1],
                 symptoms = symptoms[1])

subject1

subject1[2]
subject1[[2]]
subject1$temperature
subject1[c('temperature', 'flu_status')]


### 2.1.4 データフレーム

pt_data <- data.frame(subject_name, temperature, flu_status, gender,
                      blood, symptoms, stringsAsFactors = FALSE)
pt_data

pt_data$subject_name

pt_data[c('temperature', 'flu_status')]

pt_data[1, 2]
pt_data[c(1, 3), c(2, 4)]
pt_data[, 1]
pt_data[1, ]
pt_data[,]
pt_data[c(1, 3), c('temperature', 'gender')]
pt_data[-2, c(-1, -3, -5, -6)]

pt_data$temp_c <- (pt_data$temperature -32) * (5 / 9)
pt_data[c('temperature', 'temp_c')]


### 2.1.5 行列と配列

 m <- matrix(c(1, 2, 3, 4), nrow = 2)
 m
 
 m <- matrix(c(1, 2, 3, 4), ncol = 2)
 m

 m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2) 
 m 
 
 m <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2) 
 m 

 m[1, ] 
 m[, 1] 

 
## 2.2 Rでのデータ管理

 
### 2.2.1 Rのデータ構造の保存、読み込み、削除
 
x <- 1
y <- 2
z <- 3
 
save(x, y, z, file = 'mydata.RData') 

x <- 0
y <- 0
z <- 0

x
y
z


load('mydata.RData')

#ls()
#rm(m, subject1)
#ls()

#rm(list = ls())
#ls()


### CSVファイルを使ったデータの保存と読み込み
write.csv(pt_data, 'pt_data.csv', row.names = FALSE)

pt_data <- read.csv('pt_data.csv', stringsAsFactors = FALSE)
pt_data

mydata <- data.frame(
    V1 = c('user1', 'user2', 'user3'),
    V2 = c('MALE', 'FEMALE', 'MALE'),
    V3 = c('LEFT', 'RIGHT', 'RIGHT'),
    V4 = c(TRUE, FALSE, TRUE)
    )

write.csv(mydata, file = 'mydata.csv', row.names = FALSE)
mydata <- 
    read.csv('mydata.csv', stringsAsFactors = FALSE, header = TRUE)

mydata

## 2.3 データを調べて理解する
# usedcars <- read.csv('usedcars.csv', stringsAsFactors = FALSE)

usedcars <- read.csv(
    "http://users.ecs.soton.ac.uk/mb8/Rtutorial/usedcars.csv", 
    stringsAsFactors = FALSE
    )


### 2.3.1 データの構造を調べる

str(usedcars)


### 2.3.2 数値の特徴量を調べる

summary(usedcars$year)

summary(usedcars[c('price', 'mileage')])

(36000 + 44000 + 56000) / 3

mean(c(36000, 44000, 56000))

median(c(36000, 44000, 56000))

range(usedcars$price)

diff(range(usedcars$price))

IQR(usedcars$price)

quantile(usedcars$price)

quantile(usedcars$price, probs = c(0.01, 0.99))

quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

boxplot(usedcars$price,
        main = 'Boxplot of Used Car Prices', ylab = 'Price($)')

boxplot(usedcars$mileage,
        main = 'Boxplot of Used Car Millage', 
        ylab = 'Odometer(mi.)')


hist(usedcars$price,
     main = 'Histogram of Used Car Prices', xlab = 'Price($)')
hist(usedcars$mileage,
     main = 'Histogram of Used Car Mileage', xlab = 'Odometer(mi.)')


var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)


### 2.3.3 カテゴリ地の特徴量を調べる

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

model_table <- table(usedcars$model)
prop.table(model_table)

color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

### 2.3.4 特徴量どうしの関係を調べる

plot(
    x = usedcars$mileage, 
    y = usedcars$price,
    main =  'Scatterplot of Price vs. Mileage',
    xlab = 'Used Car Odometer(mi.)',
    ylab = 'Used Car Price($)'
)


if(!require(gmodels, character.only = TRUE)){
    install.packages('gmodels', quiet = TRUE)
    library(gmodels, character.only = TRUE)
}

usedcars$conservatibe <- 
    usedcars$color %in% c('Black', 'Gray', 'Silver', 'White')

table(usedcars$conservatibe)

gmodels::CrossTable(x = usedcars$model, y = usedcars$conservatibe, chisq = TRUE)

