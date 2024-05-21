# 3章 怠惰学習-最近謗法を使った分類
if(!require(tidyverse)){
    install.packages('tidyverse', quiet = TRUE)
}
library(tidyverse)

## 3.1 最近謗法分類を理解する

### 3.1.1 k最近謗法
wbcd <- read.csv('https://raw.githubusercontent.com/dataspelunking/MLwR/master/Machine%20Learning%20with%20R%20(3rd%20Ed.)/Chapter03/wisc_bc_data.csv', stringsAsFactors = FALSE) %>% 
    data.frame()
str(wbcd)

# idカラムを削除
wbcd <- wbcd %>% select(- id)

# 良性・悪性の値をカウントする
table(wbcd$diagnosis)

# ラベルを付けてわかりやすくする
wbcd$diagnosis <- factor(
    wbcd$diagnosis, 
    levels = c('B', 'M'), 
    labels = c('Benign', 'Mailgnant')
    )

# 良性・悪性の割合を表示
wbcd$diagnosis %>% 
    table() %>% 
    prop.table() %>% 
    round(digits = 3) * 100

# 基本統計量（抜粋）
wbcd %>% 
    select(radius_mean, area_mean, smoothness_mean) %>% 
    summary()

# データの正規化（関数定義）
normarize <- function(x){
    return((x - min(x)) / (max(x) - min(x)))
}

# 関数テスト
normarize(1: 5)
normarize(1:5 * 10)

# データの正規化
wbcd_n <- 
    wbcd %>% 
    select(-diagnosis) %>% 
    lapply(normarize) %>% 
    as.data.frame()

# 正規化の結果確認
summary(wbcd_n$area_mean)

# 教師データ・検証データの分割
wbcd_train <- wbcd_n[1: 469, ]
wbcd_test <- wbcd_n[470: 569, ]

# 目的変数の格納
wbcd_train_labels <- wbcd[1:469, "diagnosis"]
wbcd_test_labels <- wbcd[470:569, "diagnosis"]

# モデルを訓練する
if(!require(class)){
    install.packages('class', quiet = TRUE)
}
library(class)

# 検証データの分類
wbcd_test_pred <- knn(
    train = wbcd_train,
    test = wbcd_test,
    cl = wbcd_train_labels,
    k = 21
)

# モデルの性能評価
if(!require(gmodels)){
    install.packages('gmodels', quiet = TRUE)
}
library(gmodels)

CrossTable(
    x = wbcd_test_labels,
    y = wbcd_test_pred,
    prop.chisq = FALSE
)

# 最適なｋを求めてみる
# packages <- c(
#     'cluster',
#     'ggplot2'
# )
# 
# for(package in packages){
#     if(!require(package, character.only = TRUE)){
#         install.packages(package, quiet = TRUE)
#     }
#     library(package, character.only = TRUE)
# }
# 
# set.seed(71)
# 
# # 最大クラスタ数
# k_max <- 47
# 
# 
# # Ward法を利用した各データのクラスタ番号を返す関数定義
# get_cluster_id <- function(x, k){
# 
#     # データ感距離の算出
#     dist_x <- dist(x)
#     # Ward法による階層的クラスタリング
#     hc_x <- hclust(dist_x, method = 'single')
#     # 各データのクラスタ番号の算出
#     ct_x <- cutree(hc_x, k = k)
#     
#     list(cluster = ct_x)
# }
# 
# # ギャップ統計量の算出
# gap_ward_wbcd <- clusGap(
#     wbcd_train, 
#     FUNcluster = get_cluster_id, 
#     K.max = k_max
#     )
# 
# gap_ward_wbcd

# モデルの性能を向上させる

# zスコア要順化
 wbcd_z <- 
     wbcd %>% 
     select(-diagnosis) %>% 
     scale() %>% 
     as.data.frame()

 summary(wbcd_z)

 # 教師データ・検証データの分割
 wbcd_train <- wbcd_z[1: 469, ]
 wbcd_test <- wbcd_z[470: 569, ]
 
# 目的変数の格納
wbcd_train_labels <- wbcd[1:469, "diagnosis"]
wbcd_test_labels <- wbcd[470:569, "diagnosis"]

# 予測
wbcd_test_pred <- knn(
    train = wbcd_train,
    test = wbcd_test,
    cl = wbcd_train_labels,
    k = 21
)

# 評価
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
