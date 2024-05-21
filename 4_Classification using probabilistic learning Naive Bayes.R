# 4章 確率論的学習ーナイーブベイズを使った分類

## 4.1 ナイーブベイズを理解する

### 4.1.2 ナイーブベイズ法
# コードなし

## 4.2 ナイーブベイズを使ってSMSスパムをフィルタリングする

# データの読み込み
sms_raw <- read.csv('https://raw.githubusercontent.com/dataspelunking/MLwR/master/Machine%20Learning%20with%20R%20(3rd%20Ed.)/Chapter04/sms_spam.csv',stringsAsFactors = FALSE, header = TRUE)

# データの確認
str(sms_raw)

# typeを因子に変換
sms_raw$type <- factor(sms_raw$type, levels = c('ham', 'spam'))
str(sms_raw$type)
table(sms_raw$type)

# データの前処理：テキストデータのクリーニングと標準化
# ライブラリの読み込み
if(!require(tm)){
    install.packages('tm', quiet = TRUE)
}
library(tm)

# コーパスの作成
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)

# コーパスの要約
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])

lapply(sms_corpus[1:2], as.character)

# コーパスのクリーニング
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# クリーニング結果の確認
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

# 数字の削除
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

# tmに含まれている関数の一覧を表示
getTransformations()

# ストップワードのリストを表示
stopwords()

# ストップワードをクリーニング
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
# 句読点のクリーニング
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

# removePunctuationの動作確認
removePunctuation('hello...world')

# 句読点を置き換える関数（参考）
replacePunctuation <- function(x){
    gsub('[[:punct:]]+', ' ', x)
}

# ステミング（単数・現在形）変換
if (!require(SnowballC)) {
    install.packages('SnowballC', quiet = TRUE)
}
library(SnowballC)


# ステミング動作の確認
wordStem(c('learn', 'learned', 'learning', 'learns'))
# ステミングの実施
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# ステミング後の空白の削除
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# クリーニングの確認
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

# データの前処理：テキスト文章を単語に分割する
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# これまでの操作の一括処理（別解）
sms_dtm2 <- DocumentTermMatrix(
    sms_corpus,
    control = list(
        tolower = TRUE,
        removeNumbers = TRUE,
        stopwords = TRUE,
        # stopwords = function(x){removeWords(x, stopwords())} # 結果を完全一致させる場合はこちら
        removePunctuation = TRUE,
        stemming = TRUE
    )
)

# sms_dtmとsms_dtm2の比較
sms_dtm
sms_dtm2

# 教師データ・検証データの分割
sms_dtm_train <- sms_dtm[1: 4169, ]
sms_dtm_test <- sms_dtm[4170: 5559, ]

# ラベルの取り出し
sms_train_labels <- sms_raw[rownames(sms_dtm_train), ]$type
sms_test_labels <- sms_raw[rownames(sms_dtm_test), ]$type

# 教師データ・検証データのspam割合の比較
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))


# ワードクラウド
if(! require(wordcloud)){
    install.packages('wordcloud', quiet = TRUE)
}
library(wordcloud)

wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

# spam/hamのワードクラウド
spam <- subset(sms_raw, type == 'spam')
ham <- subset(sms_raw, type == 'ham')

# ２つのプロットエリアを用意し描画、その後リセット
par(mfrow = c(1, 2))
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5), random.order = FALSE)
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5), random.order = FALSE)

# プロットエリアをもとに戻す
par(mfrow = c(1, 1))

# データの前処理：頻出語を表す特徴量を作成する
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

# モデル学習用の教師データ/検証データを作成する
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# 出現語句をカウントする関数
convert_counts <- function(x){
    x <- ifelse(x > 0, 'Yes', 'No')
}

# 語句のカウント
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, FUN = convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, FUN = convert_counts)

# モデルを訓練する
if(! require(e1071)){
    install.packages('e1071', quiet = TRUE)
}
library(e1071)

# 推定器の訓練
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

# モデルの性能評価
sms_test_pred <- predict(sms_classifier, sms_test)

if(! require(gmodels)){
    install.packages('gmodels', quiet = TRUE)
}
library(gmodels)

CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# モデルの性能を向上させる
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


if(! require(tidyverse)){
    install.packages('tidyverse', quiet = TRUE)
}
library(tidyverse)

print(sms_dtm_freq_train)
