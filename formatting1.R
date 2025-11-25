# 全員回答のデータの整形

library(readxl)
# df <- read_excel("data/exceldata_158_1109.xlsx", sheet = "sheet1_1_全員回答")
df <- read_excel("data/exceldata_170_1119.xlsx", sheet = "Sheet1")

View(df)

# 整形
## ダミー変数化
df$`コンシューマーゲームで遊びますか。` <- as.numeric(df$`コンシューマーゲームで遊びますか。` == "はい")
df$`PCゲームで遊びますか。` <- as.numeric(df$`PCゲームで遊びますか。` == "はい")
df$`ゲームセンターのアーケードゲームで遊びますか。` <- as.numeric(df$`ゲームセンターのアーケードゲームで遊びますか。` == "はい")

## 数値化
stress_map <- c("全く感じない", "少し感じる", "強く感じる")
df$日常のストレス強度 <- factor(df$日常のストレス強度, levels = stress_map)
df$日常のストレス強度 <- as.numeric(df$日常のストレス強度) - 1

frequency_map <- c("選ばない", "あまり選ばない", "たまに選ぶ", "頻繁に選ぶ")
df$スポーツをする <- as.numeric(
  factor(df$スポーツをする, levels = frequency_map)
)-1

df$散歩 <- as.numeric(
  factor(df$散歩, levels = frequency_map)
)-1

df$ゲーム <- as.numeric(
  factor(df$ゲーム, levels = frequency_map)
)-1

df$`サウナ・整体` <- as.numeric(
  factor(df$`サウナ・整体`, levels = frequency_map)
)-1

df$映画鑑賞 <- as.numeric(
  factor(df$映画鑑賞, levels = frequency_map)
)-1

df$友人や家族のコミュニケーション <- as.numeric(
  factor(df$友人や家族のコミュニケーション, levels = frequency_map)
)-1

df$ショッピング <- as.numeric(
  factor(df$ショッピング, levels = frequency_map)
)-1

df$賭け事 <- as.numeric(
  factor(df$賭け事, levels = frequency_map)
)-1

df$カラオケ <- as.numeric(
  factor(df$カラオケ, levels = frequency_map)
)-1

df$睡眠 <- as.numeric(
  factor(df$睡眠, levels = frequency_map)
)-1

# dplyrパッケージをロード
library(dplyr)

#「SNSを見る」から「その他アウトドアの趣味」までの全ての列に対して「- 1」の操作を適用
df <- df %>%
  mutate(
    across(
      # 列の範囲を「開始列名:終了列名」で指定
      Game_SP:Hobby_Outdoor, 
      # 適用する関数を指定 (現在の列の値から1を引く)
      ~ .x - 1
    )
  )


## スマホゲームで遊ぶは1、それ以外は0
df$`あなたはスマートフォンゲームで遊びますか。` <- as.numeric(df$`あなたはスマートフォンゲームで遊びますか。` == "遊ぶ")

# パッケージのインストール (初回のみ)
# install.packages("writexl")
# 出力
library(writexl)
write_xlsx(df, "data/data_4.xlsx")
