# formatting2_Play_SP_Game_formatting

library(readxl)
exceldata <- read_excel("data/exceldata_158_1109.xlsx", sheet = "sheet1_2_SPGame")
View(exceldata)

# ゲームで遊ぶ人の回答を抽出
df <- exceldata[exceldata[, 5]=="遊ぶ", -5]

# ダミー変数化
df$Purchased_Paid_SPG <- as.numeric(df$Purchased_Paid_SPG == "ある")
df$Made_InApp_Purchase <- as.numeric(df$Made_InApp_Purchase == "ある")
df$Impact_Oshi_Character <- as.numeric(df$Impact_Oshi_Character == "いる")
df$Play_on_Tablet <- as.numeric(df$Play_on_Tablet == "遊ぶ")
df$Play_Console <- as.numeric(df$Play_Console == "はい")
df$Play_PC <- as.numeric(df$Play_PC == "はい")
df$Play_Arcade <- as.numeric(df$Play_Arcade == "はい")


# 数値化
played_map <- c("遊んだことがない", "少し遊んだことがある", "頻繁に遊んでいた")

df$Played_Before_Console <- as.numeric(
  factor(df$Played_Before_Console, levels = played_map)
)-1

df$Played_Before_PC <- as.numeric(
  factor(df$Played_Before_PC, levels = played_map)
)-1

df$Played_Before_Arcade <- as.numeric(
  factor(df$Played_Before_Arcade, levels = played_map)
)-1


library(dplyr)
situ_frequency_map <- c("全くしない", "たまにする", "頻繁にする")
df <- df %>%
  mutate(
    across(
      .cols = c(14:17),
      .fns = ~ as.numeric(factor(., levels = situ_frequency_map)) - 1
    )
  )


importance_map <- c("全く思わない", "あまり思わない", "思う", "とても思う")
df <- df %>%
  mutate(
    across(
      .cols = c(20:37),
      .fns = ~ as.numeric(factor(., levels = importance_map)) - 1
    )
  )

# 出力
library(writexl)
write_xlsx(df, "data/data_2.xlsx")
