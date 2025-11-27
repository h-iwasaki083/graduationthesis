library(readxl)
exceldata <- read_excel("data/exceldata_170_1119.xlsx", sheet = "Sheet1")
View(exceldata)

X <- exceldata[, c("Game_SP","SNS_Chat","SNS_View","SNS_Post",
            "Watch_Media","Reading","Study_SP","Research","Shop_SP")]

# 固有値の確認（因子数の決定）
eigen_values <- eigen(cor(X))$values
eigen_values

library(psych)


# 抽出方法: "minres" (残差最小法) 
# 因子数: nfactors=3 (3因子を抽出)
# 回転方法: rotate="varimax" (直交回転のバリマックス)
fa_result <- fa(r = X,
                nfactors = 3, 
                fm = "minres", 
                rotate = "varimax")

# 結果の表示
fa_result

# 詳細な結果の表示（負荷量のグラフ化）
# cut (負荷量の表示しきい値)
fa.diagram(fa_result, cut = 0.3)

# 因子得点
factor_scores <- fa_result$scores

# 因子負荷量のヒートマップ
# 必要なパッケージ
library(ggplot2)
library(reshape2)

# 因子負荷量を抽出（loadings を数値行列として取り出す）
loadings <- as.data.frame(unclass(fa_result$loadings))

# 因子（列）を指定（MR1〜MR3を使用）
loadings <- loadings[, 1:3]

# 項目名を列に追加
loadings$item <- rownames(loadings)

# long形式に変換（ggplot用）
df_plot <- melt(loadings, id = "item")

# ヒートマップ描画
ggplot(df_plot, aes(x = variable, y = item, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "因子負荷量ヒートマップ",
    fill = "負荷量"
  )


# クラスター分析


