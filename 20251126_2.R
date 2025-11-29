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

# --- 最適なクラスター数の決定（エルボー法） ---
# Kの数に応じたWSS (Within-Cluster Sum of Squares)を格納するベクトル
wss <- numeric(15)

# K=1からK=15までK-meansを実行し、WSSを計算
for (k in 1:15) {
  # nstart=25 は、初期値のランダムな選び方による影響を減らすため、25回繰り返す設定
  kmeans_result <- kmeans(factor_scores, centers = k, nstart = 25)
  wss[k] <- kmeans_result$tot.withinss
}

# エルボー法のプロット
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters (K)", 
     ylab = "Within-Cluster Sum of Squares (WSS)", 
     main = "Elbow Method for Optimal K")
# [Image of Elbow method graph for K-means clustering]
## 3か4かなぁ

# K=3でK-meansを実行 (最適なKはエルボー法の結果に基づいて変更してください)
final_k <- 3
set.seed(123) # 結果の再現性を確保するためにシードを設定
kmeans_final <- kmeans(factor_scores, centers = final_k, nstart = 25)

# --- 結果の統合 ---
# 1. 因子得点データにクラスターの割り当てを追加
factor_scores_with_cluster <- data.frame(factor_scores, 
                                         Cluster = kmeans_final$cluster)

# 2. 各クラスターの中心（特徴）を抽出
# これが各クラスターのプロファイルを表します
cluster_centers <- kmeans_final$centers
print("--- Cluster Centers (各クラスターの平均因子得点) ---")
print(cluster_centers)

# 3. 各クラスターのサイズ（人数）を確認
print("--- Cluster Sizes (各クラスターの人数) ---")
print(kmeans_final$size)

library(plotly)

# 因子スコア + クラスター列が入っている前提
df_temp <- as.data.frame(factor_scores[, 1:3])
df_temp$Cluster <- as.factor(kmeans_final$cluster)
factor_scores_with_cluster <- df_temp
factor_scores_with_cluster <- data.frame(factor_scores[,1:3], 
                                         Cluster = kmeans_final$cluster)

fig <- plot_ly(df_temp, 
               x = ~MR1, y = ~MR2, z = ~MR3,
               color = ~factor(Cluster),          # クラスターで色分け
               colors = c("red","blue","green"),  # 色を3クラスタに対応
               type = "scatter3d", mode = "markers",
               marker = list(size = 5))           # マーカーは全部同じ

fig <- fig %>% layout(scene = list(xaxis = list(title = 'MR1'),
                                   yaxis = list(title = 'MR2'),
                                   zaxis = list(title = 'MR3')),
                      legend = list(title = list(text='Cluster')))

fig

# クロス集計

contingency_table_time <- table(exceldata$Play_SP_Game, 
                                kmeans_final$cluster)
print(contingency_table_time)

contingency_table_time_2 <- table(exceldata$Play_SP_Game, 
                                kmeans_final$cluster)
print(contingency_table_time)


chi_sq_test_time <- chisq.test(contingency_table_time)
print("--- カイ二乗検定の結果 ---")
print(chi_sq_test_time)
