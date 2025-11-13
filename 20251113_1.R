# 20251113_1_Play_SP_Game_魅力の主成分分析

library(readxl)
exceldata_2 <- read_excel("data/data_2.xlsx", sheet = "Sheet1")
View(exceldata_2)


df <- exceldata_2[24:37]

# 主成分分析
## result <- prcomp(df, scale = TRUE)


# 因子分析もやってみる
# パッケージの読み込み
# install.packages("psych")
library(psych)

# 主成分法 (fm="pa" - Principal Axis Factor Analysisもよく使われます)
# 抽出方法: "pc" (主成分分析) 
# 因子数: nfactors=4 (4因子を抽出)
# 回転方法: rotate="varimax" (直交回転のバリマックス)
fa_result <- fa(r = df, 
                nfactors = 4, 
                fm = "pc", 
                rotate = "varimax")

# 結果の表示
fa_result

# 詳細な結果の表示（負荷量のグラフ化）
# cut (負荷量の表示しきい値)
fa.diagram(fa_result, cut = 0.3)

# 因子得点
factor_scores <- fa_result$scores

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
final_k <- 4
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


cluster_assignment <- kmeans_final$cluster
payment_vector <- exceldata_2[, 19]
# 3. 2つのベクトルを新しいデータフレームとして結合
# 行数（77）が一致していれば、この方法は最も確実です。
combined_data_final <- data.frame(
  Cluster = cluster_assignment,
  Payment_Flag = payment_vector
)

print("--- クラスターごとの課金率 ---")
print("--- クラスターごとの課金率 ---")
utilization_rate <- aggregate(
  x = combined_data_final$Made_InApp_Purchase,  # 課金フラグのベクトル
  by = list(Cluster = combined_data_final$Cluster), # クラスター番号のベクトルでグループ化
  FUN = mean # 平均（＝課金率）を計算
)
print(utilization_rate)



# combined_data_final の Play_Time_Category 列を使ってクロス集計と検定を実行
exceldata_2 <- cbind(exceldata_2, cluster_assignment)

print("--- クラスターとプレイ時間のクロス集計表 ---")
# 1. クロス集計表（分割表）の作成
# 行: クラスター (4水準), 列: プレイ時間カテゴリ (N水準)
contingency_table_time <- table(exceldata_2$cluster_assignment, 
                                exceldata_2$Avg_Hours_SP_Weekend)
print(contingency_table_time)

# カイ二乗検定的にはだめそうだけど、実数値は傾向ありそうだから使っちゃう
# 2. カイ二乗検定の実行
chi_sq_test_time <- chisq.test(contingency_table_time)
print("--- カイ二乗検定の結果 ---")
print(chi_sq_test_time)
