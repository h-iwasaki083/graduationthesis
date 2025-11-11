# 20251111_1_全員回答_余暇時間の主成分分析 / ロジスティック回帰

# 必要なパッケージのロード（通常はbase Rの機能で十分です）
# library(stats)

# データの読み込み
library(readxl)
data_1 <- read_excel("data/data_1.xlsx")
View(data_1)


# 活動頻度の変数名リスト（Rでの列名として使用）
activity_cols <- c("Side_Job", "Housework_Childcare", "Study", 
                   "GoOut_Solo", "GoOut_Group", "Watch_TV", 
                   "Sports", "Hobby_Indoor", "Hobby_Outdoor")

# 目的変数（スマホゲーム: 0/1）の変数名
target_col <- "Play_SP_Game"

# 主成分分析に使用するデータフレーム (説明変数X)
X <- data_1[, activity_cols]
# 目的変数Y
Y <- data_1[[target_col]]

# -----------------
# 主成分分析 (PCA) の実行
# -----------------
# データを標準化（scale = TRUE）してからPCAを実行
pca_result <- prcomp(X, scale = TRUE)

# 結果の確認（寄与率、累積寄与率）
print("--- 主成分分析 (PCA) の結果概要 ---")
summary(pca_result) 

# PC1-5のスコア（新しい説明変数）を取得
pc_scores <- as.data.frame(pca_result$x[, 1:5])
colnames(pc_scores) <- c("PC1", "PC2", "PC3", "PC4", "PC5") # 列名を変更

# 主成分負荷量の確認 (PC1, PC2, PC3の解釈に必要)
print("--- 主成分負荷量 (Rotation: PC1, PC2, PC3, PC4, PC5) ---")
print(pca_result$rotation[, 1:5])


# 第1主成分負荷量と第2主成分負荷量の散布図の作成
plot(pca_result$rotation[, 1], pca_result$rotation[, 2], type = "n",
     xlim = c(-1, 1), ylim = c(-1, 1), xlab = "第1主成分負荷量", ylab = "第2主成分負荷量")
text(pca_result$rotation[, 1], pca_result$rotation[, 2], rownames(pca_result$rotation))

# -----------------
# ロジスティック回帰
# -----------------

# PC1, PC2, PC3, PC4, PC5 のスコアを取得
pc_scores <- as.data.frame(pca_result$x[, 1:5])
colnames(pc_scores) <- c("PC1", "PC2", "PC3", "PC4", "PC5") # 列名をPC1〜PC5に変更

# 主成分スコアと目的変数Yを結合したデータフレームを作成
regression_data <- cbind(pc_scores, Play_SP_Game = Y)

# ロジスティック回帰モデルの構築 (PC1からPC5を説明変数に)
model <- glm(Play_SP_Game ~ PC1 + PC2 + PC3 + PC4 + PC5, 
             data = regression_data, 
             family = binomial)

# モデルの要約を出力
print("--- ロジスティック回帰分析の結果 ---")
summary(model)

# オッズ比と95%信頼区間の計算と表示
print("--- オッズ比と95%信頼区間 ---")
# オッズ比
odds_ratios <- exp(coef(model))
# 95%信頼区間 (confint.defaultを使用することで計算速度が速くなります)
conf_intervals <- exp(confint.default(model)) 

# 結果を見やすい表形式で結合
results_table <- data.frame(
  Coefficient = coef(model),
  Odds_Ratio = odds_ratios,
  `95% CI Lower` = conf_intervals[, 1],
  `95% CI Upper` = conf_intervals[, 2]
)
print(results_table)


# -----------------
# クラスター分析
# -----------------
# ⚠️ 前提: data_1 がインポートされており、pca_resultにPCA結果が格納されていること
# ⚠️ 主成分スコア（PC1からPC5）を data_1 に結合するため、pc_scoresを取得し直します。

# 主成分スコア（PC1からPC5）をデータフレームとして取得
# pc_scores <- as.data.frame(pca_result$x[, 1:5]) 
# colnames(pc_scores) <- c("PC1", "PC2", "PC3", "PC4", "PC5") 

# data_1 に主成分スコアを追加
data_1 <- cbind(data_1, pc_scores)

# K-meansクラスタリングの実行 (ここではK=3に設定)
k <- 3 # クラスター数
set.seed(42) # 結果の再現性を確保
kmeans_result <- kmeans(data_1[, c("PC1", "PC2", "PC3", "PC4", "PC5")], 
                        centers = k, 
                        nstart = 25)

# クラスターの割り当てを data_1 に追加
data_1$cluster <- as.factor(kmeans_result$cluster) 

# クラスターごとのサンプルサイズを確認
print("--- クラスターごとのサンプルサイズ ---")
table(data_1$cluster)

# 2. クラスターの特徴付け (プロファイルの作成)
# クラスターごとの平均値の計算（元の変数）
# aggregate 関数で、clusterごとに余暇活動の平均値を計算
print("--- クラスターごとの余暇活動 (元の変数) 平均値 ---")
cluster_profile_orig <- aggregate(data_1[, activity_cols], 
                                  by = list(Cluster = data_1$cluster), 
                                  FUN = mean)
print(cluster_profile_orig)

# クラスターごとの平均値の計算（主成分スコア）
# クラスタータイプをPCのスコア軸から確認
print("--- クラスターごとの主成分スコア平均値 ---")
cluster_profile_pc <- aggregate(data_1[, c("PC1", "PC2", "PC3", "PC4", "PC5")], 
                                by = list(Cluster = data_1$cluster), 
                                FUN = mean)
print(cluster_profile_pc)

# 3. スマホゲーム利用率の比較と検定
# 目的変数の変数名
target_col <- "Play_SP_Game"

# クラスターごとのスマホゲーム利用率の計算 (0/1データの平均値が利用率になります)
print("--- クラスターごとのスマホゲーム利用率 ---")
utilization_rate <- aggregate(data_1[[target_col]], 
                              by = list(Cluster = data_1$cluster), 
                              FUN = mean)
colnames(utilization_rate) <- c("Cluster", "Play_SP_Game_Rate")
print(utilization_rate)

# 統計的有意差の検定 (ANOVA)
# 利用率に統計的な差があるかを確認します。p値が0.05未満なら有意な差あり。
print("--- クラスター間の利用率の統計的検定 (ANOVA) ---")
aov_result <- aov(data_1[[target_col]] ~ cluster, data = data_1)
summary(aov_result)
