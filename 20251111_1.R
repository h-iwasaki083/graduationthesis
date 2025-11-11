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
