# 全員回答データの分析1_スマホ利用の主成分分析

library(readxl)
data_1 <- read_excel("data/data_1.xlsx", 
                     sheet = "Sheet1")
View(data_1)

data <- data_1[11:19]

Result <- prcomp(data, scale = TRUE)	# 相関係数行列による主成分分析を実行し，結果をResultに格納
Result		# 各主成分の固有値のルート(=相関係数)と，各主成分係数(Rotation)の表示
Result$sdev^2	# 固有値の計算
summary(Result)		# 寄与率(Proportion of Variance)，累積寄与率の表示

Loadings <- cor(data, Result$x)	# 主成分負荷量
Result$x	# 主成分得点の表示

cov(Result$x)	# 各主成分の分散が固有値に一致すること，異なる主成分間が無相関であることの確認

# 第1主成分負荷量と第2主成分負荷量の散布図の作成
plot(Loadings[, 1], Loadings[, 2], type = "n",
     xlim = c(-1, 1), ylim = c(-1, 1), xlab = "第1主成分負荷量", ylab = "第2主成分負荷量")
text(Loadings[, 1], Loadings[, 2], rownames(Loadings))

# 第3主成分負荷量と第4主成分負荷量の散布図の作成
plot(Loadings[, 3], Loadings[, 4], type = "n",
     xlim = c(-1, 1), ylim = c(-1, 1), xlab = "第3主成分負荷量", ylab = "第4主成分負荷量")
text(Loadings[, 3], Loadings[, 4], rownames(Loadings))


# クラスタリング
## 第1〜第4主成分の得点を取り出す
pca_scores <- Result$x[, 1:4]

set.seed(123)  # 再現性のため
clusters <- kmeans(pca_scores, centers = 3)

# 結果を確認
table(clusters$cluster)

## クラスター数の決定
wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(pca_scores, centers = k, nstart = 10)$withinss)
}

plot(1:10, wss, type = "b",
     xlab = "クラスター数 k",
     ylab = "クラスタ内分散 (WSS)",
     main = "エルボー法による最適クラスタ数の確認")
## 3で良さそう


library(ggplot2)

df_plot <- data.frame(pca_scores[, 1:2],
                      cluster = as.factor(clusters$cluster))

ggplot(df_plot, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "スマホ利用傾向のクラスタリング（PC1×PC2）",
       x = "第1主成分（利用全般の多さ）",
       y = "第2主成分（SNS中心 vs 鑑賞・読書中心）") +
  theme_minimal()
## きれいに出てきた

df_plot <- data.frame(pca_scores[, 3:4],
                      cluster = as.factor(clusters$cluster))

ggplot(df_plot, aes(x = PC3, y = PC4, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "スマホ利用傾向のクラスタリング（PC3×PC4）",
       x = "第3主成分（能動的学習・娯楽 vs 受動的鑑賞）",
       y = "第4主成分（娯楽・遊び型 vs 学習・生産型）") +
  theme_minimal()
## あんまきれいに出てこない笑

## 各クラスタでどの主成分が強いのか？
aggregate(pca_scores, by=list(cluster=clusters$cluster), mean)
## きれいに出てきた


# スマホゲームする / しないの線形判別分析

