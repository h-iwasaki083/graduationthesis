import pandas as pd
from factor_analyzer import FactorAnalyzer

# CSVファイルの読み込み
df = pd.read_csv("data/testdata2.csv")

# 欠損値の削除
df = df.dropna()

# 因子分析（例：3因子を抽出）
fa = FactorAnalyzer(n_factors=3, rotation="varimax")
fa.fit(df)

# 因子負荷量を表示
loadings = pd.DataFrame(
    fa.loadings_, index=df.columns, columns=[f"Factor{i+1}" for i in range(3)]
)
print(loadings)

# 固有値を表示（因子数の判断に使える）
ev, v = fa.get_eigenvalues()
print("固有値：", ev)
