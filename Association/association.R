#
# 参考: http://qiita.com/uchim/items/8a532945a1fb2f068b5a
#

### スーパーマーケットの買い物かごをアソシエーション分析してみる
# アソシエーション分析のライブラリ(注2)を読み込む
library(arules)
# データが格納されたファイルの指定
posData <- "posdata.csv"
# スーパーマーケットの買い物かごのデータをトランザクションとして読み込む
tranPosData <- read.transactions(posData, sep=",", rm.duplicates=TRUE)
# apriori ファンクションを実行する
aprioriTranPosData <- apriori(tranPosData, parameter=list(supp=0.02, maxlen=3, confidence=0.8))
# 分析結果の一部を参照する
inspect(head(sort(aprioriTranPosData, by="support"), n=100))

### 分析結果を、Gephi で描画する
# arules 型オブジェクトを、data.frame型に変換する
dataframeTranPosData <- as(aprioriTranPosData, "data.frame")
# ruleの内容を取り出して、ネットワークグラフのnodeオブジェクトを生成
rules <- dataframeTranPosData$rules
# データ型をcharacter型に型変換
rules <- as.character(rules)
# 条件部 {lhs} と 結果部 {rhs} を切り出して、別々のデータオブジェクトにする
rulesList <- strsplit(rules, "=>")
# for文のループ処理の中で、条件部 {lhs} と 結果部 {rhs} を分けて格納する変数を作成
# 初期値として値は0（ゼロ）を設定
nodeLabels <- as.data.frame(matrix(0, nrow=length(rulesList),ncol=2))
for(i in 1:length(rulesList)){
  nodeLabels[i,1] <-  rulesList[[i]][1]
  nodeLabels[i,2] <-  rulesList[[i]][2]
}
# nodeのID番号を採番する
nodesLhs <- rep(0, as.numeric(nrow(nodeLabels)))
nodesRhs <- rep(0, as.numeric(nrow(nodeLabels)))
for(i in 1:nrow(nodeLabels)){
  nodesLhs[i]  <- nodeLabels[i,1]
  nodesRhs[i]  <- nodeLabels[i,2]
}
#全ての条件部 {lhs} / 結果部 {rhs} を集めて、重複値を排除（一意集合をつくる）
nodesUnique <- unique(c(nodesLhs, nodesRhs))
nodesIDList <- data.frame(ID.NO=as.numeric(1:length(nodesUnique)), label=nodesUnique)
# ノード間の 条件部 {lhs} ⇒ 結果部 {rhs} ペアを、ノードID番号の数字ペアの行列データセットにする
nodesLhs <- data.frame(dummy=rep("X",length(nodesLhs)), label=as.vector(nodesLhs))
nodesRhs <- data.frame(dummy=rep("Y",length(nodesRhs)), label=as.vector(nodesRhs))
lhsNodes <- merge(nodesLhs, nodesIDList, by="label")
rhsNodes <- merge(nodesRhs, nodesIDList, by="label")
# dummy列を切り落として、ノードをつなぐエッジのデータを作成する
lhsNodes <- lhsNodes[,-2]
rhsNodes <- rhsNodes[,-2]
relations <- cbind(lhsNodes, rhsNodes)
# 条件部 {lhs} と 結果部 {rhs} のID.NOのみ取り出す
relations <- relations[ , c(-1,-3)]
# edgeの重み行列の生成
# 重み（weight）：　arules()関数 返り値の confidence 値 と定義
# support 値は小数点2桁に丸める
confidence <- dataframeTranPosData$confidence
weight <- round(confidence, 2)
support <- dataframeTranPosData$support
support <- round(support*20000000,1)
lift <- as.integer(order(dataframeTranPosData$lift)*1.5)

# GEXF ファイル出力のライブラリ(注3)を読み込む
library(rgexf)
# write.gexf() ファンクションを実行
Sys.setlocale('LC_ALL','C') 
nodesIDList2 <- nodesIDList
nodesIDList2[,2] <- iconv(nodesIDList2[,2],'SHIFT_JIS','UTF-8')
gexf.file <- write.gexf(nodes=nodesIDList2, edges=relations, edgesWeight=support)
# sink()関数で現在の作業ディレクトリにファイル出力
demogexf <- "demo.gexf"
sink(file=demogexf)
print(gexf.file)
sink()
