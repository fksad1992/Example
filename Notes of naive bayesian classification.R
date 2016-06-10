library(plyr)
library(reshape2)
## calculate the class prob of training set
class_prob = function(trainData, strClassName){
  length.train = nrow(trainData)
  dTemp = ddply(trainData, strClassName, 'nrow')
  dTemp = ddply(dTemp, strClassName, mutate, prob = nrow / length.train)
  dTemp[, -2]
}

## calculate the feature prob in each class
feature_class_prob = function(trainData, strClassName){
  data.melt = melt(trainData, id = c(strClassName))
  aa = ddply(data.melt, c(strClassName, 'variable', 'value'), 'nrow')
  bb = ddply(aa, c(strClassName, 'variable'), mutate, sum = sum(nrow), prob = nrow / sum)
  colnames(bb) = c('class.name',
                   'fearure.name',
                   'feature.value',
                   'feature.nrow',
                   'feature.sum',
                   'prob')
  bb[, c(1, 2, 3, 6)]
}
## predict the prob of class of test set
  pre_class = function(oneObs, pc, pfc){
    colnames(oneObs) = c('feature.name', 'feature.value')
    colnames(pc) = c('class.name', 'prob')
    colnames(pfc) = c('class.name', 'feature.name', 'feature.value', 'prob')
    feature.all = join(oneObs, pfc, by = c('feature.name', 'feature.value'),
                       type = 'inner')
    feature.prob = ddply(feature.all, .(class.name), summarize, prob_fea = prod(prob))
    class.all = join(feature.prob, pc, by = 'class.name', type = 'inner')
    ddply(class.all, .(class.name), mutate, pre_prob = prob_fea * prob)[, c(1, 4)]
  }
## generate the training set
  train.apple = data.frame(
                           size = c('big', 'small', 'big', 'big', 'small', 'small'),
                           weight = c('light', 'heavy', 'light', 'light', 'heavy', 'light'),
                           color = c('red', 'red', 'red', 'green', 'red', 'green'),
                           taste = c('good', 'good', 'bad', 'bad', 'bad', 'good')
                           )
## test set
  oneObs = data.frame(feature.name = c('size', 'weight', 'color'), 
                      feature.value = c('big', 'heavy', 'red')
                      )
## run the whole program
  pc = class_prob(train.apple, 'taste')
  pfc = feature_class_prob(train.apple, 'taste')
  pre_class(oneObs, pc, pfc)

## use iris to test
riris = data.frame(apply(iris[, 1:4], 2, round), Species = iris$Species)
success = vector()
for(j in 1:100){
tset = riris[order(rnorm(150)), ]  
pc = class_prob(tset[1:100, ], 'Species')   # sampling training set randomly
pfc = feature_class_prob(tset[1:120, ], 'Species')
miris = melt(tset[101:150, ], id = 'Species')
pre_Species = vector()
for(i in 1:50){
  oneObs = miris[c(i, i+50, i+100, i+150), -1]
  pre = pre_class(oneObs, pc = pc, pfc = pfc)
  pre_Species[i] = pre[which.max(pre[, 2]), 1]
}    
Species = as.numeric(tset$Species)[101:150]
success[j] = sum(ifelse(pre_Species==Species, 1, 0)) / 50  
}
mean(success)