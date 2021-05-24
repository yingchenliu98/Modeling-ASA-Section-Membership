library(rpart)
library(rpart.plot)

# Load data
setwd('/Users/yingchenliu/Desktop/university/stat149/stat149_final_project/data')
full = read_csv('full_data.csv',  na=character())
train_data = read_csv('train_data.csv',  na=character())
test_data = read_csv('test_data.csv',  na=character())

dim(train_data)
dim(test_data)

summary(full$Gender)
table(train$Gender)

member.tree.full = rpart(AnySection ~ .,
                         cp=0.001, data=train_data, method="class", parms=list(split="information"))

png(file="tree_full.png",
    width=11, height=8.5, units="in", res=300)
prp(member.tree.full,type=0,extra=106,digits=0,
    main= "Classification Tree (Full Model)")
dev.off()

# Use plotcp to see the CV results graphically
png(file="plotcp.png",
    width=11, height=8.5, units="in", res=300)
plotcp(member.tree.full)
dev.off()


# Prune the tree based on cp=0.0037
member.pruned = prune(member.tree.full, cp=0.0037)
png(file="rpart_pruned_cp=0.0037.png",
    width=11, height=8.5, units="in", res=300)
prp(member.pruned,type=0,extra=106,digits=0,
    main= "Classification Tree Pruned cp=0.0037.png")
dev.off()


# Predict on the test set and get confusion matrix (test accuracy: 0.7001)
tree_pred_test = predict(member.pruned, test_data)[,2]
confusionMatrix(data=factor(as.numeric(tree_pred_test>=0.5)), 
                reference=factor(test_data$AnySection), positive="1")

