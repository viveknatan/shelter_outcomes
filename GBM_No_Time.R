# This is a GBM model without DateTime features

# SPLIT THE DATASETS AND TRAIN THE MODELS

train <- data1[1:26729, ]
test  <- data1[26730:nrow(data1), ]

# GBM Model
set.seed(2016)

trees <- 1500
gbm1 <- gbm(
  OutcomeType ~ AnimalType+age_days+Is_active+Gender+has_name+Is_black+Is_mix+stage,
  data=train,
  distribution="multinomial",
  shrinkage=0.05,
  n.trees=trees,
  interaction.depth=6L,
  keep.data=FALSE,
  verbose=TRUE
)

# PREDICT
prediction1 <- predict(gbm1, test, type = 'response', n.trees = trees)

# Save the solution to a dataframe
solution1 <- data.frame('ID' = test$ID, prediction1)

# Write it to file
write.csv(solution1, 'viv_solution2.csv', row.names = F)


