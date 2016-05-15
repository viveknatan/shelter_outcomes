# This is a randomForest model which does not include DateTime components

# SPLIT THE DATASETS AND TRAIN THE MODELS

train <- data1[1:26729, ]
test  <- data1[26730:nrow(data1), ]

# Set a random seed
set.seed(42)

# Build the model
rf <- randomForest(OutcomeType ~ AnimalType+age_days+Is_active+Gender+has_name+Is_black+Is_mix+stage, 
                       data = train, 
                       ntree = 600, 
                       importance = TRUE)

# PREDICT
prediction <- predict(rf, test, type = 'vote')

# Save the solution to a dataframe
solution <- data.frame('ID' = test$ID, prediction)

# Write it to file
write.csv(solution, 'viv_solution1.csv', row.names = F)


