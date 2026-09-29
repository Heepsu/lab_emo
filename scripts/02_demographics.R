###################################
# IMPORT & CLEAN DEMOGRAPHIC DATA #
###################################

paths_questionnaire <- here("data/raw", c(
  "data_exp_194853-v8_questionnaire-39o4.csv",                
  "data_exp_194853-v8_questionnaire-bvet.csv",
  "data_exp_194853-v8_questionnaire-ebow.csv",
  "data_exp_194853-v8_questionnaire-umjg.csv",
  "data_exp_194853-v8_questionnaire-w1tf.csv"
))

datalist_questionnaire <- list()
for (file_path in paths_questionnaire) {
  data_questionnaire <- read.csv(file_path, header = TRUE)
  
  data_questionnaire <- data_questionnaire[, c('Participant.Public.ID', 'Question', 'Response')]
  datalist_questionnaire[[length(datalist_questionnaire) + 1]] <- data_questionnaire
}

# Bind all as one dataset
data_questionnaire <- do.call(rbind, datalist_questionnaire)
data_questionnaire <- as.data.table(data_questionnaire)

# Clean data by removing empty strings and NA values
idx <- data_questionnaire$Question == '' | is.na(data_questionnaire$Question) 
data_questionnaire <- data_questionnaire[!idx, ]

# Remove duplicates based on Participant.Public.ID and Question
data_questionnaire <- unique(data_questionnaire, by = c('Participant.Public.ID', 'Question'))

# demographics 
unique_participants <- length(unique(data_questionnaire$Participant.Public.ID))
print(unique_participants)

saveRDS(data_questionnaire, file = here("data", "preprocessed", "demographics.rds"))
rm(datalist_questionnaire, data_questionnaire, paths_questionnaire, idx, unique_participants, file_path)
