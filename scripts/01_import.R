#---------------------------------------------------------------------------#
# Imports and cleans experiment and questionnaire data for further analysis #
#---------------------------------------------------------------------------#


# --- NOTE: Data is assumed to exist in "data/raw" in the working directory. --- 
# Pattern checks for csv files that belong to experiment 194853-v8
paths <- list.files(
  path = here("data/raw"), 
  pattern = "data_exp_194853-v8_task-.*\\.csv$", 
  full.names = TRUE
)

# Participant.Public.ID = participant id
# Response = Participant's answer on a scale from 1-9
# Tag = Measured emotion
datalist <- list()
for (file_path in paths) {
  data <- read.csv(file_path, header = TRUE)
  
  if ('Spreadsheet..video1' %in% names(data)) {
    names(data)[names(data) == 'Spreadsheet..video1'] <- 'videoset1'
  }
  if ('Spreadsheet..video2' %in% names(data)) {
    names(data)[names(data) == 'Spreadsheet..video2'] <- 'videoset2'
  }
  
  data <- data[, c('Participant.Public.ID', 'Object.Name' , 'Response', 'videoset1', 'videoset2')]
  datalist[[length(datalist) + 1]] <- data
}

# Bind all as one dataset
data <- do.call(rbind, datalist)
data <- as.data.table(data)

# Clean data by removing empty strings, NA values, strings 'BEGIN' and 'END'
idx <- data$Response == '' | is.na(data$Response) | data$Response == 'BEGIN' | data$Response == 'END'
data <- data[!idx, ]

# Match rating scales to correspond measured emotion scales
data <- data %>%
  mutate(Object.Name = case_when(
    Object.Name == 'Rating Scale1' ~ 'Unpleasant-Pleasant',
    Object.Name == 'Rating Scale2' ~ 'Aroused-Calm',
    Object.Name == 'Rating Scale3' ~ 'Tense-Relaxed',
    Object.Name == 'Rating Scale4' ~ 'Constricted-Spacious',
    Object.Name == 'Rating Scale5' ~ 'Unsafe-Safe',
    Object.Name == 'Rating Scale6' ~ 'Leave-Stay',
    Object.Name == 'Rating Scale7' ~ 'Liking',
    Object.Name == 'Rating Scale8' ~ 'Excitement',
    Object.Name == 'Rating Scale9' ~ 'Joy',
    Object.Name == 'Rating Scale10' ~ 'Anxiety',
    Object.Name == 'Rating Scale11' ~ 'Fear',
    Object.Name == 'Rating Scale12' ~ 'Awe',
    TRUE ~ Object.Name # Keeps any other values in the column unchanged
  ))

# Add new column to separate the video selection participant made and the emotion ratings 
data <- data %>%
  # Group by participant
  group_by(Participant.Public.ID) %>%
  
  # Create column 'Choice', if object name is 'Response' take the value from the column Response, otherwise NA
  mutate(Choice = ifelse(Object.Name == "Response", Response, NA)) %>%
  
  # Fill columns with missing (NA) values in column Choice with the name of the video participant has selected 
  fill(Choice, .direction = "down") %>%
  ungroup() %>%
  
  # Filter out rows where objects name is 'Response' so Response column will have only numeric values 
  filter(Object.Name != "Response")

data <- data %>%
  mutate(Response = as.numeric(Response))

data <- data %>%
  mutate(
    # extract leading letters before numbers
    Environment = str_extract(Choice, "^[A-Za-z]+")
    
  )

saveRDS(data, file = here("data", "preprocessed", "data.rds"))
rm(data, idx, paths, file_path, datalist)




