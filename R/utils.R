
# custom standar error function
se <- function(x) {
  x_clean <- na.omit(x)
  if (length(x_clean) < 2) {
    return(NA) 
  }
  return(sd(x_clean) / sqrt(length(x_clean)))
}

emo_all <- c("Anxiety", "Awe", "Excitement", "Fear", "Joy", "Liking", "Aroused-Calm", "Constricted-Spacious", "Leave-Stay", "Tense-Relaxed", "Unpleasant-Pleasant", "Unsafe-Safe")
emo_uni <- c("Anxiety", "Awe", "Excitement", "Fear", "Joy", "Liking")
emo_bi <- c("Aroused-Calm", "Constricted-Spacious", "Leave-Stay", "Tense-Relaxed", "Unpleasant-Pleasant", "Unsafe-Safe")