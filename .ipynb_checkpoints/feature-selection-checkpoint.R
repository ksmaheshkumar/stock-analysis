library(RSQLite)
library(PerformanceAnalytics)
library(caret)

set.seed(7)

# Load dataframe from database
db_to_df <- function(dbname, tbname){
  sqlite_driver <- dbDriver("SQLite")
  db <- dbConnect(sqlite_driver, dbname)
  table <- dbReadTable(db, "eow")
  return(table)
}

# Convert categorical variables to factors
convert_to_factors <- function(tdf, cat_var){
  cat_var <- 
  tdf[,cat_var] <- lapply(tdf[,cat_var] , factor)
  str(tdf)
  return(tdf)
}

# Prerprocessing
preprocess_df <- function(tdf) {
  tdf <- convert_to_factors(tdf, c(22,23,27,28,31,32))
  tdf$volume <- as.double(tdf$volume)
  tdf$volume_obv <- as.double(tdf$volume_obv)
  ## Remove categorical variable
  tdf <- tdf[,-c(22,23,27,28,31,32)]
  ## Remove -c("open", "close", "high", "close", "company")
  tdf <- tdf[,-c(3, 4, 5, 6, 7)]
  ## Remove highly correlated with logret
  tdf <- tdf[,-c(58, 57)]
  str(tdf)
  return (tdf)
}

############################################################
df <- db_to_df("stocks-eow.db", "eow")
df <- preprocess_df(df)
View(df)
######################## Corelations ####################

get_mean_corr <- function(tdf) {
  total_stocks <- length(unique(tdf$symbol))
  feats <- tdf[tdf$symbol == 'A',][,3:58]
  dcor <- cor(feats)
  for (sto in unique(df$symbol)){
    if (sto != 'A'){
      feats <- tdf[tdf$symbol == sto,][,3:58]
      dcor <- dcor + cor(feats)
    }
  }
  mean_corr <- dcor / total_stocks
  return (mean_corr)
}

mean_corr <- get_mean_corr(df)


######################## Plotting ######################
corrplot::corrplot(mean_corr)
corrplot::corrplot.mixed(mean_corr)
corrplot::cor.mtest(mean_corr)

####################### Caret ##########################

cor_var <- findCorrelation(mean_corr, cutoff = 0.9, verbose = TRUE, names = TRUE, exact=TRUE)
# cor_var_ind <- findCorrelation(mean_corr, cutoff = 0.9, verbose = TRUE, names = TRUE, exact=TRUE)
# remove coorelated variables
dft <- df[,!(names(df) %in% cor_var)]


##############################################
# Create database
sqlite_driver <- dbDriver("SQLite")
db <- dbConnect(sqlite_driver, "stocks-updated")
dbWriteTable(db, "eow", dft)
