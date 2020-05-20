###########################################################################
# THE SCRIPT COMBINES TEMPORARY RESULTS INTO ONE FILE
###########################################################################

library(funr); wd <- get_script_path(); setwd(wd); print(wd)
# PROVIDE RELATIVE PATH TO THE FOLDER WITH RESULTS
savePath <- "./Hindawi_Corpus_65_results_run01/"
fileNameTSV <- "_cumulative_results.csv"

# CUSTOM FUNCTIONS

# RESULTS ARE GENERATED INTO SMALL TEMP FILES, THIS FUNCTION MERGES THEM
combine.results <- function(results.dir = "", prefix="_TEMP_") {
  library(gdata)
  library(readr)
  options(readr.num_columns = 0)
  list.of.files <- list.files(results.dir)
  tempDF = data.frame()
  for(file in list.of.files){
    if(startsWith(file, prefix)){
      message(file)
      rows <- read_delim(paste0(results.dir,file), "\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
      tempDF <- rbind(tempDF, rows)
      #file.remove(paste0(results.dir,file))
    }
  }
  print(nrow(tempDF))
  return(tempDF)
}

# RESULTS ARE GENERATED INTO SMALL TEMP FILES, THIS FUNCTION REMOVES THEM
remove.temp.results <- function(results.dir = "", prefix="_TEMP_") {
  list.of.files <- list.files(results.dir)
  tempDF = data.frame()
  for(file in list.of.files){
    if(startsWith(file, prefix)){
      message(file, " --- ", "REMOVED...")
      file.remove(paste0(results.dir,file))
    }
  }
}


# SAVE FINAL RESULTS: 

# INITIATE DATAFRAME FOR FINAL RESULTS - START
finalResults <- combine.results(results.dir = savePath, prefix = "_TEMP_")
colnames(finalResults) <- c("author_book", "clustering_method", "book_cluster_match",
                            "author_cluster_match", "distance_method", "dist_value",
                            "book_dist_matched", "book_dist_match", "author_dist_match",
                            "feature", "mff", "culling", "slices_compared", "slice_len")

# cummulative fileNameTSV: must be defined in the variable section --- this one cannot be automatically generated
write_tsv(finalResults, paste0(savePath,fileNameTSV), na = "NA", append = FALSE, col_names = TRUE)
# INITIATE DATAFRAME FOR FINAL RESULTS - END

# REMOVE RESULTS ONLY AFTER ALL OTHER STEPS ARE COMPLETE
#remove.temp.results(results.dir = savePath, prefix = "_TEMP_")