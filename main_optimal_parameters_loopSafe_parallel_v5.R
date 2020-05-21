###########################################################################
# MATCHING SLICES: CASUS ARABICUM
###########################################################################

# IMPORTANT NOTE:
# Ctrl+c will not stop multiprocessing, and the script will actually keep
# running; `killall R` appears to do the trick (after Ctrl+c).

# setting seed for reproducability (not sure if it is absolutely necessary)
set.seed(786)

###########################################################################
# CORPUS FOLDER
corpusFolder  <- "./Hindawi_Corpus_65/"
versionRun    <- "ch_WardD_run01"
cores         <- 6
###########################################################################

###########################################################################
# MAIN TEST PARAMETERS
###########################################################################
featureTypeV       <- c("c") # c("c", "w")
nGramSizeV         <- c(1, 2, 3, 4, 5, 6, 7) #seq(1, 6, 1)
mffLimitV          <- seq(100, 500, 100)
cullingV           <- seq(0, 50, 10)
samplingType       <- "slicing"
sliceStart         <- 1000 # means that the first `sliceStart` words will be skipped
sliceTotal         <- 2
sliceLengthV       <- seq(100, 11000, 100) #seq(100, 11000, 100)

# FORMATTED FOR EASY COMMENTING OUT OF SPECIFIC DISTANCES 
distanceMethodV    <- c(# >>> STYLO DIST FUNCTIONS
  "cosine",      # important
  "delta",       # important
  "argamon",     # 
  "eder",        # important
  "minmax",      # important
  "entropy",    # 
  "simple",      # important
  "wurzburg",    # important
  # >>> DIST() FUNCTION
  "maximum",
  "manhattan",
  "canberra",
  "binary",
  "minkowski",
  "euclidean")
clusteringMethodV  <- c(# HCLUST METHODS
  #"average",
  #"median",
  #"centroid",
  #"mcquitty",
  #"complete",
  "ward.D2"      # important
)

#distanceMethodV    <- c("cosine") 
#clusteringMethodV  <- c("ward.D2")

# IF TRUE, DENDROGRAMS FOR EACH RUN WILL BE GENERATED
saveGraphParameter <- FALSE 

###########################################################################
# SETTING UP ALL NECESSARY FOLDERS
###########################################################################
# THE FOLLOWING EXTRACTS THE NAME OF THE CURRENT FOLDER and FILE BUT WORKS ONLY WHEN RUN WITH `Rscript`
library(funr); wd <- get_script_path(); setwd(wd); print(wd)
library(scriptName); thisFileName <- current_filename(); print(thisFileName)
library(stringr)

projectPrefix <- str_replace_all(corpusFolder, "\\W+", "")
resultsFolder <- paste0(projectPrefix,"_results_",versionRun)
savePath      <- paste0("./",resultsFolder,"/")
corpusFolder  <- corpusFolder; print(corpusFolder)
dir.create(savePath, showWarnings = FALSE)

# TO RESTRICT THE NUMBER OF TEXT, USE THE FOLLOWING VARIABLE
listOfFiles <- list.files(corpusFolder)

# FINAL RESULTS FILES
fileNameTSV <- "_cumulative_results.csv"

# SAVING THE STATE OF THE SCRIPT AS A EASY WAY TO PRESERVE PARAMETERS WITH WITH DATA WAS PRODUCED
fileNameRscriptState <- paste0("_script_with_parameters.R")
system(paste("cp",thisFileName, paste0(savePath,"/",fileNameRscriptState)))

###########################################################################
# PREPARING MULTIPROCESSING - START
library(doParallel)
clusterCores <- as.integer(detectCores()[1]/3*2)
clusterCores <- cores
registerDoParallel((clusterCores))
cl <- makeCluster(clusterCores)
registerDoParallel(cl)
# PREPARING MULTIPROCESSING - END
###########################################################################

###########################################################################
# LOADING MAIN LIBRARIES
###########################################################################
library(stylo)
library(textshape)
library(tidyverse)
library(ggplot2)
library(dendextend)
library(ggdendro)

###########################################################################
# CUSTOM FUNCTIONS
###########################################################################

# SLIGHTLY MODIFIED stylo::load.corpus
# - loads corpus tokenized on " "
load.corpus.mgr <- function(files = "all", corpus.dir = "", encoding = "UTF-8") {
  
  # first of all, retrieve the current path name
  original.path = getwd()
  # checking if the specified directory exists
  if(is.character(corpus.dir) == TRUE & nchar(corpus.dir) > 0) {
    # checking if the desired file exists and if it is a directory
    if(file.exists(corpus.dir) == TRUE & file.info(corpus.dir)[2] == TRUE) {
      # if yes, then set the new working directory
      setwd(corpus.dir)
    } else {
      # otherwise, stop the script
      stop("there is no directory ", getwd(), "/", corpus.dir)
    }
  } else {
    # if the argument was empty, then relax
    message("using current directory...\n")
  }
  # now, checking which files were requested; usually, the user is 
  # expected to specify a vector with samples' names
  if(length(files) == 1 & files[1] == "all") {
    files = list.files()
  }
  # variable initialization
  loaded.corpus = list()
  # uploading all files listed in the vector "files"
  for (file in files) {
    if(file.exists(file) == FALSE) {
      message("!\n")
      message("\"", file, "\"? no such a file -- check your directory!\n")
    } else {
      message("loading ", file, "\t", "...")
      # loading the next file from the list "corpus.filenames";
      # if an error occurred, ignore it and send a message on the screen
      current.file = tryCatch(scan(file, what = "char", encoding = encoding,
                                   sep = " ", quiet = TRUE),
                              error = function(e) NULL)
      # if successful, append the scanned file into the corpus,
      # otherwise send a message
      if(length(current.file) > 0) {
        loaded.corpus[[file]] = current.file
      } else {
        message("!\n")
        message("the file ", file, " could not be loaded for an unknown reason\n")
      }
    }
  }
  setwd(original.path)
  
  # assigning a class, if at least one text was successfully loaded
  if( length(loaded.corpus) > 0) {
    class(loaded.corpus) = "stylo.corpus"
    # adding some information about the current function call
    # to the final list of results
    attr(loaded.corpus, "call") = match.call()
  } else {
    loaded.corpus = NULL
  }
  
  # returning the value
  return(loaded.corpus)
}

# SLIGHTLY MODIFIED stylo::make.samples
# - slices texts, instead of sampling
make.slices.mgr <- function(tokenized.text, sampling = "slicing", slice.start = 1000, slice.size = 5000, number.of.slices = 2){
  
  # checking the format of input data (vector? list?); converting to a list
  if(is.list(tokenized.text) == FALSE) {
    tokenized.text = list(tokenized.text) 
  }
  # checking if there are any names attached to the texts
  if(is.character(names(tokenized.text)) == FALSE) {
    # if not, some generic names will be assigned
    names(tokenized.text) = paste("paste", 1:length(tokenized.text), sep = "_")
  }
  # starting an empty list
  corpus.cut.into.samples = list()
  # iterating over subsequent texts of the input corpus
  for(i in 1:length(tokenized.text)) {
    # retrieving an appropriate text from the whole corpus (if applicable)
    current.text = tokenized.text[[i]]
    # sanity check for text length: abort if the current text is extremely
    # short or at least shorter than the specified sample size
    if (length(current.text) < 10) {
      warning("\n\n", head(current.text, 100), "...\t", "This text is too short!", 
              "\n")
      stop("Corpus error...")
    }
    # at this point, each text in the corpus has been tokenized
    # into an array of tokens which we can divide into samples
    samples.from.text = list()
    if (sampling == "slicing"){
      # MGR: modified from "normal.sampling"
      # initialize variables to slice the text
      text.length = length(current.text)
      if ((slice.start+(slice.size*number.of.slices)) > text.length){
        #number.of.slices.mod = as.integer((text.length-slice.start)/slice.size)
        # DIRTY WORKAROUND > for short texts: EACH SLICE == FULL TEXT
        message(names(tokenized.text)[i])
        message(paste("\t", "- text length (in words): ", text.length, sep = ""))
        message(paste("\t", "- nr. of slices: ", number.of.slices, sep = ""))
        message(paste("\t", "- WARNING: slices contain the SAME text because the text is too short"))
        # iterate over the samples:
        current.start.index = slice.start
        for(sample.index in 1:number.of.slices) {
          current.slice = current.text[10:length(current.text)]
          # flush current sample:
          samples.from.text[[sample.index]] = current.slice
          # assign a new id to current sample
          id = paste(head(str_split(names(tokenized.text)[i], "\\.")[[1]], 1),"_",sample.index,sep="")
          names(samples.from.text)[sample.index] = id
          # increment index for next iteration
          current.start.index = current.start.index + slice.size
          current.slice = c()
        }
      } else {
        #number.of.slices = floor((text.length-sample.overlap)/(sample.size-sample.overlap))
        message(names(tokenized.text)[i])
        message(paste("\t", "- text length (in words): ", text.length, sep = ""))
        message(paste("\t", "- nr. of slices: ", number.of.slices, sep = ""))
        # iterate over the samples:
        current.start.index = slice.start
        for(sample.index in 1:number.of.slices) {
          current.slice = current.text[current.start.index:(current.start.index+slice.size-1)]
          # flush current sample:
          samples.from.text[[sample.index]] = current.slice
          # assign a new id to current sample
          #ORIGINAL: id = paste(names(tokenized.text)[i],"_",sample.index,sep="")
          id = paste(head(str_split(names(tokenized.text)[i], "\\.")[[1]], 1),"_",sample.index,sep="")
          names(samples.from.text)[sample.index] = id
          # increment index for next iteration
          current.start.index = current.start.index + slice.size
          current.slice = c()
        }
      }
    }
    # estimating the number of samples already appended to the "new" corpus
    n = length(corpus.cut.into.samples)
    # appending newly created samples to the "new" corpus
    for(s in 1:length(samples.from.text)) {
      corpus.cut.into.samples[[n+s]] = samples.from.text[[s]]
      names(corpus.cut.into.samples)[n+s] = names(samples.from.text)[s]
    }
  }
  class(corpus.cut.into.samples) = "stylo.corpus"
  return(corpus.cut.into.samples)
}

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

availableColors = c("red","green","blue","black","orange","purple",
                    "darkgrey","brown","maroon4","mediumturquoise","gold4", "deepskyblue",
                    "greenyellow","grey","chartreuse4", "khaki", "navy", "palevioletred",
                    "darkolivegreen4", "chocolate4", "yellowgreen")


###########################################################################
# STARTING RUNNIN FROM HERE
###########################################################################

# LOAD CORPUS
corpus <- load.corpus.mgr(files = listOfFiles, corpus.dir = corpusFolder, encoding = "UTF-8")

# INITIATE EMPTY DATAFRAME FOR COLLECTING RESULTS
finalResults = data.frame()

# MAIN LOOP
for (distanceMethod in distanceMethodV){
  for (clusteringMethod in clusteringMethodV){
    for (featureType in featureTypeV){
      for (nGramSize in nGramSizeV){
        # FOREACH GOES HERE
        message("######################################################")
        message("MULTIPROCESSING WITH ",clusterCores," CORES")
        message("\t",distanceMethod,"::",clusteringMethod,"::",featureType,"::",nGramSize)
        
        # SINGLE PROCESSING: COMMENT OUT MULTIPROCESSING SECTION
        #for (sliceLength in sliceLengthV){
        
        # ACTIVATE MULTIPROCESSING - START (COMMENT OUT LINE ABOVE)
        foreach(sliceLength=sliceLengthV) %dopar% {
          # IT LOOKS LIKE EVERYTHING NEEDS TO BE RE-LOADED WITHIN EACH FORLOOP
          library(stylo)
          library(textshape)
          library(tidyverse)
          library(ggplot2)
          library(dendextend)
          library(ggdendro)
          library(stringr)
        # ACTIVATE MULTIPROCESSIN - END
          
          for (mffLimit in mffLimitV){
            for (culling in cullingV){
              # - Generate filename for the results with specific set of settings
              fileNameBase <- paste0("_TEMP_",featureType, nGramSize,"_SL", sliceLength,"_ST",sliceTotal,"_MFF", mffLimit,"_CUL", culling,"_", distanceMethod,"_", clusteringMethod)
              fileNameBase <- str_replace_all(fileNameBase, "\\W+", "")
              message("\t--- ", fileNameBase,".csv")
              fileCompletePath <- paste0(savePath,fileNameBase,".csv")
              
              # - check if the file exists: yes > skip; no > generate results and save
              
              if (file.exists(fileCompletePath) == FALSE){
                corpus.features <- txt.to.features(corpus, ngram.size=nGramSize, features=featureType)
                corpus.sliced <- make.slices.mgr(corpus.features, sampling = samplingType, slice.start = sliceStart,
                                                 slice.size = sliceLength, number.of.slices = sliceTotal)
                frequent.features <- make.frequency.list(corpus.sliced, head=mffLimit)
                freqs <- make.table.of.frequencies(corpus.sliced, features=frequent.features)
                
                # v5: WHAT IS NEEDED IS TO CHECK WHETHER CULLED.FREQS IS NOT EMPTY (WHICH MAY HAPPEN WITH CULLING)
                culled.freqs <- perform.culling(freqs, culling.level=culling)
                if (ncol(culled.freqs) == 0){culled.freqs = NULL}
                
                # ADDED IN v3 TO SKIP CASES WHEN THERE IS NO DATA TO PROCESS (CULLING TOO HIGH, SLICES TOO SMALL) 
                if(is.matrix(culled.freqs) == FALSE & is.data.frame(culled.freqs) == FALSE) {
                  distanceMatrix <- NULL
                } else if(length(culled.freqs[1,]) < 2 | length(culled.freqs[,1]) < 2) {
                  distanceMatrix <- NULL
                } else {
                  # GENERATE DIFFERENCE DISTANCES
                  # - USING STYLO FUNCTIONS
                  if (distanceMethod == "cosine"){distanceMatrix <- dist.cosine(culled.freqs)
                  } else if (distanceMethod == "delta"){distanceMatrix <- dist.delta(culled.freqs)
                  } else if (distanceMethod == "argamon"){distanceMatrix <- dist.argamon(culled.freqs)
                  } else if (distanceMethod == "eder"){distanceMatrix <- dist.eder(culled.freqs)
                  } else if (distanceMethod == "minmax"){distanceMatrix <- dist.minmax(culled.freqs)
                  } else if (distanceMethod == "entropy"){distanceMatrix <- dist.entropy(culled.freqs)
                  } else if (distanceMethod == "simple"){distanceMatrix <- dist.simple(culled.freqs)
                  } else if (distanceMethod == "wurzburg"){distanceMatrix <- dist.wurzburg(culled.freqs)
                  # USING DIST FUNCTIONS
                  } else if (distanceMethod == "euclidean"){distanceMatrix <- dist(culled.freqs, method="euclidean")
                  } else if (distanceMethod == "maximum"){distanceMatrix <- dist(culled.freqs, method="maximum")
                  } else if (distanceMethod == "manhattan"){distanceMatrix <- dist(culled.freqs, method="manhattan")
                  } else if (distanceMethod == "canberra"){distanceMatrix <- dist(culled.freqs, method="canberra")
                  } else if (distanceMethod == "binary"){distanceMatrix <- dist(culled.freqs, method="binary")
                  } else if (distanceMethod == "minkowski"){distanceMatrix <- dist(culled.freqs, method="minkowski")
                  }
                  distanceMatrix <- as.dist(distanceMatrix)
                }
                
                # CLUSTERING WILL NOT WORK ON VERY SMALL SAMPLES, THROWING AN ERROR: THESE CASES MUST BE SKIPPED
                clustered.data <- tryCatch(hclust(distanceMatrix, method = clusteringMethod), error = function(e) NULL)
                
                # if clustering successfull, continue processing...
                # otherwise send a message
                if(length(clustered.data) > 0) {
                  
                  # GENERATE A DENDROGRAM GRAPH AND SAVES PDF
                  if (saveGraphParameter == TRUE){
                    
                    ggDendr <- dendro_data(clustered.data)
                    ggLabels <- as.data.frame(ggDendr$labels) %>%
                      mutate(split = label) %>%
                      separate(split, c("author", "book", "part"), sep = "_")
                    
                    colorAuthors <- rep(availableColors, length(unique(ggLabels$author)))  
                    colorBooks   <- rep(availableColors, length(unique(ggLabels$book)))
                    
                    titleValue <- paste0(prefix, "; F: ", featureType, nGramSize,
                                         "; CULL: ", culling, "; DIST: ", distanceMethod,
                                         ": SL: ", sliceTotal, "; LEN: ", sliceLength)
                    
                    fileName <- paste0(str_replace_all(titleValue, "[ ;:\\.]+", "_"), ".pdf")
                    
                    ggplot() + 
                      geom_segment(data=segment(ggDendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
                      geom_text(data=ggLabels, aes(x=x, y=y, label=book, hjust=0, color=author), size=3) +
                      coord_flip() + 
                      scale_y_reverse(expand=c(0.3, 0)) + 
                      scale_color_manual(values = colorAuthors) +
                      theme(#legend.position=c(max(y),max(ggLabels$y)),
                        axis.line.y=element_blank(),
                        axis.ticks.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.title.y=element_blank(),
                        axis.title.x=element_blank(),
                        panel.background=element_rect(fill="white"),
                        panel.grid=element_blank(),
                        #legend.position=c(0.1,0.1)
                      ) + 
                      ggtitle(titleValue)
                    
                    ggsave(
                      filename = paste0(savePath,fileName),
                      plot = last_plot(),
                      device = "pdf",
                      scale = 1,
                      width = 11,
                      height = 0.7+0.15*length(listOfFiles)*sliceTotal,
                      units = "in",
                      dpi = 300
                    )
                    
                  }
                  
                  # EXTRACTING INFO ON CLUSTERS
                  clusters <- cutree(clustered.data, k=length(unique(listOfFiles)))
                  clusters <- stack(clusters)
                  distanceTidy <- tidy_matrix(as.matrix(distanceMatrix))
                  
                  # CHECK IF BOOKS MATCH THROUGH CLUSTERING
                  bookClusterMatch <- clusters %>%
                    mutate(author_book = str_replace_all(ind, "_\\d+", "")) %>%
                    group_by(author_book) %>%
                    mutate(book_cluster_match = ifelse(length(unique(values)) == 1, 1, 0)) %>%
                    select(author_book, book_cluster_match) %>%
                    unique
                  
                  # CHECK IF BOOKS MATCH THROUGH DISTANCES
                  bookDistanceMatch <- distanceTidy %>%
                    filter(value > 0) %>% unique %>%
                    mutate(author_book = str_replace_all(row, "_\\d+", "")) %>%
                    mutate(book_match = str_replace_all(col, "_\\d+", "")) %>%
                    arrange(author_book, value) %>%
                    select(author_book, book_match, value) %>% unique %>%
                    group_by(author_book) %>%
                    top_n(-1, wt=value) %>%
                    mutate(dist_value=value) %>%
                    ungroup() %>%
                    mutate(book_dist_match = ifelse(author_book == book_match, 1, 0)) %>%
                    select(author_book, book_match, book_dist_match, dist_value) %>%
                    unique
                  
                  # CHECK IF AUTHORS MATCH THROUGH CLUSTERING
                  authorClusterMatch <- clusters %>%
                    mutate(author_book = str_replace_all(ind, "_\\d+$", "")) %>%
                    mutate(author = str_replace_all(ind, "_\\w+$", "")) %>%
                    group_by(values) %>%
                    mutate(author_cluster_match = ifelse(length(unique(author)) == 1, 1, 0)) %>%
                    ungroup %>%
                    select(author_book, author, author_cluster_match) %>%
                    unique
                  
                  # CHECK IF AUTHORS MATCH THROUGH DISTANCES
                  authorDistanceMatch <- distanceTidy %>%
                    filter(value > 0) %>% unique %>%
                    mutate(author_book = str_replace_all(row, "_\\d+", "")) %>%
                    mutate(author1 = str_replace_all(row, "_\\w+$", "")) %>%
                    mutate(book_match = str_replace_all(col, "_\\d+", "")) %>%
                    mutate(author2 = str_replace_all(col, "_\\w+$", "")) %>%
                    select(author_book, book_match, author1, author2, value) %>% unique %>%
                    group_by(author_book) %>%
                    top_n(-1, wt=value) %>%
                    mutate(dist_value=value) %>%
                    ungroup() %>%
                    mutate(author_dist_match = ifelse(author1 == author2, 1, 0)) %>%
                    select(author_book, author_dist_match) %>%
                    unique
                  
                  # AGGREGATE RESULTS INTO ONE DATAFRAME
                  final <- bookClusterMatch %>%
                    left_join(bookDistanceMatch, by="author_book") %>%
                    left_join(authorClusterMatch, by="author_book") %>%
                    left_join(authorDistanceMatch, by="author_book") %>%
                    select(author_book, book_match, book_cluster_match, book_dist_match, author_cluster_match, author_dist_match, dist_value) %>%
                    mutate(feature = paste0(featureType,nGramSize)) %>%
                    mutate(mff = mffLimit) %>%
                    mutate(culling = culling) %>%
                    mutate(slices_compared = sliceTotal) %>%
                    mutate(slice_len = sliceLength) %>%
                    mutate(distance_method = distanceMethod) %>%
                    mutate(clustering_method = clusteringMethod)
                  
                  finalResults <- rbind(finalResults, final)
                  
                  # SAVE IN APPEND MODE - saving to avoid failure in script execution and losing results
                  finalResults <- finalResults %>%
                    select(author_book,
                           clustering_method, book_cluster_match, author_cluster_match,
                           distance_method, dist_value, book_match, book_dist_match, author_dist_match,
                           feature, mff, culling, slices_compared, slice_len) %>%
                    unique
                  
                  write_tsv(finalResults, fileCompletePath, na = "NA", append = FALSE, col_names = TRUE)
                  finalResults = data.frame()
                  
                } else {
                  message("\t--- no clustering is possible with these parameters. Skipping to the next set of values...")
                  
                  # SAVE A TSV FILE WITH NO VALUES TO INDICATE THAT NO ANALYSIS POSSIBLE WITH THESE PARAMETERS
                  emptyRow <- c("NO_DATA", clusteringMethod, NA, NA, distanceMethod, NA, NA, NA, NA,
                                paste0(featureType, nGramSize), mffLimit, culling, sliceTotal, sliceLength)
                  finalResults <- rbind(finalResults, emptyRow)
                  colnames(finalResults) <- c("author_book", "clustering_method", "book_cluster_match", "author_cluster_match",
                                              "distance_method", "dist_value", "book_match", "book_dist_match", "author_dist_match",
                                              "feature", "mff", "culling", "slices_compared", "slice_len")
                  write_tsv(finalResults, fileCompletePath, na = "NA", append = FALSE, col_names = TRUE)
                  finalResults = data.frame()
                  
                }
              } else {
                message("\t--- the results for these settings have already been generated. Skipping to the next set of values...")
              }
            }
          }
        }
      }
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
remove.temp.results(results.dir = savePath, prefix = "_TEMP_")

stopCluster(cl)
message("ALL DONE!")
