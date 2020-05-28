###########################################################################
# MATCHING SLICES: CASUS ARABICUM
###########################################################################

library(stylo)
library(textshape)
library(tidyverse)
library(ggplot2)
library(dendextend)
library(ggdendro)

set.seed(786)


# CUSTOM FUNCTIONS

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

availableColors = c("red","green","blue","black","orange","purple",
                    "darkgrey","brown","maroon4","mediumturquoise","gold4", "deepskyblue",
                    "greenyellow","grey","chartreuse4", "khaki", "navy", "palevioletred",
                    "darkolivegreen4", "chocolate4", "yellowgreen")

#############################################################################################
# WORKING CODE: OUT OF THE LOOP > GENERATES A GRAPH
#############################################################################################

# FOLDERS
corpusFolder <- "/Users/romanovienna/_ADHFANIS_Data/chapter_3/stylometry_opt_arabic/Hindawi_Corpus_65/"
setwd("/Users/romanovienna/_ADHFANIS_Data/chapter_3/stylometry_opt_arabic/")
savePath = "/Users/romanovienna/_ADHFANIS_Data/chapter_3/stylometry_opt_arabic/"

# CORPUS FILES
listOfFiles <- c("1392TahaHusayn_JannatHayawan.txt", "1392TahaHusayn_DucaKarawan.txt",
                 "1375MuhammadHusaynHaykal_HakadhaKhuliqat.txt", "1375MuhammadHusaynHaykal_QisasMisriyya.txt",
                 "1373NiqulaHaddad_AsrarMisr.txt", "1368IbrahimCabdQadirMazini_IbrahimKatib.txt",
                 "1373NiqulaHaddad_AdamJadid.txt", "1368CaliJarim_GhadatRashid.txt",
                 "1368IbrahimCabdQadirMazini_FiTariq.txt", "1349JibranKhalilJibran_ArwahMutamarrida.txt",
                 "1368CaliJarim_FarisBaniHamdan.txt", "1342MustafaLutfiManfaluti_Fadila.txt",
                 "1349JibranKhalilJibran_AjnihaMutakassira.txt", "1331JurjiZaydan_AminWaMamun.txt",
                 "1342MustafaLutfiManfaluti_Cabarat.txt", "1331JurjiZaydan_AhmadIbnTulun.txt",
                 "1331JurjiZaydan_AbuMuslimKhurasani.txt")
listOfFiles <- list.files(corpusFolder)

# TEST PARAMETERS
prefix            <- "Hindawi Sample"
featureType       <- "w" # w or c
nGramSize         <- 1
mffLimit          <- 300
culling           <- 0
samplingType      <- "slicing"
sliceStart        <- 1000
sliceTotal        <- 2
sliceLength       <- 3000
#distanceMethod    <- "euclidean"
distanceMethod    <- "delta"
clusteringMethod  <- "ward.D2"
saveGraphParameter<- TRUE

# FORMATTED FOR EASY COMMENTING OUT OF SPECIFIC DISTANCES 
distanceMethodV    <- c(# >>> STYLO DIST FUNCTIONS
  "cosine", "euclidean", "delta",
  "argamon", "eder", "minmax",
  "enthropy", "simple", "wurzburg",
  # >>> DIST() FUNCTION
  "euclidean", "maximum", "manhattan",
  "canberra", "binary", "minkowski"
)
clusteringMethodV  <- c(# HCLUST METHODS
  "ward.D2", "complete",
  "average", "median",
  "centroid", "mcquitty"
)

# INITIATING PROCESSING HERE

finalResults <- data.frame()
corpus <- load.corpus.mgr(files = listOfFiles, corpus.dir = corpusFolder, encoding = "UTF-8")

# LOOPABLE CODE STARTS FROM HERE

corpus <- txt.to.features(corpus, ngram.size=nGramSize, features=featureType)
corpus <- make.slices.mgr(corpus, sampling = samplingType, slice.start = sliceStart,
                            slice.size = sliceLength, number.of.slices = sliceTotal)
frequent.features <- make.frequency.list(corpus, head=mffLimit)
freqs <- make.table.of.frequencies(corpus, features=frequent.features)
culled.freqs <- perform.culling(freqs, culling.level=culling)

# GENERATE DIFFERENCE DISTANCES
# - USING STYLO FUNCTIONS
if (distanceMethod == "cosine"){distanceMatrix <- dist.cosine(culled.freqs)
} else if (distanceMethod == "delta"){distanceMatrix <- dist.delta(culled.freqs)
} else if (distanceMethod == "argamon"){distanceMatrix <- dist.argamon(culled.freqs)
} else if (distanceMethod == "eder"){distanceMatrix <- dist.eder(culled.freqs)
} else if (distanceMethod == "minmax"){distanceMatrix <- dist.minmax(culled.freqs)
} else if (distanceMethod == "enthropy"){distanceMatrix <- dist.enthropy(culled.freqs)
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
clustered.data <- hclust(distanceMatrix, method = clusteringMethod)

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

# # EXTRACTING INFO ON CLUSTERS
# clusters <- cutree(clustered.data, k=length(unique(listOfFiles)))
# clusters <- stack(clusters)
# distanceTidy <- tidy_matrix(as.matrix(distanceMatrix))
# 
# # CHECK IF BOOKS MATCH THROUGH CLUSTERING
# bookClusterMatch <- clusters %>%
#   mutate(author_book = str_replace_all(ind, "_\\d+", "")) %>%
#   group_by(author_book) %>%
#   mutate(book_cluster_match = ifelse(length(unique(values)) == 1, 1, 0)) %>%
#   select(author_book, book_cluster_match) %>%
#   unique
# 
# # CHECK IF BOOKS MATCH THROUGH DISTANCES
# bookDistanceMatch <- distanceTidy %>%
#   filter(value > 0) %>% unique %>%
#   mutate(author_book = str_replace_all(row, "_\\d+", "")) %>%
#   mutate(book_match = str_replace_all(col, "_\\d+", "")) %>%
#   arrange(author_book, value) %>%
#   select(author_book, book_match, value) %>% unique %>%
#   group_by(author_book) %>%
#   top_n(-1, wt=value) %>%
#   mutate(dist_value=value) %>%
#   ungroup() %>%
#   mutate(book_dist_match = ifelse(author_book == book_match, 1, 0)) %>%
#   select(author_book, book_match, book_dist_match, dist_value)
#   
# # CHECK IF AUTHORS MATCH THROUGH CLUSTERING
# authorClusterMatch <- clusters %>%
#   mutate(author_book = str_replace_all(ind, "_\\d+$", "")) %>%
#   mutate(author = str_replace_all(ind, "_\\w+$", "")) %>%
#   group_by(values) %>%
#   mutate(author_cluster_match = ifelse(length(unique(author)) == 1, 1, 0)) %>%
#   ungroup %>%
#   select(author_book, author, author_cluster_match) %>%
#   unique
# 
# # CHECK IF AUTHORS MATCH THROUGH DISTANCES
# authorDistanceMatch <- distanceTidy %>%
#   filter(value > 0) %>% unique %>%
#   mutate(author_book = str_replace_all(row, "_\\d+", "")) %>%
#   mutate(author1 = str_replace_all(row, "_\\w+$", "")) %>%
#   mutate(book_match = str_replace_all(col, "_\\d+", "")) %>%
#   mutate(author2 = str_replace_all(col, "_\\w+$", "")) %>%
#   select(author_book, book_match, author1, author2, value) %>% unique %>%
#   group_by(author_book) %>%
#   top_n(-1, wt=value) %>%
#   mutate(dist_value=value) %>%
#   ungroup() %>%
#   mutate(author_dist_match = ifelse(author1 == author2, 1, 0)) %>%
#   select(author_book, author_dist_match)
# 
# # AGGREGATE RESULTS INTO ONE DATAFRAME
# final <- bookClusterMatch %>%
#   left_join(bookDistanceMatch, by="author_book") %>%
#   left_join(authorClusterMatch, by="author_book") %>%
#   left_join(authorDistanceMatch, by="author_book") %>%
#   select(author_book, book_match, book_cluster_match, book_dist_match, author_cluster_match, author_dist_match, dist_value) %>%
#   mutate(feature = paste0(featureType,nGramSize)) %>%
#   mutate(mff = mffLimit) %>%
#   mutate(culling = culling) %>%
#   mutate(slices_compared = sliceTotal) %>%
#   mutate(slice_len = sliceLength) %>%
#   mutate(distance_method = distanceMethod) %>%
#   mutate(clustering_method = clusteringMethod)
# 
# finalResults <- rbind(finalResults, final)
# 
# # END OF THE LOOP
# 
# finalResults <- finalResults %>%
#   select(author_book,
#          clustering_method, book_cluster_match, author_cluster_match,
#          distance_method, dist_value, book_match, book_dist_match, author_dist_match,
#          feature, mff, culling, slices_compared, slice_len)
# 
# # cummulative fileNameTSV: must be defined in the variable section --- this one cannot be automatically generated
# fileNameTSV <- str_replace(fileName, ".pdf", ".csv")
# write_tsv(finalResults, paste0(savePath,fileNameTSV), na = "NA", append = FALSE, col_names = TRUE,
#           quote_escape = "double")
