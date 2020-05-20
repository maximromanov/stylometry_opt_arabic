# ANALYSIS OF PARAMETERS

library(readr)
library(tidyverse)
library(ggplot2)

# LOAD TEST RESULST

setwd("/Users/romanovienna/_ADHFANIS_Data/chapter_3/stylometry_opt_arabic/")
file <- "/Users/romanovienna/_ADHFANIS_Data/chapter_3/arc/arc_testing_halves_now_optimal_parameters/multiprocessing_results/_Hindawi_AllTexts_MultiProcessed_D20200517T160000.csv"

data <- read_delim(file, "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

colnames(data) <- c("author_book", "clustering_method", "book_cluster_match",
                            "author_cluster_match", "distance_method", "dist_value",
                            "book_match", "book_dist_match", "author_dist_match",
                            "feature", "mff", "culling", "slices_compared", "slice_len")

unique(data$distance_method)
unique(data$mff)
unique(data$culling)
unique(data$clustering_method)

dataTemp <- data %>%
  filter(distance_method=="cosine", feature=="w1", mff==300, culling==0) %>%
  group_by(slice_len) %>%
  summarize(matches = sum(book_cluster_match))

ggplot(dataTemp, aes(x=slice_len, y=matches))+
  geom_line()

plot(dataTemp)

# COMPLETE SET OF PARAMETERS

# RUNNING PARAMETERS (HIGH-NUMBERS VECTORS)
featureTypeV       <- c("w", "c") # c("c", "w")
nGramSizeV         <- c(1,2,3,4,5) #seq(1, 6, 1)
mffLimitV          <- seq(100, 500, 100)
cullingV           <- 0
samplingType       <- "slicing"
sliceStart         <- 1000 # means that the first `sliceStart` words will be skipped
sliceTotal         <- 2
sliceLengthV       <- seq(100, 11000, 100) #seq(100, 11000, 100)

# FORMATTED FOR EASY COMMENTING OUT OF SPECIFIC DISTANCES 
distanceMethodV    <- c(# >>> STYLO DIST FUNCTIONS
  "cosine",      # important
  "delta",       # important
  "argamon",
  "eder",        # important
  "minmax",      # important
  "enthropy",
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
  "mcquitty",
  #"complete",
  "ward.D2"      # important
)



for (distanceMethod in distanceMethodV){
  for (clusteringMethod in clusteringMethodV){
    for (featureType in featureTypeV){
      for (nGramSize in nGramSizeV){
        for (mffLimit in mffLimitV){
          for (culling in cullingV){
            # GRAPH CODE GOES HERE
            message(distanceMethod,"::",clusteringMethod,"::",featureType,"::",nGramSize,"::",mffLimit,"::",culling)
          }
        }
      }
    }
  }
}

###############################################################
hindawi65 <- "/Users/romanovienna/_ADHFANIS_Data/chapter_3/optimal_parameters/Hindawi_Corpus_65/"
hindawi65files <- unlist(list.files(hindawi65))

hindawi65df <- as.data.frame(hindawi65files)

hindawi65df <- hindawi65df %>%
  mutate(authors = str_replace_all(hindawi65files, "_[\\w\\.]+$", "")) %>%
  group_by(authors) %>%
  count()

"
  authors                        n
  <chr>                      <int>
1 1331JurjiZaydan               22
2 1342MustafaLutfiManfaluti      5
3 1349JibranKhalilJibran         2
4 1368CaliJarim                 10
5 1368IbrahimCabdQadirMazini     6
6 1373NiqulaHaddad               9
7 1375MuhammadHusaynHaykal       3
8 1392TahaHusayn                 8
"

