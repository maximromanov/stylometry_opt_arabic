# ANALYSIS OF PARAMETERS

library(readr)
library(tidyverse)
library(ggplot2)

# LOAD TEST RESULST

setwd("/Users/romanovienna/_ADHFANIS_Data/chapter_3/stylometry_opt_arabic/")

resultsFolder <- "./Hindawi_C65_ResultsMerged/"
resultsChar <- c("Hindawi_C65_c_wurzburg.csv", "Hindawi_C65_c_argamon.csv", "Hindawi_C65_c_binary.csv",
               "Hindawi_C65_c_canberra.csv", "Hindawi_C65_c_cosine.csv", "Hindawi_C65_c_delta.csv",
               "Hindawi_C65_c_eder.csv", "Hindawi_C65_c_entropy.csv", "Hindawi_C65_c_euclidean.csv",
               "Hindawi_C65_c_manhattan.csv", "Hindawi_C65_c_maximum.csv", "Hindawi_C65_c_minkowski.csv",
               "Hindawi_C65_c_minmax.csv", "Hindawi_C65_c_simple.csv")

data <- read_delim(file, "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

colnames(data) <- c("author_book", "clustering_method", "book_cluster_match",
                            "author_cluster_match", "distance_method", "dist_value",
                            "book_match", "book_dist_match", "author_dist_match",
                            "feature", "mff", "culling", "slices_compared", "slice_len")


data <- read_delim(paste0(resultsFolder, resultsChar[7]), "\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)

total <- length(unique(data$author_book))-1 # -1 is to remove "NO_DATA" value

test <- data %>%
  filter(feature == "c6", slice_len == 200, culling == 20, mff == 100) %>%
  select(-book_match, -book_dist_match, -author_dist_match) %>%
  distinct()

sum(test$book_cluster_match)
sum(test$author_cluster_match)

dataSumm <- data %>%
  distinct() %>%
  select(-author_book, -clustering_method, book_cluster_match,
         author_cluster_match, distance_method, -dist_value,
         -book_match, book_dist_match, author_dist_match,
         feature, mff, culling, -slices_compared, slice_len) %>%
  group_by(distance_method, feature, mff, culling, slice_len) %>%
  summarize(b_cl = sum(book_cluster_match),
            a_cl = sum(author_cluster_match),
            b_di = sum(book_dist_match),
            a_di = sum(author_dist_match),
            b_cl_rel = sum(book_cluster_match)/total,
            a_cl_rel = sum(author_cluster_match)/total,
            b_di_rel = sum(book_dist_match)/total,
            a_di_rel = sum(author_dist_match)/total
            ) %>%
  mutate(settings = paste(distance_method, feature, mff)) %>%
  ungroup() %>%
  select(settings, culling, slice_len, starts_with(c("b_", "a_")))

dataSumRelTidy <- dataSumm %>%
  select(-b_cl, -a_cl, -b_di, -a_di) %>%
  gather(types, values, -settings, -culling, -slice_len)


dataSumRelTidy %>%
  filter(culling == 0) %>%
  ggplot(aes(x=slice_len, y=values))+
  geom_line(aes(col=types))+
  facet_wrap(~settings, ncol=6)
 
ggsave(
  filename = "test.pdf",
  plot = last_plot(),
  device = "pdf",
  scale = 1,
  width = 15,
  height = 30,
  units = "in",
  dpi = 300
)

#fileTitle <- head(str_split(fileName, "\\.")[[1]], 1)
#
# results %>%
#   #filter(mff==mffVar) %>%
#   ggplot(aes(x=chunk, y=value))+
#   geom_point(alpha=0.1, size=0.75)+
#   geom_smooth(span=0.8)+
#   facet_wrap(~mff, nrow = 1) +
#   scale_x_reverse() +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
#   ggtitle(paste0(fileTitle, "; F: ", featureType, "; NG: ", nGramSize,
#                  "; CULL: ", culling, "; DIST: ", distanceMethod, ": Ch: ", nChunks, "; LRG: ", max(lengthOfChunksSeq)))
# 
# fileNameNew = paste0(str_replace(fileName, "\\.\\w+", ""),
#                      "_F_", featureType, "_NG_", nGramSize,
#                      "_CULL_", culling, "_DIST_", distanceMethod,
#                      "_Ch_", nChunks, "_LRG_", max(lengthOfChunksSeq), ".pdf")
# ggsave(
#   filename = paste0(savePath,fileNameNew),
#   plot = last_plot(),
#   device = "pdf",
#   scale = 1,
#   width = 15,
#   height = 5,
#   units = "in",
#   dpi = 300
# )





# for (distanceMethod in distanceMethodV){
#   for (clusteringMethod in clusteringMethodV){
#     for (featureType in featureTypeV){
#       for (nGramSize in nGramSizeV){
#         for (mffLimit in mffLimitV){
#           for (culling in cullingV){
#             # GRAPH CODE GOES HERE
#             message(distanceMethod,"::",clusteringMethod,"::",featureType,"::",nGramSize,"::",mffLimit,"::",culling)
#           }
#         }
#       }
#     }
#   }
# }

###############################################################
# hindawi65 <- "/Users/romanovienna/_ADHFANIS_Data/chapter_3/optimal_parameters/Hindawi_Corpus_65/"
# hindawi65files <- unlist(list.files(hindawi65))
# 
# hindawi65df <- as.data.frame(hindawi65files)
# 
# hindawi65df <- hindawi65df %>%
#   mutate(authors = str_replace_all(hindawi65files, "_[\\w\\.]+$", "")) %>%
#   group_by(authors) %>%
#   count()

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

