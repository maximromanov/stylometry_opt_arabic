# ANALYSIS OF PARAMETERS

library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)

set.seed(786)


# LOAD TEST RESULST

setwd("/Users/romanovienna/_ADHFANIS_Data/chapter_3/stylometry_opt_arabic/")

resultsFolder <- "./Hindawi_C65_ResultsMerged/"
resultsChar <- c("Hindawi_C65_c_wurzburg.csv", "Hindawi_C65_c_argamon.csv", "Hindawi_C65_c_binary.csv",
               "Hindawi_C65_c_canberra.csv", "Hindawi_C65_c_cosine.csv", "Hindawi_C65_c_delta.csv",
               "Hindawi_C65_c_eder.csv", "Hindawi_C65_c_entropy.csv", "Hindawi_C65_c_euclidean.csv",
               "Hindawi_C65_c_manhattan.csv", "Hindawi_C65_c_maximum.csv", "Hindawi_C65_c_minkowski.csv",
               "Hindawi_C65_c_minmax.csv", "Hindawi_C65_c_simple.csv")

resultsFiles <- list.files(resultsFolder)
resultsGraphsFolder <- "./graph_results/"

# GENERATE GRAPHS

# load file by file
# generate file name
# process data > generate 

for(file in resultsFiles){
  newFileBase = str_replace_all(file, "\\.csv", "")
  data <- read_delim(paste0(resultsFolder, file), "\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
  total <- length(unique(data$author_book))-1 # -1 is to remove "NO_DATA" value
  
  # PROCESSING DATA
  dataSumm <- data %>%
    select(-book_match, -book_dist_match, -author_dist_match) %>%
    distinct() %>%
    select(-author_book, -clustering_method, book_cluster_match,
           author_cluster_match, distance_method, -dist_value,
           feature, mff, culling, -slices_compared, slice_len) %>%
    group_by(distance_method, feature, mff, culling, slice_len) %>%
    summarize(b_cl = sum(book_cluster_match),
              a_cl = sum(author_cluster_match),
              b_cl_rel = sum(book_cluster_match)/total*100,
              a_cl_rel = sum(author_cluster_match)/total*100
    ) %>%
    mutate(settings = paste(distance_method, feature, mff)) %>%
    ungroup() %>%
    select(settings, culling, slice_len, starts_with(c("b_", "a_")))
  
  dataSumRelTidy <- dataSumm %>%
    select(-b_cl, -a_cl) %>%
    gather(types, values, -settings, -culling, -slice_len)
  
  for (cull in unique(dataSumRelTidy$culling)){
    newFileBaseCull <- paste0(newFileBase,"_cull",str_pad(cull, 2, pad = "0"))
    message(newFileBaseCull)
    dataSumRelTidy %>%
      filter(culling == cull) %>%
      ggplot(aes(x=slice_len, y=values))+
      geom_segment(aes(x = slice_len, xend=slice_len, y=0, yend=values, col=types, alpha=if_else(values==100, 1, 0)))+
      geom_point(aes(col=types), size=1)+
      scale_color_manual(values = c("grey", "black")) +
      #geom_smooth(span=0.8)+
      facet_wrap(~settings, ncol=5) + 
      theme_minimal()+ # tufte/void has better fonts, but minimal is more readable because it has the grid
      labs(x = paste0("length of compared slices (culling ", cull, "%)"),
           y = paste0("percentage of successful matches (culling ", cull, "%)")) +
      theme(legend.position="none")
    
    for (format in c("pdf", "svg")){
      ggsave(
        filename = paste0(resultsGraphsFolder,"/",format,"/", newFileBaseCull, ".", format),
        plot = last_plot(),
        device = format,
        scale = 1, width = 15, height = 21,
        units = "in"
      )    
    }
  }
}


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


#  authors                        n
#  <chr>                      <int>
#1 1331JurjiZaydan               22
#2 1342MustafaLutfiManfaluti      5
#3 1349JibranKhalilJibran         2
#4 1368CaliJarim                 10
#5 1368IbrahimCabdQadirMazini     6
#6 1373NiqulaHaddad               9
#7 1375MuhammadHusaynHaykal       3
#8 1392TahaHusayn                 8


