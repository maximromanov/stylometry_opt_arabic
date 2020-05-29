# ANALYSIS OF PARAMETERS

library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)

set.seed(786)


# LOAD TEST RESULST

setwd("/Users/romanovienna/_ADHFANIS_Data/chapter_3/stylometry_opt_arabic/")

resultsFolder <- "./clustering_test/"

resultsFiles <- list.files(resultsFolder)
resultsGraphsFolder <- "./graph_results/"

# GENERATE GRAPHS

# load file by file
# generate file name
# process data > generate 

for(file in resultsFiles){
  newFileBase = str_replace_all(file, "\\.csv", "")
  data <- read_delim(paste0(resultsFolder, file), "\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)

  total <- data %>%
    select(author_book) %>%
    unique %>% filter(author_book != "NO_DATA") %>%
    nrow
  
  # PROCESSING DATA
  dataSumm <- data %>%
    unique %>%
    select(-author_book, -clustering_method, book_cluster_match,
           author_cluster_match, distance_method,
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
        scale = 1, width = 15, height = 5,
        units = "in"
      )    
    }
  }
}


###############################################################
# hindawi65 <- "/Users/romanovienna/_ADHFANIS_Data/chapter_3/optimal_parameters/Hindawi_Corpus_65/"
# hindawi65 <- "/Users/romanovienna/Dropbox/5.Capta/Scraping/Hindawi_selected/corpus300/"
# hindawi65files <- unlist(list.files(hindawi65))
# 
# hindawi65df <- as.data.frame(hindawi65files)
# hindawi300 <- as.data.frame(hindawi65files)
# 
# hindawi65df <- hindawi65df %>%
#   mutate(authors = str_replace_all(hindawi65files, "_[\\w\\.]+$", "")) %>%
#   group_by(authors) %>%
#   count()
# 
# sum(hindawi65df$n)


#  authors                        n
#  <chr>                      <int>
#1  1331JurjiZaydan               22 (38)
#2  1342MustafaLutfiManfaluti      5
#3  1349JibranKhalilJibran         2
#4  1368CaliJarim                 10 (12)
#5  1368IbrahimCabdQadirMazini     6 (21 total)
#6  1373NiqulaHaddad               9 (12)
#7  1375MuhammadHusaynHaykal       3 (16)
#8  1392TahaHusayn                 8 (50 total)
#=====
#9  1373AhmadAmin.................25
#10 1383CabbasMahmudCaqqad........87
#11 1377SalamaMusa................35
#12 1381MarunCabbud...............20

# library(tidyverse)
# library(readr)
# 
# metadata_books <- read_delim("/Users/romanovienna/_ADHFANIS_Data/chapter_3/Hindawi_300/metadata_books_300.csv", 
#                              "\t", escape_double = FALSE, trim_ws = TRUE)
# 
# selectedAuthors <- metadata_books %>%
#   group_by(authorURI) %>%
#   count() %>%
#   arrange(desc(n))
# 
# sum(selectedAuthors$n)
# 
# knitr::kable(selectedAuthors, format="markdown")
# 
# metadata_books %>% select(-authorURI, -BookID, -BookTitle) %>%
#   knitr::kable(format="markdown")
# 
# updated <- metaLight %>%
#   left_join(selectedAuthors) %>%
#   filter(!is.na(n)) %>%
#   filter(WORD_COUNT >= 15000) %>%
#   arrange(authorURI, desc(WORD_COUNT)) %>%
#   group_by(authorURI) %>% top_n(25, WORD_COUNT) %>%
#   select(-AuthorID, -n)
# 
# 
# check <- updated %>%
#   group_by(authorURI) %>%
#   count() %>%
#   #filter(n >= 5) %>%
#   arrange(desc(n))
# 
# hindawi300 <- hindawi65df %>%
#   mutate(BookURI_STYLO = str_replace_all(hindawi65files, "\\.txt", "")) %>%
#   select(BookURI_STYLO)
# 
# updated300 <- updated %>% 
#   right_join(hindawi300)
#   
# 
# write_delim(updated300, "Dropbox/5.Capta/Scraping/Hindawi_selected/metadata_books_300.csv", delim = "\t", col_names = TRUE)
# 
# 
# 
# 
