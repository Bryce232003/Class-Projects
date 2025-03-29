library(tidyverse)
library(tidytext)
library(scales)
library(readr)
library(wordcloud2)
library(stylo)
library(networkD3)

# ---------- HARRY POTTER BOOKS ----------

#--- Importing All The Harry Potter Texts Needed ---
HarryPotterSeries <- read_csv("HarryPotterSeries.txt")


HP1 <- read_csv("Rowling_HP1.txt")
HP2 <- read_csv("Rowling_HP2.txt")
HP3 <- read_csv("Rowling_HP3.txt")
HP4 <- read_csv("Rowling_HP4.txt")
HP5 <- read_csv("Rowling_HP5.txt")
HP6 <- read_csv("Rowling_HP6.txt")
HP7 <- read_csv("Rowling_HP7.txt")



# --- Entire Series With/Without Stopwords ---

hp_series_stops <- HarryPotterSeries %>%
  count(word,sort=TRUE)

hp_series_no_stops <- HarryPotterSeries %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)



# --- Tidying Individual Texts ---
HP1_tidy <- HP1 %>%
  unnest_tokens(word,`CHAPTER ONE`)

HP2_tidy <- HP2 %>%
  unnest_tokens(word,`Chapter 1`)

HP3_tidy <- HP3 %>%
  unnest_tokens(word,`Chapter 1`)

HP4_tidy <- HP4 %>%
  unnest_tokens(word,`Chapter 1`)

HP5_tidy <- HP5 %>%
  unnest_tokens(word,`Chapter 1`)

HP6_tidy <- HP6 %>%
  unnest_tokens(word,`CHAPTER 1`)

HP7_tidy <- HP7 %>%
  unnest_tokens(word,`CHAPTER ONE`)



# --- Individual HP Books With/Without Stopwords --- 

#--Book1--
hp_book1_stops <- HP1_tidy %>%
  count(word,sort=TRUE)

hp_book1_no_stops <- HP1_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)


#--Book2--
hp_book2_stops <- HP2_tidy %>%
  count(word,sort=TRUE)

hp_book2_no_stops <- HP2_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)


#--Book3--
hp_book3_stops <- HP3_tidy %>%
  count(word,sort=TRUE)

hp_book3_no_stops <- HP3_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)

#--Book4--
hp_book4_stops <- HP4_tidy %>%
  count(word,sort=TRUE)

hp_book4_no_stops <- HP4_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)


#--Book5--
hp_book5_stops <- HP5_tidy %>%
  count(word,sort=TRUE)

hp_book5_no_stops <- HP5_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)


#--Book6--
hp_book6_stops <- HP6_tidy %>%
  count(word,sort=TRUE)

hp_book6_no_stops <- HP6_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)


#--Book7--
hp_book7_stops <- HP7_tidy %>%
  count(word,sort=TRUE)

hp_book7_no_stops <- HP7_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)



# --- Top 10 Words Per Book W/ Plots ---
#--Book1--
hp_book1_no_stop_10 <- top_n(hp_book1_no_stops,10)

ggplot(hp_book1_no_stop_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()              

hp_book1_stops_10 <- top_n(hp_book1_stops,10)

ggplot(hp_book1_stops_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()              


#--Book2--
hp_book2_no_stop_10 <- top_n(hp_book2_no_stops,10)

ggplot(hp_book2_no_stop_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  

hp_book2_stops_10 <- top_n(hp_book2_stops,10)

ggplot(hp_book2_stops_10,aes(word,n)) +    
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  

#--Book3--
hp_book3_no_stop_10 <- top_n(hp_book3_no_stops,10)

ggplot(hp_book3_no_stop_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()

hp_book3_stops_10 <- top_n(hp_book3_stops,10)

ggplot(hp_book3_stops_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  


#--Book4--
hp_book4_no_stop_10 <- top_n(hp_book4_no_stops,10)

ggplot(hp_book4_no_stop_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  

hp_book4_stops_10 <- top_n(hp_book4_stops,10)

ggplot(hp_book4_stops_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  

#--Book5--
hp_book5_no_stop_10 <- top_n(hp_book5_no_stops,10)

ggplot(hp_book5_no_stop_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  

hp_book5_stops_10 <- top_n(hp_book5_stops,10)

ggplot(hp_book5_stops_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  

#--Book6--
hp_book6_no_stop_10 <- top_n(hp_book6_no_stops,10)

ggplot(hp_book6_no_stop_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  

hp_book6_stops_10 <- top_n(hp_book6_stops,10)

ggplot(hp_book6_stops_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  

#--Book7--
hp_book7_no_stop_10 <- top_n(hp_book7_no_stops,10)

ggplot(hp_book7_no_stop_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  

hp_book7_stops_10 <- top_n(hp_book7_stops,10)

ggplot(hp_book7_stops_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()  

#--All Books Combined--
hp_series_no_stop_10 <- top_n(hp_series_no_stops,10)

ggplot(hp_series_no_stop_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()

hp_series_stops_10 <- top_n(hp_series_stops,10)

ggplot(hp_series_stops_10,aes(word,n)) +     
  geom_col()+              
  xlab(NULL)+               
  ylab(NULL)+              
  coord_flip()


#-Wordcloud Of Book3-
wordcloud2(hp_book3_no_stops)

# ---------- PERCY JACKSON BOOKS ----------

PJ1 <- read_csv("Riordan_PJ1.txt")

PJ2 <- read_csv("Riordan_PJ2.txt")

PJ3 <- read_csv("Riordan_PJ3.txt")

PJ4 <- read_delim("Riordan_PJ4.txt", 
                  delim = "\t", escape_double = FALSE,
                  trim_ws = TRUE)

PJ5 <- read_csv("Riordan_PJ5.txt")


# --- Tidying Individual Texts ---
PJ1_tidy <- PJ1 %>%
  unnest_tokens(word,`THE LIGHTNING THIEF`)

PJ2_tidy <- PJ2 %>%
  unnest_tokens(word,`Copyright © 2006 by Rick Riordan`)

PJ3_tidy <- PJ3 %>%
  unnest_tokens(word,`THE TITAN'S CURSE`)

PJ4_tidy <- PJ4 %>%
  unnest_tokens(word,'To Becky, who always guides me through the maze')

PJ5_tidy <- PJ5 %>%
  unnest_tokens(word,`Copyright © 2009 by Rick Riordan`)


# --- Individual HP Books With/Without Stopwords --- 

#--Book1--
pj_book1_stops <- PJ1_tidy %>%
  count(word,sort=TRUE)

pj_book1_no_stops <- PJ1_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)


#--Book2--
pj_book2_stops <- PJ2_tidy %>%
  count(word,sort=TRUE)

pj_book2_no_stops <- PJ2_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)


#--Book3--
pj_book3_stops <- PJ3_tidy %>%
  count(word,sort=TRUE)

pj_book3_no_stops <- PJ3_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)


#--Book4--
pj_book4_stops <- PJ4_tidy %>%
  count(word,sort=TRUE)

pj_book4_no_stops <- PJ4_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)


#--Book5--
pj_book5_stops <- PJ5_tidy %>%
  count(word,sort=TRUE)

pj_book5_no_stops <- PJ5_tidy %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words)


# --- Comparing PJ Books 1-3 With HP Books 1-3 Jittered Scatterplots ---

#-- Book 1s --

#-Getting Percents-
pj_book1_percent <-pj_book1_no_stops %>%
  mutate(
    pj_book1_percent = pj_book1_no_stops$n/sum(pj_book1_no_stops$n)
  )
hp_book1_percent <-hp_book1_no_stops %>%
  mutate(
    hp_book1_percent = hp_book1_no_stops$n/sum(hp_book1_no_stops$n)
  )

#-Joining Them Together-
Book1_Comparison <- full_join(hp_book1_percent, pj_book1_percent,
                             by = c("word"))

#-Cutting Out The Unnecessary Columns-
Book1_Percents <-subset(Book1_Comparison, select = -c(n.x,n.y) )

#-Creating The Graph-
Book1_Scatter_Plot <- ggplot(Book1_Percents, aes(x = hp_book1_percent, y = pj_book1_percent, 
                            color = abs(hp_book1_percent - pj_book1_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4",
                       high = "black") +
  theme(legend.position="none") +
  labs(y = "Percy Jackson Book 1", x = "Harry Potter Book 1")
Book1_Scatter_Plot

#-- Book 2s --

#-Getting Percents-
pj_book2_percent <-pj_book2_no_stops %>%
  mutate(
    pj_book2_percent = pj_book2_no_stops$n/sum(pj_book2_no_stops$n)
  )
hp_book2_percent <-hp_book2_no_stops %>%
  mutate(
    hp_book2_percent = hp_book2_no_stops$n/sum(hp_book2_no_stops$n)
  )

#-Joining Them Together-
Book2_Comparison <- full_join(hp_book2_percent, pj_book2_percent,
                              by = c("word"))

#-Cutting Out The Unnecessary Columns-
Book2_Percents <-subset(Book2_Comparison, select = -c(n.x,n.y) )

#-Creating The Graph-
Book2_Scatter_Plot <- ggplot(Book2_Percents, aes(x = hp_book2_percent, y = pj_book2_percent, 
  color = abs(hp_book2_percent - pj_book2_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "pink",
                       high = "navy") +
  theme(legend.position="none") +
  labs(y = "Percy Jackson Book 2", x = "Harry Potter Book 2")
Book2_Scatter_Plot


#-- Book 3s --

#-Getting Percents-
pj_book3_percent <-pj_book3_no_stops %>%
  mutate(
    pj_book3_percent = pj_book3_no_stops$n/sum(pj_book3_no_stops$n)
  )
hp_book3_percent <-hp_book3_no_stops %>%
  mutate(
    hp_book3_percent = hp_book3_no_stops$n/sum(hp_book3_no_stops$n)
  )

#-Joining Them Together-
Book3_Comparison <- full_join(hp_book3_percent, pj_book3_percent,
                              by = c("word"))

#-Cutting Out The Unnecessary Columns-
Book3_Percents <-subset(Book3_Comparison, select = -c(n.x,n.y) )

#-Creating The Graph-
Book3_Scatter_Plot <- ggplot(Book3_Percents, aes(x = hp_book3_percent, y = pj_book3_percent, 
                                                 color = abs(hp_book3_percent - pj_book3_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "lightblue",
                       high = "darkgreen") +
  theme(legend.position="none") +
  labs(y = "Percy Jackson Book 3", x = "Harry Potter Book 3")
Book3_Scatter_Plot


# --- Hapax for books 1-3 ---
#--HP Books--
#-Book1-
hp1_tidy_5k <- head(HP1_tidy,5000)

hp_book1_5k_stops <- hp1_tidy_5k %>%
  count(word,sort=TRUE)

hp_book1_hapax <- hp_book1_5k_stops[hp_book1_5k_stops$n == 1,]

#-Book2-
hp2_tidy_5k <- head(HP2_tidy,5000)

hp_book2_5k_stops <- hp2_tidy_5k %>%
  count(word,sort=TRUE)

hp_book2_hapax <- hp_book2_5k_stops[hp_book2_5k_stops$n == 1,]

#-Book3-
hp3_tidy_5k <- head(HP3_tidy,5000)

hp_book3_5k_stops <- hp3_tidy_5k %>%
  count(word,sort=TRUE)

hp_book3_hapax <- hp_book3_5k_stops[hp_book3_5k_stops$n == 1,]

#--Percy Jackson Books--
#-Book1-
pj1_tidy_5k <- head(PJ1_tidy,5000)

pj_book1_5k_stops <- pj1_tidy_5k %>%
  count(word,sort=TRUE)

pj_book1_hapax <- pj_book1_5k_stops[pj_book1_5k_stops$n == 1,]

#-Book2-
pj2_tidy_5k <- head(PJ2_tidy,5000)

pj_book2_5k_stops <- pj2_tidy_5k %>%
  count(word,sort=TRUE)

pj_book2_hapax <- pj_book2_5k_stops[pj_book2_5k_stops$n == 1,]

#-Book3-
pj3_tidy_5k <- head(PJ3_tidy,5000)

pj_book3_5k_stops <- pj3_tidy_5k %>%
  count(word,sort=TRUE)

pj_book3_hapax <- pj_book3_5k_stops[pj_book3_5k_stops$n == 1,]

#--- TTR ---
#No need to use R for this, can calculate by looking at the frames
          #--HP Books--
#-Book1-
# 1214/5000 = .2428

#-Book2-
# 1392/5000 = .2784

#-Book3-
# 1454/5000 = .2908

          #--PJ Books--
#-Book1-
# 1348/5000 = .2696

#-Book2-
# 1522/5000 = .3044

#-Book3-
# 1315/5000 = .2630

# ---------- TEXTS TO ANALYSE ----------

GC <- read_csv("Galbraith_Cuckoo.txt")

HPL <- read_csv("Rowling_Leopard.txt")

stylo()
stylo.network()
