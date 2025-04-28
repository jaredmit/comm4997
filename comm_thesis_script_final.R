Project_name<-"presspeech"                   

#@@@@@@@@@@@@@@@@
#### IMPORTS ####
#@@@@@@@@@@@@@@@@

options(scipen=999)

library(readr) #for reading files
library(sysfonts) #for fonts
library(showtext) #for fonts
library(foreign) # for the elbow searching
library(topicmodels) #running topic modeling
library(ldatuning) #choosing K
library(doParallel) # another paralleling
library(ggplot2) # plotting
library(tidyverse) #tidy
library(lubridate) # dealing with dates
library(tidytext) # DICTIONARIES
library(cld2) # for lang recog
library(dplyr) # deplyr
library(tidyr)
library(stringi) #strings
library(stringr) # strings
library(igraph) #nets (even though it is imported in func?)
library(scales)
library(quanteda)
require(parallel)
require(stopwords)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### LOAD & FORMAT DATA #####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data<- read.csv("ihe_ctc_final.csv")

# remove the CSV id variable
data <- data[,-1]
colnames(data)

# assign a new index
data$index<-(1:nrow(data))

# convert the content to text column
data$text<-as.character(data$ctc_text)

# calculate and examine text length
data$nwords <- str_count(data$text, "\\w+")

nrow(data)
length(which(data$nwords==0))
length(which(data$nwords<3))
length(which(data$nwords<50))
length(which(data$nwords<200))

# in this case 2 and up seems fine
data3<-data[which(data$nwords>2),]                         

#### PRE-PROCESSING ####

# stopwords to keep editiing based on next lines
mystopwords<-c(stopwords("en"),stopwords(source = "smart"),"http","https") 

# create tidy tokens
tidy_data<-data3 %>%
  unnest_tokens(word, text) %>% # tokenizing
  # option 1 REGEX --># Be careful this is deangerous as it gets word parts
  # option 2 Exact Match -->
  anti_join(data.frame(word=mystopwords)) %>%
  mutate(nchar=nchar(word)) %>% #counting the number of letters
  filter(nchar>2) %>% # remove from minumum number of chars
  filter(!grepl("[0-9]{1}", word)) %>% # removing numbers 
  filter(!grepl("\\W", word))  # removing any word containing non letter/number 

# choose top words by tf-idf                            
maxndoc=0.5
minndoc=0.001

# filter to tokens not too common and not too rare
templength<-length(unique(tidy_data$index))

good_common_words <- tidy_data %>%
  count(index, word, sort = TRUE) %>%
  group_by(word) %>%
  summarize(doc_freq=n()/templength) %>%
  filter(doc_freq<maxndoc) %>%
  filter(doc_freq>minndoc)

# clean tidy to fit the tokens - NOTE: this is where you might lost indexes
tidy_data_pruned<-tidy_data %>% inner_join(good_common_words)

tidy_data_pruned %>%
  group_by(word) %>%
  dplyr::summarise(n=n()) %>%
  arrange(desc(n)) %>%
  mutate(word = reorder(word, n)) %>%
  top_n(50) %>%    
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# STOP HERE - check words - correct stopwords - continue only if happy

# DFM it (termdoc)
tidy_dfm <- tidy_data_pruned %>%
  count(index, word) %>%
  cast_dfm(index, word, n)

# feed to lda object

full_data<- convert(tidy_dfm, to = "topicmodels")

# run final model
fitted <- LDA(full_data, k = 20, method = "Gibbs",
              control = list(seed=1217))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### MAKE FREQUENCY TABLES ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

LDAfit <- fitted
nwords <-100 # number of words/texts you want
ntext <-50 # number of texts you want
mydata<-data3 # change if you used for your corpus some other data like data3, trump or X


get_top_Words<-function(LDAmodel) {
  
  ## extract the beta (termXtopic) matrix
  mybeta<-data.frame(LDAmodel@beta)
  print("hi")
  ## input the words
  colnames(mybeta)<-LDAmodel@terms
  ## transpose to see it more comfortably
  mybeta<-t(mybeta)
  ## give colnames
  colnames(mybeta)<-seq(1:ncol(mybeta))
  ## exponenciate the values back as software logs them
  mybeta=exp(mybeta)
  ## Convert to dataframe 
  mybeta<-data.frame(mybeta)
  # create a container (just a table with ncols being topics, and nrows being nwords)
  topwords <- mybeta[1:nwords,]
  
  ## lopping over all topics
  for (i in 1:LDAmodel@k) {
    print(i)
    ## grab beta, select the tight column, and choose top_n based on nwords
    tempframe <- mybeta %>% select(i) %>% arrange(desc(1)) %>% top_n(nwords)
    ## now that the order is correct - grab and vectorize words (rownames)
    tempvec<-as.vector(rownames(tempframe))
    ## plug in to the i'th column (1 for topic 1, 2 for 2 etc)
    topwords[,i]<-tempvec[1:nwords]
    print("here")
  }
  
  # add row names to look pretty
  rownames(topwords)<-c(1:nwords)
  
  # write to excel
  write.csv(topwords,paste0("thesis", "_Topwordsk_", LDAmodel@k, ".csv"))
  return(topwords)
}

get_FREX_Words<-function(LDAmodel) {
  # apply formula below from Roberts et al.
  mybeta<-data.frame(LDAmodel@beta)
  colnames(mybeta)<-LDAmodel@terms
  mybeta<-t(mybeta)
  colnames(mybeta)<-seq(1:ncol(mybeta))
  mybeta=exp(mybeta)
  
  # apply formula below
  myw=0.3
  word_beta_sums<-rowSums(mybeta)
  my_beta_for_frex<-mybeta
  for (m in 1:ncol(my_beta_for_frex)) {
    for (n in 1:nrow(my_beta_for_frex)) {
      my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
    }
    print (m)
  }
  
  # print 50 frex:
  topwords <- my_beta_for_frex[1:nwords,]
  
  for (i in 1:LDAmodel@k) {
    tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
    tempframe <- tempframe[1:nwords,]
    tempvec<-as.vector(rownames(tempframe))
    topwords[,i]<-tempvec[1:nwords]
  }

  rownames(topwords)<-c(1:nwords)
  write.csv(topwords,paste0("thesis", "_TopFREXk_", LDAmodel@k, ".csv"))
  return(topwords)
}

get_top_texts<-function(LDAmodel) {
  
  textcolumn=which(colnames(mydata)=="text")
  
  if (nrow(mydata)==LDAmodel@Dim[1]) {
    meta_theta_df<-cbind(mydata[textcolumn],LDAmodel@gamma)
  } else {
    # if not - we have docs that were deleted and we need to match them:
    ## creating backup object we can work with
    data_short<-mydata
    ## we find what documents are not in both index lists (from LDA and orginal data)
    missing_docs<-setdiff(as.character(mydata$index),as.character(LDAmodel@documents))
    
    ## we filter out all docs not in both
    data_short<-data_short %>% dplyr::filter(!(as.character(data_short$index) %in% missing_docs))
    data_short<-data_short[,c(which(colnames(mydata)=="index"),textcolumn)]
    
    ## now we can add together the gamma and data (notice we only add TEXt from original data)
    TEMP_gamma<-(cbind.data.frame(LDAmodel@documents,LDAmodel@gamma))
    
    colnames(TEMP_gamma)[1]<-"index"
    meta_theta_df<-data_short %>% left_join((TEMP_gamma))
    meta_theta_df<-meta_theta_df[,-c(which(colnames(meta_theta_df)=="index"))]
    
  }
  
  mybeta<-data.frame(LDAmodel@beta)
  colnames(mybeta)<-LDAmodel@terms
  mybeta<-t(mybeta)
  colnames(mybeta)<-seq(1:ncol(mybeta))
  mybeta=exp(mybeta)
  
  # create container
  toptexts <- data.frame(mybeta[1:ntext,])
  
  # and loop for all topics
  for (i in 1:LDAmodel@k) {
    print(i)
    # make temp frames ordered by document topic loading
    # note that i+1 because first col is text, 1st topic is in col 2 etc
    tempframe <- meta_theta_df[order(-meta_theta_df[[i+1]]),]
    # cut to the right amount
    tempframe <- tempframe[1:ntext,]
    # vectorize the text column (which is now ordered correctly)
    tempvec<-as.vector(unlist(tempframe[,1]))
    # plug in to excel
    toptexts[,i]<-tempvec
  }
  
  # row names to make pretty
  rownames(toptexts)<-c(1:ntext)
  
  # write the file
  write.csv(topwords,paste0("thesis", "_TopTEXTSk_", LDAmodel@k, ".csv"))
  return(toptexts)
}

topwords<-get_top_Words(LDAfit)
topFREX<-get_FREX_Words(LDAfit)
toptexts<-get_top_texts(LDAfit)

#@@@@@@@@@@@@@@@@@@@@@@@@@@
##### ANTMN NETWORKING ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@

#LDAfit<-LDAlist[[2]]
data33<-data3
deleted_lda_texts<-(setdiff(as.character(data33$index), as.character(LDAfit@documents)))

'%!in%' <- function(x,y)!('%in%'(x,y))

data33<-data33[data33$index %!in% deleted_lda_texts,]
metadf<-data33
meta_theta_df<-cbind(metadf,LDAfit@gamma)

topic_size<-colSums(meta_theta_df[,c((ncol(data3)+1):(ncol(data3)+LDAfit@k))])      

# Enter by hand

topic_labels <- c(1:20) 
topic_labels <- c("Crisis Response",
                  "Safety",
                  "Remembrance",
                  "Labor",
                  "Budget",
                  "Higher Education",
                  "The Future",
                  "Governance",
                  "Curriculum",
                  "Financial Matters",
                  "Research",
                  "Race",
                  "Institutional Efforts",
                  "Free Speech",
                  "Diversity",
                  "Common Values",
                  "Initiatives",
                  "War",
                  "Global Reach",
                  "Ceremonies")

network_from_LDA<-function(LDAobject,deleted_topics=c(),topic_names=c(),save_filename="",topic_size=c(),bbone=FALSE) {
  # import needed packages
  require(lsa) # for cosine similarity calculation
  require(dplyr) # general utility
  require(igraph) # for graph/network managment and output
  require(corpustools)
  
  print("Importing model")
  
  # first extract the theta matrix form the topicmodel object
  theta<-LDAobject@gamma
  # add names for columns based on k
  colnames(theta)<-c(1:LDAobject@k)
  
  # calculate the adjacency matrix using cosine similarity on the theta matrix
  mycosine<-cosine(as.matrix(theta))
  colnames(mycosine)<-colnames(theta)
  rownames(mycosine)<-colnames(theta)
  
  # convert to network - undirected, weighted, no diagonal
  
  print("Creating graph")
  
  topmodnet<-graph.adjacency(mycosine,mode="undirected",weighted=T,diag=F,add.colnames="label") # Assign colnames
  # add topicnames as name attribute of node - imported from prepare meta data in previous lines
  if (length(topic_names)>0) {
    print("Topic names added")
    V(topmodnet)$name<-topic_names
  } 
  # add sizes if passed to function
  if (length(topic_size)>0) {
    print("Topic sizes added")
    V(topmodnet)$topic_size<-topic_size
  }
  newg<-topmodnet
  
  # delete 'garbage' topics
  if (length(deleted_topics)>0) {
    print("Deleting requested topics")
    
    newg<-delete_vertices(topmodnet, deleted_topics)
  }
  
  # backbone
  if (bbone==TRUE) {
    print("Backboning")
    
    nnodesBASE<-length(V(newg))
    for (bbonelvl in rev(seq(0,1,by=0.05))) {
      #print (bbonelvl)
      nnodes<-length(V(backbone_filter(newg,alpha=bbonelvl)))
      if(nnodes>=nnodesBASE) {
        bbonelvl=bbonelvl
        #  print ("great")
      }
      else{break}
      oldbbone<-bbonelvl
    }
    
    newg<-backbone_filter(newg,alpha=oldbbone)
    
  }
  
  # run community detection and attach as node attribute
  print("Calculating communities")
  
  mylouvain<-(cluster_louvain(newg)) 
  mywalktrap<-(cluster_walktrap(newg)) 
  myspinglass<-(cluster_spinglass(newg)) 
  myfastgreed<-(cluster_fast_greedy(newg)) 
  myeigen<-(cluster_leading_eigen(newg)) 
  
  V(newg)$louvain<-mylouvain$membership 
  V(newg)$walktrap<-mywalktrap$membership 
  V(newg)$spinglass<-myspinglass$membership 
  V(newg)$fastgreed<-myfastgreed$membership 
  V(newg)$eigen<-myeigen$membership 
  
  # if filename is passsed - saving object to graphml object. Can be opened with Gephi.
  if (nchar(save_filename)>0) {
    print("Writing graph")
    write_graph(newg,paste0(save_filename,".graphml"),format="graphml")
  }
  
  # graph is returned as object
  return(newg)
}


mynewnet<-network_from_LDA(LDAobject=LDAfit,
                           #deleted_topics=c(20,21,38),
                           deleted_topics=grep("DELETE",topic_labels),
                           save_filename="Uni_ALL_25",
                           #topic_names=topic_labels,
                           topic_size = topic_size, 
                           bbone=TRUE)


mynewnet_nojunk<-network_from_LDA(LDAobject=LDAfit,
                                  #deleted_topics=c(10,6,20,21,38),
                                  deleted_topics=grep("DELETE",topic_labels),
                                  save_filename="WF_NOJUNK_35",
                                  topic_names=topic_labels,
                                  topic_size = topic_size, 
                                  bbone=TRUE)


meta_theta_df_comm<-meta_theta_df

finalnet<-mynewnet
colnames(meta_theta_df_comm)
 
# change community detection to the one used # add more if needed
meta_theta_df_comm$comm1_poli<-rowSums(meta_theta_df_comm[,(as.numeric(V(finalnet)$label[which(V(finalnet)$eigen == 1)]))+ncol(data33)]) # color - name
meta_theta_df_comm$comm2_boil<-rowSums(meta_theta_df_comm[,(as.numeric(V(finalnet)$label[which(V(finalnet)$eigen == 2)]))+ncol(data33)]) # color - name
meta_theta_df_comm$comm3_race<-rowSums(meta_theta_df_comm[,(as.numeric(V(finalnet)$label[which(V(finalnet)$eigen == 3)]))+ncol(data33)]) # color - name
meta_theta_df_comm$comm4_inst<-rowSums(meta_theta_df_comm[,(as.numeric(V(finalnet)$label[which(V(finalnet)$eigen == 4)]))+ncol(data33)]) # color - name

#@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### TABLES & FIGURES #####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@

# prepare data for visualization

# import file 
meta_theta_df_comm <- read.csv("comm_results_final_testing_only.csv")

# recode topics
meta_theta_df_comm <- meta_theta_df_comm %>%
  rename("Crisis Response"=`X1`,
         "Safety"=`X2`,
         "Remembrance"=`X3`,
         "Labor"=`X4`,
         "Budget"=`X5`,
         "Higher Education"=`X6`,
         "The Future"=`X7`,
         "Governance"=`X8`,
         "Curriculum"=`X9`,
         "Financial Matters"=`X10`,
         "Research"=`X11`,
         "Race"=`X12`,
         "Institutional Efforts"=`X13`,
         "Free Speech"=`X14`,
         "Diversity"=`X15`,
         "Common Values"=`X16`,
         "Initiatives"=`X17`,
         "War"=`X18`,
         "Global Reach"=`X19`,
         "Ceremonies"=`X20`)

# re-format time periods for visualization
meta_theta_df_comm$ctc_pd_short <- stringr::str_wrap(meta_theta_df_comm$ctc_pd, 8)
meta_theta_df_comm$ctc_pd_short <- factor(meta_theta_df_comm$ctc_pd, 
                                          levels = c("Civil\nRights &\nVietnam\n(1964-1973)", 
                                                     "Reagan\n& South\nAfrica\nApartheid\n(1974-1986)", 
                                                     "Looking\nInward\n(1987-1992)",
                                                     "Sept.\n11 &\nRecession\n(1993-2011)",
                                                     "Trump &\nRacial\nJustice\n(2012-2022)",
                                                     "Israel-Hamas\nWar &\nDEI\n(2023-2024)"))
meta_theta_df_comm$ctc_pd_short <- meta_theta_df_comm$ctc_pd %>%
  recode("Civil Rights & Vietnam"="Civil Rights\n& Vietnam\n(1964-1973)", 
         "Reagan & South Africa Apartheid"="Reagan &\nSouth Africa\n(1974-1986)", 
         "Looking Inward"="Looking\nInward\n(1987-1992)",
         "Sept. 11 & Recession"="Sept.11 &\nRecession\n(1993-2011)",
         "Trump & Racial Justice"="Trump &\nRacial Justice\n(2012-2022)",
         "Israel-Hamas War & DEI"="Israel-Hamas\nWar & DEI\n(2023-2024)")

# re-format dates for visualization
meta_theta_df_comm$date_for_viz<-as.Date(meta_theta_df_comm$ctc_date,"%m/%d/%Y")

# calculate net state-level PEI and national-level PEI
meta_theta_df_comm$st_pei_net <- ifelse(meta_theta_df_comm$st_pei > 0, 1, ifelse(meta_theta_df_comm$st_pei < 0, -1, 0))
meta_theta_df_comm$us_pei_net <- ifelse(meta_theta_df_comm$us_pei > 0, 1, ifelse(meta_theta_df_comm$us_pei < 0, -1, 0))

# load in fonts and theme
font_add(family = "EB_Garamond", regular = "/users/jaredmitovich/Downloads/EB_Garamond/static/EBGaramond-Regular.ttf")
font_add_google("EB Garamond", "eb_garamond")
showtext_auto()
theme_set(theme_bw(base_family = "EB_Garamond", base_size = 18))
save.image(paste0(Project_name,"_AFTERNETOWRK.rdata"))

#@@TABLE@@#
#Proportion of Themes and Component Topics in Corpus of Contentious Topic Communications ###
#@@TABLE@@#

table_topics <- meta_theta_df_comm %>%
  summarise(Crisis_Response = mean(`Crisis Response`),
            Safety = mean(Safety),
            Remembrance = mean(Remembrance),
            Labor = mean(Labor),
            Budget = mean(Budget),
            Higher_Ed = mean(`Higher Education`),
            The_Future = mean(`The Future`),
            Governance = mean(Governance),
            Curriculum = mean(Curriculum),
            Financial_Matters = mean(`Financial Matters`),
            Research = mean(Research),
            Race = mean(Race),
            Institution = mean(`Institutional Efforts`),
            Free_Speech = mean(`Free Speech`),
            Diversity = mean(Diversity),
            Common_Values = mean(`Common Values`),
            Initiatives = mean(Initiatives),
            War = mean(War),
            Global_Reach = mean(`Global Reach`),
            Ceremonies = mean(Ceremonies))

#@@FIGURE@@#
#Proportion of Themes Present in Contentious Topic Communications Across Advent Time Periods ###
#@@FIGURE@@#

meta_theta_df_comm %>% na.omit() %>%
  group_by(ctc_pd_short) %>%  
  summarise(`Politics & Free Speech` = mean(comm1_poli),
            `Academic Boilerplate` = mean(comm2_boil),
            `Race & People` = mean(comm3_race),
            `Institutional Affairs` = mean(comm4_inst)) %>%
  pivot_longer(cols = c(`Politics & Free Speech`, `Academic Boilerplate`, `Race & People`, `Institutional Affairs`), 
               names_to = "category", 
               values_to = "value") %>%
  ggplot(aes(x = factor(ctc_pd_short,
                        levels=c("Civil Rights\n& Vietnam\n(1964-1973)", 
                                 "Reagan &\nSouth Africa\n(1974-1986)", 
                                 "Looking\nInward\n(1987-1992)",
                                 "Sept.11 &\nRecession\n(1993-2011)",
                                 "Trump &\nRacial Justice\n(2012-2022)",
                                 "Israel-Hamas\nWar & DEI\n(2023-2024)")), 
             y = value, 
             fill = category)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Politics & Free Speech" = "#66c2a5",
                               "Academic Boilerplate" = "#fc8d62", 
                               "Race & People" = "#8da0cb", 
                               "Institutional Affairs" = "#e78ac3")) +
  # Formatting
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = paste0(round(value,2)*100,"%")), 
            position = position_stack(vjust = 0.5), 
            color="white", 
            face="bold", 
            size = 4, 
            family="eb_garamond")+
  labs(title = "Thematic prominence across subperiods",
       x = "Subperiod",
       y = "Prominence",
       fill = "Theme") +
  theme_minimal(base_family = "eb_garamond", base_size=12) +
  theme(text = element_text(family = "eb_garamond"))


#@@FIGURE@@#
# Proportion of Selected Themes over Time (smoothed)
#@@FIGURE@@#

meta_theta_df_comm %>%   
  group_by(date_for_viz) %>%   
  summarise(`Politics & Free Speech` = mean(comm1_poli),
            `Race & People` = mean(comm3_race)) %>%
  pivot_longer(cols = c(`Politics & Free Speech`, `Race & People`), 
               names_to = "Theme", values_to = "Value") %>%
  ggplot(aes(x = date_for_viz, y = Value, color = Theme)) +
  
  # define ordered factor levels for proper legend order
  scale_fill_manual(
    name = "Subperiod",
    values = c(
      "Civil Rights & Vietnam" = "#f0d1d3",
      "Reagan & South Africa Apartheid" = "#fae3d7",
      "Looking Inward" = "#fdf7e4",
      "Sept. 11 & Recession" = "#f9fce8",
      "Trump & Racial Justice" = "#e8f4e5",
      "Israel-Hamas War & DEI" = "#d1e0ed"
    ),
    breaks = c(
      "Civil Rights & Vietnam",
      "Reagan & South Africa Apartheid",
      "Looking Inward",
      "Sept. 11 & Recession",
      "Trump & Racial Justice",
      "Israel-Hamas War & DEI"
    )
  ) +
  
  # shaded background for historical periods with ordered fill
  geom_rect(aes(xmin = as.Date("1964-01-01"), xmax = as.Date("1973-12-31"), 
                ymin = -Inf, ymax = Inf, fill = factor("Civil Rights & Vietnam", levels = c(
                  "Civil Rights & Vietnam",
                  "Reagan & South Africa Apartheid",
                  "Looking Inward",
                  "Sept. 11 & Recession",
                  "Trump & Racial Justice",
                  "Israel-Hamas War & DEI"
                ))), color=NA) +
  geom_rect(aes(xmin = as.Date("1974-01-01"), xmax = as.Date("1986-12-31"), 
                ymin = -Inf, ymax = Inf, fill = "Reagan & South Africa Apartheid"), color=NA) +
  geom_rect(aes(xmin = as.Date("1987-01-01"), xmax = as.Date("1992-12-31"), 
                ymin = -Inf, ymax = Inf, fill = "Looking Inward"), color=NA) +
  geom_rect(aes(xmin = as.Date("1993-01-01"), xmax = as.Date("2011-12-31"), 
                ymin = -Inf, ymax = Inf, fill = "Sept. 11 & Recession"), color=NA) +
  geom_rect(aes(xmin = as.Date("2012-01-01"), xmax = as.Date("2022-12-31"), 
                ymin = -Inf, ymax = Inf, fill = "Trump & Racial Justice"), color=NA) +
  geom_rect(aes(xmin = as.Date("2023-01-01"), xmax = as.Date("2024-12-31"), 
                ymin = -Inf, ymax = Inf, fill = "Israel-Hamas War & DEI"), color=NA) +
  
  # transparent geom lines with smooth trends
  geom_smooth(span = 0.1, se = FALSE) +
  
  # apply general theme
  theme_minimal(base_family = "eb_garamond", base_size = 16) +
  theme(text = element_text(family = "eb_garamond")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(limits = c(as.Date("1964-01-01"), as.Date("2024-12-31")),
               date_breaks = "5 years", 
               date_labels = "%Y") +
  
  # corrected legend: Use scale_color_manual() for line colors
  scale_color_manual(values = c(`Politics & Free Speech` = "#66c2a5",
                                `Race & People` = "#8da0cb")) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 0),
        legend.position = "right") +
  
  # titles and labels
  ggtitle("Prominence of Selected Themes") +
  ylab("Prominence") +
  xlab("Year")

#@@TABLE@@#
#Proportion of Themes between Republican and Democratic Partisan Environment Index ###
#@@TABLE@@#

# state PEI
st_pei_table <- meta_theta_df_comm %>% group_by(st_pei_net) %>% 
  summarise(poli = mean(comm1_poli),
            boil = mean(comm2_boil),
            race = mean(comm3_race),
            inst = mean(comm4_inst))

# national PEI
us_pei_table <- meta_theta_df_comm %>% group_by(us_pei_net) %>% 
  summarise(poli = mean(comm1_poli),
            boil = mean(comm2_boil),
            race = mean(comm3_race),
            inst = mean(comm4_inst))

# statistical significance - state
t_test_poli_st <- t.test(comm1_poli ~ st_pei_net, data = meta_theta_df_comm)
t_test_boil_st <- t.test(comm2_boil ~ st_pei_net, data = meta_theta_df_comm)
t_test_race_st <- t.test(comm3_race ~ st_pei_net, data = meta_theta_df_comm)
t_test_inst_st <- t.test(comm4_inst ~ st_pei_net, data = meta_theta_df_comm)

# print results
list(
  Politics_and_Free_Speech = t_test_poli_st,
  Boiling_Issues = t_test_boil_st,
  Race_and_People = t_test_race_st,
  Institutions = t_test_inst_st
)

# statistical significance - national
t_test_poli_us <- t.test(comm1_poli ~ us_pei_net, data = meta_theta_df_comm)
t_test_boil_us <- t.test(comm2_boil ~ us_pei_net, data = meta_theta_df_comm)
t_test_race_us <- t.test(comm3_race ~ us_pei_net, data = meta_theta_df_comm)
t_test_inst_us <- t.test(comm4_inst ~ us_pei_net, data = meta_theta_df_comm)

# print results
list(
  Politics_and_Free_Speech = t_test_poli_us,
  Boiling_Issues = t_test_boil_us,
  Race_and_People = t_test_race_us,
  Institutions = t_test_inst_us
)

#@@TABLE@@#
#Proportion of Topics between Republican and Democratic State Partisan Environment Index ###
#@@TABLE@@#

st_pei_table_topics <- meta_theta_df_comm %>% group_by(st_pei_net) %>% 
  summarise(Crisis_Response = mean(`Crisis Response`),
            Safety = mean(Safety),
            Remembrance = mean(Remembrance),
            Labor = mean(Labor),
            Budget = mean(Budget),
            Higher_Ed = mean(`Higher Education`),
            The_Future = mean(`The Future`),
            Governance = mean(Governance),
            Curriculum = mean(Curriculum),
            Financial_Matters = mean(`Financial Matters`),
            Research = mean(Research),
            Race = mean(Race),
            Institution = mean(`Institutional Efforts`),
            Free_Speech = mean(`Free Speech`),
            Diversity = mean(Diversity),
            Common_Values = mean(`Common Values`),
            Initiatives = mean(Initiatives),
            War = mean(War),
            Global_Reach = mean(`Global Reach`),
            Ceremonies = mean(Ceremonies))

# perform t-tests for each column
for (topic in topic_labels) {

  # check if column exists
  if (!(topic %in% colnames(meta_theta_df_comm))) {
    print(paste0("Warning: Column ", topic, " not found in meta_theta_df_comm"))
    next  # skip to the next iteration if column doesn't exist
  }
  
  # get the actual column values
  column_values <- meta_theta_df_comm[[topic]]  # correctly retrieves the column using [[]]
  
  # perform t-test using formula with dynamic column reference
  t_test <- t.test(column_values ~ meta_theta_df_comm$st_pei_net)
  
  print(paste0(topic, ": p-value = ", t_test$p.value, " estimate:", t_test$estimate))
}

#@@TABLE@@#
#Proportion of Topics between Republican and Democratic National Partisan Environment Index ###
#@@TABLE@@#

us_pei_table_topics <- meta_theta_df_comm %>% group_by(us_pei_net) %>% 
  summarise(Crisis_Response = mean(`Crisis Response`),
            Safety = mean(Safety),
            Remembrance = mean(Remembrance),
            Labor = mean(Labor),
            Budget = mean(Budget),
            Higher_Ed = mean(`Higher Education`),
            The_Future = mean(`The Future`),
            Governance = mean(Governance),
            Curriculum = mean(Curriculum),
            Financial_Matters = mean(`Financial Matters`),
            Research = mean(Research),
            Race = mean(Race),
            Institution = mean(`Institutional Efforts`),
            Free_Speech = mean(`Free Speech`),
            Diversity = mean(Diversity),
            Common_Values = mean(`Common Values`),
            Initiatives = mean(Initiatives),
            War = mean(War),
            Global_Reach = mean(`Global Reach`),
            Ceremonies = mean(Ceremonies))

us_pei_table_topics <- t(us_pei_table_topics)

# perform t-tests for each column
for (topic in topic_labels) {
  # check if column exists
  if (!(topic %in% colnames(meta_theta_df_comm))) {
    print(paste0("Warning: Column ", topic, " not found in meta_theta_df_comm"))
    next  # skip to the next iteration if column doesn't exist
  }
  
  # get the actual column values
  column_values <- meta_theta_df_comm[[topic]]  # correctly retrieves the column using [[]]
  
  # perform t-test using formula with dynamic column reference
  t_test <- t.test(column_values ~ meta_theta_df_comm$us_pei_net)
  
  print(paste0(topic, ": p-value = ", t_test$p.value, " estimate:", t_test$estimate))
}

#----------------------------------------------------------------------------------------------------------------------------------------#