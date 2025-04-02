Project_name<-"presspeech"                   

#### Imports ####

options(scipen=999)

library(readr) #for reading files
#library(xlsx) #for reading/writing xlsx (might be problematic on macs)
library(foreign) # for the elbow searching

library(topicmodels) #running topic modeling
library(ldatuning) #choosing K
library(doParallel) # another paralleling
require(parallel)

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

require(stopwords)
library(quanteda)

#### Get the data in #####
## import the data                                  
setwd("/Users/jmitovich/Downloads")
#imported<-readRDS("reps.rds")                           
data<- read.csv("ihe_ctc_final.csv")
data_new<- read.csv("ihe_ctc_final_corex.csv")
result <- merge(data, data_new[, c("slug", "pei", "st_pei", "us_pei")], by = "slug", all.x = TRUE)
data<-result
# assign data
#data<-imported                                     

#remove the CSV id variable
data <- data[,-1]
colnames(data)

# assign a new index
data$index<-(1:nrow(data))

## convert the content to text column
data$text<-as.character(data$ctc_text)

## prune datas (years, sources whatevr)
#data<-data[-which(is.na(data$Publication.Year)==TRUE),]
#data<-data[data$Publication.Year>1979,]

## clean for specific language
#data2<-data
#data2$CLD2<-cld2::detect_language(data2$text)
#data2<-data2 %>% filter(CLD2=="en")

## calculate and examine text length
data$nwords <- str_count(data$text, "\\w+")

nrow(data)
length(which(data$nwords==0))
length(which(data$nwords<3))
length(which(data$nwords<50))
length(which(data$nwords<200))

## check by hand the samples to decide
#TEMP<-data2[which(data2$nwords<50),]
#TEMP<-TEMP[which(TEMP$nwords>20),]

# in this case 2 and up seems fine
data3<-data[which(data$nwords>2),]                         


#### New Tidy Pre-Proc flow ####
# stopwords to keep editiing based on next lines
mystopwords<-c(stopwords("en"),stopwords(source = "smart"),"http","https") 

# creating tidy tokens
tidy_data<-data3 %>%
  unnest_tokens(word, text) %>% # tokenizing
  # option 1 REGEX --># Be careful this is deangerous as it gets word parts
  #filter(!grepl(paste(mystopwords, collapse="|"), word)) %>%  
  # option 2 Exact Match -->
  anti_join(data.frame(word=mystopwords)) %>%
  mutate(nchar=nchar(word)) %>% #counting the number of letters
  filter(nchar>2) %>% # remove from minumum number of chars
  filter(!grepl("[0-9]{1}", word)) %>% # removing numbers 
  filter(!grepl("\\W", word))  # removing any word containing non letter/number 

# choosing top words by tf-idf                            
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

# DFM-ing it (termdoc)
tidy_dfm <- tidy_data_pruned %>%
  count(index, word) %>%
  cast_dfm(index, word, n)

# feed to lda object

full_data<- convert(tidy_dfm, to = "topicmodels")


#### searchk #####

closeAllConnections() 

mycores <- detectCores()-2 # if RAM issues - reduce cores even more

#mycores <- 50

## Add your params
candidate_alpha = c(50,25,10,5,2,1) # candidates for alpha values
candidate_k = c(2, seq(5,100,by=5),seq(110,150,by=10),175,200) # candidates for how many topics
folds = 5
n = nrow(full_data)

## Run the search

# Create the splitfolds outside the loop
splitfolds = sample(1:folds, n, replace = TRUE)
# Set up the DF that will be the batch queue
validationQueueDF = data.frame(k = c(1), alpha = c(1), fold = c(1))
validationQueueDF = validationQueueDF[-1, ]
# Create all the rows for the batch queue. Alpha * K * folds
for (val_alpha in candidate_alpha) {
  for (val_k in candidate_k) {
    for (val_i in seq(from = 1, to = folds, by = 1)) {
      val_model = c(val_k, val_alpha/val_k, val_i)
      validationQueueDF[nrow(validationQueueDF) + 1, ] = val_model
    }
  }
}
# Reorganize batch queue in descending order of K
validationQueueDF = validationQueueDF[order(-validationQueueDF$k), ]
# Put the loop in a function so that it was easier to execute and debug in RStudio
validation=function() {
  print(Sys.time())
  # This console message confirms the number of items in the batch queue.
  print(paste("Starting validation of ",as.character(nrow(validationQueueDF))," models!",sep = ""))
  # Adding outfile="" to makeCluster allows print messages in the loop to go to the master console instead of being hidden.
  cluster = makeCluster(mycores, outfile="") # We are leaving one Core spare. If number of corse on pc is 1, then -1 in this line should be removed.
  registerDoParallel(cluster)
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  # Sending the batch queue, the splitfolds, and the dataset to each cluster.
  clusterExport(cluster, c("full_data", "validationQueueDF", "splitfolds"))
  results = foreach(j = 1:nrow(validationQueueDF), .combine = rbind) %dopar%{
    print(j)
    print(nrow(validationQueueDF))
    # Pull the row in the batch queue for this loop and assign the items to variables.
    model = as.numeric(as.vector(validationQueueDF[j,]))
    model_k = model[1]
    model_alpha = model[2]
    model_fold = model[3]
    # Print confirmation that the job has started.
    print(paste("Starting fit of Row ",as.character(j),". K, Alpha, Fold: ",as.character(model_k),", ",as.character(model_alpha),", ",as.character(model_fold),".",sep = ""))
    train_set = full_data[splitfolds != model_fold, ]
    valid_set = full_data[splitfolds == model_fold, ]
    fitted = LDA(train_set, k = model_k, method = "Gibbs", control = list(alpha = model_alpha))
    # Create the one row DF to be rbinded (rbound?) together by foreach once all the jobs are run.
    result_1model = data.frame(k = c(1), alpha = c(1), fold = c(1), perplexity = c(1))
    # Next four lines are kind of sad. I'm sure there's a more efficient way to replace a row in a DF but it's late and this works!
    result_1model[1,1] = model_k
    result_1model[1,2] = model_alpha
    result_1model[1,3] = model_fold
    result_1model[1,4] = perplexity(fitted,newdata = valid_set)
    # Confirm the job has finished.
    print(paste("Model fitting of Row ",as.character(j)," complete. K, Alpha, Fold: ",as.character(model_k),", ",as.character(model_alpha),", ",as.character(model_fold),".",sep = ""))
    return(result_1model)
    print(Sys.time())
    print (validationQueueDF[j,])
  }
  stopCluster(cluster)
  print("Done!")
  print(Sys.time())
  return(results)
}

# Execute the above function and end up with a four column DF: K, Alpha, Fold, and Perplexity.

save.image(paste0(Project_name,"_pre-searchk.rdata"))

print(Sys.time())
val_results=validation()
print(Sys.time())

save.image(paste0(Project_name,"_post-searchk.rdata"))

closeAllConnections() 



#### Check your results ####

MainresultDF<-val_results

MainresultDF$kalpha <- paste0(as.character(MainresultDF$k),MainresultDF$alpha) 
MainresultDF$newalpha <- as.numeric(MainresultDF$alpha*MainresultDF$k) 

ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=alpha))

ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=factor(newalpha)))+
  ggtitle("Twitter Users 5-fold cross-validation of topic modelling",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")

MainDF <- MainresultDF[MainresultDF$newalpha ==2, ] #correct alpha 

MainresultDF<-MainresultDF[order(MainresultDF$k),]

cars.spl <- with(MainresultDF, smooth.spline(k, perplexity, df = 3))
with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2))

plot(with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2)), type = "l")

# play with abline to find the point AFTER max point
abline(v=55)

#### Optional - Find Topcs Numbers ####
myalpha=2/55 #enter alpha level divided by a good k

2/55

# OPTIONAL - maybe just use k = 25
# Running the searchK command
Sys.time()
FTN_result <- FindTopicsNumber(
  full_data,
  topics = c(2, seq(5,100,by=5)), # Specify how many topics you want to try.
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  #control = list(alpha=myalpha, seed = 6723), # add random seed for reproducability
  control = list(seed = 6723), # add random seed for reproducability
  #mc.cores = 70L, # Specify the number of cores that you computer has to achieve the best performance. 
  verbose = TRUE
)
Sys.time()

save.image(paste0(Project_name,"_post-searchk2.rdata"))

## Plot the results and choose candidate models
FindTopicsNumber_plot(FTN_result)  

# Run final model
fitted <- LDA(full_data, k = 20, method = "Gibbs",
              control = list(seed=1217))

#### Analysis Part 2: Making excels ####
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
  #xlsx::write.xlsx(topwords,paste0(Project_name,"_Topwordsk_",LDAmodel@k,".xlsx"))
  return(topwords)
}

get_FREX_Words<-function(LDAmodel) {
  ## apply formula below from Roberts et al.
  ## 1/(w/(bword/sumbrow)+(1-w)/(bword)) for each cell
  
  mybeta<-data.frame(LDAmodel@beta)
  colnames(mybeta)<-LDAmodel@terms
  mybeta<-t(mybeta)
  colnames(mybeta)<-seq(1:ncol(mybeta))
  mybeta=exp(mybeta)
  
  # apply formula below
  # 1/(w/(bword/sumbrow)+(1-w)/(bword)) for each cell
  myw=0.3
  word_beta_sums<-rowSums(mybeta)
  my_beta_for_frex<-mybeta
  for (m in 1:ncol(my_beta_for_frex)) {
    for (n in 1:nrow(my_beta_for_frex)) {
      my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
    }
    print (m)
  }
  
  ########  print 50 frex:
  topwords <- my_beta_for_frex[1:nwords,]
  
  for (i in 1:LDAmodel@k) {
    tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
    tempframe <- tempframe[1:nwords,]
    tempvec<-as.vector(rownames(tempframe))
    topwords[,i]<-tempvec[1:nwords]
  }
  
  rownames(topwords)<-c(1:nwords)
  write.csv(topwords,paste0("thesis", "_TopFREXk_", LDAmodel@k, ".csv"))
  #xlsx::write.xlsx(topwords,paste0(Project_name,"_TopFREXk_",LDAmodel@k,".xlsx"))
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
  
  ## craete container
  toptexts <- data.frame(mybeta[1:ntext,])
  
  ## and loop for all topics
  for (i in 1:LDAmodel@k) {
    print(i)
    ## make temp frames ordered by document topic loading
    ## note that i+1 because first col is text, 1st topic is in col 2 etc
    tempframe <- meta_theta_df[order(-meta_theta_df[[i+1]]),]
    ## cut to the right amount
    tempframe <- tempframe[1:ntext,]
    ## vectorize the text column (which is now ordered correctly)
    tempvec<-as.vector(unlist(tempframe[,1]))
    ## plug in to excel
    toptexts[,i]<-tempvec
  }
  
  ## row names to make pretty
  rownames(toptexts)<-c(1:ntext)
  
  ## write the file
  write.csv(topwords,paste0("thesis", "_TopTEXTSk_", LDAmodel@k, ".csv"))
  #xlsx::write.xlsx(toptexts,paste0(Project_name,"_TopTEXTSk_",LDAmodel@k,".xlsx"))
  return(toptexts)
}


topwords<-get_top_Words(LDAfit)
topFREX<-get_FREX_Words(LDAfit)
toptexts<-get_top_texts(LDAfit)


##################
#### ANTMNING ####
##################
#LDAfit<-LDAlist[[2]]
data33<-data3
deleted_lda_texts<-(setdiff(as.character(data33$index), as.character(LDAfit@documents)))

'%!in%' <- function(x,y)!('%in%'(x,y))

data33<-data33[data33$index %!in% deleted_lda_texts,]
metadf<-data33
meta_theta_df<-cbind(metadf,LDAfit@gamma)

# IF you want to return duplicates
#removed_df2<-inner_join(removed_df,meta_theta_df,by="text")
#removed_df2<-removed_df2[,-c(11:19)]                 ####@ISSUE FULTURE US lets fix it and make it automatic - it should relate to ncol of data3
#colnames(removed_df2)<-gsub("\\.x","",colnames(removed_df2))
#removed_df2$index2<-as.character(removed_df2$index2)
#meta_theta_df<-bind_rows(meta_theta_df,removed_df2)

#@@@@@@@@@@@@@@@@@@@@@@@@@@
#### ANTMN NETWORKING  ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@
#chatgptlabels$label_both<-paste0(chatgptlabels$chatgpt_label_bytext," *OR* ",chatgptlabels$chatgpt_label_bywords)
#topic_labels<-chatgptlabels$label_both
topic_size<-colSums(meta_theta_df[,c((ncol(data3)+1):(ncol(data3)+LDAfit@k))])      

# Enter by hand
#topic_labels<-c("name1","name2")

topic_labels <- c(1:20) #change 25 to actual number of topics if needed
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
# or from file:
#topic_labels<-read.csv("TWREPS_TopFREXk_40_WITHLABELS.csv") # change to file with labels
#topic_labels<-colnames(topic_labels)[-1]

network_from_LDA<-function(LDAobject,deleted_topics=c(),topic_names=c(),save_filename="",topic_size=c(),bbone=FALSE) {
  # Importing needed packages
  require(lsa) # for cosine similarity calculation
  require(dplyr) # general utility
  require(igraph) # for graph/network managment and output
  require(corpustools)
  
  print("Importing model")
  
  # first extract the theta matrix form the topicmodel object
  theta<-LDAobject@gamma
  # adding names for culumns based on k
  colnames(theta)<-c(1:LDAobject@k)
  
  # claculate the adjacency matrix using cosine similarity on the theta matrix
  mycosine<-cosine(as.matrix(theta))
  colnames(mycosine)<-colnames(theta)
  rownames(mycosine)<-colnames(theta)
  
  # Convert to network - undirected, weighted, no diagonal
  
  print("Creating graph")
  
  topmodnet<-graph.adjacency(mycosine,mode="undirected",weighted=T,diag=F,add.colnames="label") # Assign colnames
  # add topicnames as name attribute of node - importend from prepare meta data in previous lines
  if (length(topic_names)>0) {
    print("Topic names added")
    V(topmodnet)$name<-topic_names
  } 
  # add sizes if passed to funciton
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
  
  # Backbone
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

################## End of Main Function LDA
#Example:

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
 
### change community detection to the one used # add more if needed
meta_theta_df_comm$comm1_poli<-rowSums(meta_theta_df_comm[,(as.numeric(V(finalnet)$label[which(V(finalnet)$eigen == 1)]))+ncol(data33)]) # color - name
meta_theta_df_comm$comm2_boil<-rowSums(meta_theta_df_comm[,(as.numeric(V(finalnet)$label[which(V(finalnet)$eigen == 2)]))+ncol(data33)]) # color - name
meta_theta_df_comm$comm3_race<-rowSums(meta_theta_df_comm[,(as.numeric(V(finalnet)$label[which(V(finalnet)$eigen == 3)]))+ncol(data33)]) # color - name
meta_theta_df_comm$comm4_inst<-rowSums(meta_theta_df_comm[,(as.numeric(V(finalnet)$label[which(V(finalnet)$eigen == 4)]))+ncol(data33)]) # color - name

# Fix date
meta_theta_df_comm$date2<-as.Date(meta_theta_df_comm$ctc_date,"%m/%d/%Y")
meta_theta_df_comm$ihe_type <- ifelse((meta_theta_df_comm$ihe_type == "Private non-religious" | meta_theta_df_comm$ihe_type == "Private religious"), "Private", "Public")
meta_theta_df_comm$pei_net <- ifelse(meta_theta_df_comm$pei > 0, 1, ifelse(meta_theta_df_comm$pei < 0, -1, 0))
data_new<- read.csv("ihe_ctc_final_corex.csv")
result <- merge(meta_theta_df_comm, data_new[, c("slug", "pei", "st_pei", "us_pei")], by = "slug", all.x = TRUE)
meta_theta_df_comm <- result
meta_theta_df_comm$pei <- meta_theta_df_comm$pei.y
#cluster and plot:

#####CHART 1#####

# group by date and summarize mean comm loading per day (can also replace that with specific topics)
meta_theta_df_comm %>% group_by(date2) %>% 
  summarise(comm1=mean(comm1_poli),
            comm2=mean(comm2_boil),
            comm3=mean(comm3_race),
            comm4=mean(comm4_inst)) %>%
            ggplot(aes(x=date2))+
              # geom lines transparent with geom smooth for trends
              geom_line(aes(y=comm1),color="skyblue3",alpha=0.1)+
              #geom_line(aes(y=comm2),color="salmon",alpha=0.1)+
              geom_line(aes(y=comm3),color="orange",alpha=0.1)+
              #geom_line(aes(y=comm4),color="lightgreen",alpha=0.1)+
              geom_smooth(aes(y=comm1),span=0.1,se=FALSE,color="skyblue3")+
              #geom_smooth(aes(y=comm2),span=0.1,se=FALSE,color="salmon")+
              geom_smooth(aes(y=comm3),span=0.1,se=FALSE,color="orange")+
              #geom_smooth(aes(y=comm4),span=0.1,se=FALSE,color="lightgreen")+
  
              # apply general Theme
              theme_bw()+
              # add dates and improve presentation - First the Breaks:
              scale_x_date(date_breaks = "5 years", 
                           #minor breaks
                           date_minor_breaks = "5 years",
                          #what labels should look like
                          date_labels = "%Y")+
              # orientation of dates
              theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=0))+
              # Add some titles
              ggtitle("Themes in Contentious Topic Communications (Comm 1 in Red, Comm 2 in Blue)")+
              ylab("Share of CTCs")+
              xlab("Year")
              
#@@@@@ FIGURE NA @@@@@#

meta_theta_df_comm %>% 
  group_by(date2) %>% 
  summarise(poli = mean(comm1_poli),
            race = mean(comm3_race),
            inst = mean(comm4_inst),
            boil = mean(comm2_boil)) %>%
  ggplot(aes(x = date2)) +
  
  # Shaded background for historical periods
  annotate("rect", xmin = as.Date("1964-01-01"), xmax = as.Date("1973-12-31"), ymin = -Inf, ymax = Inf, fill = "lightcoral", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1974-01-01"), xmax = as.Date("1986-12-31"), ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1987-01-01"), xmax = as.Date("1992-12-31"), ymin = -Inf, ymax = Inf, fill = "lightgoldenrod", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1993-01-01"), xmax = as.Date("2011-12-31"), ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2012-01-01"), xmax = as.Date("2022-12-31"), ymin = -Inf, ymax = Inf, fill = "cadetblue1", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2023-01-01"), xmax = as.Date("2024-12-31"), ymin = -Inf, ymax = Inf, fill = "plum", alpha = 0.4) +
  
  # Transparent geom lines with smooth trends
  geom_line(aes(y = poli), color = "deepskyblue4", alpha = 0.1) +
  geom_line(aes(y = race), color = "deeppink4", alpha = 0.1) +
  geom_smooth(aes(y = poli), span = 0.1, se = FALSE, color = "deepskyblue4") +
  geom_smooth(aes(y = race), span = 0.1, se = FALSE, color = "deeppink4") +
  
  # Apply general theme
  theme_bw() +
  scale_x_continuous(limits=c(as.Date("1964-01-01"), as.Date("2014-12-31")), expand=c(0,0))+
  scale_x_date(limits = c(as.Date("1964-01-01"), as.Date("2024-12-31")),
               date_breaks = "5 years", 
               #date_minor_breaks = "5 years",
               date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 0)) +
  
  # Titles and labels
  ggtitle("Themes in Contentious Topic Communications (Comm 1 in Red, Comm 2 in Blue)") +
  ylab("Share of CTCs") +
  xlab("Year")


save.image(paste0(Project_name,"_AFTERNETOWRK.rdata"))

#####CHART 2#####

#@@@@@TABLE 2@@@@@#
meta_theta_df_comm$st_pei_net <- ifelse(meta_theta_df_comm$st_pei > 0, 1, ifelse(meta_theta_df_comm$st_pei < 0, -1, 0))
meta_theta_df_comm$us_pei_net <- ifelse(meta_theta_df_comm$us_pei > 0, 1, ifelse(meta_theta_df_comm$us_pei < 0, -1, 0))

pei_table <- meta_theta_df_comm %>% group_by(st_pei_net) %>% 
  summarise(poli = mean(comm1_poli),
            boil = mean(comm2_boil),
            race = mean(comm3_race),
            inst = mean(comm4_inst))

pei_table_topics <- meta_theta_df_comm %>% group_by(us_pei_net) %>% 
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

get_quantiles <- function(meta_theta_df_comm, col, probs = c(0, 0.25, 0.5, 0.75, 1)) {
  quantile(meta_theta_df_comm[[col]], probs = probs, na.rm = TRUE)
}

get_quantiles(meta_theta_df_comm, "ctc_day_term")

assign_quantile_labels <- function(df, col, probs = c(0, 0.25, 0.5, 0.75, 1), labels = c("Q1", "Q2", "Q3", "Q4")) {
  # Compute quantiles
  quantile_values <- quantile(df[[col]], probs = probs, na.rm = TRUE)
  
  # Assign quantile labels
  df$quantile_label <- cut(df[[col]], breaks = quantile_values, labels = labels, include.lowest = TRUE)
  
  return(df)
}

meta_theta_df_comm <- assign_quantile_labels(meta_theta_df_comm, "ctc_day_term")

meta_theta_df_comm %>% group_by(quantile_label) %>%
  summarise(poli = mean(comm1_poli),
            boil = mean(comm2_boil),
            race = mean(comm3_race),
            inst = mean(comm4_inst))

meta_theta_df_comm %>%
  group_by(ihe_type) %>%
  summarise(poli = mean(comm1_poli),
            boil = mean(comm2_boil),
            race = mean(comm3_race),
            inst = mean(comm4_inst))


pei_table_topic <- meta_theta_df_comm %>% group_by(st_pei_net) %>% 
  summarise(topic_budget = mean(Budget),
            topic_safety= mean(Safety))

t_test_topic_race <- t.test(`12` ~ pei_net, data = meta_theta_df_comm)
t_test_topic_diversity <- t.test(`15` ~ pei_net, data = meta_theta_df_comm)

# Perform t-tests for each variable comparing pei_net = 1 and pei_net = -1
t_test_poli <- t.test(comm1_poli ~ us_pei_net, data = meta_theta_df_comm)
t_test_boil <- t.test(comm2_boil ~ us_pei_net, data = meta_theta_df_comm)
t_test_race <- t.test(comm3_race ~ us_pei_net, data = meta_theta_df_comm)
t_test_inst <- t.test(comm4_inst ~ us_pei_net, data = meta_theta_df_comm)

# Print results
list(
  Politics_and_Free_Speech = t_test_poli,
  Boiling_Issues = t_test_boil,
  Race_and_People = t_test_race,
  Institutions = t_test_inst
)

# Create a vector of column names (1 to 20 as character strings)
column_names <- as.character(1:20)

# Perform t-tests for each column

for (topic in topic_labels) {
  #print(topic)
  
  # Check if column exists
  if (!(topic %in% colnames(meta_theta_df_comm))) {
    print(paste0("Warning: Column ", topic, " not found in meta_theta_df_comm"))
    next  # Skip to the next iteration if column doesn't exist
  }
  
  # Get the actual column values
  column_values <- meta_theta_df_comm[[topic]]  # Correctly retrieves the column using [[]]
  
  #print(length(column_values))
  #print(length(meta_theta_df_comm$pei_net))
  
  # Perform t-test using formula with dynamic column reference
  t_test <- t.test(column_values ~ meta_theta_df_comm$us_pei_net)
  
  print(paste0(topic, ": p-value = ", t_test$p.value, " estimate:", t_test$estimate))
}

# Perform linear regression
regression_model <- lm(Diversity ~ pei_net, data = meta_theta_df_comm)

regression_model <- lm(budget ~ date2, data = meta_theta_df_comm)
#statistically signif:
#race ~ st_exec_ctrl
#poli ~ us_exec_ctrl
#financial ~ us_exec_ctrl
#safety ~ us_exec_ctrl
#budget ~ st_exec_ctrl, st_sen_ctrl
#global_reach ~ us_sen_ctrl, us_house_ctrl
#global_reach ~ pei
#comm1_poli ~ us_exec_ctrl+us_sen_ctrl+us_house_ctrl
#comm3_race ~ us_exec_ctrl+us_sen_ctrl+us_house_ctrl
# Print summary of regression results
print(summary(regression_model))
meta_theta_df_comm <- meta_theta_df_comm %>%
  rename("Diversity"="Diveristy")

meta_theta_df_comm <- meta_theta_df_comm %>%
  rename("Crisis Response"=`1`,
         "Safety"=`2`,
        "Remembrance"=`3`,
        "Labor"=`4`,
         "Budget"=`5`,
         "Higher Education"=`6`,
        "The Future"=`7`,
         "Governance"=`8`,
       "Curriculum"=`9`,
        "Financial Matters"=`10`,
         "Research"=`11`,
    "Race"=`12`,
        "Institutional Efforts"=`13`,
      "Free Speech"=`14`,
         "Diveristy"=`15`,
        "Common Values"=`16`,
         "Initiatives"=`17`,
      "War"=`18`,
         "Global Reach"=`19`,
         "Ceremonies"=`20`)
# Name the results by their respective columns
names(t_test_results) <- column_names

# Print all t-test results
t_test_results



meta_theta_df_comm %>% group_by(ctc_pd) %>% 
  summarise(poli = mean(comm1_poli),
            boil = mean(comm2_boil),
            race = mean(comm3_race),
            inst = mean(comm4_inst))


comm1_poli_distr <- meta_theta_df_comm %>% group_by(ctc_pd) %>% summarise(war = mean(war),free_speech = mean(free_speech),crisis_response = mean(crisis_response),safety = mean(safety),common_values = mean(common_values))

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
#@@@@@ FIGURE 1 @@@@@#
meta_theta_df_comm %>% 
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
                                 "Israel-Hamas\nWar & DEI\n(2023-2024)")), y = value, fill = category)) +
  
  # 100% stacked bar chart
  geom_bar(stat = "identity", position = "fill") +
  
  scale_fill_manual(values = c("Politics & Free Speech" = "#66c2a5",
                               "Academic Boilerplate" = "#fc8d62", 
                               "Race & People" = "#8da0cb", 
                               "Institutional Affairs" = "#e78ac3")) +
  
  # Formatting
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = paste0(round(value,2)*100,"%")), 
            position = position_stack(vjust = 0.5), color="white", face="bold", size = 4, family="eb_garamond")+
  labs(title = "Proportion of four themes across the seven advent time periods",
       x = "Time Period",
       y = "Proportion",
       fill = "Theme") +
  theme_minimal(base_family = "eb_garamond", base_size=12) +
  theme(text = element_text(family = "eb_garamond"))


#@@@@@ FIGURE 2 @@@@@#


###v3
# Load EB Garamond font from Google Fonts
font_add_google("EB Garamond", "eb_garamond")
showtext_auto()
meta_theta_df_comm %>%  
  group_by(date2) %>%  
  summarise(`Politics & Free Speech` = mean(comm1_poli),
            `Race & People` = mean(comm3_race)) %>%
  pivot_longer(cols = c(`Politics & Free Speech`, `Race & People`), 
               names_to = "Theme", values_to = "Value") %>%
  ggplot(aes(x = date2, y = Value, color = Theme)) +
  
  # Shaded background for historical periods
  annotate("rect", xmin = as.Date("1964-01-01"), xmax = as.Date("1973-12-31"), ymin = -Inf, ymax = Inf, fill = "lightcoral", alpha = 0.3) +
  annotate("rect", xmin = as.Date("1974-01-01"), xmax = as.Date("1986-12-31"), ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.3) +
  annotate("rect", xmin = as.Date("1987-01-01"), xmax = as.Date("1992-12-31"), ymin = -Inf, ymax = Inf, fill = "lightgoldenrod", alpha = 0.3) +
  annotate("rect", xmin = as.Date("1993-01-01"), xmax = as.Date("2011-12-31"), ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.3) +
  annotate("rect", xmin = as.Date("2012-01-01"), xmax = as.Date("2022-12-31"), ymin = -Inf, ymax = Inf, fill = "cadetblue1", alpha = 0.3) +
  annotate("rect", xmin = as.Date("2023-01-01"), xmax = as.Date("2024-12-31"), ymin = -Inf, ymax = Inf, fill = "plum", alpha = 0.3) +
  
  # Transparent geom lines with smooth trends
  geom_line(alpha = 0.1) +
  geom_smooth(span = 0.1, se = FALSE) +
  
  # Apply general theme
  theme_minimal(base_family = "eb_garamond", base_size = 16) +
  theme(text = element_text(family = "eb_garamond")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(limits = c(as.Date("1964-01-01"), as.Date("2024-12-31")),
               date_breaks = "5 years", 
               date_labels = "%Y") +
  
  # Corrected legend: Use scale_color_manual() for line colors
  scale_color_manual(values = c(`Politics & Free Speech` = "#66c2a5",
                                `Race & People` = "#8da0cb")) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 0)) +
  
  # Titles and labels
  ggtitle("Proportion of Selected Themes") +
  ylab("Proportion") +
  xlab("Year")

###V2
### V2
meta_theta_df_comm %>%   
  group_by(date2) %>%   
  summarise(`Politics & Free Speech` = mean(comm1_poli),
            `Race & People` = mean(comm3_race)) %>%
  pivot_longer(cols = c(`Politics & Free Speech`, `Race & People`), 
               names_to = "Theme", values_to = "Value") %>%
  ggplot(aes(x = date2, y = Value, color = Theme)) +
  
  # Define ordered factor levels for proper legend order
  scale_fill_manual(
    name = "Time Period",
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
  
  # Shaded background for historical periods with ordered fill
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
  
  # Transparent geom lines with smooth trends
  geom_smooth(span = 0.1, se = FALSE) +
  
  # Apply general theme
  theme_minimal(base_family = "eb_garamond", base_size = 16) +
  theme(text = element_text(family = "eb_garamond")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(limits = c(as.Date("1964-01-01"), as.Date("2024-12-31")),
               date_breaks = "5 years", 
               date_labels = "%Y") +
  
  # Corrected legend: Use scale_color_manual() for line colors
  scale_color_manual(values = c(`Politics & Free Speech` = "#66c2a5",
                                `Race & People` = "#8da0cb")) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 0),
        legend.position = "right") +
  
  # Titles and labels
  ggtitle("Proportion of Selected Themes") +
  ylab("Proportion") +
  xlab("Year")

#@@@@FORMATTING@@@@#

font_add(family = "EB_Garamond", regular = "/users/jaredmitovich/Downloads/EB_Garamond/static/EBGaramond-Regular.ttf")
showtext_auto()
theme_set(theme_bw(base_family = "EB_Garamond", base_size = 18))


meta_theta_df_comm %>%
summarise(`Politics & Free Speech` = mean(comm1_poli),
          `Academic Boilerplate` = mean(comm2_boil),
          `Race & Demographics` = mean(comm3_race),
          `Institutional Affairs` = mean(comm4_inst))

meta_theta_df_comm %>%
  summarise(`Politics & Free Speech` = mean(comm1_poli),
            `Academic Boilerplate` = mean(comm2_boil),
            `Race & Demographics` = mean(comm3_race),
            `Institutional Affairs` = mean(comm4_inst))

write.csv(meta_theta_df_comm, "comm2.csv")