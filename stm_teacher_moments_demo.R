# TITLE:  Structural Topic Modeling with Teacher Moments Data 
# AUTHOR: Joshua Littenberg-Tobias, Ph.D. (Research Scientist)
# DATE: 02-09-22
# BRIEF DESCRIPTION: Use STM to estimate topic models for PS 1 Jeremy's Journal and identify correlates 

# SETUP---------------------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load and install packages
library(pacman)
p_load(tidyr, readr, dplyr, ggplot2, stringr, janitor, httpuv, magrittr, lubridate,  bigrquery, tidytext, stm, rstudioapi, hunspell)


# Set working directory to this file's location
currentdir <- dirname(getActiveDocumentContext()$path)
if (getwd() != currentdir & currentdir != "") {
  setwd(currentdir)
}

# Disable scientific notation
options(scipen=999)


# IMPORT IN FILE----------------------------------

# Import in your data
# Note: If your file is in a different folder then you will need to change the directory. 
tm_data <-read_csv("xxxxxxx.csv")



# PROCESS TEXT DATA----------------------------

# For Teacher Moments data
# 1 - Remove non-consenting users
# 2 - Remove non-text responses
# 3 - Remove duplicates
# 4 - For audio data remove processing info
# 5 - Separate by punctuation or pauses/hesitation (optional)



# Consent granted by user
tm_data %<>% filter(consent_granted_by_user == T)

# Remove non-text or non-audio response
tm_data %<>% filter((type == "AudioPrompt" | type == "TextResponse") & !str_detect(content, "^audio"))

# Remove annotation response (optional)
tm_data %<>% filter(!str_detect(header, " Annotation-"))

# Remove duplicates by run_id
tm_data %<>%
  arrange(run_id, desc(ended_at)) %>%
  group_by(run_id, header) %>%
    mutate(row_order = row_number()) %>%
  ungroup() %>%
  filter(row_order == 1) %>%
  select(-row_order)
  
# Remove audio processing info 
tm_data %<>%
  mutate(content = word(content, 1, 1, sep = "\\(https"))


# Seperate by pauses and hesiation
tm_data %<>% mutate(content = tolower(content)) #Make all lower case
tm_data %<>% separate_rows(content, sep = "[//.//]") 
tm_data %<>% separate_rows(content, sep = "[//?//]") 
tm_data %<>% separate_rows(content, sep = "[//!//]")

tm_data %<>% mutate(content = str_replace_all(content, "%hesitation", "*"), content)  #Replace hesitation with *
tm_data %<>% mutate(content = str_replace_all(content, "\\[pause\\]", "*"), content)  #Replace pause with *
tm_data %<>% separate_rows(content, sep = "[*]") #Break on [*]
tm_data %<>% filter(content != "") # Remove all empty rows
tm_data %<>% filter(str_detect(content, "[a-z]")) #Remove all rows with no strings


# Number of responses
tm_data %>% count()


# STM - Text Processing - Remove stopwords/punctuation and stem
# Text Pre-Processing
tm_data_text <- textProcessor(tm_data$content, metadata = tm_data, lowercase = T,
                                     removestopwords =  T, removenumbers = T, stem = T)


# Remove words that do not appear in a certain number (2) of documents
out_tm_data <- prepDocuments(tm_data_text$documents, tm_data_text$vocab, tm_data_text$meta, lower.thresh = 2)
# Reports number of documents and tokens

# Pull out each component
docs_tm <- out_tm_data$documents
vocab_tm <- out_tm_data$vocab
meta_tm <-out_tm_data$meta

# Figure out ideal number of topics to extract
seed <- runif(1, 10000,99999) # Note- models will change with different randoms seeds particular if sample sizes are small


# Manual -- run model with different number of topics and select option that maximizes exclusivity and coherence
model_selection <- searchK(docs_tm, vocab_tm, K = c(2:15),
                   prevalence = ~ factor(header),
                    max.em.its =300, data = meta_tm, seed = seed)
 
# Print out results
model_selection$results
 

plot_results <- model_selection$results %>% as.data.frame() %>%
   mutate_all(~as.numeric(.))

 ggplot(data = plot_results, aes(x = semcoh, y =   exclus)) +
   geom_point() +
   geom_smooth(method = "loess", se = F) +
   geom_label(aes(label = K))
 

 
 # Once you figure out ideal number of topics run topic model
 
 # Select number of topics for your analysis (balance between coherence and exclusivty)
 k_topics <- 7

tm_fit <- stm(documents = docs_tm, vocab = vocab_tm,
                     K = k_topics, prevalence = ~ factor(header),
                     max.em.its = 300, data = meta_tm,
                     init.type = "Spectral", seed = seed)

# Topic data
labelTopics(tm_fit, n = 5)

 # Extract frex words
 frex_words <- labelTopics(tm_fit, n = 5)$frex %>%
   as.data.frame() %>%
   unite(frex, V1:5, sep = ",")
 
 # Plot labels with Frex words
 plot.STM(tm_fit, type= "summary", n = 5, labeltype = "frex")
 
 # List of Examples quotes
 examples <- lapply(as.list(1:k_topics), function(x){
   
   findThoughts(tm_fit, texts = meta_tm$content,
                n = 1, topics = x)$docs[[1]]
 }) %>% as.data.frame() %>% gather()
 
 
 # Calculate max prevalance by run
 theta_fit <- tm_fit $theta %>%
   as.data.frame()
 
 names(theta_fit) <- paste0("topic_", seq(1:k_topics))
 
 theta_fit  %<>%
   bind_cols(meta_tm %>% select(username, run_id, header, content))
 
 max_theta  <- theta_fit %>%
   select(-header, -content) %>%
   group_by(username, run_id ) %>%
   summarise_all(~max(., na.rm = T)) %>%
   ungroup() 
 
 # Calculate average prevalnce by topi
 topic_prev <- max_theta %>%
   summarise_at(vars(contains("topic_")), ~mean(., na.rm = T)) %>%
   gather()
 
 
# Print out topic topic table
 topic_table <- data.frame(
   topics = paste0("Topic ", c(1: k_topics)),
   average_prev = topic_prev$value,
   common_words = frex_words$frex,
   example_quotes = examples$value) 
 
# View topic table 
View(topic_table)

# Write out CSV 
write_csv(topic_table, "xxxxxxx.csv")

# PLOTS-------------------


library(forcats)

topic_summary <- max_theta %>%
  gather(topic, theta, -username, -run_id) %>%
  group_by(topic) %>%
  summarise(
    theta = mean(theta, na.rm = T),
  ) %>%
  mutate(type = 2)

topic_data_plot <- max_theta %>%
  gather(topic, theta, -username, -run_id)  %>%
  mutate(type = 1) %>%
  bind_rows(topic_summary) %>%
  mutate(order = as.numeric(str_extract(topic, "[0-9]+"))) %>%
  mutate(topic = str_replace(topic, "topic", "Topic")) %>%
  mutate(topic = str_replace(topic, "_", " "))

ggplot(data = topic_data_plot, aes(x = fct_reorder(topic, order, .desc = T),  y = theta, color = as.factor(type))) +
  geom_point(stat = "identity", size = 3) +
  geom_path(aes(group = topic)) +
  scale_y_continuous(limits = c(0, 1.01),
                     breaks = seq(0, 1, by = 0.1),
                     labels = paste0(seq(0, 100, by = 10), "%")) +
  scale_color_manual("Type", labels = c("Individual", "Average"), values = c('grey60', "#40BDBF")) +
  coord_flip() +
  xlab("") +
  ylab("Probability") +
  theme(
    axis.text = element_text(size = 12),
    legend.text = element_text(size =12))
