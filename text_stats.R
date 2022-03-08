library(tidyverse)

docvars <- docvars(corp_en)

# number of English reports
nrow(docvars)


# retrieve text stats by company
raw_stats <- textstat_summary(corp_en) 
save(raw_stats, file = "raw_stats.Rda")
load("raw_stats.Rda")
clean_stats <- raw_stats %>% 
  mutate(company = substr(document, 12,19)) %>%
  left_join(.,company_names) 

  

library(ggthemes)
library(ggrepel)
#determine order og high to low sentence scores for ggplot
order <- clean_stats %>% 
  group_by(retailer) %>%
  summarize(zinnen = sum(sents), country= as.factor(country), retailer = as.factor(retailer)) %>% 
  unique() %>%
  group_by(country) %>%
  summarize(z = sum(zinnen)) %>%
  arrange(desc(z)) %>%
  pull(country)

p <- clean_stats %>% 
  group_by(retailer) %>%
  summarize(zinnen = sum(sents), country= as.factor(country), retailer = as.factor(retailer)) %>%
  unique() %>%
  ggplot(aes(reorder(country, -zinnen), zinnen, label = retailer, fill = retailer)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  ggtitle("Number of sentences in food retail sustainability reports per country") +
  xlab("country") + ylab("Number of sentences") +
  theme_economist() 
p + theme(legend.position="none") +
  scale_x_discrete(limits=order) #this puts the columns in the order of the highest to lowest
  coord_flip() 


temp <- clean_stats %>% 
  group_by(retailer) %>%
  select(retailer, country, sents) %>%
  summarize(zinnen = sum(sents), country= as.factor(country), retailer = as.factor(retailer)) %>%
  unique()

