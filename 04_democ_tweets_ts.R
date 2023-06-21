library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(reshape2)
library(readr)
library(lubridate)
library(ggthemes)
library(kableExtra)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(cowplot)

shtwts <- readRDS("data/output/twts_transliterated.rds")

shtwts$stage <- rep(NA, nrow(shtwts))
shtwts$datenum <- as.numeric(shtwts$date)
shtwts$stage <- cut(shtwts$datenum, 
                    breaks = c(-Inf, 14975, 14983, Inf), 
                    labels = c("Stage1", "Stage2", "Stage3"), 
                    right = FALSE)

shtwts$tweet_trans <- gsub("élections", "elections", shtwts$tweet_trans)
shtwts$tweet_trans <- gsub("democratie", "democracy", shtwts$tweet_trans)
shtwts$tweet_trans <- gsub("démocratie", "democracy", shtwts$tweet_trans)
shtwts$tweet_trans <- gsub("aldymqraty0", "democracy", shtwts$tweet_trans)
shtwts$tweet_trans <- gsub("lantkabat", "elections", shtwts$tweet_trans)
shtwts$tweet_trans <- gsub("llantkabat", "elections", shtwts$tweet_trans)
shtwts$tweet_trans <- gsub("wantkabat", "elections", shtwts$tweet_trans)
shtwts$tweet_trans <- gsub("alantkabat", "elections", shtwts$tweet_trans)

tweets_processed <- shtwts %>%
  distinct(tweet_id, .keep_all = T)
tweets_corpus <- corpus(tweets_processed, text_field = "tweet")
docvars(tweets_corpus, field = "tweet") <- tweets_processed$stage
docvars(tweets_corpus)

toks <- tweets_corpus %>%
  tokens()

dfmkey <- dfm(toks) %>%
  dfm_group(stage) %>%
  textstat_keyness(target = "Stage3") %>%
  filter(feature %in% c("democracy", "elections"))

dfmkey$pstr <- ifelse(dfmkey$p < .001, "p < .001", paste("p = ", round(dfmkey$p, 4)))

g1 <- dfmkey %>%
  rename(word = feature) %>%
  ggplot(aes(x = word, y = chi2)) +
  geom_col(position = "dodge", width = .4, fill = "darkred") +
  geom_text(
    aes(label = pstr),
    colour = "white", size = 3,
    vjust = 0, hjust = 2
  ) +
  coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

g1
ggsave("data/plots/demtwts_keyness.png", width=250, height = 200,
       dpi=300, units="mm", bg = "white")

# daily frequencies
tidy_tweets <- shtwts %>%
  filter(!str_detect(tweet_trans, "^RT")) %>%
  # mutate(tweet = str_remove_all(tweet_trans, remove_reg)) %>%
  unnest_tokens(word, tweet_trans) %>%
  anti_join(get_stopwords(language = c("fr")) ) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

topwords <- tidy_tweets %>%
  group_by(word) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts))

#code stopwords to remove manually
topwords_coded <- read_csv("data/output/topwords_coded.csv")

topwords_coded <- topwords_coded %>%
  filter(remove==1) %>%
  pull(word)

mystopwords <- data_frame(word = topwords_coded)
dem_tidy_tweets <- anti_join(tidy_tweets, mystopwords, by = "word")

#word-level measure
targets <- topwords %>%
  filter(grepl("ntkab|democ|lection|dymqrat",word)) %>%
  pull(word)

# Note don't include "aldymqraty" because it refers mainly to PDP
targets <- targets[-c(3,7,11,22,47,49)] # remove non-relevant words
demwrdsdf <- as.data.frame(targets)
write_csv(demwrdsdf, "data/output/demwrds.csv")

targets <- paste0("\\b", targets, "\\b")
targets <- paste0(targets, collapse = "|")

dem_term_counts <- dem_tidy_tweets %>% 
  group_by(date) %>% 
  count(word, sort = TRUE)

dem_term_counts$demword <- as.integer(grepl(targets, 
                                            x = dem_term_counts$word))

dem_day_words <- dem_term_counts %>%
  filter(demword==1) %>%
  group_by(date) %>%
  summarise(ndem = sum(n)) %>%
  drop_na(date)

day_words <- dem_term_counts %>%
  group_by(date) %>%
  summarise(nall = sum(n)) %>%
  drop_na(date)

dem_all <- day_words %>%
  left_join(dem_day_words, by = "date")

dem_all$ndem <- ifelse(is.na(dem_all$ndem), 0, dem_all$ndem)
dem_all$propdem <- dem_all$ndem/dem_all$nall

g2 <- dem_all %>%
  ggplot(aes(date, propdem)) +
  geom_point(col="red", alpha=0.5, size=4) +
  geom_smooth(colour = "black", alpha=.3, method = "lm") +
  xlab("Day") +
  ylab("% democracy tweets") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

g2
ggsave("data/plots/demtwts.png", width=400, height = 400,
       dpi=300, units="mm", bg = "white")

# print democracy words for latex
targets <- topwords %>%
  filter(grepl("ntkab|democ|lection|dymqrat",word)) %>%
  pull(word)

# Note don't include "aldymqraty" because it refers mainly to PDP
targets <- targets[-c(3,7,11,22,47,49)] # remove non-relevant words
demwrdsdf <- as.data.frame(targets)
demwrdsdftex <- kable(demwrdsdf, "latex", booktabs=T)
writeLines(demwrdsdftex, "data/output/demwrdsdf.tex")



png(
  "data/plots/demtwts_combined.png",
  width = 450,
  height = 150,
  units = 'mm',
  res = 600
)
plot_grid(g2, g1, labels = "AUTO")
dev.off()
