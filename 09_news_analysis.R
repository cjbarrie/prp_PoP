library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(conText)
library(lubridate)
library(tsibble)
library(zoo)
library(ggthemes)
library(tidylog)
library(tidytext)
library(cowplot)

nws_corpus <- readRDS("data/output/nws_corpus.rds")

# daily frequencies
tidy_nws <- nws_corpus %>%
  unnest_tokens(word, content)

#get high frequency words
topwords <- tidy_nws %>%
  group_by(word) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts))

#word-level measure
targets <- topwords %>%
  filter(grepl("انتخاب|ديمقراطية",word)) %>%
  pull(word)

targets <- paste0("\\b", targets, "\\b")
targets <- paste0(targets, collapse = "|")

dem_term_counts <- tidy_nws %>% 
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

dem_all$date <- as.Date(dem_all$date)

g2 <- dem_all %>%
  ggplot(aes(date, propdem)) +
  geom_point(col="red", alpha=0.5, size=4) +
  geom_smooth(colour = "black", alpha=.3, method = "lm") +
  xlab("Day") +
  ylab("% democracy words") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

g2
ggsave("data/plots/demnws.png", width=400, height = 400,
       dpi=300, units="mm", bg = "white")


# keyness

nws_corpus$stage <- rep(NA, nrow(nws_corpus))
nws_corpus$datenum <- as.numeric(as.Date(nws_corpus$date))

nws_corpus$stage <- cut(nws_corpus$datenum, 
                    breaks = c(-Inf, 14975, 14983, Inf), 
                    labels = c("Stage1", "Stage2", "Stage3"), 
                    right = FALSE)

targelec <- topwords %>%
  filter(grepl("انتخاب",word)) %>%
  pull(word)

targdem <- topwords %>%
  filter(grepl("ديمقراطية",word)) %>%
  pull(word)


for (i in seq_along(targelec)) {
  
  targword <- targelec[[i]]
  nws_corpus$content <- gsub(targword, "elections", nws_corpus$content)
}

for (i in seq_along(targdem)) {
  
  targword <- targdem[[i]]
  nws_corpus$content <- gsub(targword, "democracy", nws_corpus$content)
}

nws_corpusq <- corpus(nws_corpus, text_field = "content")
docvars(nws_corpusq, field = "content") <- nws_corpusq$stage
docvars(nws_corpusq)

toks <- nws_corpusq %>%
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
ggsave("data/plots/demnws_keyness.png", width=250, height = 200,
       dpi=300, units="mm", bg = "white")

png(
  "data/plots/demnws_combined.png",
  width = 450,
  height = 150,
  units = 'mm',
  res = 600
)
plot_grid(g2, g1, labels = "AUTO")
dev.off()