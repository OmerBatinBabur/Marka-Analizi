install.packages(c("rvest","dplyr","stringr","purrr","tibble",
                   "tidytext","text2vec","e1071","class","caret"))
library(rvest)      # web scraping veri ??ekme
library(dplyr)      # veri manip??lasyonu
library(stringr)    # karakter ve metin manip??lasyonu
library(stringi)    # metin i??leme normalizasyon
library(ggplot2)    # veri g??rselle??tirme
library(purrr)      # fonksiyonlar
library(tibble)     # data frame
library(tidytext)   # metin verilerini analiz etmek
library(text2vec)   # metinleri say??sal verilere d??n????t??rme
library(e1071)      # Naive Bayes
library(class)      # KNN
library(caret)      # metrikler
library(wordcloud)  # kelime bulutu olu??turma


read_list_page <- function(base, brand_slug, page){
  url <- paste0(base, "?page=", page)
  Sys.sleep(runif(1, 1, 2))
  
  html <- read_html(url, encoding = "UTF-8")
  a_nodes <- html_elements(html, "h2 a")
  
  tibble(
    title = enc2utf8(html_text2(a_nodes)),
    href  = html_attr(a_nodes, "href")
  ) |>
    filter(!is.na(href), str_detect(href, paste0("^/", brand_slug, "/"))) |>
    mutate(url = paste0("https://www.sikayetvar.com", href)) |>
    distinct(url, .keep_all = TRUE)
}

collect_min_n <- function(base, brand_slug, min_n = 350, max_pages = 400){
  out <- tibble(title=character(), href=character(), url=character())
  for(p in 1:max_pages){
    one <- read_list_page(base, brand_slug, p)
    if(nrow(one) == 0) break
    out <- bind_rows(out, one) |> distinct(url, .keep_all = TRUE)
    cat(brand_slug, "page:", p, " total:", nrow(out), "\n")
    if(nrow(out) >= min_n) break
  }
  out |> slice_head(n = min_n)
}

lcw_list <- collect_min_n("https://www.sikayetvar.com/lc-waikiki", "lc-waikiki", 350) |> mutate(brand="lcw")
def_list <- collect_min_n("https://www.sikayetvar.com/defacto",    "defacto",    350) |> mutate(brand="defacto")

list_df <- bind_rows(lcw_list, def_list)

count(list_df, brand)

read_detail_page <- function(url){
  Sys.sleep(runif(1, 1, 2))
  
  html <- read_html(url, encoding = "UTF-8")
  
  text <- html %>%
    html_elements(".complaint-detail-description, .complaint-detail") %>%
    html_text2() %>%
    paste(collapse = " ") %>%
    str_squish()
  
  if (is.na(text) || nchar(text) < 50) text <- NA_character_
  
  tibble(text = text)
}

details_df <- list_df %>%
  mutate(
    text = map_chr(url, ~{
      tryCatch(read_detail_page(.x)$text[[1]],
               error = function(e) NA_character_)
    })
  )

# Kontrol
details_df %>% summarise(
  total = n(),
  text_ok = sum(!is.na(text)),
  text_na = sum(is.na(text))
)


df2 <- details_df %>%
  filter(!is.na(text)) %>%
  mutate(
    text_clean = str_to_lower(text),
    text_clean = str_replace_all(text_clean, "http\\S+|www\\S+", " "),
    text_clean = str_replace_all(text_clean, "[^\\p{L}\\s]", " "),
    text_clean = str_replace_all(text_clean, "\\s+", " "),
    text_clean = str_trim(text_clean),
    id = row_number()
  )

df2 %>% count(brand)


tr_stop <- c(
  "ve","veya","ile","de","da","ki","bir","bu","Eu","iC'in","ama","C'ok","az","daha","en",
  "mi","mD1","mu","mC<","olan","oldu","oluyor","var","yok","diye","ise","gibi","kadar",
  "lcw","defacto","waikiki","marka","firma"
)

tok <- df2 %>%
  select(id, brand, text_clean) %>%
  unnest_tokens(word, text_clean) %>%
  filter(nchar(word) >= 3) %>%
  filter(!word %in% tr_stop)

tok %>% count(word, sort = TRUE) %>% head(20)



freq_all <- tok %>% count(word, sort = TRUE)
write.csv(freq_all, "frekans_genel.csv", row.names = FALSE)
freq_all %>% head(20)



freq_brand <- tok %>%
  count(brand, word, sort = TRUE) %>%
  group_by(brand) %>%
  slice_max(n, n = 20) %>%
  ungroup()

write.csv(freq_brand, "frekans_marka.csv", row.names = FALSE)
freq_brand




positive_words <- c(
  "iyi","guzel","memnun","harika","super","tesekkur",
  "basarili","hizli","kaliteli","mutlu"
)

negative_words <- c(
  "kotu","berbat","rezalet","sikayet","sorun","problem","gecikme",
  "iptal","iade","hata","magdur","bekledim","ulasamadim","cozulmedi"
)

library(stringi)

tok2 <- tok %>%
  mutate(word_ascii = stringi::stri_trans_general(word, "Latin-ASCII"))


sentiment <- tok2 %>%
  mutate(
    duygu = case_when(
      word_ascii %in% positive_words ~ "Pozitif",
      word_ascii %in% negative_words ~ "Negatif",
      TRUE ~ "Notr"
    )
  )

sent_brand <- sentiment %>% count(brand, duygu)
sent_brand
write.csv(sent_brand, "duygu_marka.csv", row.names = FALSE)


wordcloud(
  words = word_freq$word,
  freq  = word_freq$n,
  max.words = 100,        # 150 yerine 100
  min.freq = 5,           # nadir kelimeleri at
  scale = c(4, 0.7),      # yazD1 boyutlarD1nD1 dengeler
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)



freq_lcw <- freq_brand %>% filter(brand == "lcw")

wordcloud(
  freq_lcw$word,
  freq_lcw$n,
  max.words = 80,
  min.freq = 5,
  scale = c(4, 0.7),
  random.order = FALSE,
  colors = brewer.pal(8, "Set2")
)



freq_def <- freq_brand %>% filter(brand == "defacto")

wordcloud(
  freq_def$word,
  freq_def$n,
  max.words = 80,
  min.freq = 5,
  scale = c(4, 0.7),
  random.order = FALSE,
  colors = brewer.pal(8, "Set2")
)



# Kelime bazlD1 duygu -> dokC<man bazlD1 duygu
doc_sentiment <- sentiment %>%
  group_by(id, brand) %>%
  summarise(
    pos = sum(duygu == "Pozitif"),
    neg = sum(duygu == "Negatif"),
    .groups = "drop"
  ) %>%
  mutate(
    predicted = case_when(
      neg > pos ~ "Negatif",
      pos > neg ~ "Pozitif",
      TRUE ~ "Notr"
    )
  )

head(doc_sentiment)



set.seed(42)

gold <- doc_sentiment %>%
  slice_sample(n = 20) %>%
  select(id, brand, predicted) %>%
  mutate(
    true_label = c(
      "Negatif","Negatif","Negatif","Pozitif","Negatif",
      "Negatif","Pozitif","Negatif","Negatif","Pozitif",
      "Negatif","Negatif","Pozitif","Negatif","Negatif",
      "Negatif","Pozitif","Negatif","Negatif","Pozitif"
    )
  )

library(caret)

# C6rnek: gold data frame'in var:
# gold$predicted, gold$true_label

levels_all <- c("Negatif","Pozitif","Notr")

pred <- factor(gold$predicted, levels = levels_all)
true <- factor(gold$true_label, levels = levels_all)

cm <- confusionMatrix(pred, true)
cm

accuracy <- cm$overall["Accuracy"]
error_rate <- 1 - accuracy
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
f1 <- cm$byClass["F1"]

metrics <- data.frame(
  Accuracy = as.numeric(accuracy),
  ErrorRate = as.numeric(error_rate),
  Precision = as.numeric(precision),
  Recall = as.numeric(recall),
  F1 = as.numeric(f1)
)
metrics
write.csv(metrics, "output/metrics.csv", row.names = FALSE)


names(gold)
table(gold$predicted)
table(gold$true_label)

gold_eval <- gold %>%
  filter(predicted != "Notr") %>%
  filter(true_label %in% c("Negatif", "Pozitif"))

table(gold_eval$predicted)
table(gold_eval$true_label)


library(caret)

pred <- factor(gold_eval$predicted, levels = c("Negatif","Pozitif"))
true <- factor(gold_eval$true_label, levels = c("Negatif","Pozitif"))

cm <- confusionMatrix(pred, true, positive = "Negatif")
cm



accuracy   <- cm$overall["Accuracy"]
error_rate <- 1 - accuracy
precision  <- cm$byClass["Precision"]
recall     <- cm$byClass["Recall"]
f1         <- cm$byClass["F1"]

metrics <- data.frame(
  Accuracy  = as.numeric(accuracy),
  ErrorRate = as.numeric(error_rate),
  Precision = as.numeric(precision),
  Recall    = as.numeric(recall),
  F1_Score  = as.numeric(f1)
)

metrics



dir.create("output", showWarnings = FALSE)
write.csv(metrics, "output/metrics.csv", row.names = FALSE)


library(ggplot2)

ggplot(sent_brand, aes(x = brand, y = n, fill = duygu)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Markalara GC6re Duygu DaDD1lD1mD1",
    x = "Marka",
    y = "Kelime SayD1sD1",
    fill = "Duygu"
  ) +
  theme_minimal()

sent_brand

freq_all %>% head(20)

freq_brand_percent <- freq_brand %>%
  group_by(brand) %>%
  mutate(percent = round(n / sum(n) * 100, 2))

freq_brand_percent

library(ggplot2)

freq_all %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "En SD1k GeC'en 15 Kelime",
    x = "Kelime",
    y = "Frekans"
  ) +
  theme_minimal()



wordcloud(
  words = freq_all$word,
  freq  = freq_all$n,
  max.words = 120,
  min.freq = 5,
  random.order = FALSE
)

tr_stop_ext <- c("ve","veya","ile","de","da","ki","bir","bu","su","o","i??in","ama","cok","az","daha","en","her","hem","ilgili","ragmen","mi","m","mu","mu","hem","her","ilk","iki","bana","beni","sonu??tan","ise","gibi","kadar","diye","icin","ancak","olan","oldu","oluyor","olmasi","vardi","var","yok","etti","edildi","yapildi","yapiyor","aldi","verdi","ben","sen","biz","siz","onlar","bana","sana","ona","sonra","once","simdi","bugun","yarin","artik","halen","lcw","defacto","waikiki","marka","firma","urun",)
  
tok_clean <- df2 %>%
    select(id, brand, text_clean) %>%
    unnest_tokens(word, text_clean) %>%
    filter(nchar(word) >= 3) %>%
    mutate(word = stringi::stri_trans_general(word, "Latin-ASCII")) %>%
    filter(!word %in% tr_stop_ext)

freq_all_new <- tok_clean %>%
  count(word, sort = TRUE)

freq_brand_new <- tok_clean %>%
  count(brand, word, sort = TRUE)


wordcloud(
  words = freq_all_new$word,
  freq  = freq_all_new$n,
  max.words = 120,
  min.freq = 5,
  scale = c(4, 0.6),
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)


freq_lcw_new <- freq_brand_new %>% filter(brand == "lcw")

wordcloud(
  freq_lcw_new$word,
  freq_lcw_new$n,
  max.words = 80,
  min.freq = 5,
  random.order = FALSE
)


freq_def_new <- freq_brand_new %>% filter(brand == "defacto")

wordcloud(
  freq_def_new$word,
  freq_def_new$n,
  max.words = 80,
  min.freq = 5,
  random.order = FALSE
)



  