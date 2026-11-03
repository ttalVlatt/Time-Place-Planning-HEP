## -----------------------------------------------------------------------------
##
##' [PROJ: Time and Place for (Sustainable) Campus Planning]
##' [FILE: STM]
##' [INIT: 2023-11-29]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

set.seed(2023)

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(patchwork)
library(stm)
library(stminsights)
library(tidytext)
library(gt)
library(tigris)
library(sf)

## ---------------------------
##' [Data]
## ---------------------------

data <- read_csv("Master-Plan-Text.csv")

## ---------------------------
##' [Descriptives]
## ---------------------------

data |> count(PLAN_AVAILABLE)

data <- data |> filter(PLAN_AVAILABLE == 1)

data |> summarize(char_min = min(str_length(PLAN_TEXT)),
                  char_max = max(str_length(PLAN_TEXT)),
                  char_median = median(str_length(PLAN_TEXT)),
                  word_min = min(str_count(PLAN_TEXT, "\\S+")),
                  word_max = max(str_count(PLAN_TEXT, "\\S+")),
                  word_median = median(str_count(PLAN_TEXT, "\\S+")))

data |> count(gov_plan_year)

## ---------------------------
##' [Pre-Processing]
## ---------------------------

data <- data |>
  mutate(STABBR = factor(STABBR) |> relevel(ref = "CA"),
         obereg = factor(obereg) |> relevel(ref = "Far West (AK, CA, HI, NV, OR, WA)") |> fct_drop(),
         PLAN_LEAD_CONSULT = factor(PLAN_LEAD_CONSULT) |> relevel(ref = "Other / None"),
         PLAN_YEAR_REF_2015 = factor(PLAN_YEAR) |> relevel(ref = "2015")) # Paris climate accords

plan_stop_words <- c("campus","master","planning","plan","university","college","building",
                     "space","students","student","shall","will","new","update","state",
                     "center","asf","gsf","facilities","facility","san","north","east",
                     "south","west",
                     state.name, state.abb)


## Setup as per https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf
p <- textProcessor(data$PLAN_TEXT,
                   metadata = data,
                   stem = FALSE,
                   customstopwords = plan_stop_words)
## Stem set to false after reviewing the results, seemed to be causing more issues than helping

pr <- plotRemoved(p$documents, lower.thresh = seq(1, 234, by = 1))

# word(data$PLAN_TEXT, end = 20)

plot_rem <- data.frame(Thresh = as.numeric(unlist(pr$lower.thresh)),
                       Docs = as.numeric(unlist(pr$ndocs)),
                       Words = as.numeric(unlist(pr$nwords)),
                       Tokens = as.numeric(unlist(pr$ntokens))) |>
  bind_rows(data.frame(Thresh = 0, Docs = 0, Words = 0, Tokens = 0))

plot_rem <- plot_rem |>
  pivot_longer(cols = c(Words, Tokens))


ggplot(plot_rem) +
  geom_line(aes(x = Thresh, y = value)) +
  geom_vline(aes(xintercept = 100), linetype = 2) +
  facet_wrap(~factor(name) |> relevel(ref = "Words"), scales = "free_y") +
  labs(x = "Word Must Appear in ... Documents",
       y = "Total Number of Tokens (Words) Removed",
       title = "Selecting Minimum Documnent Appearences") +
  #scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

# ggsave("R-Objects/Plot-Min-Appear.png", bg = "white")

o <- prepDocuments(p$documents, p$vocab, p$meta, lower.thresh = 100)

## ---------------------------
##' [Identify k]
## ---------------------------

if(file.exists("R-Objects/Search-K.Rdata")) {load("R-Objects/Search-K.Rdata")} else {
  
  k_spectral <- searchK(o$documents, o$vocab, data = o$meta,
               init.type = "Spectral",
               heldout.seed = 2023,
               cores = 8, ##windows < 1
               K = c(2:20))
  
  k_lda <- searchK(o$documents, o$vocab, data = o$meta,
                init.type = "LDA",
                heldout.seed = 2023,
                cores = 8, ##windows < 1
                K = c(2:20))
  
  save(list = c("k_spectral", "k_lda"),
       file = "R-Objects/Search-K.Rdata")
  
}

k_search_spectral <- data.frame(type = "Spectral",
                                k = as.numeric(unlist(k_spectral$results$K)),
                                e = as.numeric(unlist(k_spectral$results$exclus)),
                                c = as.numeric(unlist(k_spectral$results$semcoh)))

k_search_lda <- data.frame(type = "LDA",
                           k = as.numeric(unlist(k_lda$results$K)),
                           e = as.numeric(unlist(k_lda$results$exclus)),
                           c = as.numeric(unlist(k_lda$results$semcoh)))

k_search <- bind_rows(k_search_spectral, k_search_lda)
  
plot_exclusivity <- ggplot(data = k_search,
       aes(x = k, y = e, color = type)) +
  annotate(geom = "rect", xmin = 5, xmax = 15, ymin = -Inf, ymax = Inf,
           color = "lightgrey", alpha = 0.2) + # https://stackoverflow.com/questions/17521438/geom-rect-and-alpha-does-this-work-with-hard-coded-values
  geom_vline(aes(xintercept = 10), linetype = 5) +
  geom_line() +
  geom_smooth(se = FALSE, linetype = 2) +
  labs(x = "Number of Topics (k)",
       y = "Exclusivity",
       color = "Initialization") +
  scale_color_grey(start = 0, end = 0.6) +
  theme_minimal()

plot_cohearence <- ggplot(data = k_search,
       aes(x = k, y = c, color = type)) +
  annotate(geom = "rect", xmin = 5, xmax = 15, ymin = -Inf, ymax = Inf,
           color = "lightgrey", alpha = 0.2) + # https://stackoverflow.com/questions/17521438/geom-rect-and-alpha-does-this-work-with-hard-coded-values
  geom_vline(aes(xintercept = 10), linetype = 5) +
  geom_line() +
  geom_smooth(se = FALSE, linetype = 2) +
  labs(x = "Number of Topics (k)",
       y = "Semantic Coherence",
       color = "Initialization") +
  scale_color_grey(start = 0, end = 0.6) +
  theme_minimal()

plot_exclusivity + plot_cohearence +
  #plot_annotation(title = "Selecting Number of Topics") +
  plot_layout(guides = "collect")

# ggsave("R-Objects/Plot-Search-K.png", bg = "white")

## ---------------------------
##' [Fit & Evaluate STM for identified range of k]
## ---------------------------

## Runtime: > 3-4 hours
prep <- prepDocuments(o$documents, o$vocab, o$meta)
o$documents <- prep$documents
o$vocab <- prep$vocab
o$meta <- prep$meta 

if(file.exists("R-Objects/Many-Topics.Rdata")) { load("R-Objects/Many-Topics.Rdata") } else {
  
  m_spectral <- manyTopics(
    documents = o$documents,
    vocab = o$vocab,
    data = o$meta,
    prevalence = ~ PLAN_YEAR + obereg + PLAN_LEAD_CONSULT +
      app_plan_year + grant_plan_year + leg_plan_year + gov_plan_year,
    init.type = "Spectral",
    K = c(5:15),
    max.em.its = 500,
    seed = 2023,
  )
  
  m_lda <- manyTopics(
    documents = o$documents,
    vocab = o$vocab,
    data = o$meta,
    prevalence = ~ PLAN_YEAR + obereg + PLAN_LEAD_CONSULT +
      app_plan_year + grant_plan_year + leg_plan_year + gov_plan_year,
    init.type = "LDA",
    K = c(5:15),
    max.em.its = 500,
    seed = 2023
  )
  
  save(list = c("m_spectral", "m_lda"),
       file = "R-Objects/Many-Topics.Rdata")
  
}

labelTopics(m_spectral$out[[1]], n = 10) # 1 academic building renovation, 2 mixed policy/transport/conservation, 3 mixed utilities/landscape/parking, 4 mixed transport/grounds, 5 utilization
labelTopics(m_spectral$out[[2]], n = 10) # 1 academic building renovation, 2 mixed policy, 3 mixed sustainability, 4 mixed transport/grounds, 5 utilization, 6 mixed transportation
labelTopics(m_spectral$out[[3]], n = 10) # 1 academic building renovation, 2 mixed economics/enroll, 3 mixed utilities/parking/landscape, 4 streets, 5 utilization, 6 mixed transportation/design, 7 mixed utilities
labelTopics(m_spectral$out[[4]], n = 10) # 1 academic building renovation, 2 enrollment/growth, 3 sustainability/utilities, 4 streets/transportation, 5 utilization, 6 mixed design/transport, 7 conservation, 8 mixed ladnscape/pedestrian/housing
labelTopics(m_spectral$out[[5]], n = 10) # 1 academic building renovation, 2 research, 3 utilities, 4 streets/mobility, 5 utilization, 6 urban design, 7 conservation policies, 8 mixed transport/landscape, 9 parking/costs
labelTopics(m_spectral$out[[6]], n = 10) # 1 academic building renovation, 2 research/tech/market, 3 utilities/sustainability, 4 streets/mobility, 5 utilities, 6 urban design, 7 conservation policies, 8 mixed transport/athletics/landscape, 9  parking, 10 utilization
labelTopics(m_spectral$out[[7]], n = 10) # 1 renovation, 2 enrollment forcasting, 3 research/science, 4 sustainability collaboration, 5 utilities, 6 parking/transport, 7 conservation policy, 8 mixed transport/landscape, 9 parking, 10 utilization, 11 streets/transport
labelTopics(m_spectral$out[[8]], n = 10) # 1 buildings/halls/renovatio , 2 parking/enrollment, 3 utilities/sustainability, 4 mixed sustainability/collaberation/welcome, 5 rennovation, 6 urban design, 7 conservation, 8 mixed transport/land, 9 parking, 10 utilization, 11 streets, 12 science
labelTopics(m_spectral$out[[9]], n = 10) # 1 buildings/renovation, 2 parking/enrollment, 3 sustainability, 4 mixed grounds/fluffy colab, 5 utilities, 6 urban design, 7 conservation, 8 mixed housing/development, 9 very mixed, 10 utilization, 11 streets/districts, 12 science, 13 mixed athletics/parking
labelTopics(m_spectral$out[[10]], n = 10) # 1 buildings/renovation, 2 mixed parking/enroll, 3 mixed transport/sustainability, 4 mixed grounds/fluff, 5 renovation, 6 utilities, 7 conservation, 8 mixed land use, 9 very mixed, 10 utilization, 11 streets/districts, 12 science, 13 mixed parking/housing/athletics, 14 urba design
labelTopics(m_spectral$out[[11]], n = 10) # 1 mixed hall/renovation, 2 mixed estate/enroll/parking, 3 mixed/sustainability,  4 mixed grounds/fluff, 5 renovation, 6 utilities, 7 conservation, 8 mixed, 9 very mixed, 10 utilization, 11 mixed districts, 12 science, 13 mixed parking/housing/athletics, 14 streets, 15 reserach/science


labelTopics(m_lda$out[[1]], n = 10) # 1 generic/land/street/hall, 2 mixed research/housing, 3 water/construction, 4 mixed, 5 mixed growth/utilization
labelTopics(m_lda$out[[2]], n = 10) # 1 street/transporation, 2 buildings/pedestrain, 3 utilization, 4 mixed utiliites/tranpsort, 5 mixed planning, 6 occupancy/enrollment
labelTopics(m_lda$out[[3]], n = 10) # 1 buildings/outside spaces, 2 parking/mixed growth, 3 utilziation, 4 mixed land uses, 5 infrastructure improvement, 6 mixed systems, 7 mixed green
labelTopics(m_lda$out[[4]], n = 10) # 1 street/hall/pedestrian, 2 mixed parking/landscape, 3 mobility/housing, 4 parking/growth, 5 mixed policies/conservation, 6 utilites, 7 rennovation, 8 occupancy/utilization
labelTopics(m_lda$out[[5]], n = 10) # 1 generic grounds, 2 occupancy, 3 conservation, 4 mixed construction , 5 sustainability collab, 6 growth/enrollment, 7 rennovation, 8 parking, 9 urban design 
labelTopics(m_lda$out[[6]], n = 10) # 1 utilization, 2 architecutre/landscape, 3 mixed utility conservation, 4 mobility/streets, 5 labs, 6 sustainability collab, 7 mixed, 8 academic buildings/innovation, 9 mixed land use, 10 utilities
labelTopics(m_lda$out[[7]], n = 10) # 1 mixed mobility, 2 streets/urban design, 3 sciences, 4 mixed conservation, 5 utils, 6 mixed parking, 7 utilization, 8 housing/campus community, 9 parking, 10 enrollment, 11 sustainability
labelTopics(m_lda$out[[8]], n = 10) # 1 axis/annex, 2 parking, 3 mixed buildings, 4 housing transport, 5 mixed landscape, 6 occupancy, 7 interdisciplanry research, 8 renovation/hall/academic, 9 generic/land/development, 10 policy/management/conservation, 11 mixed, 12 streets
labelTopics(m_lda$out[[9]], n = 10) # 1 mixed agriculature, 2 science, 3 urban design, 4 building infrasture, 5 conservation policies, 6 generic/parking/landscape, 7 transportation, 8 parking, 9 mixed, 10 mixed parking, 11 utilization, 12 renovation, 13 mixed mobility collab
labelTopics(m_lda$out[[10]], n = 10) # 1 utilities, 2 mixed academic, 3 downtown/parking, 4 mixed, 5 community, 6 utilization, 7 mixed, 8 parking/mixed, 9 mixed, 10 traffic, 11 parking/demand, 12 mixed, 13 generic street/district, 14 mixed policies
labelTopics(m_lda$out[[11]], n = 10) # 1 utilties, 2 science, 3 mixed housing, 4 utilization, 5 devlopment/city, 6 mixed, 7 sustainability, 8 housing/enrollment, 9 mixed, 10 mixed, 11 mixed parking, 12 rennovation, 13 mixed policies, 14 mixed transport, 15 street/greenspace/pedestrian

stm_model <- m_spectral$out[[6]]

## ---------------------------
##' [Explore Selected Topic Model]
## ---------------------------

##'[Topic Labels]

keywords <- labelTopics(stm_model, n = 10)
keywords

topic_labels <- c("1: Renovation",
                  "2: Research & Science",
                  "3: Sustainability",
                  "4: Campus Design: Development",
                  "5: Utilities",
                  "6: Campus Design: Landscape",
                  "7: Conservation Policies",
                  "8: Campus Facilities",
                  "9: Parking",
                  "10: Occupancy & Utilization")

prob <- keywords[["prob"]] |>
  as_tibble() |>
  unite(col = "Highest Probability", everything(), sep = ", ")

frex <- keywords[["frex"]] |>
  as_tibble() |>
  unite(col = "Frequent & Exclusive", everything(), sep = ", ")

lift <- keywords[["lift"]] |>
  as_tibble() |>
  unite(col = "Lift Score", everything(), sep = ", ")

score <- keywords[["score"]] |>
  as_tibble() |>
  unite(col = "LDA Score", everything(), sep = ", ")

topic_table <- bind_cols(prob, frex, lift, score) |>
  mutate(Topic = topic_labels, .before = `Highest Probability`)

table_keywords <- gt(topic_table) |>
  #tab_header(title = md("Identified Topic Keywords")) |>
  tab_style(locations = list(cells_column_labels(), cells_body(columns = 1)),
                             cell_text(weight = "bold")) |>
  tab_style(locations = cells_body(),
            style = "vertical-align:top") |>
  tab_options(table_body.hlines.color = "lightgrey",
              table_body.hlines.width =  px(2))

# save(list = "table_keywords",
#      file = "R-Objects/Table-Keywords.Rdata")

##'[Topic Proportions]

t <- tidy(stm_model, "theta") |>
  pivot_wider(names_from = topic, names_prefix = "topic_", values_from = gamma)

max_topic_docs <- tibble()

for(i in colnames(t)[2:10]) {
  
  max_topic_doc <- t |>
    slice_max(.data[[i]]) |>
    select(document, i)
  
  max_topic_docs <- bind_rows(max_topic_docs, max_topic_doc)
  
}

max_topic_docs <- max_topic_docs |>
  mutate(UNITID = data$UNITID[max_topic_docs$document],
         INSTNM = data$INSTNM[max_topic_docs$document],
         .after = document)


topic_proportions <- t |>
  summarize(across(starts_with("topic_"), ~median(.))) |>
  pivot_longer(cols = everything(), names_to = "topic", values_to = "proportion") |>
  mutate(topic_labels = topic_labels)

ggplot(topic_proportions) +
  geom_col(aes(y = reorder(topic_labels, proportion), x = proportion),
           width = 0.33, color = "black", fill = "lightgrey") +
  theme_minimal() +
  labs(y = NULL,
       x = "Median Proportion of Documents")
       #title = "Topic Document Proportions")

# ggsave("R-Objects/Plot-Topic-Proportions.png", bg = "white")

## ---------------------------
##' [STM-ing Original Model]
## ---------------------------

set.seed(1933) # Need to reset seed each time if re-running the model (rather than running whole script) to get exact same result
linear_model <- estimateEffect(1:10 ~ PLAN_YEAR + obereg + PLAN_LEAD_CONSULT,
                               stm_model,
                               meta = o$meta)

summary(linear_model)

model_tables <- tibble()

for(i in 1:10) {
  
  model_table <- summary(linear_model)[["tables"]][[i]] |>
    as_tibble(rownames = "Variable") |>
    filter(`Pr(>|t|)` < 0.05) |>
    mutate(topic = topic_labels[[i]], .before = Variable)
  
  model_tables <- bind_rows(model_tables, model_table)
  
}

model_tables <- model_tables |>
  filter(Variable != "(Intercept)") |>
  mutate(Variable = str_remove_all(Variable, "obereg|PLAN_LEAD_CONSULT"),
         Variable = str_replace_all(Variable, "PLAN_YEAR", "Plan Year"),
         Variable = str_replace(Variable, "app_plan_year", "% Revenue: State Appropriations"),
         Variable = str_replace(Variable, "grant_plan_year", "% Revenue: State Grants"),
         Variable = str_replace(Variable, "grant_plan_year", "% Revenue: State Grants"),
         Variable = str_replace(Variable, "leg_plan_year", "State Legislature: "),
         Variable = str_replace(Variable, "gov_plan_year", "Governor: ")
         )

table_model <- gt(model_tables) |>
  fmt_number(decimals = 3) |>
  tab_row_group(label = topic_labels[10], rows = str_detect(topic, topic_labels[10])) |>
  tab_row_group(label = topic_labels[9], rows = str_detect(topic, topic_labels[9])) |>
  tab_row_group(label = topic_labels[8], rows = str_detect(topic, topic_labels[8])) |>
  tab_row_group(label = topic_labels[7], rows = str_detect(topic, topic_labels[7])) |>
  tab_row_group(label = topic_labels[6], rows = str_detect(topic, topic_labels[6])) |>
  tab_row_group(label = topic_labels[5], rows = str_detect(topic, topic_labels[5])) |>
  tab_row_group(label = topic_labels[4], rows = str_detect(topic, topic_labels[4])) |>
  tab_row_group(label = topic_labels[3], rows = str_detect(topic, topic_labels[3])) |>
  tab_row_group(label = topic_labels[2], rows = str_detect(topic, topic_labels[2])) |>
  tab_row_group(label = topic_labels[1], rows = str_detect(topic, topic_labels[1])) |>
  cols_hide(c(topic, `t value`)) |>
  text_transform(locations = cells_body(`Pr(>|t|)`),
                 fn = function(x) ifelse(x == "0.000", "<0.001", x)) |> # https://stackoverflow.com/questions/68911736/special-zero-value-in-gt-tables
  text_transform(locations = cells_body(`Pr(>|t|)`),
                 fn = function(x) case_when(x < "0.001" ~ paste(x, "***"),
                                            x < "0.01" ~ paste(x, "**"),
                                            x < "0.05" ~ paste(x, "*"),
                                            TRUE ~ x)) |> 
  tab_style(locations = cells_row_groups(),
            cell_text(weight = "bold")) |>
  tab_style(locations = cells_row_groups(),
            cell_text(weight = "bold")) |>
  cols_label(Variable = md("**Variable**"),
             Estimate = md("***\U03B2***"),
             `Std. Error` = md("***SE***"),
             `Pr(>|t|)` = md("***p* value**")) |>
 # tab_header(title = md("Statistically Significant Changes in Topic Prevalence"))
  tab_options(table_body.hlines.color = "transparent") 
#|>
  #tab_style(style = cell_borders(sides = c("top"),  weight = px(2), color = "lightgrey"),
           # locations = cells_body(rows = c(1,5,9,10,16,18,22,27,34,36)))


table_model

# gtsave(table_model, "rr_table2.docx")

## ---------------------------
##' [STM-ing with Political Variables without Region]
## ---------------------------

set.seed(1933) # Need to reset seed each time if re-running the model (rather than running whole script) to get exact same result
linear_model <- estimateEffect(1:10 ~ PLAN_YEAR  + PLAN_LEAD_CONSULT + # obereg +
                                 app_plan_year + grant_plan_year + leg_plan_year + gov_plan_year,
                               stm_model,
                               meta = o$meta)

summary(linear_model)

model_tables <- tibble()

for(i in 1:10) {
  
  model_table <- summary(linear_model)[["tables"]][[i]] |>
    as_tibble(rownames = "Variable") |>
    filter(`Pr(>|t|)` < 0.05) |>
    mutate(topic = topic_labels[[i]], .before = Variable)
  
  model_tables <- bind_rows(model_tables, model_table)
  
}

model_tables <- model_tables |>
  filter(Variable != "(Intercept)") |>
  mutate(Variable = str_remove_all(Variable, "obereg|PLAN_LEAD_CONSULT"),
         Variable = str_replace_all(Variable, "PLAN_YEAR", "Plan Year"),
         Variable = str_replace(Variable, "app_plan_year", "% Revenue: State Appropriations"),
         Variable = str_replace(Variable, "grant_plan_year", "% Revenue: State Grants"),
         Variable = str_replace(Variable, "grant_plan_year", "% Revenue: State Grants"),
         Variable = str_replace(Variable, "leg_plan_year", "State Legislature: "),
         Variable = str_replace(Variable, "gov_plan_year", "Governor: ")
         )

table_model <- gt(model_tables) |>
  fmt_number(decimals = 3) |>
  tab_row_group(label = topic_labels[10], rows = str_detect(topic, topic_labels[10])) |>
  tab_row_group(label = topic_labels[9], rows = str_detect(topic, topic_labels[9])) |>
  tab_row_group(label = topic_labels[8], rows = str_detect(topic, topic_labels[8])) |>
  tab_row_group(label = topic_labels[7], rows = str_detect(topic, topic_labels[7])) |>
  tab_row_group(label = topic_labels[6], rows = str_detect(topic, topic_labels[6])) |>
  tab_row_group(label = topic_labels[5], rows = str_detect(topic, topic_labels[5])) |>
  tab_row_group(label = topic_labels[4], rows = str_detect(topic, topic_labels[4])) |>
  tab_row_group(label = topic_labels[3], rows = str_detect(topic, topic_labels[3])) |>
  tab_row_group(label = topic_labels[2], rows = str_detect(topic, topic_labels[2])) |>
  tab_row_group(label = topic_labels[1], rows = str_detect(topic, topic_labels[1])) |>
  cols_hide(c(topic, `t value`)) |>
  text_transform(locations = cells_body(`Pr(>|t|)`),
                 fn = function(x) ifelse(x == "0.000", "<0.001", x)) |> # https://stackoverflow.com/questions/68911736/special-zero-value-in-gt-tables
  text_transform(locations = cells_body(`Pr(>|t|)`),
                 fn = function(x) case_when(x < "0.001" ~ paste(x, "***"),
                                            x < "0.01" ~ paste(x, "**"),
                                            x < "0.05" ~ paste(x, "*"),
                                            TRUE ~ x)) |> 
  tab_style(locations = cells_row_groups(),
            cell_text(weight = "bold")) |>
  tab_style(locations = cells_row_groups(),
            cell_text(weight = "bold")) |>
  cols_label(Variable = md("**Variable**"),
             Estimate = md("***\U03B2***"),
             `Std. Error` = md("***SE***"),
             `Pr(>|t|)` = md("***p* value**")) |>
 # tab_header(title = md("Statistically Significant Changes in Topic Prevalence"))
  tab_options(table_body.hlines.color = "transparent") 
#|>
  #tab_style(style = cell_borders(sides = c("top"),  weight = px(2), color = "lightgrey"),
           # locations = cells_body(rows = c(1,5,9,10,16,18,22,27,34,36)))


table_model

# gtsave(table_model, "rr_table3.docx")

## ---------------------------
##' [STM-ing with All Variables]
## ---------------------------

set.seed(1933) # Need to reset seed each time if re-running the model (rather than running whole script) to get exact same result
linear_model <- estimateEffect(1:10 ~ PLAN_YEAR  + PLAN_LEAD_CONSULT + obereg +
                                 app_plan_year + grant_plan_year + leg_plan_year + gov_plan_year,
                               stm_model,
                               meta = o$meta)

summary(linear_model)

model_tables <- tibble()

for(i in 1:10) {
  
  model_table <- summary(linear_model)[["tables"]][[i]] |>
    as_tibble(rownames = "Variable") |>
    filter(`Pr(>|t|)` < 0.05) |>
    mutate(topic = topic_labels[[i]], .before = Variable)
  
  model_tables <- bind_rows(model_tables, model_table)
  
}

model_tables <- model_tables |>
  filter(Variable != "(Intercept)") |>
  mutate(Variable = str_remove_all(Variable, "obereg|PLAN_LEAD_CONSULT"),
         Variable = str_replace_all(Variable, "PLAN_YEAR", "Plan Year"),
         Variable = str_replace(Variable, "app_plan_year", "% Revenue: State Appropriations"),
         Variable = str_replace(Variable, "grant_plan_year", "% Revenue: State Grants"),
         Variable = str_replace(Variable, "grant_plan_year", "% Revenue: State Grants"),
         Variable = str_replace(Variable, "leg_plan_year", "State Legislature: "),
         Variable = str_replace(Variable, "gov_plan_year", "Governor: ")
         )

table_model <- gt(model_tables) |>
  fmt_number(decimals = 3) |>
  tab_row_group(label = topic_labels[10], rows = str_detect(topic, topic_labels[10])) |>
  tab_row_group(label = topic_labels[9], rows = str_detect(topic, topic_labels[9])) |>
  tab_row_group(label = topic_labels[8], rows = str_detect(topic, topic_labels[8])) |>
  tab_row_group(label = topic_labels[7], rows = str_detect(topic, topic_labels[7])) |>
  tab_row_group(label = topic_labels[6], rows = str_detect(topic, topic_labels[6])) |>
  tab_row_group(label = topic_labels[5], rows = str_detect(topic, topic_labels[5])) |>
  tab_row_group(label = topic_labels[4], rows = str_detect(topic, topic_labels[4])) |>
  tab_row_group(label = topic_labels[3], rows = str_detect(topic, topic_labels[3])) |>
  tab_row_group(label = topic_labels[2], rows = str_detect(topic, topic_labels[2])) |>
  tab_row_group(label = topic_labels[1], rows = str_detect(topic, topic_labels[1])) |>
  cols_hide(c(topic, `t value`)) |>
  text_transform(locations = cells_body(`Pr(>|t|)`),
                 fn = function(x) ifelse(x == "0.000", "<0.001", x)) |> # https://stackoverflow.com/questions/68911736/special-zero-value-in-gt-tables
  text_transform(locations = cells_body(`Pr(>|t|)`),
                 fn = function(x) case_when(x < "0.001" ~ paste(x, "***"),
                                            x < "0.01" ~ paste(x, "**"),
                                            x < "0.05" ~ paste(x, "*"),
                                            TRUE ~ x)) |> 
  tab_style(locations = cells_row_groups(),
            cell_text(weight = "bold")) |>
  tab_style(locations = cells_row_groups(),
            cell_text(weight = "bold")) |>
  cols_label(Variable = md("**Variable**"),
             Estimate = md("***\U03B2***"),
             `Std. Error` = md("***SE***"),
             `Pr(>|t|)` = md("***p* value**")) |>
 # tab_header(title = md("Statistically Significant Changes in Topic Prevalence"))
  tab_options(table_body.hlines.color = "transparent") 
#|>
  #tab_style(style = cell_borders(sides = c("top"),  weight = px(2), color = "lightgrey"),
           # locations = cells_body(rows = c(1,5,9,10,16,18,22,27,34,36)))


table_model

# gtsave(table_model, "rr_table4.docx")