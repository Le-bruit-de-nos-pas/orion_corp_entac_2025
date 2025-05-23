library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)


LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

uniques <- data.frame(unique(LEDD_Concomitant_Medication_Log_12Feb2025$LEDTRT))

# fwrite(uniques, "unique_LEDD_Concomitant_Medication_v2.txt")


# Read data
drug_data <- read.table("unique_LEDD_Concomitant_Medication_v2.txt", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

colnames(drug_data) <- "Therapy"





drug_class_map <- list(
  # Levodopa and combinations
  "levodopa" = "Levodopa",
  "carbidopa" = "Levodopa",
  "sinemet" = "Levodopa",
  "sinement" = "Levodopa",
  "rytary" = "Levodopa",
  "madopar" = "Levodopa",
  "inbrija" = "Levodopa",
  "duopa" = "Intestinal levodopa Gel",
  "duodopa" = "Intestinal levodopa Gel",
  "isicom" = "Levodopa",
  "levocomp" = "Levodopa",
  "prolopa" = "Levodopa",
  "nacom" = "Levodopa",
  "dopicar" = "Levodopa",
  "levopar" = "Levodopa",
  "co-careldopa" = "Levodopa",
  "l-dopa" = "Levodopa",
  "levodapa" = "Levodopa",
  "levidopa" = "Levodopa",
  "cabidopa" = "Levodopa",
  "carb/levo" = "Levodopa",
  "ledopoda" = "Levodopa",
  "levodop" = "Levodopa",
  "co-carledopa" = "Levodopa",
  "levodpa" = "Levodopa",
  "azilekt" = "MAO Inhibitor",
  "sinamet" = "Levodopa",
  "rytari" = "Levodopa",
  "rytari er" = "Levodopa",
  "rytari (36.25/145mg)" = "Levodopa",
  "rytray 61.25mg/245mg" = "Levodopa",
  "gocoveri" = "Levodopa",
  "levadopa" = "Levodopa",


  # MAO Inhibitors
  "selegiline" = "MAO Inhibitor",
  "selegline" = "MAO Inhibitor",
  "rasagiline" = "MAO Inhibitor",
  "rasigiline" = "MAO Inhibitor",
  "azilect" = "MAO Inhibitor",
  "safinamide" = "MAO Inhibitor",
  "xadago" = "MAO Inhibitor",
  "eldepryl" = "MAO Inhibitor",
  "rasagilina" = "MAO Inhibitor",
  "selegelina" = "MAO Inhibitor",
  "rasagilima" = "MAO Inhibitor",
  "rasagilene" = "MAO Inhibitor",
  "rasagalin" = "MAO Inhibitor",
  "rasalgiline" = "MAO Inhibitor",
  "rasageline" = "MAO Inhibitor",
  "selegelin" = "MAO Inhibitor",
  "selegelene" = "MAO Inhibitor",
  "selegeline" = "MAO Inhibitor",
  "selegrine" = "MAO Inhibitor",
  "rasagaline" = "MAO Inhibitor",
  "rasagilin" = "MAO Inhibitor",
  "rasalgilina" = "MAO Inhibitor",
  "RASIGALINE" = "MAO Inhibitor",
  "selegilin" = "MAO Inhibitor",
  "selegilene" = "MAO Inhibitor",
  "safinamida" = "MAO Inhibitor",

  # Dopamine Agonists
  "ropinirole" = "Dopamine Agonist",
  "ropinirol" = "Dopamine Agonist",
  "pramipexole" = "Dopamine Agonist",
  "mirapex" = "Dopamine Agonist",
  "requip" = "Dopamine Agonist",
  "rotigotine" = "Dopamine Agonist",
  "neupro" = "Dopamine Agonist",
  "sifrol" = "Dopamine Agonist",
  "ropinerole" = "Dopamine Agonist",
  "ropinirone" = "Dopamine Agonist",
  "pramiprexole" = "Dopamine Agonist",
  "pramiprexol" = "Dopamine Agonist",
  "ropirinol" = "Dopamine Agonist",
  "ropinrole" = "Dopamine Agonist",
  "ropinirone" = "Dopamine Agonist",
  "rotigotina" = "Dopamine Agonist",
  "rotigine" = "Dopamine Agonist",
  "leganto" = "Dopamine Agonist",
  "clarium" = "Dopamine Agonist",
  "piribedil" = "Dopamine Agonist",
  "pramipexol" = "Dopamine Agonist",
  "prampexole" = "Dopamine Agonist",
  "neurpro" = "Dopamine Agonist",
  "nupro" = "Dopamine Agonist",
  "pramiplexole" = "Dopamine Agonist",
  "roprinirole" = "Dopamine Agonist",
  "rotigotin" = "Dopamine Agonist",
  "rotigotin patch" = "Dopamine Agonist",
  "nuepro patch" = "Dopamine Agonist",
  "neuropatch" = "Dopamine Agonist",

  # Amantadine
  "amantadine" = "Amantadine",
  "amantdine" = "Amantadine",
  "gocovri" = "Amantadine",
  "osmolex" = "Amantadine",
  "pk-merz" = "Amantadine",
  "pk merz" = "Amantadine",
  "amandtadine" = "Amantadine",
  "amandatine" = "Amantadine",
  "amandatin" = "Amantadine",
  "amantadin" = "Amantadine",
  "amandadina" = "Amantadine",
  "amandadin" = "Amantadine",
  "pk- merz" = "Amantadine",
  "liquid amantidine" = "Amantadine",

  # COMT Inhibitors
  "entacapone" = "COMT inhibitor",
  "comtan" = "COMT inhibitor",
  "ongentys" = "COMT inhibitor",
  "opicapone" = "COMT inhibitor",
  "opicapon" = "COMT inhibitor",
  "entacapon" = "COMT inhibitor",
  "entacapona" = "COMT inhibitor",
  "entacapon" = "COMT inhibitor",
  "entacaopne" = "COMT inhibitor",
  "entacaprone" = "COMT inhibitor",
  "entcapone" = "COMT inhibitor",
  "tasmar" = "COMT inhibitor",
  "tolcapone" = "COMT inhibitor",
  "stalevo" = "Levodopa/COMT inhibitor",

  # Subcutaneous Apomorphine
  "apomorphine" = "Subcutaneous Apomorphine",
  "apokyn" = "Subcutaneous Apomorphine",
  "apo-go" = "Subcutaneous Apomorphine",
  "kynmobi" = "Subcutaneous Apomorphine",
  "apomorfin" = "Subcutaneous Apomorphine",
  "aphoorphine" = "Subcutaneous Apomorphine",
  "apomorphine" = "Subcutaneous Apomorphine",
  "apokyn" = "Subcutaneous Apomorphine",
  "apo-go" = "Subcutaneous Apomorphine",
  "kynmobi" = "Subcutaneous Apomorphine",
  "apomorfin" = "Subcutaneous Apomorphine",
  
  # Levodopa/Intestinal levodopa Gel
  "dopa pump" = "Levodopa/Intestinal levodopa Gel",

  # Anticholinergic
  "trihexyphenidyl" = "Anticholinergic",
  "TRIHEXYPHENIDATE" = "Anticholinergic",
  "TRIHEXYPHENIDOL" = "Anticholinergic",
  "trihexyphenidyl (artane)" = "Anticholinergic",
  "trihexyphenidyl tablet" = "Anticholinergic",
  "trihexphenidyl" = "Anticholinergic",
  "trihexypenidate" = "Anticholinergic",
  "trihexypenidol" = "Anticholinergic",
  "trihexifenidyl" = "Anticholinergic",
  "artane (trihexyphenidyl tablet)" = "Anticholinergic",
  "artane (trihexyphenidyl elixir)" = "Anticholinergic",
  "hipokinon (trihexyphenidyl)" = "Anticholinergic",
  "cogentin (benztropine)" = "Anticholinergic",
  "benztropine" = "Anticholinergic",
  
  # Other
  "istradefylline" = "Other",
  "zonegran" = "Other"
)


drug_class_map


# Map function
map_therapy_to_class <- function(therapy) {
  hits <- sapply(names(drug_class_map), function(keyword) {
    if (str_detect(tolower(therapy), keyword)) return(drug_class_map[[keyword]]) else return(NA)
  })
  hits <- na.omit(hits)
  if (length(hits) == 0) return(NA)
  return(paste(unique(hits), collapse = "/"))
}


# Apply mapping
drug_data$Class <- sapply(drug_data$Therapy, map_therapy_to_class)


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>%
  select(REC_ID, PATNO, EVENT_ID, LEDTRT, STARTDT, STOPDT, ORIG_ENTRY) %>%
  left_join(drug_data, by=c("LEDTRT"="Therapy"))

LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(Class) %>% count() %>%
  arrange(-n)

LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(is.na(Class)) %>%
  select(LEDTRT) %>% distinct()



LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>%
  mutate(ORIG_ENTRY=as.Date(paste("01/", as.character(ORIG_ENTRY)), "%d/%m/%Y"))  %>%
  mutate(STARTDT=as.Date(paste("01/", as.character(STARTDT)), "%d/%m/%Y"))  %>%
  mutate(STOPDT=as.Date(paste("01/", as.character(STOPDT)), "%d/%m/%Y")) 


unique(LEDD_Concomitant_Medication_Log_12Feb2025$Class)


PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

unique(PPMI_Curated_Data_Cut_Public_20241211$subgroup)
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(duration_yrs, hy_on, PATNO, visit_date)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 

data <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  select(PATNO, visit_date) %>%
  inner_join(LEDD_Concomitant_Medication_Log_12Feb2025 %>% mutate(PATNO=as.character(PATNO))) 

length(unique(data$PATNO)) #1184

unique(data$Class)

data %>% filter(grepl("COMT inhibitor", Class)) %>% filter(visit_date>=STARTDT) %>%
  filter(is.na(STOPDT) | visit_date<=STOPDT) %>% select(PATNO) %>% distinct()  # 189 -> 0.16

range(data$visit_date) # "2010-07-01" "2024-11-01"

dim(data)[1] # 63,391



data.frame(data %>% select(PATNO, visit_date) %>% distinct() %>% group_by(visit_date) %>% count() %>%
  rename("den"="n") %>%
  left_join(
    data %>% filter(grepl("COMT inhibitor", Class)) %>% 
      filter(visit_date>=STARTDT) %>%
  filter(is.na(STOPDT) | visit_date<=STOPDT) %>%
    select(PATNO, visit_date) %>% distinct() %>%
  group_by(visit_date) %>% count()
  ) %>% mutate(n=ifelse(is.na(n),0,n)) %>%
  mutate(perc=n/den))

# CAGR 15%, doubling every 5 years

plot <- data %>% select(PATNO, visit_date) %>% distinct() %>% group_by(visit_date) %>% count() %>%
  rename("den"="n") %>%
  left_join(
    data %>% filter(grepl("COMT inhibitor", Class)) %>% 
      filter(visit_date>=STARTDT) %>%
  filter(is.na(STOPDT) | visit_date<=STOPDT) %>%
    select(PATNO, visit_date) %>% distinct() %>%
  group_by(visit_date) %>% count()
  ) %>% mutate(n=ifelse(is.na(n),0,n)) %>%
  mutate(perc=n/den) %>%
  ggplot(aes(visit_date, perc)) + 
  geom_smooth(fill = "#234b6a",colour = "#234b6a", method = "gam", formula = y ~ s(x, bs = "cs", k = 6), se = TRUE, size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "6 months") +
  labs(title = "% Unique Patients ON Entacapone",
       x = "\n Exact Month Date",
       y = "% Patient on Entacapone \n") +
  theme_minimal(base_family = "bahnschrift") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 


ggsave("monthly_all_v1.svg", plot = plot, width = 8, height = 6, device = "svg")




pats_2plus <- data %>% select(PATNO, visit_date) %>% distinct() %>% group_by(PATNO) %>% count() %>%
  ungroup() %>%  filter(n>=2) %>% select(PATNO) %>% distinct() %>% mutate(Nr_visits="A) 2+ visits")

pats_5plus <- data %>% select(PATNO, visit_date) %>% distinct() %>% group_by(PATNO) %>% count() %>%
  ungroup() %>%  filter(n>=5) %>% select(PATNO) %>% distinct() %>% mutate(Nr_visits="B) 5+ visits")

pats_10plus <- data %>% select(PATNO, visit_date) %>% distinct() %>% group_by(PATNO) %>% count() %>%
  ungroup() %>%  filter(n>=10) %>% select(PATNO) %>% distinct() %>% mutate(Nr_visits="C) 10+ visits")


groups <- pats_10plus %>% bind_rows(pats_5plus) %>% bind_rows(pats_2plus) 


plot <- data %>% 
  left_join(groups) %>% filter(!is.na(Nr_visits)) %>% 
  select(PATNO, visit_date, Nr_visits) %>% distinct() %>%
  group_by(Nr_visits, visit_date) %>% count() %>%
  rename("den"="n") %>%
  left_join(
    data %>% 
      left_join(groups) %>% filter(!is.na(Nr_visits)) %>% 
      filter(grepl("COMT inhibitor", Class)) %>% 
      filter(visit_date>=STARTDT) %>%
      filter(is.na(STOPDT) | visit_date<=STOPDT) %>%
      select(PATNO, visit_date, Nr_visits) %>% distinct() %>%
      group_by(Nr_visits, visit_date) %>% count()
  ) %>% 
  mutate(n=ifelse(is.na(n),0,n)) %>%
  mutate(perc=n/den) %>%
  ggplot(aes(visit_date, perc, colour=Nr_visits, fill=Nr_visits)) + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 6), se = TRUE, size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "6 months") +
  labs(title = "% Unique Patients ON Entacapone",
       x = "\n Exact Month Date",
       y = "% Patient on Entacapone \n") +
  theme_minimal(base_family = "bahnschrift") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#f7dc6f", "#234b6a", "#ca5858")) +
  scale_fill_manual(values=c("#f7dc6f", "#234b6a", "#ca5858"))




ggsave("monthly_nr_visits_v1.svg", plot = plot, width = 8, height = 6, device = "svg")


sysfonts::font_add(family = "bahnschrift", regular = "bahnschrift.ttf")  # Adjust path if needed


plot <- PPMI_Curated_Data_Cut_Public_20241211 %>% distinct() %>%
  ggplot(aes(visit_date, duration_yrs)) +
  geom_smooth(fill="#234b6a", colour="#234b6a", method = "gam", formula = y ~ s(x, bs = "cs", k = 5), se = TRUE, size = 2) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "6 months") +
    coord_cartesian(ylim=c(0,25)) +
  labs(title = "PPMI Disease Duration Over Time",
       x = "\n Exact Month Date",
       y = "Cohort Disease Duration (years) \n") +
  theme_minimal(base_family = "bahnschrift") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

ggsave("monthly_disease_duration.svg", plot = plot, width = 8, height = 6, device = "svg")




plot <- PPMI_Curated_Data_Cut_Public_20241211 %>% distinct() %>%
  filter(hy_on>=1) %>%
   group_by(visit_date, hy_on) %>% count() %>%
  ungroup() %>%
  group_by(visit_date) %>% mutate(tot=sum(n)) %>% mutate(n=n/tot) %>%
  ungroup() %>%
  mutate(hy_on=as.factor(hy_on)) %>%
  drop_na() %>%
  ggplot(aes(visit_date, n, colour=hy_on, fill=hy_on)) +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs", k = 5), se = TRUE, size = 2) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "6 months") +
  scale_color_manual(values=c( "#f7dc6f", "#212f3d", "#943126")) +
  scale_fill_manual(values=c(  "#f7dc6f", "#212f3d", "#943126")) +
  labs(title = "PPMI H&Y (ON) Over Time",
       x = "\n Exact Month Date",
       y = "Cohort H&Y ON \n") +
  theme_minimal(base_family = "bahnschrift") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

plot
ggsave("monthly_hy.svg", plot = plot, width = 8, height = 6, device = "svg")



persist <- data %>% select(PATNO, visit_date,  STARTDT, STOPDT, Class) %>% distinct() %>% 
      filter(visit_date>=STARTDT) %>% filter(is.na(STOPDT) | visit_date<=STOPDT) %>%
    distinct() %>%
  mutate(Class=ifelse(grepl("COMT", Class), 1,0)) %>% distinct() %>%
  group_by(PATNO) %>% mutate(max=max(visit_date)) %>%
  filter(Class==1) %>% select(-visit_date) %>% distinct() %>%
  group_by(PATNO) %>% filter(STARTDT==min(STARTDT)) %>%
  ungroup() %>%
  mutate(Elapsed=ifelse(is.na(STOPDT), max-STARTDT, STOPDT-STARTDT)) %>%
  mutate(group=ifelse(is.na(STOPDT),0,1))

persist 




surv_obj <- survival::Surv(time = persist$Elapsed, event = persist$group)

km_fit <- survival::survfit(surv_obj ~ 1)  # ~1 means no grouping, just overall survival

survminer::ggsurvplot(
  km_fit,
  data = persist,
  conf.int = TRUE,
  xlab = "\n Days since Entacapone initiation",
  ylab = "Proportion persistent \n Still ON Entacapone \n",
  title = "Entacapone Persistence",
  legend.title = "Visit Count",
  surv.median.line = "hv",
  risk.table = TRUE,
  cumevents=TRUE,
  cumsensor=TRUE,
  tables.height = 0.2,
  palette = c("#6f0a4f", "#043A4F", "#840d0d")
)


comt_data <- data %>% select(PATNO, visit_date,  STARTDT, STOPDT, Class) %>% distinct() %>% 
      filter(visit_date>=STARTDT) %>% filter(is.na(STOPDT) | visit_date<=STOPDT) %>%
    distinct() %>%
  mutate(Class=ifelse(grepl("COMT", Class), 1,0)) %>% distinct() %>%
  group_by(PATNO) %>% mutate(max=max(visit_date)) %>%
  filter(Class==1)  %>% mutate(STOPDT = if_else(is.na(STOPDT), max, STOPDT))  %>%
  select(PATNO, STARTDT, STOPDT)


# BiocManager::install("IRanges")

library(IRanges)

# Function to merge intervals per patient
merge_intervals <- function(df) {
  ir <- IRanges(start = as.integer(df$STARTDT), end = as.integer(df$STOPDT))
  merged <- reduce(ir)
  tibble(
    start = as.Date(start(merged), origin = "1970-01-01"),
    end = as.Date(end(merged), origin = "1970-01-01"),
    duration = as.integer(end(merged) - start(merged) + 1)
  )
}


library(purrr)

persistency_df <- comt_data %>%
  group_by(PATNO) %>%
  group_split() %>%
  map_dfr(~{
    merged <- merge_intervals(.x)
    merged$PATNO <- unique(.x$PATNO)
    merged
  }) %>%
  select(PATNO, start, end, duration)


total_persistency <- persistency_df %>%
  group_by(PATNO) %>%
  summarise(total_days = sum(duration))

mean(total_persistency$total_days)

total_persistency <- total_persistency %>% left_join(
  data %>% select(PATNO, visit_date,  STARTDT, STOPDT, Class) %>% distinct() %>% 
      filter(visit_date>=STARTDT) %>% filter(is.na(STOPDT) | visit_date<=STOPDT) %>%
    distinct() %>%
  mutate(Class=ifelse(grepl("COMT", Class), 1,0)) %>% distinct() %>%
  group_by(PATNO) %>% mutate(max=max(visit_date)) %>%
  filter(Class==1)  %>% mutate(STOPDT = if_else(is.na(STOPDT), max, STOPDT))  %>%
  select(PATNO, STARTDT, STOPDT, max) %>% mutate(group=ifelse(STOPDT==max, 0,1)) %>%
  group_by(PATNO) %>% summarise(group=min(group))
) 


# Create survival object
surv_obj <- survival::Surv(time = total_persistency$total_days, event = total_persistency$group)

# Kaplan-Meier fit
km_fit <- survival::survfit(surv_obj ~ 1)






# Plot it
survminer::ggsurvplot(
  km_fit,
  data=total_persistency,
  conf.int = TRUE,
  risk.table = TRUE,
   cumevents=TRUE,
  cumsensor=TRUE,
   xlab = "\n Days since Entacapone initiation",
  ylab = "Proportion persistent \n Still ON Entacapone \n",
  title = "Entacapone Persistence",
  surv.median.line = "hv",
  palette = c("#043A4F" )
)


all_episodes <- persist %>% select(PATNO, Elapsed, group) %>% rename("Elapsed"="total_days") %>% mutate(Episodes="First") %>%
  bind_rows(total_persistency %>% mutate(Episodes="All"))


all_episodes <- all_episodes %>% inner_join(persist %>% select(PATNO) %>% distinct()) %>%
  inner_join(total_persistency %>% select(PATNO) %>% distinct())


all_episodes <- all_episodes %>% filter(PATNO!="41282"&PATNO!="43045")

# Create survival object
surv_obj <- survival::Surv(time = all_episodes$total_days, event = all_episodes$group)

# Kaplan-Meier fit
km_fit <- survival::survfit(surv_obj ~ all_episodes$Episodes)



# Plot it
survminer::ggsurvplot(
  km_fit,
  data=all_episodes,
  conf.int = TRUE,
  risk.table = TRUE,
   cumevents=TRUE,
  cumsensor=TRUE,
   xlab = "\n Days since Entacapone initiation",
  ylab = "Proportion persistent \n Still ON Entacapone \n",
  title = "Entacapone Persistence",
  surv.median.line = "hv",
  palette = c("#043A4F" , "#840d0d"),
  tables.height = 0.15
)











# MDS UPDRS I Motor Complications 
MDS_UPDRS_Part_IV__Motor_Complications_12Feb2025 <- fread("Motor___MDS-UPDRS/MDS-UPDRS_Part_IV__Motor_Complications_12Feb2025.csv")


pats_on_comt <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(grepl("COMT inhibitor", Class)) %>% 
  select(PATNO) %>% distinct() 


summary_df <- MDS_UPDRS_Part_IV__Motor_Complications_12Feb2025 %>%
  select(PATNO,
         NP4DYSKI, NP4DYSTN, NP4DYSTNDEN, NP4DYSTNNUM, NP4DYSTNPCT,
NP4FLCTI, NP4FLCTX, NP4OFF, NP4OFFDEN, NP4OFFNUM, NP4OFFPCT,
NP4TOT, NP4WDYSK, NP4WDYSKDEN,NP4WDYSKNUM,NP4WDYSKPCT) %>%
  group_by(PATNO) %>%
  summarise(across(everything(), ~ max(.x, na.rm = TRUE))) %>%
  ungroup()


summary_df <- summary_df %>% left_join(pats_on_comt %>% mutate(Group="COMT Inhib-exp")) %>%
  mutate(Group=ifelse(is.na(Group), "Never COMT", Group))


group_summary <- summary_df %>%
  group_by(Group) %>%
  summarise(across(starts_with("NP4"), ~ mean(.x[!is.na(.x) & is.finite(.x)], na.rm = TRUE))) %>%
  ungroup()


standardized_df <- summary_df %>%
  mutate(across(
    starts_with("NP4"),
    ~ {
      x <- .x
      x[!is.finite(x)] <- NA  # convert -Inf, Inf, NaN to NA
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    }
  ))

dim(group_z_scores)

group_z_scores <- standardized_df %>%
  group_by(Group) %>%
  summarise(across(starts_with("NP4"), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()


radar_df <- group_z_scores %>%
  as.data.frame()



# ggradar requires rownames
rownames(radar_df) <- radar_df$Group
radar_df <- radar_df %>% select(-Group)

radar_df <- tibble::rownames_to_column(radar_df, "Group")

# 4. Plot the radar chart
plot <- ggradar::ggradar(radar_df,
        grid.min = -1, grid.mid = 0, grid.max = 1,
        group.line.width = 2.2,
        group.point.size = 3,
        values.radar = c("-1", "0", "1"),
        background.circle.colour = "white",
        axis.label.size = 3.5,
        group.colours = c("#ca5858", "#234b6a"),
        legend.position = "top")

plot

ggsave("radar.svg", plot = plot, width = 8, height = 8, device = "svg")



