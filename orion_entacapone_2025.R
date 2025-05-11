
# Paulo BASTOS, Orion Corporation

# All rights reserved, May 2025


# How has the market penetrance of Entacapone changed over time? --------------------------

# Packages

libraries <- c("data.table", "tidyverse", "readxl", "showtext", "sandwich", "lmtest")

lapply(libraries, 
       function(libraries) {
         if (!require(libraries, character.only=TRUE)) {
           install.packages(libraries); require(libraries)
           }}
       )

(.packages())

# Inclusion/Demographics data

Inclusion_20250106 <- read_excel(path = "../data/Inclusion_20250106.xlsx")

Inclusion_20250106 <- Inclusion_20250106[Inclusion_20250106$diag=="MP",]

length(unique(Inclusion_20250106$anonyme_id)) # 30331

# Consultation/Visit data

Consultation_20250106 <- read_excel(path = "../data/Consultation_20250106.xlsx")

Consultation_20250106 <- merge(
  Consultation_20250106,
  unique(Inclusion_20250106["anonyme_id"]),
  by = "anonyme_id"
)

length(unique(Consultation_20250106$anonyme_id)) # 25603

# Select relevant columns for Entacapone (yes/no)


Consultation_subset <- Consultation_20250106[
  , c("anonyme_id", 
      "act_datedeb",  
      "ttt_comptan_entac_yn___yes",
      "ttt_tasm_talc100_yn___yes",
      "ttt_stalevo_50_cpr_yn___yes",
      "ttt_stalevo_75_cpr_yn___yes",
      "ttt_stalevo_100_cpr_yn___yes",
      "ttt_stalevo_125_cpr_yn___yes",
      "ttt_stalevo_150_cpr_yn___yes",
      "ttt_stalevo_175_cpr_yn___yes",
      "ttt_stalevo_200_cpr_yn___yes")
]

# Create Entacapone column (TRUE if any drug column == 1)

drug_cols <- c(
  "ttt_comptan_entac_yn___yes",
  "ttt_tasm_talc100_yn___yes",
  "ttt_stalevo_50_cpr_yn___yes",
  "ttt_stalevo_75_cpr_yn___yes",
  "ttt_stalevo_100_cpr_yn___yes",
  "ttt_stalevo_125_cpr_yn___yes",
  "ttt_stalevo_150_cpr_yn___yes",
  "ttt_stalevo_175_cpr_yn___yes",
  "ttt_stalevo_200_cpr_yn___yes"
)

Consultation_subset$Entacapone <- as.numeric(
  rowSums(Consultation_subset[, drug_cols] == 1, 
  na.rm = TRUE) > 0)

head(Consultation_subset)

Consultation_subset$act_datedeb <- as.Date(Consultation_subset$act_datedeb)

Consultation_subset <- Consultation_subset[
  format(Consultation_subset$act_datedeb, "%Y") >= "2012" &
  format(Consultation_subset$act_datedeb, "%Y") <= "2025",
]

Consultation_subset <- Consultation_subset[!is.na(Consultation_subset$Entacapone), ]

length(unique(Consultation_subset$anonyme_id))

dim(Consultation_subset)[1]

# If filtering for patients >X number of visits


distinct_visits <- unique(Consultation_subset[c("anonyme_id", "act_datedeb")])

visit_counts <- as.data.frame(table(distinct_visits$anonyme_id))

names(visit_counts) <- c("anonyme_id", "n")

eligible_ids <- visit_counts$anonyme_id[visit_counts$n >= 1]

Consultation_subset <- Consultation_subset[Consultation_subset$anonyme_id %in% eligible_ids, ]

length(unique(Consultation_subset$anonyme_id))

dim(Consultation_subset)[1]




monthly_summary <- Consultation_subset %>%
  mutate(month = as.Date(format(act_datedeb, "%Y-%m-01"))) %>%
  group_by(month) %>%
  summarise(pct_entacapone = mean(Entacapone == 1, na.rm = TRUE), count=n())

yearly_summary <- Consultation_subset %>%
  mutate(year = as.Date(format(act_datedeb, "%Y-01-01"))) %>%
  group_by(year) %>%
  summarise(pct_entacapone = mean(Entacapone == 1, na.rm = TRUE), count=n()) %>%
  mutate(level = "Yearly")

daily_pct <- Consultation_subset %>%
  group_by(act_datedeb) %>%
  summarise(pct_entacapone = mean(Entacapone, na.rm = TRUE), count=n()) %>%
  ungroup()





# Plots

sysfonts::font_add(family = "bahnschrift", regular = "bahnschrift.ttf")  # Adjust path if needed

# Daily

plot <- ggplot(daily_pct, aes(x = act_datedeb, y = pct_entacapone)) +
  geom_col(fill = "#83587d") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "90 days") +
  labs(title = "% Daily Patient-visits ON Entacapone",
       x = "\n Exact Date",
       y = "% Patient-visits on Entacapone \n") +
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


ggsave("../out/daily_v1.svg", plot = plot, width = 8, height = 6, device = "svg")



plot <- ggplot(daily_pct, aes(x = act_datedeb, y = count)) +
  geom_col(fill = "#83587d") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "90 days") +
  labs(title = "Number of Patient-visits",
       x = "\n Exact Date",
       y = "Number of Patient-visits \n") +
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


ggsave("../out/daily_counts_v1.svg", plot = plot, width = 8, height = 6, device = "svg")



# Monthly

plot <- ggplot(monthly_summary, aes(x = month, y = pct_entacapone)) +
  geom_col(fill = "#234b6a") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "% Monthly Patient-visits ON Entacapone",
       x = "\n Exact Date",
       y = "% Patient-visits on Entacapone \n") +
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

ggsave("../out/monthly_v1.svg", plot = plot, width = 8, height = 6, device = "svg")


plot <- ggplot(monthly_summary, aes(x = month, y = count)) +
  geom_col(fill = "#234b6a") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(title = "Number of Patient-visits",
       x = "\n Exact Date",
       y = "Number of Patient-visits \n") +
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

ggsave("../out/monthlycounts__v1.svg", plot = plot, width = 8, height = 6, device = "svg")




# Yearly


plot <- ggplot(yearly_summary, aes(x = year, y = pct_entacapone)) +
  geom_col(fill = "#ca5858") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "% Yearly Patient-visits ON Entacapone",
       x = "\n Exact Date",
       y = "% Patient-visits on Entacapone \n") +
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


ggsave("../out/yearly_v1.svg", plot = plot, width = 8, height = 6, device = "svg")



plot <- ggplot(yearly_summary, aes(x = year, y = count)) +
  geom_col(fill = "#ca5858") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "Number of Patient-visits",
       x = "\n Exact Date",
       y = "Number of Patient-visits \n") +
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


ggsave("../out/yearly_counts_v1.svg", plot = plot, width = 8, height = 6, device = "svg")




# Smooth Daily, Monthly, Yearly

plot <- ggplot() +
  geom_smooth(data = daily_pct,
              aes(x = act_datedeb, y = pct_entacapone, color = "Daily"),
              method = "gam", formula = y ~ s(x, bs = "cs", k = 5),  se = FALSE, size = 2) +
    geom_smooth(data = monthly_summary,
              aes(x = month, y = pct_entacapone, color = "Monthly"),
              method = "gam", formula = y ~ s(x, bs = "cs", k = 5), se = FALSE, size = 2) +
    geom_smooth(data = yearly_summary,
              aes(x = year, y = pct_entacapone, color = "Yearly"),
              method = "gam", formula = y ~ s(x, bs = "cs", k = 6), se = FALSE, size = 2) +
    labs(x = "\n Exact Date", 
       y = "% on Entacapone \n",
       title = "% Patient-visits ON Entacapone",
       subtitle = " Daily vs. Monthly vs. Yearly",
       color = "Time Period") +
    theme_minimal(base_family = "bahnschrift") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
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
  scale_color_manual(values=c("#83587d", "#234b6a", "#ca5858"))

plot

ggsave("../out/smooth_fit_v1.svg", plot = plot, width = 8, height = 6, device = "svg")


plot <- ggplot() +
  geom_smooth(data = daily_pct,
              aes(x = act_datedeb, y = pct_entacapone, color = "Daily"),
              method = "gam", formula = y ~ s(x, bs = "cs", k = 8),  se = FALSE, size = 2) +
    geom_smooth(data = monthly_summary,
              aes(x = month, y = pct_entacapone, color = "Monthly"),
              method = "gam", formula = y ~ s(x, bs = "cs", k = 8), se = FALSE, size = 2) +
    geom_smooth(data = yearly_summary,
              aes(x = year, y = pct_entacapone, color = "Yearly"),
              method = "gam", formula = y ~ s(x, bs = "cs", k = 12), se = FALSE, size = 2) +
    labs(x = "\n Exact Date", 
       y = "% on Entacapone \n",
       title = "% Patient-visits ON Entacapone",
       subtitle = " Daily vs. Monthly vs. Yearly",
       color = "Time Period") +
    theme_minimal(base_family = "bahnschrift") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
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
  scale_color_manual(values=c("#83587d", "#234b6a", "#ca5858"))

plot

ggsave("../out/smooth_fit_v2.svg", plot = plot, width = 8, height = 6, device = "svg")



# Stats tests and forecast



trend_data  <- Consultation_subset %>% select(anonyme_id, act_datedeb, Entacapone) %>% drop_na()

trend_data <- trend_data %>% filter(as.Date(act_datedeb)>="2016-01-01")

model <- glm(as.factor(Entacapone) ~ act_datedeb, family = binomial(), data = trend_data)

lmtest::coeftest(model, vcov = vcovCL, cluster = ~anonyme_id)


cochran_model <- DescTools::CochranArmitageTest(table(trend_data$act_datedeb, trend_data$Entacapone))



model <- lme4::glmer(Entacapone ~ act_datedeb + (1 | anonyme_id), 
               data = trend_data, 
               family = binomial)

summary(model)


library(prophet)

trend_data <- trend_data %>%
  group_by(act_datedeb) %>%
  summarise(daily_mean = mean(Entacapone, na.rm = TRUE))

trend_data <- trend_data %>%
  rename(ds = act_datedeb, y = daily_mean)


m <- prophet(trend_data)

future <- make_future_dataframe(m, periods = 365*2)

forecast <- predict(m, future)

forecast <- forecast %>% select(ds, yhat, yhat_lower, yhat_upper)

forecast$period <- ifelse(as.Date(forecast$ds) > as.Date("2025-01-01"), "Post-2025", "Pre-2025")

plot <- ggplot(forecast, aes(x = as.Date(ds), y = yhat)) +
  geom_line(aes(color = period), linewidth = 0.5) +  # Line color based on the 'period' variable
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper, fill = period), alpha = 0.3) +  # Ribbon color based on the 'period' variable
  labs(x = "\n Exact Date", 
       y = "Forecasted % on Entacapone \n",
       title = "Forecast Trend with Confidence Interval") +
    theme_minimal(base_family = "bahnschrift") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  scale_color_manual(values = c("Pre-2025" = "#234b6a", "Post-2025" = "#ca5858")) +  # Custom colors for the two periods
  scale_fill_manual(values = c("Pre-2025" = "#234b6a", "Post-2025" = "#ca5858")) +  # Custom fill colors for the ribbon
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
  coord_cartesian(ylim=c(0,0.6))


ggsave("../out/prophet_v2.svg", plot = plot, width = 14, height = 6, device = "svg")













# Entacapone/Tolcapone share


plot <- Consultation_subset %>% select(ttt_comptan_entac_yn___yes,ttt_tasm_talc100_yn___yes, Entacapone, act_datedeb) %>%
    mutate(month = as.Date(format(act_datedeb, "%Y-%m-01"))) %>%
  group_by(month) %>% 
  mutate(Entacapone_sums=sum(Entacapone, na.rm=T)) %>%
  mutate(Comptan_sums=sum(ttt_comptan_entac_yn___yes, na.rm=T)) %>%
  mutate(Talcapone_sums=sum(ttt_tasm_talc100_yn___yes, na.rm=T)) %>% 
  select(month, Entacapone_sums,Comptan_sums, Talcapone_sums) %>% distinct() %>%
  mutate(Comptan_Entacapone_share=Comptan_sums/Entacapone_sums) %>%
  mutate(Tasmar_Tolcapone_Share=Talcapone_sums/Entacapone_sums) %>%
  mutate(Stalevo_Share=1-Tasmar_Tolcapone_Share-Comptan_Entacapone_share) %>%
  gather(group, share, Comptan_Entacapone_share:Stalevo_Share) %>%
  ggplot(aes(month, share, colour=group, fill=group)) +
  geom_jitter(alpha=0.5, shape=1, stroke=2) +
  geom_smooth(method="gam") +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  coord_cartesian(ylim=c(0,1.1)) +
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
    scale_color_manual(values=c("#83587d", "#234b6a", "#ca5858")) +
  scale_fill_manual(values=c("#83587d", "#234b6a", "#ca5858")) +
  labs(x = "\n Exact Date", 
       y = "Share/penetrance \n",
       title = "COMT share month-over-month")



ggsave("../out/brands_share.svg", plot = plot, width = 10, height = 6, device = "svg")

















# Cohort composition over time

cohort_compos_over_time <- Inclusion_20250106 %>% select(anonyme_id, diag_date_a, pat_ddn_a, redcap_data_access_group) %>%
  mutate(diag_date_a=as.numeric(diag_date_a), pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  inner_join(Consultation_20250106 %>% select(anonyme_id, act_datedeb, hoehn_yahr_on, fluct_motrice)) %>% 
  mutate(act_datedeb=as.Date(act_datedeb)) %>%
  mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
    mutate(fluct_motrice=as.numeric(fluct_motrice)) %>%
   mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) %>%
  mutate(age=year-pat_ddn_a) %>% select(-c(pat_ddn_a)) %>%
  mutate(disease_dur=year-diag_date_a)


plot <- cohort_compos_over_time %>%
  filter(act_datedeb>="2012-01-01" & act_datedeb<="2024-12-31") %>%
  ggplot(aes(act_datedeb, age)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 12), fill="#83587d", colour="#83587d") +
  coord_cartesian(ylim=c(0,100)) +
   labs(x = "\n Exact Date", 
       y = "Age (years)\n",
       title = "Patient age over time",
       subtitle = " NS-Park cohort composition",
       color = "Time Period") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
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


ggsave("../out/age_v1.svg", plot = plot, width = 8, height = 6, device = "svg")


plot <-cohort_compos_over_time %>%
  filter(act_datedeb>="2012-01-01" & act_datedeb<="2024-12-31") %>%
  ggplot(aes(act_datedeb, disease_dur)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 12), fill="#83587d", colour="#83587d") +
  coord_cartesian(ylim=c(0,25)) +
  labs(x = "\n Exact Date", 
       y = "Disease duration (years)\n",
       title = "Patient disease duration over time",
       subtitle = " NS-Park cohort composition",
       color = "Time Period") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
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


ggsave("../out/dis_dur_v1.svg", plot = plot, width = 8, height = 6, device = "svg")



cohort_compos_over_time %>%
  filter(act_datedeb>="2012-01-01" & act_datedeb<="2024-12-31") %>%
  group_by(year) %>% summarise(mean=mean(hoehn_yahr_on, na.rm=T))


plot <- cohort_compos_over_time %>%
  filter(act_datedeb>="2012-01-01" & act_datedeb<="2024-12-31") %>%
  select(act_datedeb, hoehn_yahr_on) %>% drop_na() %>%
  filter(hoehn_yahr_on!=0) %>%
  group_by(act_datedeb, hoehn_yahr_on) %>% count() %>%
  ungroup() %>%
  group_by(act_datedeb) %>% mutate(tot=sum(n)) %>% mutate(n=n/tot) %>%
  ungroup() %>%
  mutate(hoehn_yahr_on=as.factor(hoehn_yahr_on)) %>%
  ggplot(aes(act_datedeb, n, colour=hoehn_yahr_on, fill=hoehn_yahr_on)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 12)) +
  coord_cartesian(ylim=c(0,1)) +
  labs(x = "\n Exact Date", 
       y = "Hoehn & Yahr (proportion)\n",
       title = "Patient Hoehn & Yahr propotion over time",
       subtitle = " NS-Park cohort composition") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
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
  scale_color_manual(values=c( "#808b96", "#212f3d", "#f7dc6f", "#d98880", "#943126")) +
  scale_fill_manual(values=c( "#808b96", "#212f3d", "#f7dc6f", "#d98880", "#943126"))

ggsave("../out/hoeyeah_v1.svg", plot = plot, width = 8, height = 6, device = "svg")



plot <- cohort_compos_over_time %>%
  filter(act_datedeb>="2012-01-01" & act_datedeb<="2024-12-31") %>%
  select(act_datedeb, fluct_motrice) %>% drop_na() %>%
  group_by(act_datedeb, fluct_motrice) %>% count() %>%
  ungroup() %>%
  group_by(act_datedeb) %>% mutate(tot=sum(n)) %>% mutate(n=n/tot) %>%
  ungroup() %>%
  mutate(fluct_motrice=as.factor(fluct_motrice)) %>%
  ggplot(aes(act_datedeb, n, colour=fluct_motrice, fill=fluct_motrice)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 12)) +
  coord_cartesian(ylim=c(0,1)) +
  labs(x = "\n Exact Date", 
       y = "Motor fluctuations score (proportion)\n",
       title = "Patient motor fluctuations score proportion over time",
       subtitle = " NS-Park cohort composition") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
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
  scale_color_manual(values=c("#d5d8dc", "#808b96", "#212f3d",  "#d98880", "#943126")) +
  scale_fill_manual(values=c("#d5d8dc", "#808b96", "#212f3d",  "#d98880", "#943126"))


ggsave("../out/motor_fluc_v1.svg", plot = plot, width = 8, height = 6, device = "svg")



subtotals <- cohort_compos_over_time %>%
  filter(act_datedeb>="2012-01-01" & act_datedeb<="2024-12-31") %>%
    mutate(month = as.Date(format(act_datedeb, "%Y-%m-01"))) %>%
  select(month, redcap_data_access_group) %>% drop_na() %>%
  group_by(month, redcap_data_access_group) %>% count() %>%
  ungroup() %>%
  group_by(month) %>% mutate(tot=sum(n)) %>% mutate(n=n/tot) %>%
  ungroup() %>% select(-tot)




plot <- cohort_compos_over_time %>%
  filter(act_datedeb>="2012-01-01" & act_datedeb<="2024-12-31") %>%
    mutate(month = as.Date(format(act_datedeb, "%Y-%m-01"))) %>%
  select(month, redcap_data_access_group) %>% drop_na() %>%
  group_by(month, redcap_data_access_group) %>% count() %>%
  ungroup() %>%
  group_by(month) %>% mutate(tot=sum(n)) %>% mutate(n=n/tot) %>%
  ungroup() %>% select(-tot) %>%
  ggplot(aes(x = month, y = n, fill = redcap_data_access_group)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(x = "\n Exact Month", 
       y = "Proportion of patient-visits\n",
       title = "Proportion of patient-visits per center over time",
       subtitle = " NS-Park cohort composition",
       fill="Center") + 
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
  scale_fill_viridis_d(option = "D") +
  scale_colour_viridis_d(option = "D")


ggsave("../out/entac_pene_time_center_v1.svg", plot = plot, width = 8, height = 6, device = "svg")




subtotals <- cohort_compos_over_time %>%
  filter(act_datedeb>="2012-01-01" & act_datedeb<="2024-12-31") %>%
    mutate(month = as.Date(format(act_datedeb, "%Y-%m-01"))) %>%
  select(month, redcap_data_access_group) %>% drop_na() %>%
  group_by(month, redcap_data_access_group) %>% count() %>%
  ungroup() %>%
  group_by(month) %>% mutate(tot=sum(n)) %>% mutate(n=n/tot) %>%
  ungroup() %>% select(-tot)




# Get list of unique centers
centers <- unique(subtotals$redcap_data_access_group)

# Loop through each center and save the plot
for (center in centers) {
  plot <- subtotals %>%
    filter(redcap_data_access_group == center) %>%
    ggplot(aes(x = month, y = n)) +
    geom_line(color = "#808b96", size = 1) +
    geom_point(color = "#808b96", size = 2, alpha=0.5) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, color = "#943126") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    coord_cartesian(ylim=c(0,1)) +
    coord_cartesian(xlim=c(as.Date("2012-01-01"),as.Date("2024-12-31")), ylim=c(0,1)) +
    labs(
      title = paste("Patient-visits Share \nOver Time:", center),
      x = "\n Exact Month",
      y = "Proportion of visits \n"
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
  
  # Save as SVG
  ggsave(
    filename = paste0("../out/", center, ".svg"),
    plot = plot,
    width = 4, height = 4, device = "svg"
  )
}





entac_centers_over_time <- Inclusion_20250106 %>% select(anonyme_id,  redcap_data_access_group) %>%
  inner_join(Consultation_20250106 %>% 
               select(anonyme_id, act_datedeb, 
               ttt_comptan_entac_yn___yes,
               ttt_tasm_talc100_yn___yes,
               ttt_stalevo_50_cpr_yn___yes,
               ttt_stalevo_75_cpr_yn___yes,
               ttt_stalevo_100_cpr_yn___yes,
               ttt_stalevo_125_cpr_yn___yes,
               ttt_stalevo_150_cpr_yn___yes,
               ttt_stalevo_175_cpr_yn___yes,
               ttt_stalevo_200_cpr_yn___yes)) %>% 
  mutate(act_datedeb=as.Date(act_datedeb)) 



drug_cols <- c(
  "ttt_comptan_entac_yn___yes",
  "ttt_tasm_talc100_yn___yes",
  "ttt_stalevo_50_cpr_yn___yes",
  "ttt_stalevo_75_cpr_yn___yes",
  "ttt_stalevo_100_cpr_yn___yes",
  "ttt_stalevo_125_cpr_yn___yes",
  "ttt_stalevo_150_cpr_yn___yes",
  "ttt_stalevo_175_cpr_yn___yes",
  "ttt_stalevo_200_cpr_yn___yes"
)

entac_centers_over_time$Entacapone <- as.numeric(
  rowSums(entac_centers_over_time[, drug_cols] == 1, 
  na.rm = TRUE) > 0)

head(entac_centers_over_time)

entac_centers_over_time$act_datedeb <- as.Date(entac_centers_over_time$act_datedeb)

entac_centers_over_time <- entac_centers_over_time[
  format(entac_centers_over_time$act_datedeb, "%Y") >= "2012" &
  format(entac_centers_over_time$act_datedeb, "%Y") <= "2025",
]

entac_centers_over_time <- entac_centers_over_time[!is.na(entac_centers_over_time$Entacapone), ]

plot <- data.frame(entac_centers_over_time %>% group_by(redcap_data_access_group) %>% 
             mutate(mean=round(mean(Entacapone),2), counts=n())) %>% 
  filter(!is.na(redcap_data_access_group)) %>%
  select(redcap_data_access_group, counts, mean) %>% distinct() %>%
  filter(redcap_data_access_group!="grenoble") %>%
  rename("Penetrance"="mean") %>%
  ggplot(aes(x = counts, y = Penetrance, size = counts, label = redcap_data_access_group, colour=Penetrance, fill=Penetrance)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  ggrepel::geom_text_repel(size = 4, max.overlaps = 100, colour="black", fontface = "bold") +
  scale_size_continuous(range = c(2, 12)) +
  ylim(0,0.3) +
  labs(
    title = "Visits vs Penetrance: \n Entacapone per Center",
    x = "\n Overall number of visits (n)",
    y = "Overall Entacapone penetrance/share \n",
    size = "Counts"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
    scale_fill_gradient(low = "#aae7ff", high = "#000354", na.value = "#f1f1f1") +
   scale_colour_gradient(low = "#aae7ff", high = "#000354", na.value = "#f1f1f1") 

ggsave("../out/visits_penetrance_center_v1.svg", plot = plot, width = 8, height = 6, device = "svg")



df_map <-  data.frame(entac_centers_over_time %>% group_by(redcap_data_access_group) %>% 
             mutate(mean=round(mean(Entacapone),2), counts=n())) %>% 
  filter(!is.na(redcap_data_access_group)) %>%
  select(redcap_data_access_group, counts, mean) %>% distinct() %>%
  filter(redcap_data_access_group!="grenoble") %>%
  rename("Penetrance"="mean", "city"="redcap_data_access_group")


france_dept <- sf::st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson")


city_to_dept <- tribble(
  ~city, ~code,  # Département code as string
  "limoges", "87",
  "lyon", "69",
  "lille", "59",
  "paris", "75",
  "marseille", "13",
  "montpellier", "34",
  "toulouse", "31",
  "poitiers", "86",
  "creteil", "94",
  "rouen", "76",
  "nice", "06",
  "strasbourg", "67",
  "aix", "13",
  "caen", "14",
  "nancy", "54",
  "reims", "51",
  "amiens", "80",
  "nantes", "44",
  "rennes", "35",
  "bordeaux", "33",
  "besancon", "25",
  "nmes", "30",
  "dijon", "21",
  "clermontferrand", "63",
  "avicenne", "93"  # Bobigny (Seine-Saint-Denis)
)

df_map <- df_map %>% left_join(city_to_dept, by = "city")

france_data <- france_dept %>%
  left_join(df_map, by = c("code" = "code"))

labeled_depts <- france_data %>%
  filter(!is.na(Penetrance)) %>%
  mutate(
    centroid = sf::st_centroid(geometry),
    lon = sf::st_coordinates(centroid)[, 1],
    lat = sf::st_coordinates(centroid)[, 2],
    label = paste0(city, "\n", "Pen: ", 100*Penetrance, "%")
  )



plot <- ggplot(france_data) +
  geom_sf(aes(fill = Penetrance), color = "white") +
  scale_fill_gradient(low = "#aae7ff", high = "#000354", na.value = "#f1f1f1") +
  ggrepel::geom_label_repel(
    data = labeled_depts,
    aes(x = lon, y = lat, label = label),
    size = 3, min.segment.length = 0,
    box.padding = 0.3
  ) +
  labs(
    title = "Entacapone Penetrance per Département [2012-2025]",
    fill = "Penetrance"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.position = "right",
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(5, 5, 5, 5, "pt")
  )



ggsave("../out/map_ent_pen_v1.svg", plot = plot, width = 8, height = 6, device = "svg")



subtotals <- entac_centers_over_time %>%
  filter(act_datedeb>="2012-01-01" & act_datedeb<="2024-12-31") %>%
    mutate(month = as.Date(format(act_datedeb, "%Y-%m-01"))) %>%
  select(month, redcap_data_access_group, Entacapone) %>% drop_na() %>%
  group_by(month, redcap_data_access_group) %>%  summarise(Entacapone=mean(Entacapone)) %>%
  ungroup() 





# Get list of unique centers
centers <- unique(subtotals$redcap_data_access_group)

# Loop through each center and save the plot
for (center in centers) {
  plot <- subtotals %>%
    filter(redcap_data_access_group == center) %>%
    ggplot(aes(x = month, y = Entacapone)) +
    geom_line(color = "#808b96", size = 1) +
    geom_point(color = "#808b96", size = 2, alpha=0.5) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, color = "#943126") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    coord_cartesian(ylim=c(0,1)) +
    coord_cartesian(xlim=c(as.Date("2012-01-01"),as.Date("2024-12-31")), ylim=c(0,1)) +
    labs(
      title = paste("Entacapone penetrance/share \nOver Time:", center),
      x = "\n Exact Month",
      y = "Entacapone share \n"
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
  
  # Save as SVG
  ggsave(
    filename = paste0("../out/", center, ".svg"),
    plot = plot,
    width = 4, height = 4, device = "svg"
  )
}



# ----------------------


# Which factors drive Entacapone usage? --------------------------------

# Packages

libraries <- c("data.table", "tidyverse", "readxl", "showtext", "sandwich", "lmtest")

lapply(libraries, 
       function(libraries) {
         if (!require(libraries, character.only=TRUE)) {
           install.packages(libraries); require(libraries)
           }}
       )

(.packages())

# Inclusion/Demographics data

Inclusion_20250106 <- read_excel(path = "../data/Inclusion_20250106.xlsx")

Inclusion_20250106 <- Inclusion_20250106[Inclusion_20250106$diag=="MP",]

length(unique(Inclusion_20250106$anonyme_id)) # 30331

# Consultation/Visit data

Consultation_20250106 <- read_excel(path = "../data/Consultation_20250106.xlsx")

Consultation_20250106 <- merge(
  Consultation_20250106,
  unique(Inclusion_20250106["anonyme_id"]),
  by = "anonyme_id"
)

length(unique(Consultation_20250106$anonyme_id)) # 25603

names(Consultation_20250106)





# si le patient a pris le traitement A
IMAOB <- c("ttt_azil_rasag1_yn___yes","ttt_safinamide_50_yn___yes","ttt_safinamide_100_yn___yes"  ,"ttt_selegiline_yn___yes")
Consultation_20250106$A <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% IMAOB], na.rm = TRUE)
Consultation_20250106$A <- ifelse(Consultation_20250106$A != 0, 1, 0)


# si le patient a pris le traitement B
Levodopa <- c("ttt_modopar_125_cpr_yn___yes","ttt_modopar_62_5_gel_yn___yes", "ttt_modopar_125_gel_yn___yes" , "ttt_modopar_250_gel_yn___yes","ttt_modopar_lp125_gel_yn___yes","ttt_sinemet_100_cpr_yn___yes" ,"ttt_sinemet_250_cpr_yn___yes" ,"ttt_sinemet_lp100_cpr_yn___yes" ,"ttt_sinemet_lp200_cpr_yn___yes" ,"ttt_stalevo_50_cpr_yn___yes" , "ttt_stalevo_75_cpr_yn___yes" , "ttt_stalevo_100_cpr_yn___yes" ,"ttt_stalevo_125_cpr_yn___yes"  ,"ttt_stalevo_150_cpr_yn___yes" , "ttt_stalevo_175_cpr_yn___yes" ,"ttt_stalevo_200_cpr_yn___yes")
Consultation_20250106$B <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Levodopa], na.rm = TRUE)
Consultation_20250106$B <- ifelse(Consultation_20250106$B != 0, 1, 0)

# si le patient a pris le traitement C
Agonistes <- c( "ttt_neu_rot2_yn___yes","ttt_neu_rot4_yn___yes"       ,"ttt_neu_rot6_yn___yes","ttt_neu_rot8_yn___yes","ttt_ral_brom5_yn___yes"        ,"ttt_ral_brom10_yn___yes"    ,"ttt_ral_brom2_5m_yn___yes","req_rop2_yn___yes","ttt_req_rop4_yn___yes"         ,"ttt_req_rop8_yn___yes" ,"ttt_req_rop025_yn___yes","ttt_req_rop050_yn___yes","ttt_req_rop1_yn___yes","ttt_req_rop2_yn___yes","ttt_req_rop5_yn___yes", "ttt_sif_pram026_yn___yes", "ttt_sif_pram052_yn___yes"     ,"ttt_sif_pram105_yn___yes","ttt_sif_pram210_yn___yes","ttt_sif_pram018_yn___yes", "ttt_sif_pram070_yn___yes","ttt_triv_prim20_yn___yes"     ,"ttt_triv_per_lp50_yn___yes","ttt_apo_stylo_yn___yes")
Consultation_20250106$C <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Agonistes], na.rm = TRUE)
Consultation_20250106$C <- ifelse(Consultation_20250106$C != 0, 1, 0)

# si le patient a pris le traitement D
Consultation_20250106$D <- Consultation_20250106$ttt_amantadine_yn___yes

# si le patient a pris le traitement E
ICOMB <- c("ttt_comptan_entac_yn___yes","ttt_tasm_talc100_yn___yes","ttt_stalevo_50_cpr_yn___yes" , "ttt_stalevo_75_cpr_yn___yes" , "ttt_stalevo_100_cpr_yn___yes" ,"ttt_stalevo_125_cpr_yn___yes"  ,"ttt_stalevo_150_cpr_yn___yes" , "ttt_stalevo_175_cpr_yn___yes" ,"ttt_stalevo_200_cpr_yn___yes")
Consultation_20250106$E <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% ICOMB], na.rm = TRUE)
Consultation_20250106$E <- ifelse(Consultation_20250106$E != 0, 1, 0)

# si le patient a pris le traitement Traitements Oraux
Consultation_20250106$pompe <- ifelse(is.na(Consultation_20250106$pompe_dose), 0,  ifelse((Consultation_20250106$pompe_dose == "DM" & is.na(Consultation_20250106$pompe_date)), 0,ifelse(!is.na(Consultation_20250106$pompe_dose) & Consultation_20250106$pompe_dose != 0, 1, 0)))

Consultation_20250106$pompe_2 <- ifelse(is.na(Consultation_20250106$pompe_dose_2), 0,  ifelse((Consultation_20250106$pompe_dose_2 == "DM" & is.na(Consultation_20250106$pompe_date_2)), 0,ifelse(!is.na(Consultation_20250106$pompe_dose_2) & Consultation_20250106$pompe_dose_2 != 0, 1, 0)))

Consultation_20250106$TO <- ifelse(Consultation_20250106$A == 1 | Consultation_20250106$B == 1 | Consultation_20250106$C == 1 | 
                              Consultation_20250106$D == 1 | Consultation_20250106$E == 1,1,0)

# si le patient a pris le traitement Stimulation Cérébrale Profonde
SCP <- c("cible___2","cible___1", "cible___3")
Consultation_20250106$SCP <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% SCP], na.rm = TRUE)
Consultation_20250106$SCP <- ifelse(Consultation_20250106$SCP != 0, 1, 0)

# si le patient a pris le traitement Lévodopa Gel Intestinal
LGI <- c("pompe_2")
Consultation_20250106$LGI <- Consultation_20250106$pompe_2

# si le patient a pris le traitement Apomorphine Sous Cutanée
ASC <- c("pompe")
Consultation_20250106$ASC <- Consultation_20250106$pompe

# si le patient a pris le traitement psychotique
Antipsychotique <- c("ttt_leponex_100_yn___yes","ttt_quetiapine_50_yn___yes","ttt_quetiapine_300_yn___yes","ttt_quetiapine_400_yn___yes")
Anticholinestherasique <- c("ttt_ache_yn___yes","ttt_exelon_yn___yes")
Consultation_20250106$Antipsychotique <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Antipsychotique], na.rm = TRUE)
Consultation_20250106$Antipsychotique <- ifelse(Consultation_20250106$Antipsychotique != 0, 1, 0)
Consultation_20250106$Anticholinestherasique <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Anticholinestherasique], na.rm = TRUE)
Consultation_20250106$Anticholinestherasique <- ifelse(Consultation_20250106$Anticholinestherasique != 0, 1, 0)



drug_cols <- c(
  "ttt_comptan_entac_yn___yes",
  "ttt_tasm_talc100_yn___yes",
  "ttt_stalevo_50_cpr_yn___yes",
  "ttt_stalevo_75_cpr_yn___yes",
  "ttt_stalevo_100_cpr_yn___yes",
  "ttt_stalevo_125_cpr_yn___yes",
  "ttt_stalevo_150_cpr_yn___yes",
  "ttt_stalevo_175_cpr_yn___yes",
  "ttt_stalevo_200_cpr_yn___yes"
)

Consultation_20250106$Entacapone <- as.numeric(
  rowSums(Consultation_20250106[, drug_cols] == 1, 
  na.rm = TRUE) > 0)



Consultation_20250106 <- Consultation_20250106 %>% 
  select(anonyme_id, act_datedeb,
         fluct_motrice:hoehn_yahr_off,ttt_ledd_totale,
         Anticholinestherasique, Antipsychotique, ASC, LGI, SCP, TO, A, B, C, D ,
         Entacapone)


Consultation_20250106$fluct_motrice <- as.numeric(Consultation_20250106$fluct_motrice)
Consultation_20250106$dyskinesie <- as.numeric(Consultation_20250106$dyskinesie)
Consultation_20250106$douleur  <- as.numeric(Consultation_20250106$douleur )
Consultation_20250106$dysarthrie  <- as.numeric(Consultation_20250106$dysarthrie )
Consultation_20250106$freezing  <- as.numeric(Consultation_20250106$freezing )
Consultation_20250106$chute_instab  <- as.numeric(Consultation_20250106$chute_instab )
Consultation_20250106$deform_post   <- as.numeric(Consultation_20250106$deform_post  )
Consultation_20250106$tr_degl   <- as.numeric(Consultation_20250106$tr_degl  )
Consultation_20250106$somnolence    <- as.numeric(Consultation_20250106$somnolence   )
Consultation_20250106$insomnie    <- as.numeric(Consultation_20250106$insomnie   )
Consultation_20250106$fatigue     <- as.numeric(Consultation_20250106$fatigue    )
Consultation_20250106$hypotension    <- as.numeric(Consultation_20250106$hypotension   )
Consultation_20250106$digestif    <- as.numeric(Consultation_20250106$digestif   )
Consultation_20250106$urine    <- as.numeric(Consultation_20250106$urine   )
Consultation_20250106$poids     <- as.numeric(Consultation_20250106$poids    )
Consultation_20250106$apathie    <- as.numeric(Consultation_20250106$apathie   )
Consultation_20250106$depression     <- as.numeric(Consultation_20250106$depression    )
Consultation_20250106$anxiete     <- as.numeric(Consultation_20250106$anxiete    )
Consultation_20250106$halluc_psy       <- as.numeric(Consultation_20250106$halluc_psy      )
Consultation_20250106$tci      <- as.numeric(Consultation_20250106$tci     )
Consultation_20250106$add_ldopa      <- as.numeric(Consultation_20250106$add_ldopa     )
Consultation_20250106$punding      <- as.numeric(Consultation_20250106$punding     )
Consultation_20250106$tr_cognitif       <- as.numeric(Consultation_20250106$tr_cognitif      )
Consultation_20250106$hoehn_yahr_on       <- as.numeric(Consultation_20250106$hoehn_yahr_on      )
Consultation_20250106$hoehn_yahr_off       <- as.numeric(Consultation_20250106$hoehn_yahr_off      )
Consultation_20250106$ttt_ledd_totale      <- as.numeric(Consultation_20250106$ttt_ledd_totale     )

Consultation_20250106 <- Consultation_20250106 %>% 
  mutate(sjsr=ifelse(sjsr=="Non",0,1)) %>%
  mutate(sas=ifelse(sas=="Non",0,1)) %>%
  mutate(rbd=ifelse(rbd=="Non",0,1)) %>%
  mutate(chute=ifelse(chute=="Non",0,1)) %>%
  mutate(neuropathique=ifelse(neuropathique!="OUI",0,1)) %>%
  mutate(nociceptive=ifelse(nociceptive!="OUI",0,1)) 


Consultation_20250106 <- Inclusion_20250106 %>% select(anonyme_id, diag_date_a, pat_ddn_a) %>% drop_na() %>%
  mutate(diag_date_a=as.numeric(diag_date_a), pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  inner_join(Consultation_20250106) %>% 
  mutate(act_datedeb=as.Date(act_datedeb)) %>%
  mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(age=year-pat_ddn_a) %>% 
  mutate(disease_dur=year-diag_date_a) %>% select(-year, -diag_date_a, -pat_ddn_a)



Consultation_20250106 %>% select(fluct_motrice, Entacapone) %>% 
  mutate(fluct_motrice=ifelse(fluct_motrice==0,0,1)) %>%
  group_by(fluct_motrice,Entacapone) %>% count() %>% drop_na()


names(Consultation_20250106)

# UNIVARIATE LOGISTIC REGRESSION

df <- Consultation_20250106

predictors <- setdiff(names(df), c("anonyme_id", "act_datedeb", "Entacapone"))

df <- df %>%
  mutate(across(
    -c(anonyme_id, act_datedeb, age, disease_dur, ttt_ledd_totale, hoehn_yahr_on, hoehn_yahr_off, Entacapone),
    ~ ifelse(. == 0, 0, 1)
  )) %>%
  mutate(across(
    c(hoehn_yahr_on, hoehn_yahr_off),
    ~ factor(., ordered = TRUE)
  ))







# Variable groups
continuous_vars <- c("age", "disease_dur", "ttt_ledd_totale")
ordered_vars <- c("hoehn_yahr_on", "hoehn_yahr_off")
binary_vars <- setdiff(names(df), c("anonyme_id", "act_datedeb", "Entacapone", continuous_vars, ordered_vars))

df <- df %>% filter(ttt_ledd_totale<=10000)

df <- df %>% mutate(ttt_ledd_totale=ttt_ledd_totale/100)
df <- df %>% mutate(age=age/10)
df <- df %>% mutate(disease_dur=disease_dur/10)

df$Entacapone <- as.factor(df$Entacapone)







continuous_vars <- c("age", "disease_dur", "ttt_ledd_totale")
results_cont <- data.frame(
  predictor = character(),
  estimate = numeric(),
  conf.low = numeric(),
  conf.high = numeric(),
  p.value = numeric(),
  stringsAsFactors = FALSE
)

for (var in continuous_vars) {
  formula <- as.formula(paste("Entacapone ~", var))
  
  fit <- try(glm(formula, data = df, family = "binomial"), silent = TRUE)
  
  if (!inherits(fit, "try-error")) {
    coef_est <- coef(summary(fit))[2, "Estimate"]
    p_val <- coef(summary(fit))[2, "Pr(>|z|)"]
    conf <- confint.default(fit)[2, ]  # Wald CI

    results_cont <- rbind(
      results_cont,
      data.frame(
        predictor = var,
        estimate = exp(coef_est),        # OR
        conf.low = exp(conf[1]),
        conf.high = exp(conf[2]),
        p.value = p_val
      )
    )
  }
}

print(results_cont)





results_binary <- data.frame(
  predictor = character(),
  estimate = numeric(),
  conf.low = numeric(),
  conf.high = numeric(),
  p.value = numeric(),
  stringsAsFactors = FALSE
)

for (var in binary_vars) {
  # Simplify variable: 0 stays 0, all else becomes 1
  bin_vec <- ifelse(df[[var]] == 0, 0, 1)

  # Avoid all-0 or all-1 edge case
  if (length(unique(bin_vec[!is.na(bin_vec)])) < 2) next

  # Build model
  fit <- try(glm(df$Entacapone ~ bin_vec, family = "binomial"), silent = TRUE)

  if (!inherits(fit, "try-error")) {
    coef_est <- coef(summary(fit))[2, "Estimate"]
    p_val <- coef(summary(fit))[2, "Pr(>|z|)"]
    conf <- confint.default(fit)[2, ]

    results_binary <- rbind(
      results_binary,
      data.frame(
        predictor = var,
        estimate = exp(coef_est),
        conf.low = exp(conf[1]),
        conf.high = exp(conf[2]),
        p.value = p_val
      )
    )
  }
}

print(results_binary)










# List of ordered variables (treated as continuous)
ordered_vars_cont <- c("hoehn_yahr_on", "hoehn_yahr_off")

results_cont_ordered <- data.frame(
  predictor = character(),
  estimate = numeric(),
  conf.low = numeric(),
  conf.high = numeric(),
  p.value = numeric(),
  stringsAsFactors = FALSE
)

for (var in ordered_vars_cont) {
  # Build logistic regression model (treating as continuous)
  fit <- try(glm(df$Entacapone ~ df[[var]], family = "binomial"), silent = TRUE)

  if (!inherits(fit, "try-error")) {
    coef_est <- coef(summary(fit))[2, "Estimate"]
    p_val <- coef(summary(fit))[2, "Pr(>|z|)"]
    
    # Confidence intervals
    conf <- confint(fit, level = 0.95)[2, ]
    
    # Store results
    results_cont_ordered <- rbind(
      results_cont_ordered,
      data.frame(
        predictor = var,
        estimate = exp(coef_est),
        conf.low = exp(conf[1]),
        conf.high = exp(conf[2]),
        p.value = p_val
      )
    )
  }
}

print(results_cont_ordered)

results_cont_ordered %>% bind_rows(results_cont) %>% bind_rows(results_binary)

odds_ratios <- read_excel(path = "../data/odds ratios.xlsx")

odds_ratios <- odds_ratios %>%
  arrange(type, estimate) %>%
  mutate(predictor = factor(predictor, levels = rev(predictor)))  

sysfonts::font_add(family = "bahnschrift", regular = "bahnschrift.ttf")  # Adjust path if needed


# Continuous variables plot
continuous_plot <- odds_ratios %>%
  filter(type == "continuous") %>%
  filter(predictor!="H&Y OFF") %>%
  ggplot(aes(x = predictor, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "#212f3d", size = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Odds Ratios for Entacapone usage \n (Continuous Variables)\n ",
    x = "",
    y = "\n Odds Ratio (log scale)"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))

ggsave(
    filename = paste0("../out/continuous_plot.svg"),
    plot = continuous_plot,
    width = 6, height = 2, device = "svg"
  )

# Binary variables plot
binary_plot <- odds_ratios %>%
  filter(type == "binary") %>%
  filter(predictor!="Levodopa") %>%
  ggplot(aes(x = predictor, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "#d98880", size = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Odds Ratios for Entacapone usage \n (Binary Variables)\n ",
    x = "",
    y = "\n Odds Ratio (log scale)"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))
  

ggsave(
    filename = paste0("../out/binary_plot.svg"),
    plot = binary_plot,
    width = 6, height = 8, device = "svg"
  )


summary(glm(df$Entacapone ~ df$fluct_motrice + df$disease_dur  , family = "binomial"))



# RANDOM FOREST (cause 30% missing data)


sum(is.na(Consultation_20250106))/(dim(Consultation_20250106)[1]*dim(Consultation_20250106)[2])


df_model <- Consultation_20250106 

df_model <- df_model %>%
  mutate(across(
    -c(anonyme_id, act_datedeb, age, disease_dur, ttt_ledd_totale, hoehn_yahr_on, hoehn_yahr_off, Entacapone),
    ~ ifelse(. == 0, 0, 1)
  )) %>%
  mutate(across(
    c(hoehn_yahr_on, hoehn_yahr_off),
    ~ factor(., ordered = TRUE)
  ))


# C: agonists (26% vs 8%), D: amantadine (39% vs 11%), A: IMAOB (25% 12%)
vars_to_exclude <- c("anonyme_id", "act_datedeb", "Levodopa", "B", "TO", "ttt_ledd_totale", "disease_dur", "age", "poids")

df_model <- df_model[, !(names(df_model) %in% vars_to_exclude)]

df_model <- df_model[!is.na(df_model$Entacapone), ]

df_model %>% group_by(fluct_motrice, Entacapone) %>% count()# (24% vs 6%)

df_model %>% group_by(dyskinesie, Entacapone) %>% count() # (22% vs 10%)

df_model %>% group_by(freezing, Entacapone) %>% count() # (21% vs 13%)

df_model <- df_model %>%  rename("Dopa_Agonist"="C", "Amantadine"="D", "IMAOB"="A" ) 

X <- data.matrix(df_model[, names(df_model) != "Entacapone"])

y <- df_model$Entacapone

X[is.nan(X)] <- NA
X[!is.finite(X)] <- NA

xgb_model <- xgboost::xgboost(
  data = X,
  label = y,
  objective = "binary:logistic",
  nrounds = 500,
  missing = NA,
  eval_metric = "auc",
  verbose = 1
)

preds <- predict(xgb_model, X)

library(pROC)
roc_obj <- roc(y, preds)
roc_obj_value <- auc(roc_obj)


# Plot the ROC curve
plot(roc_obj, main = paste("AUC:", round(roc_obj_value, 3)))





importance <- xgboost::xgb.importance(model = xgb_model)

(gg <- xgboost::xgb.plot.importance(importance_matrix = importance))

gg <- gg %>% dplyr::arrange((Importance))

gg$Feature <- factor(gg$Feature, levels = gg$Feature)

plot <- gg %>% 
  ggplot(aes(Importance, Feature)) +
  geom_col(fill="#212f3d", color="#212f3d") +
   labs(
    title = "XGBoost Relative feature importance \n Entacapone usage \n ",
    x = "\n Relative feature importance",
    y = "Feature \n"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))
  


ggsave(
    filename = paste0("../out/importance.svg"),
    plot = plot,
    width = 6, height = 8, device = "svg"
  )



# Split data into train and test sets
set.seed(123)
train_index <- sample(1:nrow(df_model), size = 0.7 * nrow(df_model))  # 70% for training
train_data <- df_model[train_index, ]
test_data <- df_model[-train_index, ]

# Prepare data for xgboost
y_train <- train_data$Entacapone
y_test <- test_data$Entacapone

# Ensure the data is numeric


X_train <- data.matrix(train_data[, names(df_model) != "Entacapone"])
X_test <- data.matrix(test_data[, names(df_model) != "Entacapone"])



# Apply cross-validation
xgb_cv <- xgboost::xgb.cv(
  data = X_train,
  label = y_train,
  objective = "binary:logistic",
  nrounds = 100,
  eta=0.01,
  nfold = 10,  # 10-fold cross-validation
  eval_metric = "auc",
  verbose = 0
)


# Plot cross-validation results
cv_results <- data.frame(
  nrounds = 1:100,
  train_roc_auc = xgb_cv$evaluation_log$train_auc_mean,
  test_roc_auc = xgb_cv$evaluation_log$test_auc_mean
)


ggplot(cv_results, aes(x = nrounds)) +
  #geom_line(aes(y = train_roc_auc, color = "Train AUC")) +
  geom_line(aes(y = test_roc_auc, color = "Test AUC")) +
  labs(title = "Cross-validation ROC AUC", x = "Number of Trees (Rounds)", y = "AUC") +
  theme_minimal()


 
preds <- predict(xgb_model, X_train)
preds_class <- ifelse(preds > 0.5, 1, 0)  # Convert probabilities to class labels

# Compute AUC on test set
roc_curve <- pROC::roc(y_train, preds_class)

auc_value <- auc(roc_curve)

# Plot the ROC curve
plot(roc_curve, main = paste("AUC:", round(auc_value, 3)))


# ------------------------

# Interaction Entacapone vs other 2nd line therapies -----------


# Packages

libraries <- c("data.table", "tidyverse", "readxl", "showtext", "sandwich", "lmtest")

lapply(libraries, 
       function(libraries) {
         if (!require(libraries, character.only=TRUE)) {
           install.packages(libraries); require(libraries)
           }}
       )

(.packages())

# Inclusion/Demographics data

Inclusion_20250106 <- read_excel(path = "../data/Inclusion_20250106.xlsx")

Inclusion_20250106 <- Inclusion_20250106[Inclusion_20250106$diag=="MP",]

length(unique(Inclusion_20250106$anonyme_id)) # 30331

# Consultation/Visit data

Consultation_20250106 <- read_excel(path = "../data/Consultation_20250106.xlsx")

Consultation_20250106 <- merge(
  Consultation_20250106,
  unique(Inclusion_20250106["anonyme_id"]),
  by = "anonyme_id"
)

length(unique(Consultation_20250106$anonyme_id)) # 25603

names(Consultation_20250106)





# si le patient a pris le traitement A
IMAOB <- c("ttt_azil_rasag1_yn___yes","ttt_safinamide_50_yn___yes","ttt_safinamide_100_yn___yes"  ,"ttt_selegiline_yn___yes")
Consultation_20250106$A <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% IMAOB], na.rm = TRUE)
Consultation_20250106$A <- ifelse(Consultation_20250106$A != 0, 1, 0)


# si le patient a pris le traitement B
Levodopa <- c("ttt_modopar_125_cpr_yn___yes","ttt_modopar_62_5_gel_yn___yes", "ttt_modopar_125_gel_yn___yes" , "ttt_modopar_250_gel_yn___yes","ttt_modopar_lp125_gel_yn___yes","ttt_sinemet_100_cpr_yn___yes" ,"ttt_sinemet_250_cpr_yn___yes" ,"ttt_sinemet_lp100_cpr_yn___yes" ,"ttt_sinemet_lp200_cpr_yn___yes" ,"ttt_stalevo_50_cpr_yn___yes" , "ttt_stalevo_75_cpr_yn___yes" , "ttt_stalevo_100_cpr_yn___yes" ,"ttt_stalevo_125_cpr_yn___yes"  ,"ttt_stalevo_150_cpr_yn___yes" , "ttt_stalevo_175_cpr_yn___yes" ,"ttt_stalevo_200_cpr_yn___yes")
Consultation_20250106$B <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Levodopa], na.rm = TRUE)
Consultation_20250106$B <- ifelse(Consultation_20250106$B != 0, 1, 0)

# si le patient a pris le traitement C
Agonistes <- c( "ttt_neu_rot2_yn___yes","ttt_neu_rot4_yn___yes"       ,"ttt_neu_rot6_yn___yes","ttt_neu_rot8_yn___yes","ttt_ral_brom5_yn___yes"        ,"ttt_ral_brom10_yn___yes"    ,"ttt_ral_brom2_5m_yn___yes","req_rop2_yn___yes","ttt_req_rop4_yn___yes"         ,"ttt_req_rop8_yn___yes" ,"ttt_req_rop025_yn___yes","ttt_req_rop050_yn___yes","ttt_req_rop1_yn___yes","ttt_req_rop2_yn___yes","ttt_req_rop5_yn___yes", "ttt_sif_pram026_yn___yes", "ttt_sif_pram052_yn___yes"     ,"ttt_sif_pram105_yn___yes","ttt_sif_pram210_yn___yes","ttt_sif_pram018_yn___yes", "ttt_sif_pram070_yn___yes","ttt_triv_prim20_yn___yes"     ,"ttt_triv_per_lp50_yn___yes","ttt_apo_stylo_yn___yes")
Consultation_20250106$C <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Agonistes], na.rm = TRUE)
Consultation_20250106$C <- ifelse(Consultation_20250106$C != 0, 1, 0)

# si le patient a pris le traitement D
Consultation_20250106$D <- Consultation_20250106$ttt_amantadine_yn___yes

# si le patient a pris le traitement E
ICOMB <- c("ttt_comptan_entac_yn___yes","ttt_tasm_talc100_yn___yes","ttt_stalevo_50_cpr_yn___yes" , "ttt_stalevo_75_cpr_yn___yes" , "ttt_stalevo_100_cpr_yn___yes" ,"ttt_stalevo_125_cpr_yn___yes"  ,"ttt_stalevo_150_cpr_yn___yes" , "ttt_stalevo_175_cpr_yn___yes" ,"ttt_stalevo_200_cpr_yn___yes")
Consultation_20250106$E <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% ICOMB], na.rm = TRUE)
Consultation_20250106$E <- ifelse(Consultation_20250106$E != 0, 1, 0)

# si le patient a pris le traitement Traitements Oraux
Consultation_20250106$pompe <- ifelse(is.na(Consultation_20250106$pompe_dose), 0,  ifelse((Consultation_20250106$pompe_dose == "DM" & is.na(Consultation_20250106$pompe_date)), 0,ifelse(!is.na(Consultation_20250106$pompe_dose) & Consultation_20250106$pompe_dose != 0, 1, 0)))

Consultation_20250106$pompe_2 <- ifelse(is.na(Consultation_20250106$pompe_dose_2), 0,  ifelse((Consultation_20250106$pompe_dose_2 == "DM" & is.na(Consultation_20250106$pompe_date_2)), 0,ifelse(!is.na(Consultation_20250106$pompe_dose_2) & Consultation_20250106$pompe_dose_2 != 0, 1, 0)))

Consultation_20250106$TO <- ifelse(Consultation_20250106$A == 1 | Consultation_20250106$B == 1 | Consultation_20250106$C == 1 | 
                              Consultation_20250106$D == 1 | Consultation_20250106$E == 1,1,0)

# si le patient a pris le traitement Stimulation Cérébrale Profonde
SCP <- c("cible___2","cible___1", "cible___3")
Consultation_20250106$SCP <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% SCP], na.rm = TRUE)
Consultation_20250106$SCP <- ifelse(Consultation_20250106$SCP != 0, 1, 0)

# si le patient a pris le traitement Lévodopa Gel Intestinal
LGI <- c("pompe_2")
Consultation_20250106$LGI <- Consultation_20250106$pompe_2

# si le patient a pris le traitement Apomorphine Sous Cutanée
ASC <- c("pompe")
Consultation_20250106$ASC <- Consultation_20250106$pompe

# si le patient a pris le traitement psychotique
Antipsychotique <- c("ttt_leponex_100_yn___yes","ttt_quetiapine_50_yn___yes","ttt_quetiapine_300_yn___yes","ttt_quetiapine_400_yn___yes")
Anticholinestherasique <- c("ttt_ache_yn___yes","ttt_exelon_yn___yes")
Consultation_20250106$Antipsychotique <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Antipsychotique], na.rm = TRUE)
Consultation_20250106$Antipsychotique <- ifelse(Consultation_20250106$Antipsychotique != 0, 1, 0)
Consultation_20250106$Anticholinestherasique <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Anticholinestherasique], na.rm = TRUE)
Consultation_20250106$Anticholinestherasique <- ifelse(Consultation_20250106$Anticholinestherasique != 0, 1, 0)



drug_cols <- c(
  "ttt_comptan_entac_yn___yes",
  "ttt_tasm_talc100_yn___yes",
  "ttt_stalevo_50_cpr_yn___yes",
  "ttt_stalevo_75_cpr_yn___yes",
  "ttt_stalevo_100_cpr_yn___yes",
  "ttt_stalevo_125_cpr_yn___yes",
  "ttt_stalevo_150_cpr_yn___yes",
  "ttt_stalevo_175_cpr_yn___yes",
  "ttt_stalevo_200_cpr_yn___yes"
)

Consultation_20250106$Entacapone <- as.numeric(
  rowSums(Consultation_20250106[, drug_cols] == 1, 
  na.rm = TRUE) > 0)



Consultation_20250106 <- Consultation_20250106 %>% 
  select(anonyme_id, act_datedeb,
         fluct_motrice:hoehn_yahr_off,ttt_ledd_totale,
         Anticholinestherasique, Antipsychotique, ASC, LGI, SCP, TO, A, B, C, D ,
         Entacapone)


Consultation_20250106$fluct_motrice <- as.numeric(Consultation_20250106$fluct_motrice)
Consultation_20250106$dyskinesie <- as.numeric(Consultation_20250106$dyskinesie)
Consultation_20250106$douleur  <- as.numeric(Consultation_20250106$douleur )
Consultation_20250106$dysarthrie  <- as.numeric(Consultation_20250106$dysarthrie )
Consultation_20250106$freezing  <- as.numeric(Consultation_20250106$freezing )
Consultation_20250106$chute_instab  <- as.numeric(Consultation_20250106$chute_instab )
Consultation_20250106$deform_post   <- as.numeric(Consultation_20250106$deform_post  )
Consultation_20250106$tr_degl   <- as.numeric(Consultation_20250106$tr_degl  )
Consultation_20250106$somnolence    <- as.numeric(Consultation_20250106$somnolence   )
Consultation_20250106$insomnie    <- as.numeric(Consultation_20250106$insomnie   )
Consultation_20250106$fatigue     <- as.numeric(Consultation_20250106$fatigue    )
Consultation_20250106$hypotension    <- as.numeric(Consultation_20250106$hypotension   )
Consultation_20250106$digestif    <- as.numeric(Consultation_20250106$digestif   )
Consultation_20250106$urine    <- as.numeric(Consultation_20250106$urine   )
Consultation_20250106$poids     <- as.numeric(Consultation_20250106$poids    )
Consultation_20250106$apathie    <- as.numeric(Consultation_20250106$apathie   )
Consultation_20250106$depression     <- as.numeric(Consultation_20250106$depression    )
Consultation_20250106$anxiete     <- as.numeric(Consultation_20250106$anxiete    )
Consultation_20250106$halluc_psy       <- as.numeric(Consultation_20250106$halluc_psy      )
Consultation_20250106$tci      <- as.numeric(Consultation_20250106$tci     )
Consultation_20250106$add_ldopa      <- as.numeric(Consultation_20250106$add_ldopa     )
Consultation_20250106$punding      <- as.numeric(Consultation_20250106$punding     )
Consultation_20250106$tr_cognitif       <- as.numeric(Consultation_20250106$tr_cognitif      )
Consultation_20250106$hoehn_yahr_on       <- as.numeric(Consultation_20250106$hoehn_yahr_on      )
Consultation_20250106$hoehn_yahr_off       <- as.numeric(Consultation_20250106$hoehn_yahr_off      )
Consultation_20250106$ttt_ledd_totale      <- as.numeric(Consultation_20250106$ttt_ledd_totale     )

Consultation_20250106 <- Consultation_20250106 %>% 
  mutate(sjsr=ifelse(sjsr=="Non",0,1)) %>%
  mutate(sas=ifelse(sas=="Non",0,1)) %>%
  mutate(rbd=ifelse(rbd=="Non",0,1)) %>%
  mutate(chute=ifelse(chute=="Non",0,1)) %>%
  mutate(neuropathique=ifelse(neuropathique!="OUI",0,1)) %>%
  mutate(nociceptive=ifelse(nociceptive!="OUI",0,1)) 


Consultation_20250106 <- Inclusion_20250106 %>% select(anonyme_id, diag_date_a, pat_ddn_a) %>% drop_na() %>%
  mutate(diag_date_a=as.numeric(diag_date_a), pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  inner_join(Consultation_20250106) %>% 
  mutate(act_datedeb=as.Date(act_datedeb)) %>%
  mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(age=year-pat_ddn_a) %>% 
  mutate(disease_dur=year-diag_date_a) %>% select(-year, -diag_date_a, -pat_ddn_a)











confusion_data <- Consultation_20250106 %>% 
  mutate(SCP=ifelse(SCP==0, "OFF", "ON")) %>%
  mutate(Entacapone=ifelse(Entacapone==0, "OFF", "ON")) %>%
  rename("DBS"="SCP") %>%
  group_by(DBS, Entacapone) %>% count()

# Create a 2x2 matrix from your data
contingency_matrix <- matrix(c(
  58701, 9663,  # DBS OFF: Entacapone OFF, ON
  656,   266    # DBS ON: Entacapone OFF, ON
), nrow = 2, byrow = TRUE)

# Optionally, name rows and columns
dimnames(contingency_matrix) <- list(
  DBS = c("OFF", "ON"),
  Entacapone = c("OFF", "ON")
)

fisher.test(contingency_matrix)

# 	Fisher's Exact Test for Count Data
# 
# data:  contingency_matrix
# p-value < 2.2e-16
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  2.124684 2.849555
# sample estimates:
# odds ratio 
#   2.463219 


plot_numbers <- ggplot(confusion_data, aes(x = factor(Entacapone), y = factor(DBS), fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_fill_gradient(low = "#f3fbfe", high = "#043a4f") +
  labs(
    x = "Entacapone (ON/OFF)",
    y = "DBS (ON/OFF)",
    fill = "Count",
    title = "Number of patient-visits ON/OFF"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))

ggsave("../out/plot_numbers.svg", plot = plot_numbers, width = 6, height = 5, device = "svg")


confusion_data <- Consultation_20250106 %>% 
  mutate(SCP=ifelse(SCP==0, "OFF", "ON")) %>%
  mutate(Entacapone=ifelse(Entacapone==0, "OFF", "ON")) %>%
  rename("DBS"="SCP") %>%
  group_by(DBS, Entacapone) %>% count() %>%
  ungroup() %>% group_by(DBS) %>% mutate(DBStot=sum(n)) %>%
  mutate(n=n/DBStot)

plot_dbs_perc <- ggplot(confusion_data, aes(x = factor(Entacapone), y = factor(DBS), fill = round(n,2) )) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(n,2)), color = "black", size = 5) +
  scale_fill_gradient(low = "#f3fbfe", high = "#043a4f") +
  labs(
    x = "Entacapone (ON/OFF)",
    y = "DBS (ON/OFF)",
    fill = "Proportion",
    title = "Proportion of DBS ON/OFF patient-visits"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))

ggsave("../out/plot_dbs_perc.svg", plot = plot_dbs_perc, width = 6, height = 5, device = "svg")


confusion_data <- Consultation_20250106 %>% 
  mutate(SCP=ifelse(SCP==0, "OFF", "ON")) %>%
  mutate(Entacapone=ifelse(Entacapone==0, "OFF", "ON")) %>%
  rename("DBS"="SCP") %>%
  group_by(DBS, Entacapone) %>% count() %>%
  ungroup() %>% group_by(Entacapone) %>% mutate(Entacaponetot=sum(n)) %>%
  mutate(n=n/Entacaponetot)

plot_entacapone_perc <- ggplot(confusion_data, aes(x = factor(Entacapone), y = factor(DBS), fill = round(n,2) )) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(n,2)), color = "black", size = 5) +
  scale_fill_gradient(low = "#f3fbfe", high = "#043a4f") +
  labs(
    x = "Entacapone (ON/OFF)",
    y = "DBS (ON/OFF)",
    fill = "Proportion",
    title = "Proportion of Entacapone ON/OFF patient-visits"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))

ggsave("../out/plot_entacapone_perc.svg", plot = plot_entacapone_perc, width = 6, height = 5, device = "svg")








confusion_data <- Consultation_20250106 %>% 
  mutate(ASC=ifelse(ASC==0, "OFF", "ON")) %>%
  mutate(Entacapone=ifelse(Entacapone==0, "OFF", "ON")) %>%
  rename("SCAI"="ASC") %>%
  group_by(SCAI, Entacapone) %>% count()

# Create a 2x2 matrix from your data
contingency_matrix <- matrix(c(
  58889, 9651,  # SCAI OFF: Entacapone OFF, ON
  468,   278    # SCAI ON: Entacapone OFF, ON
), nrow = 2, byrow = TRUE)

# Optionally, name rows and columns
dimnames(contingency_matrix) <- list(
  SCAI = c("OFF", "ON"),
  Entacapone = c("OFF", "ON")
)

fisher.test(contingency_matrix)

# 	Fisher's Exact Test for Count Data
# 
# data:  contingency_matrix
# p-value < 2.2e-16
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  3.108267 4.220873
# sample estimates:
# odds ratio 
#   3.624576 

plot_numbers <- ggplot(confusion_data, aes(x = factor(Entacapone), y = factor(SCAI), fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_fill_gradient(low = "#fdf1f9", high = "#6f0a4f") +
  labs(
    x = "Entacapone (ON/OFF)",
    y = "SCAI (ON/OFF)",
    fill = "Count",
    title = "Number of patient-visits ON/OFF"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))

ggsave("../out/plot_numbers.svg", plot = plot_numbers, width = 6, height = 5, device = "svg")




confusion_data <- Consultation_20250106 %>% 
  mutate(ASC=ifelse(ASC==0, "OFF", "ON")) %>%
  mutate(Entacapone=ifelse(Entacapone==0, "OFF", "ON")) %>%
  rename("SCAI"="ASC") %>%
  group_by(SCAI, Entacapone) %>% count() %>%
  ungroup() %>% group_by(SCAI) %>% mutate(SCAItot=sum(n)) %>%
  mutate(n=n/SCAItot)

plot_scai_perc <- ggplot(confusion_data, aes(x = factor(Entacapone), y = factor(SCAI), fill = round(n,2) )) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(n,2)), color = "black", size = 5) +
  scale_fill_gradient(low = "#fdf1f9", high = "#6f0a4f") +
  labs(
    x = "Entacapone (ON/OFF)",
    y = "SCAI (ON/OFF)",
    fill = "Proportion",
    title = "Proportion of SCAI ON/OFF patient-visits"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))

ggsave("../out/plot_scai_perc.svg", plot = plot_scai_perc, width = 6, height = 5, device = "svg")


confusion_data <- Consultation_20250106 %>% 
  mutate(ASC=ifelse(ASC==0, "OFF", "ON")) %>%
  mutate(Entacapone=ifelse(Entacapone==0, "OFF", "ON")) %>%
  rename("SCAI"="ASC") %>%
  group_by(SCAI, Entacapone) %>% count() %>%
  ungroup() %>% group_by(Entacapone) %>% mutate(Entacaponetot=sum(n)) %>%
  mutate(n=n/Entacaponetot)

plot_entacapone_perc <- ggplot(confusion_data, aes(x = factor(Entacapone), y = factor(SCAI), fill = round(n,2) )) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(n,2)), color = "black", size = 5) +
  scale_fill_gradient(low = "#fdf1f9", high = "#6f0a4f") +
  labs(
    x = "Entacapone (ON/OFF)",
    y = "SCAI (ON/OFF)",
    fill = "Proportion",
    title = "Proportion of Entacapone ON/OFF patient-visits"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))

ggsave("../out/plot_entacapone_perc.svg", plot = plot_entacapone_perc, width = 6, height = 5, device = "svg")











confusion_data <- Consultation_20250106 %>% 
  mutate(LGI=ifelse(LGI==0, "OFF", "ON")) %>%
  mutate(Entacapone=ifelse(Entacapone==0, "OFF", "ON")) %>%
  rename("LCIG"="LGI") %>%
  group_by(LCIG, Entacapone) %>% count()

# Create a 2x2 matrix from your data
contingency_matrix <- matrix(c(
  59278, 9908,  # LCIG OFF: Entacapone OFF, ON
  79,   21    # LCIG ON: Entacapone OFF, ON
), nrow = 2, byrow = TRUE)

# Optionally, name rows and columns
dimnames(contingency_matrix) <- list(
  LCIG = c("OFF", "ON"),
  Entacapone = c("OFF", "ON")
)

fisher.test(contingency_matrix)

# 	Fisher's Exact Test for Count Data
# 
# data:  contingency_matrix
# p-value = 0.06299
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  0.9328166 2.6013525
# sample estimates:
# odds ratio 
#   1.590355 

plot_numbers <- ggplot(confusion_data, aes(x = factor(Entacapone), y = factor(LCIG), fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_fill_gradient(low = "#f1dada", high = "#840d0d") +
  labs(
    x = "Entacapone (ON/OFF)",
    y = "LCIG (ON/OFF)",
    fill = "Count",
    title = "Number of patient-visits ON/OFF"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))

ggsave("../out/plot_numbers.svg", plot = plot_numbers, width = 6, height = 5, device = "svg")




confusion_data <- Consultation_20250106 %>% 
  mutate(LGI=ifelse(LGI==0, "OFF", "ON")) %>%
  mutate(Entacapone=ifelse(Entacapone==0, "OFF", "ON")) %>%
  rename("LCIG"="LGI") %>%
  group_by(LCIG, Entacapone) %>% count() %>%
  ungroup() %>% group_by(LCIG) %>% mutate(LCIGtot=sum(n)) %>%
  mutate(n=n/LCIGtot)

plot_LCIG_perc <- ggplot(confusion_data, aes(x = factor(Entacapone), y = factor(LCIG), fill = round(n,2) )) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(n,2)), color = "black", size = 5) +
  scale_fill_gradient(low = "#f1dada", high = "#840d0d") +
  labs(
    x = "Entacapone (ON/OFF)",
    y = "LCIG (ON/OFF)",
    fill = "Proportion",
    title = "Proportion of LCIG ON/OFF patient-visits"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))

ggsave("../out/plot_LCIG_perc.svg", plot = plot_LCIG_perc, width = 6, height = 5, device = "svg")


confusion_data <- Consultation_20250106 %>% 
  mutate(LGI=ifelse(LGI==0, "OFF", "ON")) %>%
  mutate(Entacapone=ifelse(Entacapone==0, "OFF", "ON")) %>%
  rename("LCIG"="LGI") %>%
  group_by(LCIG, Entacapone) %>% count() %>%
  ungroup() %>% group_by(Entacapone) %>% mutate(Entacaponetot=sum(n)) %>%
  mutate(n=n/Entacaponetot)

plot_entacapone_perc <- ggplot(confusion_data, aes(x = factor(Entacapone), y = factor(LCIG), fill = round(n,2) )) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(n,2)), color = "black", size = 5) +
  scale_fill_gradient(low = "#f1dada", high = "#840d0d") +
  labs(
    x = "Entacapone (ON/OFF)",
    y = "LCIG (ON/OFF)",
    fill = "Proportion",
    title = "Proportion of Entacapone ON/OFF patient-visits"
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
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))

ggsave("../out/plot_entacapone_perc.svg", plot = plot_entacapone_perc, width = 6, height = 5, device = "svg")






device_cols <- c(
  "SCP",
  "LGI",
  "ASC")

Consultation_20250106$device_assisted <- as.numeric(
  rowSums(Consultation_20250106[, device_cols] == 1, 
  na.rm = TRUE) > 0)


Consultation_20250106 %>% group_by(device_assisted, Entacapone) %>% count()


Entac_pats <- Consultation_20250106 %>% filter(Entacapone==1) %>% select(anonyme_id) %>% distinct()


Consultation_20250106 %>% group_by(anonyme_id) %>% summarise(device_assisted=max(device_assisted, na.rm=T)) %>%
  left_join(Consultation_20250106 %>% group_by(anonyme_id) %>% filter(Entacapone==0&lag(Entacapone)==1) %>% 
              select(anonyme_id) %>% distinct() %>% mutate(group="stop")) %>%
  mutate(group=ifelse(is.na(group), "cont", group)) %>%
  inner_join(Entac_pats) %>%
  group_by(group, device_assisted) %>% count()


# Create a 2x2 matrix from your data
contingency_matrix <- matrix(c(
  2270, 159,  # Device OFF: Stop No, yes
  946,   190    # Device ON: Stop No, yes
), nrow = 2, byrow = TRUE)


# Optionally, name rows and columns
dimnames(contingency_matrix) <- list(
  Device = c("OFF", "ON"),
  Stop = c("OFF", "ON")
)

fisher.test(contingency_matrix)






Consultation_20250106 %>% group_by(anonyme_id) %>% filter(Entacapone==1) %>% 
  filter(act_datedeb==min(act_datedeb, na.rm=T)) %>% # 3550 patients
  ungroup() %>% summarise(disease_dur=sd(disease_dur))


Consultation_20250106 %>% group_by(anonyme_id) %>% filter(device_assisted==1) %>% 
  filter(act_datedeb==min(act_datedeb, na.rm=T)) %>% # 842 patients
  ungroup() %>% summarise(disease_dur=sd(disease_dur))



plot <- Consultation_20250106 %>% group_by(anonyme_id) %>% filter(Entacapone==1) %>% 
  filter(act_datedeb==min(act_datedeb, na.rm=T)) %>% # 3550 patients
  ungroup() %>% select(anonyme_id, disease_dur) %>% mutate(Group="Entacapone") %>%
  bind_rows(
    Consultation_20250106 %>% group_by(anonyme_id) %>% filter(device_assisted==1) %>% 
    filter(act_datedeb==min(act_datedeb, na.rm=T)) %>% # 842 patients
    ungroup() %>% select(anonyme_id, disease_dur) %>% mutate(Group="Device-assisted")
    ) %>%
  filter(disease_dur>=0&disease_dur<=50) %>%
  ggplot(aes(disease_dur, colour=Group, fill=Group)) +
  geom_density(alpha=0.5) +
  labs(x = "\n Disease duration to start (years)", y = "Patient density \n", 
       title = "Disease duration at the start of \nEntacapone ando/or device-assisted therapy") +
  theme_minimal() +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text( angle = 45, vjust = 1.0, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))  +
  scale_fill_manual(values=c( "#043A4F", "#6B0B0B")) +
  scale_colour_manual(values=c( "#043A4F", "#6B0B0B")) 

ggsave("../out/time_to_start.svg", plot = plot, width = 6, height = 4, device = "svg")





  
 

Consultation_20250106 %>% filter(device_assisted==1) %>% select(anonyme_id) %>% distinct() %>%
  inner_join(Consultation_20250106 %>% filter(Entacapone==1) %>% select(anonyme_id) %>% distinct()) %>% #349
  inner_join(Consultation_20250106) %>%
  group_by(anonyme_id) %>%
  filter(device_assisted==1|Entacapone==1) %>%  filter(act_datedeb==min(act_datedeb, na.rm=T)) %>% 
  select(anonyme_id, disease_dur, device_assisted, Entacapone) %>% distinct() %>%
  group_by(device_assisted, Entacapone) %>% count() %>% mutate(n=n/346)



Consultation_20250106 %>% filter(device_assisted==1) %>% select(anonyme_id) %>% distinct() %>%
  inner_join(Consultation_20250106 %>% filter(Entacapone==1) %>% select(anonyme_id) %>% distinct()) %>% #349
  inner_join(Consultation_20250106)  %>%
   group_by(anonyme_id) %>%
  filter(device_assisted==1|Entacapone==1) %>%  filter(act_datedeb==min(act_datedeb, na.rm=T)) %>% 
  ungroup() %>% summarise(disease_dur=mean(disease_dur))
  
  


Consultation_20250106 %>% group_by(anonyme_id) %>% filter(device_assisted==1) %>% 
  filter(act_datedeb==min(act_datedeb, na.rm=T)) %>% # 842 patients
  ungroup() %>% summarise(disease_dur=mean(disease_dur)


plot <- data.frame(Consultation_20250106 %>% 
             select(device_assisted, Entacapone, fluct_motrice) %>%
             drop_na() %>% group_by(device_assisted, Entacapone, fluct_motrice) %>%
             count()) %>% ungroup() %>% group_by(device_assisted, Entacapone) %>%
  mutate(tot=sum(n)) %>% mutate(n=n/tot) %>% select(-tot) %>%
  mutate(group=paste0(Entacapone, device_assisted)) %>%
  mutate(group=ifelse(group=="00", "1- None",
                      ifelse(group=="10", "2- Entacapone-only",
                             ifelse(group=="01", "3- Device-assisted only", "4- Entacapne + Device-assisted")))) %>%
  ggplot( aes(x = factor(group), y = n, fill = factor(fluct_motrice))) +
  geom_bar(stat = "identity", alpha=0.8) +
    geom_text(aes(label = scales::percent(n, accuracy = 1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3.5) +
  labs(x = "", y = "Patient-visits proportion \n", 
       fill = "Motor Flucations Score", 
       title = "Motor flucations scores by \n Device-assisted therapy and Entacapone status") +
  theme_minimal() +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text( angle = 45, vjust = 1.0, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))  +
  scale_fill_manual(values=c("#F1FAFD", "#3D5E70", "#043A4F", "#860C60", "#6B0B0B"))

    
plot

ggsave("../out/motor_fluct.svg", plot = plot, width = 6, height = 6, device = "svg")




data.frame(Consultation_20250106 %>% 
  group_by(device_assisted, Entacapone) %>%
    drop_na() %>%
  summarise(fluct_motrice=mean(fluct_motrice, na.rm=T), sd=sd(fluct_motrice, na.rm=T)))


df <- Consultation_20250106 %>% select(fluct_motrice, Entacapone, device_assisted) %>% drop_na()

df$Entacapone <- factor(df$Entacapone)
df$device_assisted <- factor(df$device_assisted)

model <- MASS::polr(as.ordered(fluct_motrice) ~ Entacapone * device_assisted, data = df, method = "logistic")

# Call:
# MASS::polr(formula = as.ordered(fluct_motrice) ~ Entacapone * 
#     device_assisted, data = df, method = "logistic")
# 
# Coefficients:
#                  Entacapone1             device_assisted1 Entacapone1:device_assisted1 
#                     1.238744                     2.130841                    -1.018234 
# 
# Intercepts:
#       0|1       1|2       2|3       3|4 
# 0.2888782 3.0207038 4.4362760 6.8938388 
# 
# Residual Deviance: 73773.19 
# AIC: 73787.19 


df$group <- interaction(df$Entacapone, df$device_assisted)

kruskal.test(fluct_motrice ~ group, data = df)
# 
# 	Kruskal-Wallis rank sum test
# 
# data:  fluct_motrice by group
# Kruskal-Wallis chi-squared = 3201.9, df = 3, p-value < 2.2e-16

pairwise.wilcox.test(df$fluct_motrice, df$group, p.adjust.method = "bonferroni")


# -----------
# Persistency ON Entacapone ----------------------

# Packages

libraries <- c("data.table", "tidyverse", "readxl", "showtext", "sandwich", "lmtest")

lapply(libraries, 
       function(libraries) {
         if (!require(libraries, character.only=TRUE)) {
           install.packages(libraries); require(libraries)
           }}
       )

(.packages())

# Inclusion/Demographics data

Inclusion_20250106 <- read_excel(path = "../data/Inclusion_20250106.xlsx")

Inclusion_20250106 <- Inclusion_20250106[Inclusion_20250106$diag=="MP",]

length(unique(Inclusion_20250106$anonyme_id)) # 30331

# Consultation/Visit data

Consultation_20250106 <- read_excel(path = "../data/Consultation_20250106.xlsx")

Consultation_20250106 <- merge(
  Consultation_20250106,
  unique(Inclusion_20250106["anonyme_id"]),
  by = "anonyme_id"
)

length(unique(Consultation_20250106$anonyme_id)) # 25603

names(Consultation_20250106)





# si le patient a pris le traitement A
IMAOB <- c("ttt_azil_rasag1_yn___yes","ttt_safinamide_50_yn___yes","ttt_safinamide_100_yn___yes"  ,"ttt_selegiline_yn___yes")
Consultation_20250106$A <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% IMAOB], na.rm = TRUE)
Consultation_20250106$A <- ifelse(Consultation_20250106$A != 0, 1, 0)


# si le patient a pris le traitement B
Levodopa <- c("ttt_modopar_125_cpr_yn___yes","ttt_modopar_62_5_gel_yn___yes", "ttt_modopar_125_gel_yn___yes" , "ttt_modopar_250_gel_yn___yes","ttt_modopar_lp125_gel_yn___yes","ttt_sinemet_100_cpr_yn___yes" ,"ttt_sinemet_250_cpr_yn___yes" ,"ttt_sinemet_lp100_cpr_yn___yes" ,"ttt_sinemet_lp200_cpr_yn___yes" ,"ttt_stalevo_50_cpr_yn___yes" , "ttt_stalevo_75_cpr_yn___yes" , "ttt_stalevo_100_cpr_yn___yes" ,"ttt_stalevo_125_cpr_yn___yes"  ,"ttt_stalevo_150_cpr_yn___yes" , "ttt_stalevo_175_cpr_yn___yes" ,"ttt_stalevo_200_cpr_yn___yes")
Consultation_20250106$B <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Levodopa], na.rm = TRUE)
Consultation_20250106$B <- ifelse(Consultation_20250106$B != 0, 1, 0)

# si le patient a pris le traitement C
Agonistes <- c( "ttt_neu_rot2_yn___yes","ttt_neu_rot4_yn___yes"       ,"ttt_neu_rot6_yn___yes","ttt_neu_rot8_yn___yes","ttt_ral_brom5_yn___yes"        ,"ttt_ral_brom10_yn___yes"    ,"ttt_ral_brom2_5m_yn___yes","req_rop2_yn___yes","ttt_req_rop4_yn___yes"         ,"ttt_req_rop8_yn___yes" ,"ttt_req_rop025_yn___yes","ttt_req_rop050_yn___yes","ttt_req_rop1_yn___yes","ttt_req_rop2_yn___yes","ttt_req_rop5_yn___yes", "ttt_sif_pram026_yn___yes", "ttt_sif_pram052_yn___yes"     ,"ttt_sif_pram105_yn___yes","ttt_sif_pram210_yn___yes","ttt_sif_pram018_yn___yes", "ttt_sif_pram070_yn___yes","ttt_triv_prim20_yn___yes"     ,"ttt_triv_per_lp50_yn___yes","ttt_apo_stylo_yn___yes")
Consultation_20250106$C <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Agonistes], na.rm = TRUE)
Consultation_20250106$C <- ifelse(Consultation_20250106$C != 0, 1, 0)

# si le patient a pris le traitement D
Consultation_20250106$D <- Consultation_20250106$ttt_amantadine_yn___yes

# si le patient a pris le traitement E
ICOMB <- c("ttt_comptan_entac_yn___yes","ttt_tasm_talc100_yn___yes","ttt_stalevo_50_cpr_yn___yes" , "ttt_stalevo_75_cpr_yn___yes" , "ttt_stalevo_100_cpr_yn___yes" ,"ttt_stalevo_125_cpr_yn___yes"  ,"ttt_stalevo_150_cpr_yn___yes" , "ttt_stalevo_175_cpr_yn___yes" ,"ttt_stalevo_200_cpr_yn___yes")
Consultation_20250106$E <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% ICOMB], na.rm = TRUE)
Consultation_20250106$E <- ifelse(Consultation_20250106$E != 0, 1, 0)

# si le patient a pris le traitement Traitements Oraux
Consultation_20250106$pompe <- ifelse(is.na(Consultation_20250106$pompe_dose), 0,  ifelse((Consultation_20250106$pompe_dose == "DM" & is.na(Consultation_20250106$pompe_date)), 0,ifelse(!is.na(Consultation_20250106$pompe_dose) & Consultation_20250106$pompe_dose != 0, 1, 0)))

Consultation_20250106$pompe_2 <- ifelse(is.na(Consultation_20250106$pompe_dose_2), 0,  ifelse((Consultation_20250106$pompe_dose_2 == "DM" & is.na(Consultation_20250106$pompe_date_2)), 0,ifelse(!is.na(Consultation_20250106$pompe_dose_2) & Consultation_20250106$pompe_dose_2 != 0, 1, 0)))

Consultation_20250106$TO <- ifelse(Consultation_20250106$A == 1 | Consultation_20250106$B == 1 | Consultation_20250106$C == 1 | 
                              Consultation_20250106$D == 1 | Consultation_20250106$E == 1,1,0)

# si le patient a pris le traitement Stimulation Cérébrale Profonde
SCP <- c("cible___2","cible___1", "cible___3")
Consultation_20250106$SCP <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% SCP], na.rm = TRUE)
Consultation_20250106$SCP <- ifelse(Consultation_20250106$SCP != 0, 1, 0)

# si le patient a pris le traitement Lévodopa Gel Intestinal
LGI <- c("pompe_2")
Consultation_20250106$LGI <- Consultation_20250106$pompe_2

# si le patient a pris le traitement Apomorphine Sous Cutanée
ASC <- c("pompe")
Consultation_20250106$ASC <- Consultation_20250106$pompe

# si le patient a pris le traitement psychotique
Antipsychotique <- c("ttt_leponex_100_yn___yes","ttt_quetiapine_50_yn___yes","ttt_quetiapine_300_yn___yes","ttt_quetiapine_400_yn___yes")
Anticholinestherasique <- c("ttt_ache_yn___yes","ttt_exelon_yn___yes")
Consultation_20250106$Antipsychotique <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Antipsychotique], na.rm = TRUE)
Consultation_20250106$Antipsychotique <- ifelse(Consultation_20250106$Antipsychotique != 0, 1, 0)
Consultation_20250106$Anticholinestherasique <- rowSums(Consultation_20250106[, names(Consultation_20250106) %in% Anticholinestherasique], na.rm = TRUE)
Consultation_20250106$Anticholinestherasique <- ifelse(Consultation_20250106$Anticholinestherasique != 0, 1, 0)



drug_cols <- c(
  "ttt_comptan_entac_yn___yes",
  "ttt_tasm_talc100_yn___yes",
  "ttt_stalevo_50_cpr_yn___yes",
  "ttt_stalevo_75_cpr_yn___yes",
  "ttt_stalevo_100_cpr_yn___yes",
  "ttt_stalevo_125_cpr_yn___yes",
  "ttt_stalevo_150_cpr_yn___yes",
  "ttt_stalevo_175_cpr_yn___yes",
  "ttt_stalevo_200_cpr_yn___yes"
)

Consultation_20250106$Entacapone <- as.numeric(
  rowSums(Consultation_20250106[, drug_cols] == 1, 
  na.rm = TRUE) > 0)



Consultation_20250106 <- Consultation_20250106 %>% 
  select(anonyme_id, act_datedeb,
         fluct_motrice:hoehn_yahr_off,ttt_ledd_totale,
         Anticholinestherasique, Antipsychotique, ASC, LGI, SCP, TO, A, B, C, D ,
         Entacapone)


Consultation_20250106$fluct_motrice <- as.numeric(Consultation_20250106$fluct_motrice)
Consultation_20250106$dyskinesie <- as.numeric(Consultation_20250106$dyskinesie)
Consultation_20250106$douleur  <- as.numeric(Consultation_20250106$douleur )
Consultation_20250106$dysarthrie  <- as.numeric(Consultation_20250106$dysarthrie )
Consultation_20250106$freezing  <- as.numeric(Consultation_20250106$freezing )
Consultation_20250106$chute_instab  <- as.numeric(Consultation_20250106$chute_instab )
Consultation_20250106$deform_post   <- as.numeric(Consultation_20250106$deform_post  )
Consultation_20250106$tr_degl   <- as.numeric(Consultation_20250106$tr_degl  )
Consultation_20250106$somnolence    <- as.numeric(Consultation_20250106$somnolence   )
Consultation_20250106$insomnie    <- as.numeric(Consultation_20250106$insomnie   )
Consultation_20250106$fatigue     <- as.numeric(Consultation_20250106$fatigue    )
Consultation_20250106$hypotension    <- as.numeric(Consultation_20250106$hypotension   )
Consultation_20250106$digestif    <- as.numeric(Consultation_20250106$digestif   )
Consultation_20250106$urine    <- as.numeric(Consultation_20250106$urine   )
Consultation_20250106$poids     <- as.numeric(Consultation_20250106$poids    )
Consultation_20250106$apathie    <- as.numeric(Consultation_20250106$apathie   )
Consultation_20250106$depression     <- as.numeric(Consultation_20250106$depression    )
Consultation_20250106$anxiete     <- as.numeric(Consultation_20250106$anxiete    )
Consultation_20250106$halluc_psy       <- as.numeric(Consultation_20250106$halluc_psy      )
Consultation_20250106$tci      <- as.numeric(Consultation_20250106$tci     )
Consultation_20250106$add_ldopa      <- as.numeric(Consultation_20250106$add_ldopa     )
Consultation_20250106$punding      <- as.numeric(Consultation_20250106$punding     )
Consultation_20250106$tr_cognitif       <- as.numeric(Consultation_20250106$tr_cognitif      )
Consultation_20250106$hoehn_yahr_on       <- as.numeric(Consultation_20250106$hoehn_yahr_on      )
Consultation_20250106$hoehn_yahr_off       <- as.numeric(Consultation_20250106$hoehn_yahr_off      )
Consultation_20250106$ttt_ledd_totale      <- as.numeric(Consultation_20250106$ttt_ledd_totale     )

Consultation_20250106 <- Consultation_20250106 %>% 
  mutate(sjsr=ifelse(sjsr=="Non",0,1)) %>%
  mutate(sas=ifelse(sas=="Non",0,1)) %>%
  mutate(rbd=ifelse(rbd=="Non",0,1)) %>%
  mutate(chute=ifelse(chute=="Non",0,1)) %>%
  mutate(neuropathique=ifelse(neuropathique!="OUI",0,1)) %>%
  mutate(nociceptive=ifelse(nociceptive!="OUI",0,1)) 


Consultation_20250106 <- Inclusion_20250106 %>% select(anonyme_id, diag_date_a, pat_ddn_a) %>% drop_na() %>%
  mutate(diag_date_a=as.numeric(diag_date_a), pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  inner_join(Consultation_20250106) %>% 
  mutate(act_datedeb=as.Date(act_datedeb)) %>%
  mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(age=year-pat_ddn_a) %>% 
  mutate(disease_dur=year-diag_date_a) %>% select(-year, -diag_date_a, -pat_ddn_a)





# PERSISTENCY 





df <- Consultation_20250106 %>% select(anonyme_id, act_datedeb, Entacapone) %>%
  arrange(anonyme_id, act_datedeb, Entacapone)


df <- df %>%
  group_by(anonyme_id) %>%
  filter(n() >= 2) %>%
  arrange(anonyme_id, act_datedeb)

first_on <- df %>%
  filter(Entacapone == 1) %>%
  group_by(anonyme_id) %>%
  slice_min(order_by = act_datedeb, n = 1, with_ties = FALSE) %>%
  rename(entacapone_start_date = act_datedeb)

df <- df %>%
  inner_join(first_on %>% select(-Entacapone)) %>%
  filter(!is.na(entacapone_start_date)) %>%
  filter(act_datedeb  >= entacapone_start_date)


df <- df %>% mutate(diff=as.numeric(act_datedeb-entacapone_start_date )) %>% filter(diff<=3653) %>% select(-diff)


df_2 <- df %>%
  group_by(anonyme_id) %>%
  filter(n() >= 2) %>%
  arrange(anonyme_id, act_datedeb)


df_5 <- df %>%
  group_by(anonyme_id) %>%
  filter(n() >= 5) %>%
  arrange(anonyme_id, act_datedeb)


df_10 <- df %>%
  group_by(anonyme_id) %>%
  filter(n() >= 10) %>%
  arrange(anonyme_id, act_datedeb)



persistence_status <- df_10 %>%
  group_by(anonyme_id) %>%
  slice_max(act_datedeb) %>%
  mutate(persistent = Entacapone == 1)


df_10 <- df_10 %>%
  group_by(anonyme_id) %>%
  mutate(persistent = cumsum(Entacapone == 0 & lag(Entacapone, default = 1) == 1) == 0)



persist_df <- df_10 %>%
  group_by(anonyme_id) %>%
  mutate(event = ifelse(Entacapone == 0 & lag(Entacapone, default = 1) == 1, 1, 0)) %>%
  summarise(
    time = as.numeric(min(act_datedeb[event == 1], na.rm = TRUE) - min(act_datedeb)),
    status = ifelse(any(event == 1), 1, 0)
  )

persist_df <- persist_df %>%
  mutate(time = ifelse(is.infinite(time), NA, time)) %>%
  left_join(df %>% group_by(anonyme_id) %>%
              summarise(max_followup = as.numeric(max(act_datedeb) - min(act_datedeb))),
            by = "anonyme_id") %>%
  mutate(
    time = ifelse(is.na(time), max_followup, time)
  )


#persist_df_2 <- persist_df
#persist_df_5 <- persist_df
persist_df_10 <- persist_df



persist_df <- persist_df_2 %>% mutate(VISITS=" A) >2") %>%
  bind_rows(persist_df_5 %>% mutate(VISITS=" B) >5 ")) %>%
  bind_rows(persist_df_10 %>% mutate(VISITS=" C) >10"))


km_fit_all <- survival::survfit(survival::Surv(time, status) ~ VISITS, data = persist_df)

summary(km_fit_all)


survminer::ggsurvplot(
  km_fit_all,
  data = persist_df,
  conf.int = TRUE,
  pval = TRUE,
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






reg <- persist_df %>% 
  left_join(
    df_10 %>% select(anonyme_id, entacapone_start_date) %>% rename(act_datedeb=entacapone_start_date) %>%
  left_join(Consultation_20250106 %>% select(anonyme_id, act_datedeb, disease_dur)) %>% ungroup()  %>%
  select(-act_datedeb) %>% drop_na() %>% distinct() 
  ) %>% drop_na() %>%
  filter(disease_dur>=0) 


cox_model <- survival::coxph(survival::Surv(time, status) ~ disease_dur, data = reg)
summary(cox_model)





# REASONS TO STOP


# COMPARE OVERALL TRAJECTORIES OVER TIME 



df <- Consultation_20250106 %>% select(anonyme_id, act_datedeb, Entacapone) %>%
  arrange(anonyme_id, act_datedeb, Entacapone) %>%
  inner_join(Consultation_20250106 %>% filter(Entacapone==1) %>% select(anonyme_id) %>% distinct())

length(unique(df$anonyme_id))


df <- df %>% arrange(anonyme_id, act_datedeb, Entacapone) %>%
  group_by(anonyme_id, act_datedeb) %>% summarise(Entacapone=max(Entacapone)) %>% ungroup()

pats_stopped <- df %>% group_by(anonyme_id) %>% filter(Entacapone==0&lag(Entacapone)==1) %>% select(anonyme_id) %>% distinct()


df <- df %>% left_join(pats_stopped %>% mutate(group="stopped")) %>% mutate(group=ifelse(is.na(group),"cont", group ))


df %>% left_join(Consultation_20250106 %>% select(anonyme_id, act_datedeb, digestif)) %>%
  group_by(group) %>% summarise(mean=mean(digestif, na.rm=T))

names(Consultation_20250106)

df <- df %>% left_join(Consultation_20250106 %>% select(anonyme_id:tr_cognitif))

df <- df %>% left_join(
  Consultation_20250106 %>% filter(Entacapone==1) %>% group_by(anonyme_id) %>% 
  summarise(act_datedeb=min(act_datedeb, na.rm=T)) %>%
  rename(first_entac=act_datedeb))

df <- df %>% mutate(elapsed=as.numeric(act_datedeb-first_entac )) %>%
  select(anonyme_id:group, first_entac , elapsed, fluct_motrice:tr_cognitif)


names(df)



symptoms <- c("fluct_motrice", "dyskinesie", "douleur", "nociceptive", "neuropathique", 
              "dysarthrie", "freezing", "chute_instab", "deform_post", "tr_degl", "chute", 
              "somnolence", "insomnie", "fatigue", "rbd", "sas", "sjsr", "hypotension", 
              "digestif", "urine", "poids", "apathie", "depression", "anxiete", 
              "halluc_psy", "tci", "add_ldopa", "punding", "tr_cognitif")


# Filter once to keep things efficient
df_filtered <- df %>%
  filter(abs(elapsed) <= 4000, group %in% c("cont", "stopped"))

df_filtered <- df_filtered %>% mutate(group=ifelse(group=="cont", "Continuously", "Stopped"))




for (symptom in symptoms) {
   p <- ggplot(df_filtered, aes_string(x = "elapsed", y = symptom, color = "group", fill = "group")) +
     geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
     labs(title = paste("Trajectory of '", symptom, "'"),
          x = "\n Days since Entacapone initiation",
          y = "Score \n",
          color = "Group", fill = "Group") +
     theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
     scale_fill_manual(values=c( "#043A4F", "#6B0B0B")) +
    scale_colour_manual(values=c( "#043A4F", "#6B0B0B")) 

   
   print(p)
   
   ggsave(paste0("../out/", symptom, ".svg"), plot = p, width = 5, height = 4, device = "svg")

}



# Align Patients at Discontinuation ("Stop" Anchoring)


df <- Consultation_20250106 %>% select(anonyme_id, act_datedeb, Entacapone) %>%
  arrange(anonyme_id, act_datedeb, Entacapone) %>%
  inner_join(Consultation_20250106 %>% filter(Entacapone==1) %>% select(anonyme_id) %>% distinct())

length(unique(df$anonyme_id)) # 3565


df <- df %>% arrange(anonyme_id, act_datedeb, Entacapone) %>%
  group_by(anonyme_id, act_datedeb) %>% summarise(Entacapone=max(Entacapone)) %>% ungroup()


pats_stopped <- df %>% group_by(anonyme_id) %>% filter(Entacapone==0&lag(Entacapone)==1) %>% select(anonyme_id) %>% distinct()


df <- df %>% inner_join(pats_stopped)


df <- df %>% left_join(
  df %>% group_by(anonyme_id) %>% 
  filter(Entacapone==0&lag(Entacapone)==1)  %>%
  filter(act_datedeb==min(act_datedeb)) %>% select(-Entacapone) %>%
  distinct() %>%
  rename(first_stop=act_datedeb)
  ) %>%
  mutate(elapsed=as.numeric(act_datedeb-first_stop)) %>%
  filter(abs(elapsed)<=730)


df <- df %>%
  left_join(Consultation_20250106 %>% select(anonyme_id, act_datedeb, fluct_motrice:tr_cognitif))



symptoms <- c("fluct_motrice", "dyskinesie", "douleur", "nociceptive", "neuropathique", 
              "dysarthrie", "freezing", "chute_instab", "deform_post", "tr_degl", "chute", 
              "somnolence", "insomnie", "fatigue", "rbd", "sas", "sjsr", "hypotension", 
              "digestif", "urine", "poids", "apathie", "depression", "anxiete", 
              "halluc_psy", "tci", "add_ldopa", "punding", "tr_cognitif")



df

for (symptom in symptoms) {
   p <- ggplot(df, aes_string(x = "elapsed", y = symptom)) +
     geom_smooth(method = "loess", se = TRUE, alpha = 0.3, colour="#6B0B0B", fill="#043A4F") +
     labs(title = paste("Trajectory of '", symptom, "'"),
          x = "\n Days since Entacapone STOP",
          y = "Score \n") +
     theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text( angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

   
   print(p)
   
  ggsave(paste0("../out/", symptom, ".svg"), plot = p, width = 5, height = 4, device = "svg")

}



symptoms <- c("fluct_motrice", "dyskinesie", "douleur", "nociceptive", "neuropathique", 
              "dysarthrie", "freezing", "chute_instab", "deform_post", "tr_degl", "chute", 
              "somnolence", "insomnie", "fatigue", "rbd", "sas", "sjsr", "hypotension", 
              "digestif", "urine", "poids", "apathie", "depression", "anxiete", 
              "halluc_psy", "tci", "add_ldopa", "punding", "tr_cognitif")

library(lme4)
library(broom.mixed)

results <- purrr::map_dfr(symptoms, function(symptom) {
  fml <- as.formula(paste0(symptom, " ~ Entacapone + (1 | anonyme_id)"))
  fit <- try(lme4::lmer(fml, data = df, na.action = na.omit), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  tidy(fit, effects = "fixed") %>%
    filter(term == "Entacapone") %>%
    mutate(symptom = symptom)
})


data.frame(results)


df %>% group_by(Entacapone) %>% summarise(mean=mean(chute_instab, na.rm=T))



results <- map_dfr(symptoms, function(symptom) {
  formula <- as.formula(paste(symptom, "~ Entacapone + (1 | anonyme_id)"))
  fit <- lmer(formula, data = df, na.action = na.omit)
  coef <- summary(fit)$coefficients
  data.frame(
    symptom = symptom,
    estimate = coef["Entacapone", "Estimate"],
    std_error = coef["Entacapone", "Std. Error"],
    p_value = coef["Entacapone", "Pr(>|t|)"]
  )
})


# Optionally, add stars
results <- results %>%
  mutate(
    signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

print(results)




symptoms <- results$symptom  

means_sds <- df %>%
  select(Entacapone, all_of(symptoms)) %>%
  pivot_longer(-Entacapone, names_to = "symptom", values_to = "value") %>%
  drop_na() %>%
  group_by(symptom, Entacapone) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    .groups = "drop"
  ) %>%
  mutate(
    mean_sd = sprintf("%.2f ± %.2f", mean, sd)
  ) 

results %>% 
  left_join(means_sds %>% filter(Entacapone==0) %>% select(symptom, mean_sd) %>% rename("NO Entacapone"="mean_sd")) %>%
  left_join(means_sds %>% filter(Entacapone==1) %>% select(symptom, mean_sd) %>% rename("ON Entacapone"="mean_sd")) 








# Case-Crossover Design


df <- Consultation_20250106 %>% select(anonyme_id, act_datedeb, Entacapone) %>%
  arrange(anonyme_id, act_datedeb, Entacapone) %>%
  inner_join(Consultation_20250106 %>% filter(Entacapone==1) %>% select(anonyme_id) %>% distinct())

length(unique(df$anonyme_id)) # 3565


df <- df %>% arrange(anonyme_id, act_datedeb, Entacapone) %>%
  group_by(anonyme_id, act_datedeb) %>% summarise(Entacapone=max(Entacapone)) %>% ungroup()


pats_stopped <- df %>% group_by(anonyme_id) %>% filter(Entacapone==0&lag(Entacapone)==1) %>% select(anonyme_id) %>% distinct()


df <- df %>% left_join(pats_stopped %>% mutate(group="stopped")) %>% mutate(group=ifelse(is.na(group),"cont", group ))

df <- df %>% inner_join(pats_stopped)


df <- df %>% left_join(
  df %>% group_by(anonyme_id) %>% 
  filter(Entacapone==0&lag(Entacapone)==1)  %>%
  filter(act_datedeb==min(act_datedeb)) %>% select(-Entacapone) %>%
  distinct() %>%
  rename(first_stop=act_datedeb)
  ) %>%
  mutate(elapsed=as.numeric(act_datedeb-first_stop)) %>%
  filter(abs(elapsed)<=5000)




df_casecross <- df %>%
  left_join(Consultation_20250106 %>% select(anonyme_id, act_datedeb, fluct_motrice:tr_cognitif)) %>%
  filter(!is.na(first_stop)) %>%
  mutate(window = case_when(
    elapsed >= -120 & elapsed < -60 ~ "baseline",
    elapsed >= -60 & elapsed <= 60 ~ "hazard",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(window)) %>%
  group_by(anonyme_id, window) %>%
  summarise(across(c(fluct_motrice:tr_cognitif), mean, na.rm = TRUE), .groups = "drop")

df_wide <- df_casecross %>%
  pivot_wider(names_from = window, values_from = c(fluct_motrice:tr_cognitif), names_sep = "_")




results <- purrr::map_dfr(names(df_wide)[grepl("_baseline$", names(df_wide))], function(baseline_col) {
  symptom <- gsub("_baseline", "", baseline_col)
  hazard_col <- paste0(symptom, "_hazard")
  data <- df_wide %>% select(all_of(baseline_col), all_of(hazard_col)) %>% drop_na()

  if (nrow(data) >= 10) {
    test <- wilcox.test(data[[hazard_col]], data[[baseline_col]], paired = TRUE)
    tibble(
      symptom = symptom,
      n = nrow(data),
      p_value = test$p.value,
      median_diff = median(data[[hazard_col]] - data[[baseline_col]], na.rm = TRUE)
    )
  } else {
    tibble(symptom = symptom, n = nrow(data), p_value = NA, median_diff = NA)
  }
})


# ------------