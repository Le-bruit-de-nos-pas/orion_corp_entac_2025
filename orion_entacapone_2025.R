
# Paulo BASTOS, Orion Corporation

# All rights reserved, May 2025

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

eligible_ids <- visit_counts$anonyme_id[visit_counts$n >= 5]

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

# z test of coefficients:
# 
#                Estimate  Std. Error z value  Pr(>|z|)    
# (Intercept) -3.0228e+00  5.3731e-01 -5.6259 1.845e-08 ***
# act_datedeb  8.5855e-05  2.9535e-05  2.9069   0.00365 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# z test of coefficients:
# 
#                Estimate  Std. Error z value Pr(>|z|)    
# (Intercept) -3.1340e+00  8.9452e-01 -3.5036 0.000459 ***
# act_datedeb  1.0536e-04  4.8675e-05  2.1646 0.030419 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


cochran_model <- DescTools::CochranArmitageTest(table(trend_data$act_datedeb, trend_data$Entacapone))

# 	Cochran-Armitage test for trend
# 
# data:  table(trend_data$act_datedeb, trend_data$Entacapone)
# Z = -5.2822, dim = 2271, p-value = 1.277e-07
# alternative hypothesis: two.sided
# 

# 	Cochran-Armitage test for trend
# 
# data:  table(trend_data$act_datedeb, trend_data$Entacapone)
# Z = -4.3836, dim = 2029, p-value = 1.167e-05
# alternative hypothesis: two.sided


model <- lme4::glmer(Entacapone ~ act_datedeb + (1 | anonyme_id), 
               data = trend_data, 
               family = binomial)

summary(model)


# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#  Family: binomial  ( logit )
# Formula: Entacapone ~ act_datedeb + (1 | anonyme_id)
#    Data: trend_data
# 
#      AIC      BIC   logLik deviance df.resid 
#  12400.8  12425.9  -6197.4  12394.8    32270 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -7.8220 -0.0083 -0.0061 -0.0048  4.8226 
# 
# Random effects:
#  Groups     Name        Variance Std.Dev.
#  anonyme_id (Intercept) 161.3    12.7    
# Number of obs: 32273, groups:  anonyme_id, 4427
# 
# Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -2.436e+01  5.411e-01  -45.02   <2e-16 ***
# act_datedeb  8.005e-04  2.803e-05   28.56   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# act_datedeb -0.949
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# Model failed to converge with max|grad| = 14.207 (tol = 0.002, component 1)
# Model is nearly unidentifiable: very large eigenvalue
#  - Rescale variables?
# Model is nearly unidentifiable: large eigenvalue ratio
#  - Rescale variables?



# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#  Family: binomial  ( logit )
# Formula: Entacapone ~ act_datedeb + (1 | anonyme_id)
#    Data: trend_data
# 
#      AIC      BIC   logLik deviance df.resid 
#   4823.3   4845.3  -2408.7   4817.3    11025 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -7.2298 -0.0114 -0.0081 -0.0059  4.6911 
# 
# Random effects:
#  Groups     Name        Variance Std.Dev.
#  anonyme_id (Intercept) 118.3    10.88   
# Number of obs: 11028, groups:  anonyme_id, 906
# 
# Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -2.247e+01  4.725e-01  -47.55   <2e-16 ***
# act_datedeb  7.273e-04  2.402e-05   30.28   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# act_datedeb -0.711
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# Model failed to converge with max|grad| = 1.51291 (tol = 0.002, component 1)
# Model is nearly unidentifiable: very large eigenvalue
#  - Rescale variables?
# Model is nearly unidentifiable: large eigenvalue ratio
#  - Rescale variables?

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

