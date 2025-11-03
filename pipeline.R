# Load libraries
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)
library(ggplot2)
library(scales)
library(knitr)
library(patchwork)
library(ineq)

# Load raw data
raw <- bind_rows(
  read_excel("Epic Signal PEP Data.xlsx", sheet = "Messages"),
  read_excel("Epic Signal PEP Data.xlsx", sheet = "Time")
)


# Initial exploration
str(raw)
head(raw)
sort(unique(raw$Metric))


# Data cleaning
clean <- raw |>
  mutate(
    
    id = as.character(DE_ID),
    
    type = factor(
      case_when(
        Grouper == "PHYSICIAN + PSYCHIATRIST" ~ "Attending Physician",
        Grouper == "NURSE PRACTITIONER" ~ "Nurse Practitioner",
        Grouper == "RESIDENT + FELLOW" ~ "Resident/Fellow",
        TRUE ~ Grouper
      )),
    
    # fix typo
    metric = str_replace(Metric, "Recieved", "Received")
    
  ) |>
  
  # remove duplicates
  distinct(id, type, metric, .keep_all = TRUE) |>
  
  # select columns
  select(id, type, metric, starts_with(c("24-", "25-")))


# Reshape
reshaped <- clean |>
  pivot_longer(starts_with(c("24-", "25-")),
               names_to = "month",
               values_to = "value") |>
  pivot_wider(names_from = metric, values_from = value) |>
  clean_names()

# Create full dataset with calculated fields
full <- reshaped |>
  transmute(
    
    id, type,
    
    month = ym(month),
    
    obs_days = count_of_days_in_reporting_period,
    
    obs_weeks = obs_days / 7,
    
    appts = count_of_appointments,
    
    sch_days = count_of_scheduled_days,
    
    sch_hpd = scheduled_hours_per_day,
    
    sch_hrs = sch_days * sch_hpd,
    
    sys_days = count_of_days_in_system,
    
    sys_hrs = count_of_minutes_in_the_system / 60,
    
    ah_days = count_of_days_of_afterhours_activity,
    
    ah_ost = count_of_minutes_active_outside_scheduled_time_30_min_buffer / 60,
    
    ah_oud = count_of_minutes_active_on_unscheduled_days / 60,
    
    ah_sat = count_of_saturday_minutes / 60,
    
    ah_sun = count_of_sunday_minutes / 60,
    
    ah_hrs = ah_ost + ah_oud + ah_sat + ah_sun,
    
    ib_hrs = count_of_in_basket_minutes / 60,
    
    msg_mar = count_of_patient_medical_advice_requests_messages_received,
    
    msg_pcm = count_of_patient_call_messages_received,
    
    msg_res = count_of_result_messages_received,
    
    msg_rxa = count_of_rx_auth_messages_received,
    
    msg_all = msg_mar + msg_pcm + msg_res + msg_rxa,
    
    msg_mar_inc = count_of_patient_medical_advice_requests_messages_incomplete,
    
    msg_pcm_inc = count_of_patient_call_messages_incomplete,
    
    msg_res_inc = count_of_result_messages_incomplete,
    
    msg_rxa_inc = count_of_rx_auth_messages_incomplete,
    
    msg_all_inc = msg_mar_inc + msg_pcm_inc + msg_res_inc + msg_rxa_inc,
    
    dtc_mar = average_days_until_patient_medical_advice_request_message_marked_done,
    
    dtc_pcm = average_days_until_patient_call_messages_marked_done,
    
    dtc_res = average_days_until_result_message_marked_done,
    
    dtc_rxa = average_days_until_rx_auth_message_marked_done,
    
    dtc_all = (dtc_mar + dtc_pcm + dtc_res + dtc_rxa) / 4,
    
    mpa_cr = minutes_in_clinical_review_per_appointment,
    
    mpa_nl = minutes_in_notes_letters_per_appointment,
    
    mpa_od = minutes_in_orders_per_appointment,
    
    mpa_ib = minutes_in_in_basket_per_appointment,
    
    mpa_all = mpa_cr + mpa_nl + mpa_od + mpa_ib,
    
    mpd_cr = minutes_in_clinical_review_per_day,
    
    mpd_ib = minutes_in_in_basket_per_day,
    
    mpd_ot = minutes_in_other_per_day,
    
    mpd_all = mpd_cr + mpd_ib + mpd_ot
    
  ) |> 
  
  left_join(full |> group_by(month) |>
              summarise(
                month_providers = n_distinct(id[!is.na(sch_days)]),
                month_bsn_days = max(sch_days, na.rm = TRUE),
                ),
            by = "month")
  
# Table 1: n (%), range, mean (SD), median (IQR) and CV for ib_hrs, sch_hrs and sys_hrs

# generate dynamically
table1_metrics <- c("ib_hrs", "sch_hrs", "sys_hrs")


# Bar plot of ranked ib_hrs per sch_day (bars colored by type)
fig0 <- ggplot(
  full |>
    group_by(id) |>
    summarise(
      type = first(type),
      total_ib_hrs = sum(ib_hrs, na.rm = TRUE),
      total_sch_days = sum(sch_days, na.rm = TRUE),
      ib_hrs_per_sch_week = total_ib_hrs / total_sch_days * 5
    ) |>
    arrange(desc(ib_hrs_per_sch_week)) |>
    mutate(rank = row_number()),
  aes(x = rank, y = ib_hrs_per_sch_week, fill = type)
) +
  geom_bar(stat = "identity") +
  labs(
    title = "Ranked In-Basket Hours per Scheduled Week",
    x = "Provider",
    y = "In-Basket Hours per Scheduled Week",
    fill = "Provider Type"
  ) +
  theme_minimal()

# Fig 1: Cumulative shares over time
fig1 <- ggplot(
  full |> group_by(month) |> summarise(msg_all = sum(msg_all, na.rm = TRUE),
                                       sch_hrs = sum(sch_hrs, na.rm = TRUE),
                                       appts = sum(appts, na.rm = TRUE)),
  aes(x = month)) +
  geom_line(aes(y = cumsum(msg_all), color = "Messages Received")) +
  geom_line(aes(y = cumsum(sch_hrs), color = "Scheduled Hours")) +
  geom_line(aes(y = cumsum(appts), color = "Appointments")) +
  scale_x_date(
    breaks = c(min(full$month), max(full$month)),
    minor_breaks = seq(min(full$month), max(full$month), by = "1 month"),
    date_labels = "%b %Y",
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    name = NULL,
    minor_breaks = seq(0, 300000, by = 1000)
  ) +
  labs(
    title = "Monthly Cummulative Shares",
    x = "Month",
    color = "Metric"
  ) +
  theme_minimal()

# Figure 2: Lorenz Curve of In-Basket Hours
lorenz_data <- full |>
  group_by(ib_hrs) |>
  summarise(ib_hrs = sum(ib_hrs, na.rm = TRUE)) |>
  arrange(ib_hrs) |>
  mutate(
    cum_prov_pct = cumsum(rep(1, n())) / n(),
    cum_hours_pct = cumsum(ib_hrs) / sum(ib_hrs, na.rm = TRUE)
  )

gini_coeff <- Gini(full |> group_by(id) |> summarise(ib_hrs = sum(ib_hrs)) |> pull(ib_hrs), na.rm = TRUE)

fig2 <- ggplot(lorenz_data, aes(x = cum_prov_pct, y = cum_hours_pct)) +
  geom_ribbon(
    aes(ymin = cum_hours_pct, ymax = cum_prov_pct),
    fill = "#990000", alpha = 0.3
  ) +
  geom_line(size = 1.2, color = "#990000") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#011F5B", size = 1) +
  annotate(
    "text",
    x = 0.5, y = 0.175,
    label = paste0("g = ", sprintf("%.2f", gini_coeff)),
    hjust = 0, vjust = 0,
    size = 4, color = "#011F5B", fontface = "bold"
  ) +
  labs(
    title = "Lorenz Curve of In-Basket Hours",
    subtitle = "g = Gini Coefficient",
    x = "Cumulative Providers (Ranked)",
    y = "Cumulative In-Basket Hours"
  ) +
  scale_x_continuous(labels = percent, limits = c(0, 1), expand = expansion(mult = c(0.01, 0.06))) +
  scale_y_continuous(labels = percent, limits = c(0, 1), expand = expansion(mult = c(0.01, 0.06))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "gray30", margin = margin(b = 8)),
    aspect.ratio = 1,
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

# Fig 3: Scatter Plot of In-Basket Hours vs After-Hours Work (dots colored by type, but single line)
fig3 <- ggplot(
  full |> filter(ib_hrs <= 40 | is.na(ib_hrs)),
  aes(x = ib_hrs, y = ah_hrs, color = type)
) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Monthly In-Basket Hours vs After-Hours Work",
    x = "In-Basket Hours",
    y = "After-Hours Work",
    color = "Provider Type"
  ) +
  theme_minimal()



# Save datasets
if (!dir.exists("data")) {
  dir.create("data")
}
raw |> saveRDS("data/raw.rds")
clean |> saveRDS("data/clean.rds")
full |> saveRDS("data/full.rds")

# Save tables and figures
if (!dir.exists("outputs")) {
  dir.create("outputs")
}
ggsave("outputs/fig1.png", fig1, width = 10, height = 7, dpi = 300, scale = 1.5)
ggsave("outputs/fig2.png", fig2, width = 10, height = 7, dpi = 300, scale = 1.5)
ggsave("outputs/fig3.png", fig3, width = 10, height = 7, dpi = 300, scale = 1.5)