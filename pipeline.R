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
  read_excel("Epic Signal PEP Data.xlsx", sheet = "Time"))

# Initial exploration
str(raw)
head(raw)
sort(unique(raw$Metric))

# --- Preprocessing ---

# Data cleaning
clean <- raw |>
  
  # rename and recode variables
  mutate(
    
    id = as.character(DE_ID),
    
    type = factor(
      case_when(
        Grouper == "PHYSICIAN + PSYCHIATRIST" ~ "Attending Physician",
        Grouper == "NURSE PRACTITIONER" ~ "Nurse Practitioner",
        Grouper == "RESIDENT + FELLOW" ~ "Resident/Fellow",
        TRUE ~ Grouper
      )),
    
    group = if_else(
      type == "Resident/Fellow",
      "Trainee",
      "Staff"
    ),
    
    # fix typo
    metric = str_replace(Metric, "Recieved", "Received")) |>
  
  select(-DE_ID, -Grouper, -Metric) |>
  
  # remove duplicates
  distinct(id, type, metric, .keep_all = TRUE) |>
  
  # reshape data
  pivot_longer(starts_with(c("24-", "25-")),
               names_to = "month",
               values_to = "value") |>
  pivot_wider(names_from = metric, values_from = value) |>
  
  # clean names
  clean_names() |>
  
  # sort, rename and derive variables
  transmute(
    
    # identifiers
    id, type, group,
    
    # time periods
    month = ym(month),
    obs_days = count_of_days_in_reporting_period,
    obs_weeks = obs_days / 7,
    
    # scheduled work
    apt_n = count_of_appointments,
    sch_days = count_of_scheduled_days,
    sch_hpd = scheduled_hours_per_day,
    sch_hrs = sch_days * sch_hpd,

    # after-hours
    ah_ost = count_of_minutes_active_outside_scheduled_time_30_min_buffer / 60,
    ah_oud = count_of_minutes_active_on_unscheduled_days / 60,
    ah_7t7 = count_of_minutes_active_outside_7am_to_7pm / 60,
    ah_sat = count_of_saturday_minutes / 60,
    ah_sun = count_of_sunday_minutes / 60,
    
    # system usage
    sys_days = count_of_days_in_system,
    sys_days_wah = count_of_days_of_afterhours_activity,
    sys_days_wbh = sys_days - sys_days_wah,
    sys_hrs = count_of_minutes_in_the_system / 60,
    sys_aft_hrs = ah_ost + ah_7t7 + ah_oud + ah_sat + ah_sun,
    sys_ib_hrs = count_of_in_basket_minutes / 60,
    
    # received messages (msg)
    msg_mar = count_of_patient_medical_advice_requests_messages_received,
    msg_pcm = count_of_patient_call_messages_received,
    msg_res = count_of_result_messages_received,
    msg_rxa = count_of_rx_auth_messages_received,
    msg_all = msg_mar + msg_pcm + msg_res + msg_rxa,
    
    # incomplete (inc) messages
    msg_mar_inc = count_of_patient_medical_advice_requests_messages_incomplete,
    msg_pcm_inc = count_of_patient_call_messages_incomplete,
    msg_res_inc = count_of_result_messages_incomplete,
    msg_all_inc = msg_mar_inc + msg_pcm_inc + msg_res_inc,
    
    # days to completion (dtc)
    msg_mar_dtc =
      average_days_until_patient_medical_advice_request_message_marked_done,
    msg_pcm_dtc = average_days_until_patient_call_messages_marked_done,
    msg_res_dtc = average_days_until_result_message_marked_done,
    msg_rxa_dtc = average_days_until_rx_auth_message_marked_done,
    msg_all_dtc = (msg_mar_dtc + msg_pcm_dtc + msg_res_dtc + msg_rxa_dtc) / 4,
    
    # completion rates
    msg_mar_crt = 1 - (msg_mar_inc / msg_mar),
    msg_pcm_crt = 1 - (msg_pcm_inc / msg_pcm),
    msg_res_crt = 1 - (msg_res_inc / msg_res),
    msg_all_crt = 1 - (msg_all_inc / (msg_mar + msg_pcm + msg_res)),
    
    # minutes per appointment (mpa)
    mpa_cr = minutes_in_clinical_review_per_appointment,
    mpa_nl = minutes_in_notes_letters_per_appointment,
    mpa_od = minutes_in_orders_per_appointment,
    mpa_ib = minutes_in_in_basket_per_appointment,
    mpa_all = mpa_cr + mpa_nl + mpa_od + mpa_ib,
    
    # minutes per day (mpd)
    mpd_cr = minutes_in_clinical_review_per_day,
    mpd_ib = minutes_in_in_basket_per_day,
    mpd_ot = minutes_in_other_per_day,
    mpd_all = mpd_cr + mpd_ib + mpd_ot,
    
    # per-schedule hour (psh) rates
    psh_sys_hrs = sys_hrs / sch_hrs,
    psh_sys_aft_hrs = sys_aft_hrs / sch_hrs,
    psh_sys_ib_hrs = sys_ib_hrs / sch_hrs,
    psh_msg_mar = msg_mar / sch_hrs,
    psh_msg_pcm = msg_pcm / sch_hrs,
    psh_msg_res = msg_res / sch_hrs,
    psh_msg_rxa = msg_rxa / sch_hrs,
    psh_msg_all = msg_all / sch_hrs,
    
  ) |>
  
  # remove NA rows
  filter(!is.na(sch_days))

# --- Derive intermediate datasets ---

# Monthly totals
monthly <- clean |>
  group_by(month) |>
  summarise(
    prv_n = n_distinct(id),
    obs_days = max(obs_days, na.rm = TRUE),
    bsn_days = max(sch_days, na.rm = TRUE),
    bsn_hrs = max(sch_hrs, na.rm = TRUE))

# Effort calculation
effort <- clean |>
  group_by(id) |>
  summarise(
    fte = sch_hrs / ,
  )

# Merge into full dataset
full <- clean |>
  left_join(
    monthly |> select(month, prv_n, bsn_days),
    by = "month") |>
  left_join(
    fte,
    by = "id")

# --- Tables and Figures ---

# Sub-tables
prv <- full |>
  group_by(type) |>
  summarise(
    sum = n_distinct(id),
    pct = sum / n_distinct(full$id) * 100) |>
  ungroup() |>
  bind_rows(
    full |> 
  summarise(
    type = "All",
    sum = n_distinct(id),
    pct = sum / n_distinct(full$id) * 100,
    min = min(prv_n, na.rm = TRUE),
    max = max(prv_n, na.rm = TRUE),
    mean = mean(prv_n, na.rm = TRUE),
    sd = sd(prv_n, na.rm = TRUE),
    median = median(prv_n, na.rm = TRUE),
    iqr = IQR(prv_n, na.rm = TRUE),
    cv = sd / mean * 100)) |>
  rename(metric = type)

days <- monthly |>
  summarise(
    across(
      c(bsn_days, obs_days),
      list(
        sum = ~ sum(.x, na.rm = TRUE),
        pct = ~ sum(.x, na.rm = TRUE) / sum(obs_days, na.rm = TRUE) * 100,
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        iqr = ~ IQR(.x, na.rm = TRUE),
        cv = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{col}-{fn}")) |>
  pivot_longer(everything(),
               names_to = c("metric","stat"),
               names_sep = "-",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

sch <- full |> 
  summarise(
    across(
      c(sch_hrs, apt_n),
      list(
        sum = ~ sum(.x, na.rm = TRUE),
        pct = ~ NA,
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        iqr = ~ IQR(.x, na.rm = TRUE),
        cv = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{col}-{fn}")) |>
  pivot_longer(everything(),
               names_to = c("metric","stat"),
               names_sep = "-",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

sys <- full |> 
  summarise(
    across(
      c(sys_days_wah, sys_days_wbh, sys_days),
      list(
        sum = ~ sum(.x, na.rm = TRUE),
        pct = ~ sum(.x, na.rm = TRUE) / sum(full$sys_days, na.rm = TRUE) * 100,
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        iqr = ~ IQR(.x, na.rm = TRUE),
        cv = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{col}-{fn}")) |>
  pivot_longer(everything(),
               names_to = c("metric","stat"),
               names_sep = "-",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

hrs <- full |> 
  summarise(
    across(
      c(sys_aft_hrs, sys_ib_hrs, sys_hrs),
      list(
        sum = ~ sum(.x, na.rm = TRUE),
        pct = ~ sum(.x, na.rm = TRUE) / sum(full$sys_hrs, na.rm = TRUE) * 100,
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        iqr = ~ IQR(.x, na.rm = TRUE),
        cv = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{col}-{fn}")) |>
  pivot_longer(everything(),
               names_to = c("metric","stat"),
               names_sep = "-",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

mpa <- full |>
  summarise(
    across(
      c(mpa_cr, mpa_nl, mpa_od, mpa_ib, mpa_all),
      list(
        sum = ~ sum(.x, na.rm = TRUE),
        pct = ~ sum(.x, na.rm = TRUE) / sum(full$mpa_all, na.rm = TRUE) * 100,
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        iqr = ~ IQR(.x, na.rm = TRUE),
        cv = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{col}-{fn}")) |>
  pivot_longer(everything(),
               names_to = c("metric","stat"),
               names_sep = "-",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

mpd <- full |>
  summarise(
    across(
      c(mpd_cr, mpd_ib, mpd_ot, mpd_all),
      list(
        sum = ~ sum(.x, na.rm = TRUE),
        pct = ~ sum(.x, na.rm = TRUE) / sum(full$mpd_all, na.rm = TRUE) * 100,
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        iqr = ~ IQR(.x, na.rm = TRUE),
        cv = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{col}-{fn}")) |>
  pivot_longer(everything(),
               names_to = c("metric","stat"),
               names_sep = "-",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

msg <- full |>
  summarise(
    across(
      c(msg_mar, msg_pcm, msg_res, msg_rxa, msg_all),
      list(
        sum = ~ sum(.x, na.rm = TRUE),
        pct = ~ sum(.x, na.rm = TRUE) / sum(full$msg_all, na.rm = TRUE) * 100,
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        iqr = ~ IQR(.x, na.rm = TRUE),
        cv = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{col}-{fn}")) |>
  pivot_longer(everything(),
               names_to = c("metric","stat"),
               names_sep = "-",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

inc <- full |> 
  summarise(
    across(
      c(msg_mar_inc, msg_pcm_inc, msg_res_inc, msg_all_inc),
      list(
        sum = ~ sum(.x, na.rm = TRUE),
        pct = ~ sum(.x, na.rm = TRUE) / sum(full$msg_all_inc,
                                            na.rm = TRUE) * 100,
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        iqr = ~ IQR(.x, na.rm = TRUE),
        cv = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{col}-{fn}")) |>
  pivot_longer(everything(),
               names_to = c("metric","stat"),
               names_sep = "-",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

dtc <- full |> 
  summarise(
    across(
      c(msg_mar_dtc, msg_pcm_dtc, msg_res_dtc, msg_rxa_dtc, msg_all_dtc),
      list(
        sum = ~ NA,
        pct = ~ NA,
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        iqr = ~ IQR(.x, na.rm = TRUE),
        cv = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100),
      .names = "{col}-{fn}")) |>
  pivot_longer(everything(),
               names_to = c("metric","stat"),
               names_sep = "-",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

tbl1 <- rbind(prv, days, sch, sys, hrs, mpa, mpd, msg, inc, dtc) |>
  rowwise() |>
  mutate(
    across(where(is.integer), ~ round(.x, 0)),
    across(where(is.numeric) & ends_with("pct"), ~ round(.x, 1)),
    across(where(is.numeric) & !ends_with("pct"), ~ round(.x, 2))) |>
  transmute(
    Metric = metric,
    `n (%)` = coalesce(
      paste0(sum, " (", pct, ")"),
      as.character(sum),
      as.character(pct),
      "—"
    ),
    `range (CV)` = coalesce(
      paste0(min, " - ", max, " (", cv, ")"),
      paste0("Min: ", min, " (", cv, ")"),
      paste0("Max: ", max, " (", cv, ")"),
      "—"
    ),
    `mean (SD)` = coalesce(
      paste0(mean, " (", sd, ")"),
      paste0("Mean: ", mean, " (", sd, ")"),
      "—"
    ),
    `median (IQR)` = coalesce(
      paste0(median, " (", iqr, ")"),
      paste0("Median: ", median, " (", iqr, ")"),
      "—"
    )
  ) |>
  mutate(
    across(everything(), ~ str_replace_all(.x, "NA - NA \\(NA\\)", "—")),
    across(everything(), ~ str_replace_all(.x, "NA \\(NA\\)", "—")),
    across(everything(), ~ str_replace_all(.x, "\\(NA\\)", "(—)"))
  )

# Figure 1
fig1_df <- full |> group_by(month) |>
  summarise(
    sch_hrs = sum(sch_hrs, na.rm = TRUE),
    apt_n = sum(apt_n, na.rm = TRUE),
    sys_hrs = sum(sys_hrs, na.rm = TRUE),
    sys_aft_hrs = sum(sys_aft_hrs, na.rm = TRUE),
    sys_ib_hrs = sum(sys_ib_hrs, na.rm = TRUE),
    msg_mar = sum(msg_mar, na.rm = TRUE),
    msg_pcm = sum(msg_pcm, na.rm = TRUE),
    msg_res = sum(msg_res, na.rm = TRUE),
    msg_rxa = sum(msg_rxa, na.rm = TRUE),
    msg_all = sum(msg_all, na.rm = TRUE),
    msg_mar_inc = sum(msg_mar_inc, na.rm = TRUE),
    msg_pcm_inc = sum(msg_pcm_inc, na.rm = TRUE),
    msg_res_inc = sum(msg_res_inc, na.rm = TRUE),
    msg_all_inc = sum(msg_all_inc, na.rm = TRUE),
    prov_n = n_distinct(id),
    bsn_days = max(sch_days, na.rm = TRUE)
  )

fig1_long <- fig1_df |>
  transmute(
    month,
    `Active Providers` = prov_n / max(prov_n, na.rm = TRUE) * 100,
    `Business Days`    = bsn_days / max(bsn_days, na.rm = TRUE) * 100,
    `Scheduled Hours`  = sch_hrs / max(sch_hrs, na.rm = TRUE) * 100
  ) |>
  pivot_longer(-month, names_to = "metric", values_to = "pct")


# Sub-figures
# Clustered barchart per month of Active Providers, Business days and Scheduled Hours (one bar per variable, expressed as a percentage of max value)

fig1.0 <- ggplot(fig1_long,
  aes(x = month, y = pct, fill = metric)) +
  geom_col(position = position_dodge(width = 25), alpha = 0.5) +
  scale_x_date(
    breaks = c(min(fig1_long$month), max(fig1_long$month)),
    minor_breaks = seq(min(fig1_long$month), max(fig1_long$month), by = "1 month"),
    date_labels = "%b %Y",
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    name = "",
    labels = NULL,
    minor_breaks = seq(0, 100, by = 5)
  ) +
  labs(
    fill = "",
  ) +
  theme_minimal()

fig1.1 <- ggplot(fig1_df,
  aes(x = month)) +
  geom_line(aes(y = cumsum(sch_hrs), color = "Scheduled Hours")) +
  geom_line(aes(y = cumsum(apt_n), color = "Appointments")) +
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
    title = "Scheduled Work",
    x = "Month",
    color = "Cummulative Counts",
  ) +
  theme_minimal()

fig1.2 <- ggplot(fig1_df,
  aes(x = month)) +
  geom_line(aes(y = cumsum(sys_hrs), color = "System Hours")) +
  geom_line(aes(y = cumsum(sys_aft_hrs), color = "After-Hours")) +
  geom_line(aes(y = cumsum(sys_ib_hrs), color = "In-Basket Hours")) +
  scale_x_date(
    breaks = c(min(full$month), max(full$month)),
    minor_breaks = seq(min(full$month), max(full$month), by = "1 month"),
    date_labels = "%b %Y",
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    name = NULL
  ) +
  labs(
    title = "System Time",
    x = "Month",
    color = "Cummulative Counts"
  ) +
  theme_minimal()

fig1.3 <- ggplot(fig1_df,
  aes(x = month)) +
  geom_line(aes(y = cumsum(msg_mar), color = "Medical Advice Request")) +
  geom_line(aes(y = cumsum(msg_pcm), color = "Patient Call")) +
  geom_line(aes(y = cumsum(msg_res), color = "Results")) +
  geom_line(aes(y = cumsum(msg_rxa), color = "Rx Authorization")) +
  geom_line(aes(y = cumsum(msg_all), color = "All Messages")) +
  scale_x_date(
    breaks = c(min(full$month), max(full$month)),
    minor_breaks = seq(min(full$month), max(full$month), by = "1 month"),
    date_labels = "%b %Y",
    expand = c(0.01, 0)
  ) +
    scale_y_continuous(
      name = NULL,
      minor_breaks = seq(by = 1000)
    ) +
    labs(
      title = "Received Messages",
      x = "Month",
      color = "Cummulative Counts"
    ) +
    theme_minimal()

fig1.4 <- ggplot(fig1_df,
  aes(x = month)) +
  geom_line(aes(y = cumsum(msg_mar_inc), color = "Medical Advice Request")) +
  geom_line(aes(y = cumsum(msg_pcm_inc), color = "Patient Call")) +
  geom_line(aes(y = cumsum(msg_res_inc), color = "Results")) +
  geom_line(aes(y = cumsum(msg_all_inc), color = "All Messages")) +
  scale_x_date(
    breaks = c(min(full$month), max(full$month)),
    minor_breaks = seq(min(full$month), max(full$month), by = "1 month"),
    date_labels = "%b %Y",
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    name = NULL,
    minor_breaks = seq(by = 100)
  ) +
  labs(
    title = "Incomplete Messages",
    x = "Month",
    color = "Cummulative Counts"
  ) +
  theme_minimal()


# Fig 1
fig1 <- (fig1.1 | fig1.2) / (fig1.3 | fig1.4) +
  plot_annotation(
    title = "Monthly Overview",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  )

# Fig 2
fig2 <- ggplot(
  full |>
    group_by(id) |>
    summarise(
      type = first(type),
      total_sys_ib_hrs = sum(sys_ib_hrs, na.rm = TRUE),
      total_sch_days = sum(sch_days, na.rm = TRUE),
      sys_ib_hrs_per_sch_week = total_sys_ib_hrs / total_sch_days * 5
    ) |>
    arrange(sys_ib_hrs_per_sch_week) |>
    mutate(rank = row_number()),
  aes(x = rank, y = sys_ib_hrs_per_sch_week, fill = type)
) +
  geom_bar(stat = "identity") +
  geom_hline(
    yintercept = median(
      full |>
        group_by(id) |> summarise(
          total_sys_ib_hrs = sum(sys_ib_hrs, na.rm = TRUE),
          total_sch_days = sum(sch_days, na.rm = TRUE),
          sys_ib_hrs_per_sch_week = total_sys_ib_hrs / total_sch_days * 5) |>
        pull(sys_ib_hrs_per_sch_week), na.rm = TRUE),
    linetype = "dashed", color = "red") +
  labs(
    title = "In-Basket Hours per Week by Provider",
    x = "Providers (Ranked)",
    y = "In-Basket Hours per Week",
    fill = "Provider Type"
  ) +
  theme_minimal()


# Fig 3
lorenz_data <- full |> group_by(sys_ib_hrs) |>
  summarise(sys_ib_hrs = sum(sys_ib_hrs, na.rm = TRUE)) |>
  arrange(sys_ib_hrs) |>
  mutate(
    cum_prov_pct = cumsum(rep(1, n())) / n(),
    cum_hours_pct = cumsum(sys_ib_hrs) / sum(sys_ib_hrs, na.rm = TRUE)
  )

gini_coeff <- Gini(full |> group_by(id) |>
                     summarise(sys_ib_hrs = sum(sys_ib_hrs)) |>
                     pull(sys_ib_hrs), na.rm = TRUE)

fig3 <- ggplot(lorenz_data, aes(x = cum_prov_pct, y = cum_hours_pct)) +
  geom_ribbon(
    aes(ymin = cum_hours_pct, ymax = cum_prov_pct),
    fill = "#F8766D", alpha = 0.3
  ) +
  geom_line(size = 1.2, color = "#F8766D") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "#00BA38", size = 1) +
  annotate(
    "text",
    x = 0.5, y = 0.175,
    label = paste0("g = ", sprintf("%.2f", gini_coeff)),
    hjust = 0, vjust = 0,
    size = 4, color = "#F8766D", fontface = "bold"
  ) +
  labs(
    title = "Lorenz Curve of In-Basket Hours",
    subtitle = "g = Gini Coefficient",
    x = "Providers (Ranked)",
    y = "In-Basket Hours"
  ) +
  scale_x_continuous(labels = percent, limits = c(0, 1),
                     expand = expansion(mult = c(0.01, 0.06))) +
  scale_y_continuous(labels = percent, limits = c(0, 1),
                     expand = expansion(mult = c(0.01, 0.06))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "gray30",
                                 margin = margin(b = 8)),
    aspect.ratio = 1,
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

# Fig 4
fig4 <- ggplot(
  full |> filter(sys_ib_hrs <= 40 | is.na(sys_ib_hrs)),
  aes(x = sys_ib_hrs, y = sys_aft_hrs, color = type)
) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Monthly In-Basket Hours vs After-Hours",
    x = "In-Basket Hours",
    y = "After-Hours",
    color = "Provider Type"
  ) +
  theme_minimal()

# Fig 5
# cor_matrix for all numeric variables
cor_matrix <- cor(full |> select(where(is.numeric)) |> drop_na(),
                  use = "pairwise.complete.obs")

cor_melt <- as.data.frame(as.table(cor_matrix))
fig5 <- ggplot(cor_melt, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limit = c(-1,1), name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  coord_fixed() +
  labs(title = "Correlation Matrix Heatmap")

# Fig 6: Boxplots by FTE
fig6 <- ggplot(
  full,
  aes(x = fte_4tile, y = rt_ib_hr_per_sch_hr, fill = fte_4tile)
) +
  geom_boxplot() +
  labs(
    title = "In-Basket Hours by FTE",
    x = "FTE",
    y = "In-Basket Hours",
    fill = "FTE"
  ) +
  theme_minimal()

# --- Save outputs ---

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
tbl1 |> write_csv("outputs/table1.csv")
ggsave("outputs/fig1.png", fig1, width = 10, height = 7, dpi = 300, scale = 1.5)
ggsave("outputs/fig2.png", fig2, width = 10, height = 7, dpi = 300, scale = 1.5)
ggsave("outputs/fig3.png", fig3, width = 10, height = 7, dpi = 300, scale = 1.5)
ggsave("outputs/fig4.png", fig4, width = 10, height = 7, dpi = 300, scale = 1.5)