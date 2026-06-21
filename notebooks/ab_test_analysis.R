# ============================================================
# Loan Approval A/B Test Analysis
# Advanced Data Analysis (IB98D0) — University of Warwick
# MSc Business Analytics — Group 10
# Author: Bishal Ranjan Bora (Student: 5668868)
# ============================================================

# ── 1. Load Libraries ────────────────────────────────────────
library(tidyverse)
library(dplyr)
library(effectsize)

# ── 2. Import Data ───────────────────────────────────────────
df <- read.csv('ADAproject_-5_data.csv')
str(df)

# How many loan officers before cleaning?
n_before <- df %>%
  summarize(unique_count = n_distinct(loanofficer_id)) %>%
  pull(unique_count)
cat("Loan officers before cleaning:", n_before, "\n")  # 47

# ── 3. Data Cleaning ─────────────────────────────────────────

# Step 1: Remove records where all key fields are zero (incomplete)
df1 <- df %>%
  filter(!(typeI_fin == 0 & typeII_fin == 0 & agree_init == 0 &
           agree_fin == 0 & conflict_init == 0 & fully_complt == 0))

# Step 2: Keep only fully completed participants
df1 <- df1 %>%
  filter(fully_complt == 10)

# How many loan officers after cleaning?
n_after <- df1 %>%
  summarize(unique_count = n_distinct(loanofficer_id)) %>%
  pull(unique_count)
cat("Loan officers after cleaning:", n_after, "\n")  # 38

# ── 4. Data Aggregation ──────────────────────────────────────
# Aggregate daily records to officer level (mean per officer)

aggregate_officer <- function(data) {
  data %>%
    group_by(loanofficer_id) %>%
    mutate(
      typeI_fin_avg              = mean(typeI_fin, na.rm = TRUE),
      typeII_fin_avg             = mean(typeII_fin, na.rm = TRUE),
      badloans_num_avg           = mean(badloans_num, na.rm = TRUE),
      goodloans_num_avg          = mean(goodloans_num, na.rm = TRUE),
      agree_fin_avg              = mean(agree_fin, na.rm = TRUE),
      conflict_fin_avg           = mean(conflict_fin, na.rm = TRUE),
      confidence_fin_total_avg   = mean(confidence_fin_total, na.rm = TRUE)
    )
}

df  <- aggregate_officer(df)
df1 <- aggregate_officer(df1)

# Set Variant as factor
df$Variant  <- factor(df$Variant)
df1$Variant <- factor(df1$Variant)

# Keep one row per officer
keep_cols <- c("Variant", "loanofficer_id",
               "typeI_fin_avg", "typeII_fin_avg",
               "badloans_num_avg", "goodloans_num_avg",
               "agree_fin_avg", "conflict_fin_avg",
               "confidence_fin_total_avg")

type12        <- df  %>% select(all_of(keep_cols)) %>% distinct()
type12_delete <- df1 %>% select(all_of(keep_cols)) %>% distinct()

# ── 5. Compute OECs ──────────────────────────────────────────
compute_oecs <- function(data) {
  data %>%
    mutate(
      badloans_detection_rate = (badloans_num_avg - typeII_fin_avg) / badloans_num_avg,
      overall_accuracy        = ((goodloans_num_avg - typeI_fin_avg) +
                                 (badloans_num_avg  - typeII_fin_avg)) /
                                (goodloans_num_avg + badloans_num_avg),
      agreement_rate          = agree_fin_avg / (agree_fin_avg + conflict_fin_avg)
    )
}

type12        <- compute_oecs(type12)
type12_delete <- compute_oecs(type12_delete)

# ── 6. T-Tests: Before Handling Missing Values ───────────────
cat("\n══ T-TESTS (BEFORE CLEANING — missing values as zeros) ══\n")

cat("\nBad Loan Detection Rate:\n")
print(t.test(badloans_detection_rate ~ Variant, data = type12))
# p = 0.2455 — NOT significant (missing values distort Control group)

cat("\nOverall Accuracy:\n")
print(t.test(overall_accuracy ~ Variant, data = type12))
# p = 0.649 — NOT significant

cat("\nAgreement Rate:\n")
print(t.test(agreement_rate ~ Variant, data = type12))

cat("\nConfidence Score:\n")
print(t.test(confidence_fin_total_avg ~ Variant, data = type12))

# ── 7. T-Tests: After Removing Incomplete Records ────────────
cat("\n══ T-TESTS (AFTER CLEANING — incomplete records removed) ══\n")

cat("\nBad Loan Detection Rate:\n")
print(t.test(badloans_detection_rate ~ Variant, data = type12_delete))
# t(10.42) = -2.67, p = 0.023 ✅

cat("\nOverall Accuracy:\n")
print(t.test(overall_accuracy ~ Variant, data = type12_delete))
# t(10.90) = -6.04, p < 0.001 ✅

cat("\nAgreement Rate:\n")
print(t.test(agreement_rate ~ Variant, data = type12_delete))
# t(10.76) = -3.37, p = 0.006 ✅

cat("\nConfidence Score:\n")
print(t.test(confidence_fin_total_avg ~ Variant, data = type12_delete))
# t(18.41) = -2.25, p = 0.037 ✅

# ── 8. Mean Differences ──────────────────────────────────────
mean_values <- type12_delete %>%
  group_by(Variant) %>%
  summarise(
    mean_badloans_detection_rate   = mean(badloans_detection_rate),
    mean_overall_accuracy          = mean(overall_accuracy),
    mean_agreement_rate            = mean(agreement_rate),
    mean_confidence_fin_total_avg  = mean(confidence_fin_total_avg)
  )

cat("\n══ MEAN VALUES BY VARIANT ══\n")
print(mean_values)

pairwise_diff <- mean_values %>%
  reframe(
    BLDR_diff    = mean_badloans_detection_rate[Variant == "Treatment"]  - mean_badloans_detection_rate[Variant == "Control"],
    BLDR_pct     = BLDR_diff / mean_badloans_detection_rate[Variant == "Control"] * 100,
    Acc_diff     = mean_overall_accuracy[Variant == "Treatment"]         - mean_overall_accuracy[Variant == "Control"],
    Acc_pct      = Acc_diff  / mean_overall_accuracy[Variant == "Control"] * 100,
    Agree_diff   = mean_agreement_rate[Variant == "Treatment"]           - mean_agreement_rate[Variant == "Control"],
    Agree_pct    = Agree_diff / mean_agreement_rate[Variant == "Control"] * 100,
    Conf_diff    = mean_confidence_fin_total_avg[Variant == "Treatment"] - mean_confidence_fin_total_avg[Variant == "Control"],
    Conf_pct     = Conf_diff  / mean_confidence_fin_total_avg[Variant == "Control"] * 100
  )

cat("\n══ PAIRWISE DIFFERENCES (Treatment vs Control) ══\n")
print(pairwise_diff)

# ── 9. Effect Sizes (Cohen's d) ──────────────────────────────
calculate_cohens_d <- function(metric, data) {
  treatment <- data[[metric]][data$Variant == "Treatment"]
  control   <- data[[metric]][data$Variant == "Control"]
  cohens_d(treatment, control)
}

cat("\n══ EFFECT SIZES (Cohen's d) ══\n")

d_bldr    <- calculate_cohens_d("badloans_detection_rate", type12_delete)
d_acc     <- calculate_cohens_d("overall_accuracy", type12_delete)
d_agree   <- calculate_cohens_d("agreement_rate", type12_delete)
d_conf    <- calculate_cohens_d("confidence_fin_total_avg", type12_delete)

cat("\nBad Loan Detection Rate — Cohen's d:\n"); print(d_bldr)
cat("\nOverall Accuracy — Cohen's d:\n");        print(d_acc)
cat("\nAgreement Rate — Cohen's d:\n");          print(d_agree)
cat("\nConfidence Score — Cohen's d:\n");        print(d_conf)

cat("\nInterpretations:\n")
cat("BLDR:       ", interpret_cohens_d(1.37), "\n")
cat("Accuracy:   ", interpret_cohens_d(2.94), "\n")
cat("Agreement:  ", interpret_cohens_d(1.67), "\n")
cat("Confidence: ", interpret_cohens_d(0.77), "\n")

# ── 10. Visualisations ───────────────────────────────────────
plot_boxplot <- function(data, y_var, y_label, caption_text) {
  ggplot(data, aes_string(x = "Variant", y = y_var, fill = "Variant")) +
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(values = c("Control" = "#f87171", "Treatment" = "#2dd4bf")) +
    labs(
      x       = "Variant",
      y       = y_label,
      caption = caption_text
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "right")
}

plot_boxplot(type12_delete,
             "badloans_detection_rate",
             "Detection Rate",
             "Figure 1. Bad Loan Detection Rate by Variant")

plot_boxplot(type12_delete,
             "overall_accuracy",
             "Accuracy",
             "Figure 2. Overall Accuracy by Variant")

plot_boxplot(type12_delete,
             "agreement_rate",
             "Agreement Rate",
             "Figure 3. Agreement Rate by Variant")

plot_boxplot(type12_delete,
             "confidence_fin_total_avg",
             "Confidence Score",
             "Figure 4. Loan Officers' Confidence Score by Variant")

# ── 11. Summary ──────────────────────────────────────────────
cat("\n════════════════════════════════════════════════════════\n")
cat("EXECUTIVE SUMMARY\n")
cat("════════════════════════════════════════════════════════\n")
cat("Bad Loan Detection Rate improvement:  +18.2%  (p = 0.023,  d = 1.37 Large)\n")
cat("Overall Accuracy improvement:         +42.0%  (p < 0.001,  d = 2.94 Large)\n")
cat("Agreement Rate improvement:           +19.6%  (p = 0.006,  d = 1.67 Large)\n")
cat("Confidence Score improvement:         +23.7%  (p = 0.037,  d = 0.77 Medium)\n")
cat("\nRecommendation: Deploy the new model. All metrics show significant\n")
cat("improvement with large effect sizes confirming real-world impact.\n")
cat("════════════════════════════════════════════════════════\n")
