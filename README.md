# Loan Approval A/B Test Analysis

**Tools:** R · Welch Two-Sample t-Test · Cohen's d · ggplot2 · tidyverse  
**Domain:** Financial Services · Risk Analytics · Experimental Design  
**Context:** Group Assignment — Advanced Data Analysis (IB98D0), University of Warwick MSc Business Analytics  
**Status:**  Complete

---

##  Project Overview

A consumer lending company was experiencing high error rates in loan review decisions, leading to increased bad loan approvals and significant financial losses. The Analytics Department designed an A/B test to evaluate whether a new computer-assisted model could improve loan officers' decision quality.

**Business Question:** Does the new loan review model improve bad loan detection, overall accuracy, agreement rate, and officer confidence compared to the existing model?

**Experimental Design:**
- 47 loan officers initially enrolled; 38 fully completed the experiment
- **Control group:** Continued using the existing decision tool
- **Treatment group:** Used the new model's recommendations
- Analysis conducted at the loan-officer level (daily metrics aggregated per officer)
- Missing/incomplete records excluded to avoid distorting results

---

##  Key Results

| Metric | Control (Legacy) | Treatment (New Model) | Change | p-value | Cohen's d |
|---|---|---|---|---|---|
| Bad Loan Detection Rate | 0.625 | 0.739 | **+18.2%** | < 0.05 | 1.37 (Large) |
| Overall Accuracy | 0.508 | 0.722 | **+42.0%** | < 0.001 | 2.94 (Large) |
| Agreement Rate | 0.737 | 0.881 | **+19.6%** | < 0.05 | 1.67 (Large) |
| Confidence Score | 611.1 | 756.2 | **+23.7%** | < 0.05 | 0.77 (Medium) |

All four metrics showed statistically significant improvement in the Treatment group.

---

##  Repository Structure

```
loan-approval-ab-test/
│
├── notebooks/
│   └── ab_test_analysis.R      # Full R analysis: cleaning, aggregation, t-tests, effect sizes, visualisation
│
└── README.md
```

---

##  Methodology

### 1. Data Cleaning
- Records where all key fields (`typeI_fin`, `typeII_fin`, `agree_init`, etc.) were zero were removed as incomplete
- Only loan officers with `fully_complt == 10` were retained
- Final sample: **38 loan officers** (down from 47)

### 2. Data Aggregation
- Daily records consolidated into a single officer-level record using grouped means
- Prevents overcounting across multiple days and ensures valid officer-level comparisons

### 3. OEC Definition
```r
# Bad Loan Detection Rate
badloans_detection_rate = (badloans_num_avg - typeII_fin_avg) / badloans_num_avg

# Overall Accuracy
overall_accuracy = ((goodloans_num_avg - typeI_fin_avg) + (badloans_num_avg - typeII_fin_avg)) /
                    (goodloans_num_avg + badloans_num_avg)

# Agreement Rate
agreement_rate = agree_fin_avg / (agree_fin_avg + conflict_fin_avg)
```

### 4. Hypothesis Testing
- **Two-stage approach:** t-tests run on raw data first (insignificant due to distorted Control metrics from missing values), then re-run after excluding incomplete records (significant improvements revealed)
- **Test used:** Welch Two-Sample t-test (unequal variances)
- **Significance level:** α = 0.05

### 5. Effect Size
- Cohen's d computed using the `effectsize` package
- Bad loan detection and overall accuracy both show **large** effect sizes, confirming practical as well as statistical significance

---

##  Selected Findings

### Bad Loan Detection Rate
- Treatment mean: **0.739** vs Control mean: **0.625** (+18.2%)
- t(10.42) = -2.67, p = 0.023
- Cohen's d = 1.37 — *Large effect*
- Treatment group also showed **smaller IQR**, indicating more consistent performance

### Overall Accuracy
- Treatment mean: **0.722** vs Control mean: **0.508** (+42.0%)
- t(10.90) = -6.04, p < 0.001
- Cohen's d = 2.94 — *Very large effect*
- Fewer Type I errors (good loans rejected) and fewer Type II errors (bad loans approved)

### Two-Stage T-Test Insight
Running the test on unprocessed data (missing values treated as zeros) produced **no significant differences** — demonstrating the critical importance of proper data cleaning before hypothesis testing.

---

##  Recommendation

The evidence strongly supports **rolling out the new model**. Every metric improved significantly, with large effect sizes confirming real-world impact beyond statistical significance. Additional recommendations:

1. Provide training to address the missing data issue observed in the Control group
2. Extend the experiment to 3–4 weeks with a larger sample for greater confidence
3. Collect demographic data (e.g. officer tenure) to identify subgroup effects
4. Refine confidence and agreement metrics for richer performance measurement

---

##  Tools & Packages

```r
library(tidyverse)   # Data manipulation and visualisation
library(dplyr)       # Data wrangling
library(effectsize)  # Cohen's d calculation
library(ggplot2)     # Boxplot visualisations
```

---

##  Contact

**Bishal Ranjan Bora**  
[LinkedIn](https://linkedin.com/in/bishalbora) | [Email](mailto:bora.bishal@gmail.com) | [GitHub](https://github.com/bishalbora1998)
