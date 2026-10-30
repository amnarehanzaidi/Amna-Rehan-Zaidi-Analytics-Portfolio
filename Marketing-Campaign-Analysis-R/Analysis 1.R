install.packages("readr")
install.packages("janitor")
library(readr)
library(janitor)
marketing_raw <- read_csv("marketing_data.csv")
marketing <- clean_names(marketing_raw)
str(marketing)
missing_counts <- sapply(marketing, function(x) sum(is.na(x)))
missing_summary <- data.frame(
  variable   = names(missing_counts),
  n_missing  = as.integer(missing_counts)
)
missing_summary <- missing_summary[order(-missing_summary$n_missing), ]
print(missing_summary)
# Keep only rows where campaign and product fields we need are not missing
needed_cols <- c("accepted_cmp1", "accepted_cmp2", "accepted_cmp3",
                 "accepted_cmp4", "accepted_cmp5", "response",
                 "mnt_wines", "mnt_fruits", "mnt_meat_products",
                 "mnt_fish_products", "mnt_sweet_products", "mnt_gold_prods")
marketing_simple <- marketing[complete.cases(marketing[ , needed_cols]), ]
# Check how many rows we kept
nrow(marketing)
nrow(marketing_simple)
# Question 1 – Which marketing campaign is most successful?

# Helper function for acceptance rate (0/1 variables)
acceptance_rate <- function(x) {
  mean(x == 1, na.rm = TRUE)
}

# Acceptance rates for each campaign and the last campaign
rate_cmp1 <- acceptance_rate(marketing_simple$accepted_cmp1)
rate_cmp2 <- acceptance_rate(marketing_simple$accepted_cmp2)
rate_cmp3 <- acceptance_rate(marketing_simple$accepted_cmp3)
rate_cmp4 <- acceptance_rate(marketing_simple$accepted_cmp4)
rate_cmp5 <- acceptance_rate(marketing_simple$accepted_cmp5)
rate_last <- acceptance_rate(marketing_simple$response)

campaign_names <- c("Campaign 1", "Campaign 2", "Campaign 3",
                    "Campaign 4", "Campaign 5", "Last Campaign")

campaign_rates <- c(rate_cmp1, rate_cmp2, rate_cmp3,
                    rate_cmp4, rate_cmp5, rate_last)

campaign_results <- data.frame(
  campaign        = campaign_names,
  acceptance_rate = campaign_rates
)
# Add percentage version
campaign_results$acceptance_pct <- round(campaign_results$acceptance_rate * 100, 2)
print(campaign_results)

# Question 2 – Which products are performing best?

# Total spend by product category (over all customers)
total_wines   <- sum(marketing_simple$mnt_wines,          na.rm = TRUE)
total_fruits  <- sum(marketing_simple$mnt_fruits,         na.rm = TRUE)
total_meat    <- sum(marketing_simple$mnt_meat_products,  na.rm = TRUE)
total_fish    <- sum(marketing_simple$mnt_fish_products,  na.rm = TRUE)
total_sweets  <- sum(marketing_simple$mnt_sweet_products, na.rm = TRUE)
total_gold    <- sum(marketing_simple$mnt_gold_prods,     na.rm = TRUE)

product_names <- c("Wines", "Fruits", "Meat products",
                   "Fish products", "Sweet products", "Gold products")

total_spend   <- c(total_wines, total_fruits, total_meat,
                   total_fish, total_sweets, total_gold)

product_results <- data.frame(
  product     = product_names,
  total_spend = total_spend
)

# Add average spend per customer (optional)
n_customers <- nrow(marketing_simple)
product_results$avg_spend_per_customer <- round(product_results$total_spend / n_customers, 2)

print(product_results)

# Simple bar chart for campaign acceptance rates
library(ggplot2)
ggplot(campaign_results,
       aes(x = campaign, y = acceptance_pct)) +
  geom_col(fill = "#4E79A7") +
  labs(title = "Campaign Acceptance Rates",
       x = "Campaign",
       y = "Acceptance rate (%)")

ggplot(product_results,
       aes(x = product, y = total_spend)) +
  geom_col(fill = "#F28E2B") +
  labs(title = "Total Spend by Product Category",
       x = "Product category",
       y = "Total spend (last 2 years)")