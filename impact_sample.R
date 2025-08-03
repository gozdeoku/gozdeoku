# Required package
library(dplyr)

# Sample data: Changes in sub-targets (in percentage)
impact_df <- tibble(
  region = "Investment Region",
  school_enrollment_change = 10,   # Increase in school enrollment (%)
  student_success_change = 5,      # Increase in student success (%)
  dropout_rate_decrease = 3,       # Decrease in school dropout rate (%)
  life_skills_grade_increase = 7   # Increase in average life skills grade (%)
)

# Calculate average sub-target, discount, and final interest rate
impact_df <- impact_df %>%
  mutate(
    # Average of all sub-targets (all expressed as positive impacts)
    avg_sub_target = (school_enrollment_change +
                        student_success_change +
                        dropout_rate_decrease +
                        life_skills_grade_increase) / 4,
    target_increase = 5,                        # Target % increase
    success_excess = avg_sub_target - target_increase, # % above target
    discount_pct = if_else(success_excess > 0, success_excess, 0), # discount for each % above target
    initial_interest = 15,                      # Initial interest rate (%)
    final_interest = initial_interest - discount_pct,  # Final interest rate after discount
    investment_amount = 100000,                 # Investment amount (TL)
    profit = investment_amount * final_interest / 100 # Investor's profit
  )

# Display the result
print(impact_df)

