##########################################
#Info about the data

cat_vars <- c("User Behavior Class" = "user_behavior_class",
              "Device Model" = "device_model",
              "Operating System" = "operating_system",
              "Gender" = "gender",
              "User ID" = "user_id")

clean_cat_vars <- c("User Behavior Class" = "User Behavior Class",
                    "Device Model" = "Device Model",
                    "Operating System" = "Operating System",
                    "Gender" = "Gender",
                    "User ID" = "User ID")

numeric_vars <- c("App Usage (min/day)" = "app_usage_time_min_day",
                  "Screen Time (hours/day)" = "screen_on_time_hours_day",
                  "Battery Drain (mAh/day)" = "battery_drain_m_ah_day",
                  "# of Apps Installed" = "number_of_apps_installed",
                  "Data Usage (MB/day)" = "data_usage_mb_day")

behavior_vals <- c("Low" = "1",
                   "Low-Moderate" = "2",
                   "Moderate" = "3",
                   "Moderate-High" = "4",
                   "High" = "5")

opsystem_vals <- c("Android", "iOS")

devicemodel_vals <- c("Google Pixel 5",
                      "OnePlus 9",
                      "Xiaomi Mi 11",
                      "iPhone 12",
                      "Samsung Galaxy S21")

gender_vals <- c("Male", "Female")