course_content_1 <-
  tibble::tribble(
    ~Day, ~Time, ~Topic,
    "Wednesday", "10:00 - 11:15", "Getting Started with R and RStudio",
    "Wednesday", "11:15 - 11:30", "Coffee Break",
    "Wednesday", "11:30 - 13:00", "Data Import & Export",
    "Wednesday", "13:00 - 14:00", "Lunch Break",
    "Wednesday", "14:00 - 15:30", "Data Wrangling - Part 1",
    "Wednesday", "15:30 - 15:45", "Coffee Break",
    "Wednesday", "15:45 - 17:00", "Data Wrangling - Part 2",
  ) %>%
  knitr::kable() %>%
  kableExtra::kable_styling() %>%
  kableExtra::column_spec(1, color = "#1E8CC8") %>%
  kableExtra::column_spec(2, color = "#1E8CC8") %>%
  kableExtra::column_spec(3, bold = TRUE) %>%
  kableExtra::row_spec(2, color = "#1E8CC8") %>%
  kableExtra::row_spec(4, color = "#1E8CC8") %>%
  kableExtra::row_spec(6, color = "#1E8CC8")

course_content_2 <-
  tibble::tribble(
    ~Day, ~Time, ~Topic,
    "Thursday", "10:00 - 11:15", "Exploratory Data Analysis",
    "Thursday", "11:15 - 11:30", "Coffee Break",
    "Thursday", "11:30 - 13:00", "Data Visualization - Part 1",
    "Thursday", "13:00 - 14:00", "Lunch Break",
    "Thursday", "14:00 - 15:30", "Confirmatory Data Analysis",
    "Thursday", "15:30 - 15:45", "Coffee Break",
    "Thursday", "15:45 - 17:00", "Data Visualization - Part 2",
  ) %>%
  knitr::kable() %>%
  kableExtra::kable_styling() %>%
  kableExtra::column_spec(1, color = "#1E8CC8") %>%
  kableExtra::column_spec(2, color = "#1E8CC8") %>%
  kableExtra::column_spec(3, bold = TRUE) %>%
  kableExtra::row_spec(2, color = "#1E8CC8") %>%
  kableExtra::row_spec(4, color = "#1E8CC8") %>%
  kableExtra::row_spec(6, color = "#1E8CC8")