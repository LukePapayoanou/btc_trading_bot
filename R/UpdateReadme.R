library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(scales)
library(knitr)

# Connect to the local database
conn <- dbConnect(SQLite(), "database/btc_data.db")

# Get one row per date — the latest row for each day
btc_data <- dbGetQuery(conn, "
  SELECT * FROM (
    SELECT *, ROW_NUMBER() OVER (PARTITION BY date ORDER BY timestamp DESC) as rn
    FROM btc_outputs
  ) WHERE rn = 1
  ORDER BY date DESC
  LIMIT 30
")

# Convert to Date and arrange ascending
btc_data$date <- as.Date(btc_data$date)
btc_data <- btc_data |> arrange(date)

# Plot: BTC closing price with signal color
p <- ggplot(btc_data, aes(x = date, y = close, color = final_signal)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(HOLD = "gray", BUY = "green", SELL = "red")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "BTC Price & Trading Signals", x = "Date", y = "Close Price (USD)", color = "Signal") +
  theme_minimal()

# Save plot
ggsave("btc_chart.png", p, width = 8, height = 4)

# Make summary table (latest 5 days)
summary_table <- btc_data |> 
  select(date, close, final_signal, position_state, mean_sentiment, std_sentiment) |>
  arrange(desc(date)) |>
  head(5)

# Convert to markdown
table_md <- knitr::kable(summary_table, format = "markdown")

# Write to README
readme_text <- c(
  "# BTC Trading Bot Summary",
  "",
  "## 📈 Chart",
  "![BTC Chart](btc_chart.png)",
  "",
  "## 📋 Latest Signals",
  table_md
)

writeLines(readme_text, "README.md")
