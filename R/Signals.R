library(DBI)
library(RSQLite)
library(lubridate)
library(dplyr)
library(TTR)
library(purrr)
library(zoo)

conn <- dbConnect(SQLite(), "/Users/lukepapayoanou/Downloads/SummerResearch/btc_data.db")

daily_table <-dbReadTable(conn, "btc_prices_daily")
daily_updated <- daily_table |> tail(401)
daily_updated <- daily_updated %>%
  mutate(timestamp = ymd_hms(timestamp)) %>%  
  arrange(timestamp)  

daily_btc <- daily_updated

macd_vals <- MACD(daily_btc$close, nFast=12, nSlow=26, nSig=9)

# Make HLC matrix manually
hlc_data <- daily_btc %>%
  select(high, low, close)

# Compute ATR
atr_output <- ATR(hlc_data, n=14)
atr_vals <- atr_output[, "atr"]

# Add to your data frame
daily_btc <- daily_btc %>%
  mutate(
    SMA_7 = SMA(close, n=7),
    SMA_20 = SMA(close, n=20),
    SMA_50 = SMA(close, n=50),
    SMA_100 = SMA(close, n=100),
    SMA_200 = SMA(close, n=200),
    RSI_14 = RSI(close, n=14),
    MACD_line = macd_vals[, "macd"],
    MACD_signal = macd_vals[, "signal"],
    ATR_14 = atr_vals,
    BB = BBands(close, n = 20, sd = 2)
  )

daily_btc <- daily_btc |> tail(200)

btc_signals <- daily_btc |>
  mutate(
    golden_cross = lag(SMA_50) < lag(SMA_200) & SMA_50 >= SMA_200,
    death_cross  = lag(SMA_50) > lag(SMA_200) & SMA_50 <= SMA_200
  )

btc_signals <- btc_signals |>
  mutate(
    gold_signal = case_when(
      golden_cross ~ 1,
      death_cross ~ -1,
      TRUE ~ 0
    )
  )

btc_signals <- btc_signals |>
  mutate(
    RSI_signal = case_when(
      RSI_14 > 70 ~ -1,
      RSI_14 < 30 ~ 1,
      TRUE ~ 0
    )
  )

btc_signals <- btc_signals |>
  mutate(
    macd_signal = case_when(
      lag(MACD_line) < lag(MACD_signal) & MACD_line >= MACD_signal ~ 1,
      lag(MACD_line) > lag(MACD_signal) & MACD_line <= MACD_signal ~ -1,
      TRUE ~ 0
    )
  )

atr_med <- median(btc_signals$ATR_14, na.rm = TRUE)
atr_hi  <- quantile(btc_signals$ATR_14, 0.75, na.rm = TRUE)

btc_signals <- btc_signals |> 
  mutate(
    volatility_level = case_when(
      ATR_14 < atr_med ~ "low",
      ATR_14 < atr_hi  ~ "medium",
      TRUE ~ "high"
    )
  )


btc_signals <- btc_signals |> 
  mutate(
    bb_up = BB[, "up"],
    bb_dn = BB[, "dn"],
    bb_signal = case_when(
      close < bb_dn ~ 1,
      close > bb_up ~ -1,
      TRUE ~ 0
    )
  )

stoch_rsi <- stoch(daily_btc$RSI_14, nFastK = 14, nFastD = 3, nSlowD = 3)

btc_signals <- btc_signals |> 
  mutate(
    stoch_K = stoch_rsi[, "fastK"],
    stoch_D = stoch_rsi[, "fastD"],
    stoch_signal = case_when(
      stoch_K > stoch_D & stoch_K < 20 ~ 1,       # Bullish crossover in oversold
      stoch_K < stoch_D & stoch_K > 80 ~ -1,      # Bearish crossover in overbought
      TRUE ~ 0
    )
  )


btc_signals <- btc_signals |> 
  mutate(
    is_uptrend = SMA_50 > SMA_200)


weights <- list(
  gold = 2,
  rsi = 1.5,
  macd = 1.5,
  bollinger = 1.5,
  stoch = 1
)

btc_signals <- btc_signals |>
  mutate(
    raw_score = (
      gold_signal * weights$gold +
        RSI_signal * weights$rsi +
        macd_signal * weights$macd +
        bb_signal * weights$bollinger +
        stoch_signal * weights$stoch
    )
  )

btc_signals <- btc_signals|>
  mutate(final_score = raw_score * ifelse(is_uptrend, 1.2, 0.8))

btc_signals <- btc_signals |>
  mutate(
    bullish_streak = rollapply(close > lag(close), width = 3, FUN = all, fill = NA, align = "right")
  )

btc_signals <- btc_signals |>
  mutate(
    skip_signal = abs(final_score) < 3 
  )

btc_signals <- btc_signals |> 
  mutate(
    temp_signal = case_when(
      final_score >= 4.5 & is_uptrend & bullish_streak ~ "BUY",
      final_score <= -4 ~ "SELL",
      TRUE ~ "HOLD"
    )
  )

btc_signals$cooldown <- 0
cooldown_days <- 0
last_sell_index <- NA

for (i in seq_len(nrow(btc_signals))) {
  signal_i <- btc_signals$temp_signal[i]
  
  if (isTRUE(!is.na(signal_i) && signal_i == "SELL")) {
    last_sell_index <- i
  }
  
  if (!is.na(last_sell_index) && (i - last_sell_index) <= cooldown_days) {
    btc_signals$cooldown[i] <- 1
  }
}

btc_signals <- btc_signals |>
  mutate(
    final_signal = case_when(
      skip_signal ~ "HOLD",
      final_score >= 4.5 & is_uptrend & bullish_streak == T ~ "BUY",
      final_score <= -4 ~ "SELL",
      TRUE ~ "HOLD"
    )
  ) 



state_machine <- function(prev_state, signal) {
  case_when(
    signal == "BUY"  & prev_state == "SIDELINES" ~ "HOLDING",
    signal == "SELL" & prev_state == "HOLDING"   ~ "SIDELINES",
    TRUE ~ prev_state
  )
}

btc_signals <- btc_signals |> 
  mutate(
    position_state = accumulate(final_signal, state_machine, .init = "SIDELINES")[-1]
    )

btc_signals$final_signal |> tail(1)

btc_signals$position_state |> tail(1)

daily_sent <- dbReadTable(conn, "daily_sentiment_stats")

dbRemoveTable(conn, "btc_outputs")

dbExecute(conn, "CREATE TABLE IF NOT EXISTS btc_outputs (
  timestamp TEXT PRIMARY KEY,
  date TEXT,
  close REAL,
  final_signal TEXT,
  position_state TEXT,
  mean_sentiment REAL,
  std_sentiment REAL
);")

daily_sent$date <- as.Date(daily_sent$date)

output_df <- btc_signals |>
  mutate(date=as.Date(timestamp))

combined_df <- output_df %>%
  left_join(daily_sent, by = "date")

combined_df <- combined_df |>
  select(timestamp,date, close, final_signal,position_state,mean_sentiment,std_sentiment) |>
  mutate(date=as.character(date),
         timestamp = as.character(timestamp))

dbWriteTable(conn, "btc_outputs", combined_df, append = TRUE, row.names = FALSE)

daily_sent |> tail(1)



dbDisconnect(conn)
