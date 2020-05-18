# Styling
CREDITS_ICON = "award"
NETWORK_ICON = "wifi"
LIVE_ICON = "heartbeat"
PROCESSOR_ICON = "microchip"
FOLDING_ICON_FINISHED = "stop"
FOLDING_ICON_RUNNING = "play"
FOLDING_ICON_NOT_STARTED = "clock"
FOLDING_ICON_ERROR = "exclamation"
IDLE_TIME_ICON = "clock"

FOLDING_VALUE_BOX_HEIGHT = 200
OVERVIEW_PLOT_WIDTH = 9
OVERVIEW_PLOT_HEIGHT = 9

# Logs
FAH_CLIENT_FOLDER_PATH <- file.path("~", "..", "AppData", "Roaming", "FAHClient")
FAH_CLIENT_RECENT_LOGS_PATH <- file.path(FAH_CLIENT_FOLDER_PATH, "logs")
FAH_CLIENT_LIVE_LOG_FILE_NAME <- "log.txt"
NUMBER_LOGS_READ <- 8
RENDER_PERIOD_MINUTES <- 5

# Functions
difftime_to_period <- function(time_diff, seconds_in_unit) {
  lubridate::as.period(time_diff) %>% 
    as.numeric() %>% 
    (function(x) x * seconds_in_unit) %>% 
    round() %>% 
    as.integer() %>% 
    lubridate::seconds_to_period()
}