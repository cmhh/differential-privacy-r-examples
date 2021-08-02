library(rJava)
library(data.table)
library(magrittr)

# initialise the JVM -----------------------------------------------------------
.jinit(classpath = "java/differentialprivacy.jar")

# read visits data -------------------------------------------------------------
visits <- fread("data-raw/day_data.csv")

# non-private counts -----------------------------------------------------------
get_hour <- function(x) hour(as.POSIXct(x, format="%I:%M:%S %p"))
visits[, hour := sapply(time_entered, get_hour)]
non_private_counts <- visits[, .(nvisit = .N), by = hour][order(hour)]

# private counts ---------------------------------------------------------------
hours <- sort(unique(visits$hour))

counts <- lapply(hours, function(x) {
  cls <- "com.google.privacy.differentialprivacy.Count"
  J(cls)$builder()$epsilon(log(3))$maxPartitionsContributed(1L)$build()
}) %>% setNames(., as.character(hours))

for (x in as.character(visits$hour)) {
  counts[[x]]$increment()
}

private_counts <- data.table(
  hour = hours,
  nvisit = sapply(counts, function(x) { x$computeResult() })
)
