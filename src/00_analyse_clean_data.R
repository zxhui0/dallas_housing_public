# This script aims at providing some basic insights of the data available,
# as well as providing some guidance to improve future data adquisition.

# Settings --------------------------------------------------------------------------------------------------------
source("utils/utils.R")
library(broom)
library(data.table)
library(purrr)
library(rgdal)
library(scales)
library(sp)
setwd("..")

dataPath <- LoadConfigFile()[['data_path']]
figPath <- "figures/analysis"

# Read evictions data ---------------------------------------------------------------------------------------------

evictionsDt <- fread('data/clean/eviction_clean.csv')
names(evictionsDt) <- tolower(names(evictionsDt))


# Data sanity check -----------------------------------------------------------------------------------------------

summary(evictionsDt, includeNA = T)
sapply(evictionsDt, class)

# Convert numeric columns from strings to numeric
numericColumns <- c("court", "date.filed", "year.filed", "month.filed", "day.filed")
evictionsDt[, (numericColumns) := map(.SD, function(x) as.numeric(x)), 
            .SDcols = numericColumns]

# Change None values to NA
for (col in names(evictionsDt)) {
    set(evictionsDt, i = which(evictionsDt[[col]] == 'None'), j = col, value = NA)
}

# Check null values across the data.table: There are none
sapply(evictionsDt, function(x) sum(is.na(x)))

# There are 11 records with filed_mont = 0, which makes little sense
evictionsDt[month.filed == 0] %>% nrow()

evictionsDt[, ":="(filedMonth = as.Date(with(evictionsDt, paste(year.filed, month.filed, 1, 
                                                                sep = "-")), "%y-%m-%d"),
                   filedYear = as.numeric(paste0(20, year.filed)))]
evictionsDt[, ":="(filedDate = as.Date(as.character(date.filed), "%y%m%d"),
                   judgementDate = as.Date(as.character(judgement1), "%y%m%d"))]
evictionsDt[, ":="(nDiffDays = as.numeric(judgementDate - filedDate))]

summaryByJudgement <- evictionsDt[, .(nCases = .N), 
                                  by = is.na(nDiffDays)][, p := round(nCases / sum(nCases) * 100, 2)]
print(summaryByJudgement)

# There are 49 cases where judgement date comes before filed date!
evictionsDt[judgementDate < filedDate, .N]


# Descriptive stats -----------------------------------------------------------------------------------------------

# Average
evictionsDt[, .(nEvictions = .N), by = filedMonth][, mean(nEvictions)]
evictionsDt[filedMonth != '2019-07-01', .(nEvictions = .N), by = filedMonth][, mean(nEvictions)]
evictionsDt[(filedMonth != '2019-07-01') & (filedMonth < '2020-02-01'), 
            .(nEvictions = .N), by = filedMonth][, mean(nEvictions)]

# Add column to know whether the eviction went into judgement or not
evictionsDt[, judgement := judgement1 != 0]
# This column makes little sense as it is defined


# Count evictions by demandant's city
top10Cities <- evictionsDt[, .(nCases = .N), by = d.city][order(-nCases)][c(1:10)]
top10Cities[, cityName := factor(d.city, levels = top10Cities[, d.city][order(nCases)])]

ggplot(top10Cities, aes(x = cityName, y = nCases)) +
    geom_bar(stat = "identity", fill = cpalPalette[1]) +
    theme_anne() +
    coord_flip() +
    labs(title = "Top 10 cities (Jan 2017 - Jul 2020)",
         x = "Demandant city", y = "Number of cases") +
    theme(axis.text.x = element_text(angle = 90))

# ggsave(paste0(figPath, "/top_10_cities_demandant_2020.png"),
#        width = 15, height = 9, units = 'cm')

# Count evictions by plaintiff's city
top10CitiesP <- evictionsDt[!is.na(p.city), .(nCases = .N), by = p.city][order(-nCases)][c(1:10)]
top10CitiesP[, cityName := factor(p.city, levels = top10CitiesP[, p.city][order(nCases)])]

ggplot(top10CitiesP, aes(x = cityName, y = nCases)) +
    geom_bar(stat = "identity", fill = cpalPalette[1]) +
    theme_anne() +
    coord_flip() +
    labs(title = "Top 10 cities (Jan 2017 - Jul 2020)",
         x = "Plaintiff city", y = "Number of cases") +
    theme(axis.text.x = element_text(angle = 90))

# ggsave(paste0(figPath, "/top_10_cities_plaintiff_2020.png"),
#        width = 15, height = 9, units = 'cm')

# Top 10 landlords
top10Landlords <- evictionsDt[, .(landlordName = 
                                      sub("\\s+", " ", 
                                          tolower(p.namegroup)))][, .(nCases = .N), 
                                                                  by = landlordName][order(-nCases)][c(1:10)]

# Count evictions by year-month
evictionsByMonth <- evictionsDt[, .(nCases = .N), 
                                by = .(filedMonth)][order(filedMonth)][, p := nCases / sum(nCases), 
                                                                       by = filedMonth]

evictionsBySemester <- evictionsDt[, .(n = .N), by = .(firstSemester = month.filed < 7, filedYear)][order(filedYear, -firstSemester)]
evictionsBySemester[, ":="(p = n / sum(n)), by = filedYear]
firstSemesterPerc <- mean(evictionsBySemester[(firstSemester == T) & (filedYear != 2019) & (filedYear != 2020), p])

forecastedEvictions2019 <- round(nrow(evictionsDt[filedYear == 2019]) / firstSemesterPerc)
forecastedEvictions2020 <- round(nrow(evictionsDt[filedYear == 2020]) / firstSemesterPerc)
print(forecastedEvictions2019)

# Number of cases per year
ggplot(evictionsDt, aes(filedYear)) +
    geom_bar(fill = cpalPalette[1]) +
    theme_anne() +
    labs(title = "Evolution of evictions (Jan 2017 - Jul 2020)",
         x = "Filed year", y = "Number of cases") +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/number_cases_per_year_with_2020.png"),
       width = 15, height = 9, units = 'cm')

ggplot(evictionsDt, aes(filedMonth)) +
    geom_bar(fill = cpalPalette[1]) +
    theme_anne() +
    labs(title = "Evolution of evictions (Jan 2017 - Jul 2020)",
         x = "Filed month", y = "Number of cases") +
    scale_x_date(breaks = "2 months", labels = date_format("%m-%Y")) +
    theme(axis.text.x = element_text(angle = 90))
# Clear yearly pattern: evictions increase on January and yearly minimums on March.

ggsave(paste0(figPath, "/number_cases_per_month_with_2020.png"),
       width = 15, height = 9, units = 'cm')

# ggplot(evictionsByMonth[judgement == F], aes(x = filedMonth, y = p * 100)) +
#     geom_bar(stat = "identity", fill = cpalPalette[3]) +
#     geom_hline(yintercept = mean(evictionsByMonth[judgement == F, p]) * 100, 
#                linetype = "dashed", color = "black") +
#     theme_anne() +
#     labs(title = "Evolution of percentage of non-judged cases (Jan 2017 - Jul 2020)",
#          x = "Filed month", y = "Non-judged cases (%)") +
#     scale_x_date(breaks = "2 months", labels = date_format("%m-%Y")) +
#     theme(axis.text.x = element_text(angle = 90))
# 
# ggsave(paste0(figPath, "/evolution_percentage_non_judged_cases_with_2020.png"),
#        width = 15, height = 9, units = 'cm')

# Check amount distribution
summary(evictionsDt[, judgement.amt])
evictionsDt[, .(n = .N, nZeros = sum(judgement.amt == 0), avg = mean(judgement.amt))]
avgAmount <- evictionsDt[(judgement.amt != 0 & !is.na(judgement.amt)), mean(judgement.amt)]

outlierUpperBound <- mean(evictionsDt[judgement.amt != 0, judgement.amt]) + 
    5 * sd(evictionsDt[, judgement.amt])
print(outlierUpperBound)

avgAmount <- evictionsDt[(judgement.amt != 0 & !is.na(judgement.amt) & 
                              (judgement.amt <= outlierUpperBound)), median(judgement.amt)]

ggplot(evictionsDt[judgement.amt <= outlierUpperBound & judgement.amt != 0], aes(x = judgement.amt)) +
    geom_histogram(aes(y = ..count..), fill = cpalPalette[2], position = "identity", binwidth = 500) +
    geom_vline(aes(xintercept = avgAmount), linetype = "dashed") + 
    theme_anne() +
    labs(title = "Judgment amount (Jan 2017 - Jul 2020)",
         x = "Amount ($)", y = "Number of cases",
         fill = "Judgement") +
    scale_fill_manual(values = cpalPalette) +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/distribution_judgment_amount_2020.png"),
       width = 15, height = 9, units = 'cm')
