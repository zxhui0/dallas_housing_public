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

# Load complete geo file ------------------------------------------------------------------------------------------

geoFile <- paste0(dataPath, 'DSSG_updated.gdb')
fcList <- ogrListLayers(geoFile)
print(fcList)

# Read evictions data ---------------------------------------------------------------------------------------------

evictionsLayer <- LoadLayer(geoFile, "EvictionRecords_D", 'geometry')
raw <- setDT(evictionsLayer@data)
evictionsDt <- copy(raw)
names(evictionsDt) <- tolower(names(evictionsDt))


# Save to data folder ---------------------------------------------------------------------------------------------

fwrite(evictionsDt, file = 'data/clean/evictions_d.csv', row.names = F)


# Data sanity check -----------------------------------------------------------------------------------------------

summary(evictionsDt, includeNA = T)
sapply(evictionsDt, class)

# Convert numeric columns from strings to numeric
numericColumns <- c("court", "filed_date", "year_filed", "filed_mont", "day_filed", "amount")
evictionsDt[, (numericColumns) := map(.SD, function(x) as.numeric(x)), 
            .SDcols = numericColumns]

# Change None values to NA
for (col in names(evictionsDt)) {
    set(evictionsDt, i = which(evictionsDt[[col]] == 'None'), j = col, value = NA)
}

# Check null values across the data.table: There are none
sapply(evictionsDt, function(x) sum(is.na(x)))

# There are 11 records with filed_mont = 0, which makes little sense
evictionsDt[filed_mont == 0] %>% nrow()

evictionsDt[, ":="(filedMonth = as.Date(with(evictionsDt, paste(year_filed, filed_mont, 1, 
                                                                sep = "-")), "%y-%m-%d"),
                   filedYear = as.numeric(paste0(20, year_filed)))]
evictionsDt[, ":="(filedDate = as.Date(as.character(filed_date), "%y%m%d"),
                   judgementDate = as.Date(as.character(judgement_), "%y%m%d"))]
evictionsDt[, ":="(nDiffDays = as.numeric(judgementDate - filedDate))]

summaryByJudgement <- evictionsDt[, .(nCases = .N), by = is.na(nDiffDays)][, p := round(nCases / sum(nCases) * 100, 2)]
print(summaryByJudgement)

# There are 42 cases where judgement date comes before filed date!
evictionsDt[judgementDate < filedDate, .N]


# Descriptive stats -----------------------------------------------------------------------------------------------

# Removing July 2019 since it seems to be incomplete
evictionsDt <- evictionsDt[filedMonth != "2019-07-01"]

# Add column to know whether the eviction went into judgement or not
evictionsDt[, judgement := judgemen_1 != 0]


# Count evictions by demandant's city
top10Cities <- evictionsDt[, .(nCases = .N), by = d_city][order(-nCases)][c(1:10)]
top10Cities[, cityName := factor(d_city, levels = top10Cities[, d_city][order(nCases)])]

ggplot(top10Cities, aes(x = cityName, y = nCases)) +
    geom_bar(stat = "identity", fill = cpalPalette[1]) +
    theme_anne() +
    coord_flip() +
    labs(title = "Top 10 cities (Jan 2017 - Jun 2019)",
         x = "Demandant city", y = "Number of cases") +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/top_10_cities_demandant.png"),
       width = 15, height = 9, units = 'cm')

# Count evictions by plaintiff's city
top10CitiesP <- evictionsDt[!is.na(p_city), .(nCases = .N), by = p_city][order(-nCases)][c(1:10)]
top10CitiesP[, cityName := factor(p_city, levels = top10CitiesP[, p_city][order(nCases)])]

ggplot(top10CitiesP, aes(x = cityName, y = nCases)) +
    geom_bar(stat = "identity", fill = cpalPalette[1]) +
    theme_anne() +
    coord_flip() +
    labs(title = "Top 10 cities (Jan 2017 - Jun 2019)",
         x = "Plaintiff city", y = "Number of cases") +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/top_10_cities_plaintiff.png"),
       width = 15, height = 9, units = 'cm')

# Count evictions by year-month
evictionsByMonth <- evictionsDt[, .(nCases = .N), 
                                by = .(judgement, filedMonth)][order(filedMonth)][, p := nCases / sum(nCases), by = filedMonth]

evictionsBySemester <- evictionsDt[, .(n = .N), by = .(firstSemester = filed_mont < 7, filedYear)][order(filedYear, -firstSemester)]
evictionsBySemester[, ":="(p = n / sum(n)), by = filedYear]
firstSemesterPerc <- mean(evictionsBySemester[(firstSemester == T) & (filedYear != 2019), p])

forecastedEvictions2019 <- round(nrow(evictionsDt[filedYear == 2019]) / firstSemesterPerc)
print(forecastedEvictions2019)

# Number of cases per year
ggplot(evictionsDt, aes(filedYear, fill = judgement)) +
    geom_bar() +
    theme_anne() +
    labs(title = "Evolution of evictions (Jan 2017 - Jun 2019)",
         x = "Filed year", y = "Number of cases",
         fill = "Judgement") +
    scale_fill_manual(values = cpalPalette) +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/number_cases_per_year_by_judgement.png"),
       width = 15, height = 9, units = 'cm')

ggplot(evictionsDt, aes(filedMonth, fill = judgement)) +
    geom_bar() +
    theme_anne() +
    labs(title = "Evolution of evictions (Jan 2017 - Jun 2019)",
         x = "Filed month", y = "Number of cases",
         fill = "Judgement") +
    scale_fill_manual(values = cpalPalette) +
    scale_x_date(breaks = "2 months", labels = date_format("%m-%Y")) +
    theme(axis.text.x = element_text(angle = 90))
# Clear yearly pattern: evictions increase on January and yearly minimums on March.

ggsave(paste0(figPath, "/number_cases_per_month_by_judgement.png"),
       width = 15, height = 9, units = 'cm')

ggplot(evictionsByMonth[judgement == F], aes(x = filedMonth, y = p * 100)) +
    geom_bar(stat = "identity", fill = cpalPalette[3]) +
    geom_hline(yintercept = mean(evictionsByMonth[judgement == F, p]) * 100, 
               linetype = "dashed", color = "black") +
    theme_anne() +
    labs(title = "Evolution of percentage of non-judged cases (Jan 2017 - Jun 2019)",
         x = "Filed month", y = "Non-judged cases (%)") +
    scale_x_date(breaks = "2 months", labels = date_format("%m-%Y")) +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/evolution_percentage_non_judged_cases.png"),
       width = 15, height = 9, units = 'cm')

# Check amount distribution
summary(evictionsDt[, amount])

outlierUpperBound <- mean(evictionsDt[, amount]) + 5 * sd(evictionsDt[, amount])
print(outlierUpperBound)

ggplot(evictionsDt[amount <= outlierUpperBound], aes(x = amount, fill = judgement)) +
    # geom_histogram(position = "dodge", binwidth = 200) +
    geom_histogram(aes(y = ..density..), alpha = 0.6, position = "identity", binwidth = 500) +
    theme_anne() +
    labs(title = "Owed amount (Jan 2017 - Jun 2019)",
         x = "Amount ($)", y = "Distribution of cases",
         fill = "Judgement") +
    scale_fill_manual(values = cpalPalette) +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/distribution_owed_amount_by_judgement.png"),
       width = 15, height = 9, units = 'cm')

# Number of days between filing and judgement
print(mean(evictionsDt[!is.na(nDiffDays) & (nDiffDays > 0), nDiffDays]))
summary(evictionsDt[!is.na(nDiffDays) & (nDiffDays > 0), nDiffDays])

ggplot(evictionsDt[!is.na(nDiffDays) & (nDiffDays > 0)], aes(nDiffDays)) +
    geom_histogram(binwidth = 10, fill = cpalPalette[3]) +
    theme_anne() +
    labs(title = "Number of days between filing and judgement (Jan 2017 - Jun 2019)",
         x = "Number of days", y = "Number of cases") +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/distribution_number_diff_days_filing_judgement.png"),
       width = 15, height = 9, units = 'cm')

ggplot(evictionsDt[!is.na(nDiffDays) & (nDiffDays > 0)], aes(x = amount, y = nDiffDays)) +
    geom_point(color = cpalPalette[1]) +
    # geom_smooth() +
    theme_anne() +
    labs(title = "Owed amount vs number of diff days (Jan 2017 - Jun 2019)",
         x = "Amount ($)", y = "Number of days") +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/owed_amount_vs_number_diff_days.png"),
       width = 15, height = 9, units = 'cm')

# Percentage by court
evictionsDt[, .(nCases = .N), by = court][order(-nCases)]

nCasesByCourt <- evictionsDt[, .(nCases = .N), 
                             by = .(judgement, court)][order(court)]
nCasesByCourt[, p := nCases / sum(nCases), by = court]

ggplot(nCasesByCourt[, .(nCases = sum(nCases)), by = court], aes(x = factor(court), y = nCases)) +
    geom_bar(stat = "identity", fill = cpalPalette[2]) +
    theme_anne() +
    labs(title = "Number of cases by court (Jan 2017 - Jun 2019)",
         x = "Court", y = "Number of cases") +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/number_cases_by_court.png"),
       width = 15, height = 9, units = 'cm')

ggplot(nCasesByCourt[judgement == T], aes(x = factor(court), y = round(p * 100, 2))) +
    geom_bar(stat = "identity", fill = cpalPalette[3]) +
    geom_hline(yintercept = mean(nCasesByCourt[judgement == T, p]) * 100, 
               linetype = "dashed", color = "black") +
    theme_anne() +
    labs(title = "Percentage of judged cases by court (Jan 2017 - Jun 2019)",
         x = "Court", y = "Judged cases (%)") +
    theme(axis.text.x = element_text(angle = 90))

ggsave(paste0(figPath, "/percentage_judged_cases_by_court.png"),
       width = 15, height = 9, units = 'cm')
