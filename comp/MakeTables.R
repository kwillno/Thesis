# Creation of Score Table

# TOC
#   Load libraries
#   Load data
#   Calculate aggregates
#   Export Table


modelName <- "distStat"


# ------ Load libraries ------
library(dplyr)


# ------ Load data ------

bias_df <- readRDS(paste0("../../data/objects/comps/", modelName, "/bias.rds"))
crps_df <- readRDS(paste0("../../data/objects/comps/", modelName, "/crps.rds"))

months <- readRDS("../../data/objects/months.rds")

# ------ Calculate aggregates ------

df <- subset(cbind(bias_df, crps_df), select = c("month", "bias", "error", "crps", "ign"))
df$days <- c(31, 28, 31,
             30, 31, 30,
             31, 31, 30,
             31, 30, 31)


aggr_row <- df %>%
  summarise(
    month = "Agr",
    bias = sum(bias*days) / sum(days),
    error = sum(error*days) / sum(days),
    crps = sum(crps*days) / sum(days),
    ign = sum(ign*days) / sum(days),
    days = sum(days)
    )



df <- rbind(df, aggr_row)

# ------ Export table ------


lines <- c(
  "\\begin{table}[!ht]",
  "\\centering",
  "\\begin{tabular}{llll}",
  "\\hline",
  "\\textbf{Month}  & $\\text{Bias}^{m}$ & $\\text{Error}^{m}$ & $\\text{CRPS}^{m}$\\\\ ",
  "\\hline "
)

for (i in 1:12){
  lines <- c(
    lines,
    paste0(months[i], " & ", round(df$bias[i], 3), " & ", round(df$error[i], 3), " & ", round(df$crps[i], 3), "\\\\")
  )
}

lines <- c(
  lines,
  "\\hline",
  paste0("Aggregate & ", round(df$bias[13], 3), " & ", round(df$error[13], 3), " & ", round(df$crps[13], 3), "\\\\")
)

lines <- c(
  lines,
  "\\hline \\\\",
  "\\end{tabular}",
  "\\caption{Bias, Error and CRPS for each of the models in the model set averaged over the domain and the timeframe. The scores aggregated over all the months is also given.}",
  "\\label{tab:06-meanCRPS}",
  "\\end{table}"
)

fileConnection <- file(paste0("../../doc/Chapters/Tables/comps.tex" ))
writeLines(lines, fileConnection)
close(fileConnection)
