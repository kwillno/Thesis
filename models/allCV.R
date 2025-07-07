# File for comparing CV scores for all the models

# TOC
#   Load CV scores
#   Make calculations on scores
#   Reverse orienation of DSS
#   Creation of Latex tables

# Libaries
library(dplyr)
library(stringr)



# ----- Load cross validation scores for all models ------

months <- readRDS("../../data/objects/months.rds")
monthNames <- c("January", "February", "March",
                "April", "May", "June",
                "July", "August", "September",
                "October", "November", "December")

modelNames <- c(
  "altRW2" = "Model 1",
  "altTime" = "Model 2",
  "altCoor" = "Model 3",
  "altDistCut" = "Model 4",
  "distStat" = "Model 5"
)

basePath <- "../../data/objects/models/"
models <- c("altRW2", "altTime", "altCoor", "altDistCut", "distStat")

cv <- data.frame()
for (i in 1:length(models)){
  cv <- rbind(cv, readRDS(paste0(basePath, models[i], "/CV.rds")))
}


# ---- Reverse scores for DSS from calculations -----
# This is done to make all scores oriented the same way
# lower scores are better.
cv$DSS <- - cv$DSS


# ----- Make calculations on scores ------

cvCalc <- cv %>%
  group_by(month) %>%
  mutate(DIC_diff  = DIC  -  min(DIC),
         WAIC_diff = WAIC - min(WAIC),
         MLS_diff  = MLS  -  min(MLS),
         DSS_diff  = DSS  -  min(DSS))


# ------ Create latex tables for each month ------

for (i in 1:12){
  cvResult <- cvCalc %>%
    group_by(month) %>% 
    filter(month == i) %>% 
    select(name, DIC_diff, WAIC_diff, MLS_diff, DSS_diff)
  
  lines <- c(
    "\\begin{table}[!ht]",
    "\\centering",
    "\\begin{tabular}{lrrrr}",
    "\\hline",
    "\\textbf{Model Name}  & \\textbf{DIC}  & \\textbf{WAIC}  & \\textbf{LS} & \\textbf{DSS}\\\\",
    "\\hline")
  
  for (j in 1:nrow(cvResult)){
    row <- paste0(
      str_pad(modelNames[cvResult[j,]$name], width = 20, side = "right", pad = " "), # Month name
      " & ",
      str_pad(round(cvResult[j,]$DIC_diff, 2),  width = 9, pad = " "), # DIC  Difference 
      " & ",
      str_pad(round(cvResult[j,]$WAIC_diff, 2), width = 9, pad = " "), # WAIC Difference 
      " & ",
      str_pad(round(cvResult[j,]$MLS_diff, 6),  width = 9, pad = " "), # MLS  Difference 
      " & ",
      str_pad(round(cvResult[j,]$DSS_diff, 6),  width = 9, pad = " "), # DSS  Difference 
      "\\\\\\\\ "
    )
    
    lines <- c(lines, row)
  }
  
  lines <- c(lines, 
             "\\hline \\\\",
             "\\end{tabular}",
             paste0("\\caption*{Comparison of cross validation scores for ", monthNames[i], ". Difference against lowest comparable score.}"),
             paste0("\\label{tab:CV-", months[i], "}"),
             "\\end{table}"
             )
  # 
  # fileConnection <- file(paste0("../../doc/Appendices/cv/", str_pad(i, 2, pad="0"), "-cv.tex" ))
  # writeLines(lines, fileConnection)
  # close(fileConnection)
}

# ------ Create common latex tables ------


# Need to create two separate tables for space

## JAN -> APR
lines <- c(
          "\\begin{table}",
          "\\centering",
          "\\begin{tabular}{lcccc@{}}",
          "\\toprule",
          "\\textbf{Model} & \\textbf{DIC} & \\textbf{WAIC}  & \\textbf{LS} & \\textbf{DSS} \\\\ \\midrule")

for (i in 1:4){
  # Calculate cv differences by month
  cvResult <- cvCalc %>%
    group_by(month) %>% 
    filter(month == i) %>% 
    select(name, DIC_diff, WAIC_diff, MLS_diff, DSS_diff)
  
  # Add to table
  lines <- c(
    lines,
    paste0("\\multicolumn{5}{@{}l}{\\textbf{", monthNames[i], "}}\\\\")
  )
  
  for (j in 1:length(modelNames)){
    lines <- c(
      lines,
      paste0(
        cvResult$name[j], "&",
        round(cvResult$DIC_diff[j], 2), "&",
        round(cvResult$WAIC_diff[j], 2), "&",
        round(cvResult$MLS_diff[j], 6), "&",
        round(cvResult$DSS_diff[j], 6), "\\\\")
    )
  }
  lines <- c(
    lines,
    "\\midrule"
  )  
             
}


lines <- c(
  lines,
  "\\bottomrule",        
  "\\end{tabular}",
  "\\caption*{Comparison of cross validation scores for January until April. Difference against lowest comparable score.}",
  "\\end{table}"
)

fileConnection <- file(paste0("../../doc/Appendices/cv/JAN-APR-cv.tex" ))
writeLines(lines, fileConnection)
close(fileConnection)


## MAY -> AUG
lines <- c(
  "\\begin{table}",
  "\\centering",
  "\\begin{tabular}{lcccc@{}}",
  "\\toprule",
  "\\textbf{Model} & \\textbf{DIC} & \\textbf{WAIC}  & \\textbf{LS} & \\textbf{DSS} \\\\ \\midrule")

for (i in 5:8){
  # Calculate cv differences by month
  cvResult <- cvCalc %>%
    group_by(month) %>% 
    filter(month == i) %>% 
    select(name, DIC_diff, WAIC_diff, MLS_diff, DSS_diff)
  
  # Add to table
  lines <- c(
    lines,
    paste0("\\multicolumn{5}{@{}l}{\\textbf{", monthNames[i], "}}\\\\")
  )
  
  for (j in 1:length(modelNames)){
    lines <- c(
      lines,
      paste0(
        cvResult$name[j], "&",
        round(cvResult$DIC_diff[j], 2), "&",
        round(cvResult$WAIC_diff[j], 2), "&",
        round(cvResult$MLS_diff[j], 6), "&",
        round(cvResult$DSS_diff[j], 6), "\\\\")
    )
  }
  lines <- c(
    lines,
    "\\midrule"
  )  
  
}


lines <- c(
  lines,
  "\\bottomrule",        
  "\\end{tabular}",
  "\\caption*{Comparison of cross validation scores for May until August. Difference against lowest comparable score.}",
  "\\end{table}"
)

fileConnection <- file(paste0("../../doc/Appendices/cv/MAY-AUG-cv.tex" ))
writeLines(lines, fileConnection)
close(fileConnection)


## SEP -> DEC
lines <- c(
  "\\begin{table}",
  "\\centering",
  "\\begin{tabular}{lcccc@{}}",
  "\\toprule",
  "\\textbf{Model} & \\textbf{DIC} & \\textbf{WAIC}  & \\textbf{LS} & \\textbf{DSS} \\\\ \\midrule")

for (i in 9:12){
  # Calculate cv differences by month
  cvResult <- cvCalc %>%
    group_by(month) %>% 
    filter(month == i) %>% 
    select(name, DIC_diff, WAIC_diff, MLS_diff, DSS_diff)
  
  # Add to table
  lines <- c(
    lines,
    paste0("\\multicolumn{5}{@{}l}{\\textbf{", monthNames[i], "}}\\\\")
  )
  
  for (j in 1:length(modelNames)){
    lines <- c(
      lines,
      paste0(
        cvResult$name[j], "&",
        round(cvResult$DIC_diff[j], 2), "&",
        round(cvResult$WAIC_diff[j], 2), "&",
        round(cvResult$MLS_diff[j], 6), "&",
        round(cvResult$DSS_diff[j], 6), "\\\\")
    )
  }
  lines <- c(
    lines,
    "\\midrule"
  )  
  
}


lines <- c(
  lines,
  "\\bottomrule",        
  "\\end{tabular}",
  "\\caption*{Comparison of cross validation scores for September until December. Difference against lowest comparable score.}",
  "\\end{table}"
)

fileConnection <- file(paste0("../../doc/Appendices/cv/SEP-DEC-cv.tex" ))
writeLines(lines, fileConnection)
close(fileConnection)
