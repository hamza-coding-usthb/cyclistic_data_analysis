library(knitr)
# 'input' is your R Markdown file
# 'output' is the name you want for the new pure R script
purl(input = "data_analysis.Rmd",
     output = "data_analysis.R",
     documentation = 0)