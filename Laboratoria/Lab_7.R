#########################################
###    WSTĘP DO EKSPLORACJI DANYCH    ###
###         LABORATORIUM  7           ###
#########################################


# Generowanie raportów z analizą danych

# 1) R Markdown - możliwości

# https://bookdown.org/yihui/rmarkdown/
# https://bookdown.org/yihui/rmarkdown-cookbook/

# a) theme & highlight

# theme: cerulean, journal, flatly, darkly, readable, spacelab, united, cosmo, lumen, paper, sandstone, simplexand yeti
# highlight: espresso, tango, pygments, kate, monochrome, zenburn, haddock and textmate
 
# https://www.datadreaming.org/post/r-markdown-theme-gallery/

# b) functions 

# toc: true 
# number_sections: true
# toc_depth: 4
# toc_float: true 

# c) tabels

# knitr::kable()
# https://www.tablesgenerator.com/latex_tables

# kableEkstra
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

# d) message, error, warning

# e) figure


# 2) "dlookr"
# https://github.com/choonghyunryu/dlookr
install.packages("dlookr")
library(dlookr)
library(dplyr)
summary(mtcars)

describe(mtcars)

mtcars %>% 
  describe() %>% 
  select(variable, mean, IQR, p50)

normality(mtcars)
plot_normality(mtcars, mpg)

plot_normality(mtcars, cyl)

plot_correlate(mtcars)

# 3) "dataReporter"
# https://github.com/ekstroem/dataMaid
# https://github.com/ekstroem/dataReporter
install.packages("dataReporter")
library("dataReporter")

makeDataReport(mtcars)
data("mtcars")
