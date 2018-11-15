# Uncomment next two lines for installing CHAID package
# install.packages("partykit")
# install.packages("CHAID", repos="http://R-Forge.R-project.org")

# Setting up enviroment for CHAID classification
require(rsample) # for dataset and splitting also loads broom and tidyr
require(dplyr)
require(ggplot2)
theme_set(theme_bw()) # set theme
require(CHAID)
require(purrr)
require(caret)

