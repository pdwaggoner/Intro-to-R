# For the text preprocessing and models
library(quanteda)
library(tm)

# For plotting
library(ggplot2)

# For getting data directly from Benoit's GitHub
library(devtools)
library(quantedaData)

# Extra packages
library(data.table)
library(grid)

#if (!require(devtools)) install.packages("devtools") # devtools required to install quanteda from Github
#devtools::install_github("kbenoit/quantedaData") # install the latest version quanteda from Github
#slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
#install_url(slam_url)
#library(Rtools)

# Set working directory

# Store full data as a simpler object to work with
d <- data_corpus_irishbudget2010


####
####
#### Worscores model (supervised)
####
####


# Score all of the texts (including the reference texts) using two reference texts: 
# the first set to +1, and the second set to -1. 
# This involves first fitting the wordscores model, then predicting the text score for all 
# texts (including the reference texts.)

# First set training texts
irish.dfm <- dfm(d, 
                 remove_punct = TRUE,
                 verbose = TRUE)

reference.scores <- c(rep(NA, 4), -1, 1, rep(NA, 8))

ws.model <- textmodel_wordscores(irish.dfm, 
                      reference.scores, 
                      smooth=1)

#print(ws.model)
summary(ws.model)


# Now generate predictions for every other MP based on distances from the training texts
ws.full.model <- predict(ws.model, level = 0.95)
ws.full.model


####
####
#### Wordfish model (unsupervised)
####
####


irish.dfm <- dfm(d, 
                 remove_punct = TRUE,
                 verbose = TRUE)

wf.full.model <- textmodel_wordfish(irish.dfm)

print(wf.full.model)


####
####
#### Plots
####
####


vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

labels <- paste(docvars(irish.dfm, "name"), 
                docvars(irish.dfm, "party"))

ws <- textplot_scale1d(ws.full.model, 
                       doclabels = labels)

wf <- textplot_scale1d(wf.full.model, 
                       doclabels = labels)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ws, vp = vplayout(1, 1))
print(wf, vp = vplayout(1, 2))


# Group by Party
ws.p <- textplot_scale1d(ws.full.model, 
                         doclabels = labels, 
                         groups = docvars(irish.dfm, "party"))

wf.p <- textplot_scale1d(wf.full.model, 
                         doclabels = labels, 
                         groups = docvars(irish.dfm, "party"))

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ws.p, vp = vplayout(1, 1))
print(wf.p, vp = vplayout(1, 2))



# We can also arrange on the same plot, by highlighting keywords of interest
ws.scale <- textplot_scale1d(ws.model, margin = "features", 
                 highlighted_color = "red",
                 highlighted = c("environment", "energy", "business",
                                 "bank", "economy", "the", "government", "global", 
                                 "citizenship", "productivity", "deficit"))

wf.scale <- textplot_scale1d(wf.full.model, margin = "features", 
                 highlighted_color = "red",
                 highlighted = c("environment", "energy", "business",
                                 "bank", "economy", "the", "government", "global", 
                                 "citizenship", "productivity", "deficit"))

wf.scale

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ws.scale, vp = vplayout(1, 1))
print(wf.scale, vp = vplayout(1, 2))
