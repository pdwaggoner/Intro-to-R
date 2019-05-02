# Basic Webscraping in R

# Load libraries (first, install)
library(XML)
library(RCurl)

### FIRST: use RCurl to scrape and build a table from a url (adapted/updated from Gelman's code)

# Read in the wiki page for men's 1500 meter records
wikipediaPage <- "https://en.wikipedia.org/wiki/1500_metres_world_record_progression"

# Extract data from it (using RCurl)
htmlContent <- getURL(wikipediaPage)

# Turn HTML data into manipulable data
result <- readHTMLTable(htmlContent)

tableWR <- readHTMLTable(htmlContent, which = 2,
                         stringsAsFactors = FALSE)

tableWR <- tableWR[2:39, ] # drop old column vars for manipulation next

# Change var coding
tempTime <- as.POSIXlt(tableWR$V1, format = "%M:%OS") # V1 = time

# Create a new DF based on data
finalTable <- data.frame(runTime = (tempTime[2:39, 2] * 60) + tempTime$sec,
                         recordSet = as.Date(tableWR$V4), # V4 = date
                         format = "%Y-%m-%d")

# Plot the result and interpret
plot(runTime ~ recordSet, data = finalTable,
     type = "s", xlab = "Year", ylab = "Time (Seconds)",
     main = "World Records in Men's 1500 Meter")


### NEXT: use rvest to scrape from a url (adapted/updated from Silge's code)
library(rvest)
library(tidyverse)

# Read in the website by "Scraping" the URL
data <- read_html("https://en.wikipedia.org/wiki/Current_members_of_the_United_States_House_of_Representatives")

# Turn the data from the wiki page into readable data using tidyverse code (i.e., pipes)
MCs <- data %>% # pass the data URL to a new object "MCs" (members of conrgess)
  html_node(xpath = '//*[@id="votingmembers"]')  %>% # specify the CSS code, which is the node storing the interesting information you want to scrape
  html_table(fill = TRUE) # then turn it into a readable html table

# Grab only the relevant information by filerting a bit
MCs <- MCs[,c(1:2,4:9)] %>% # keep relevant info (inspect the actual wiki page to note the columns you would like to grab)
  as_tibble() # remember a "tibble" in the tidyverse is == table in base R, with a few slight differences

# Now, inspect your new data frame containing the full membership of the current U.S. Congress
MCs # full frame
sample_n(MCs, 5) # random sample of length 5

# Visualize your data via a histogram from ggplot2
ggplot(MCs, aes(`Assumed office`, colour = Party, fill = Party)) + # specify data, the information you want to plot (the year the MC assumed office), and the conditional colors (Party, in this case)
  geom_histogram(stat = "count") + # tell ggplot2 to count up observations at each year in the data set
  theme(axis.text.x = element_text(angle=75, vjust=0.5), # angle the x axis text to make it more readable (change the "angle" argument based on degrees, e.g., 90 = vertically flipped)
        plot.title = element_text(hjust = 0.5)) + # Center the ggplot2 title
  labs(x = "Assumed Office (Year)", # change/update your axis and plot labels
       y = "Count of MCs",
       title = "Counts of MCs in the Current Congress\nBy Year Assumed Office")
 

### FINALLY: use selectorgadet to specify and select CSS nodes for your own site of interest

# Note that in the above code using rvest, we had to specify the CSS "node". This determines where on the page to grab the information.
# In this example, I grabbed it for you, but you can change this by selecting the CSS code yourself. To do this, there is a great tool,
# called a selectorgadet, which is in the form of an easy to use extension for Chrome. You can do it manually as well. To get it, go here:
# https://selectorgadget.com/
# After you have downloaded it, just follow the instructions on usage, which entail you going to a website and initiating the selectorgadet,
# And then clicking on whatever data you want to grab. Then, it will provide the CSS code (which will be used as the "node" in the code above).
# Copy and paste that into the code above to change the specific information you are grabbing.
# This will allow you to grab any information from any website you wish with very little code. 
