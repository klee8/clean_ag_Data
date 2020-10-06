#  Combine and clean datasets (2017 livestock counts (on Table 1) and 1994 livestock counts) to create a single dataset

library(tidyverse)
install.packages("svglite")
library(svglite)

# read in 1994 data and transform
oldcount <- read.delim("AGR075701_20190717_045133_73_table1_delim.txt", skip = 2, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE, sep = "\t")
oldcount <- oldcount[1:5,]
# remove repeated region with no data
oldcount$Nelson.Marlborogh.Region..Disc.1992. <- NULL
# transform rows to columns
rownames(oldcount) <- oldcount$X
oldcount$X <- NULL
oldcount<- t(oldcount)
oldcount <- as.data.frame(oldcount)
# divide all counts by 1000 
oldcount <- oldcount/1000
# clean region names
oldcount$region <- rownames(oldcount)
rownames(oldcount) <- NULL
oldcount$region <- gsub(".Region", "", oldcount$region)
oldcount$region <- gsub("Is\\.", "Islands", oldcount$region)
oldcount$region <- gsub("\\.+", "\\.", oldcount$region)
colnames(oldcount) <- c( "dairy_1994", "beef_1994", "cattle_1994", "sheep_1994","deer_1994", "region")


# add total north island counts
total_NI <- c("Northland", "Auckland", "Waikato", "Bay.of.Plenty", "Gisborne",  
              "Hawke.s.Bay",  "Taranaki", "Manawatu.Wanganui", "Wellington")
NorthIsland <- oldcount[oldcount$region %in% total_NI, c( "dairy_1994", "beef_1994", "cattle_1994", "sheep_1994", "deer_1994")]
Sum_North <- c(as.list(mapply(sum,NorthIsland)),  region = "Total.North.Island")
oldcount <- rbind.data.frame(oldcount, Sum_North )

# add total south island counts
total_SI <- c("Tasman", "Nelson", "Marlborough", "West.Coast", "Canterbury", 
              "Otago", "Southland", "Chatham.Islands")
SouthIsland <- oldcount[oldcount$region %in% total_SI, c( "dairy_1994", "beef_1994", "cattle_1994", "sheep_1994", "deer_1994")]
Sum_South <- c(as.list(mapply(sum,SouthIsland)),  region = "Total.South.Island")
oldcount <-rbind.data.frame(oldcount, Sum_South )



# read in 2017 data
newcount <- read.delim("agricultural-production-statistics-jun17-final-tables-v2.txt", skip = 8, na.strings = c("S", "-"))
newcount <- newcount[1:24, c("X" , "X.4", "X.10", "X.16","X.22")]
colnames(newcount) <- c("region", "sheep_2017", "dairy_2017", "beef_2017", "deer_2017")
# get rid of empty rows
newcount <- newcount[!apply(is.na(newcount) | newcount == "", 1, all),]
# coerce "C" and "-" to NA
newcount$sheep_2017 <- as.numeric(gsub(",", "", newcount$sheep_2017))
newcount$dairy_2017 <- as.numeric(gsub(",", "", newcount$dairy_2017))
newcount$beef_2017  <- as.numeric(gsub(",", "", newcount$beef_2017))
# clean region names
newcount$region <- make.names(newcount$region)
newcount$region <- gsub("\\.*$", "", newcount$region)
str(newcount)
newcount
# merge and order by regions in 2017 table
region_order <- structure(.Names = newcount$region, 1:length(newcount$region))
all_counts <- merge(newcount, oldcount, by = "region")
all_counts <- all_counts[order(region_order[all_counts$region]),]

# calculate percentage change from 1994 to 2017
all_counts <- all_counts %>% mutate(sheep_perc = (sheep_2017 - sheep_1994)*100/sheep_1994,
                      dairy_perc = (dairy_2017 - dairy_1994)*100/dairy_1994,
                      beef_perc = (beef_2017 - beef_1994)*100/beef_1994, 
                      deer_perc = (deer_2017 - deer_1994)*100/deer_1994)

# re-order to match original table 1
all_counts <- all_counts[, c("region", 
                             "sheep_1994", "sheep_2017",  "sheep_perc",
                             "dairy_1994", "dairy_2017", "dairy_perc",
                             "beef_1994", "beef_2017",  "beef_perc",
                             "deer_1994", "deer_2017", "deer_perc")]
all_counts
write.table(all_counts, "cleaned_ag_data.txt", sep = "\t", row.names = FALSE, quote = FALSE)


# order for long data
order <- rev(names(region_order))
all_counts$region <- factor(all_counts$region, levels = order)

# make bar chart of each animal by region in 1994 and 2017
graph_animals <- function(name) {
  animal <- all_counts[, c("region", paste(name, "_1994", sep = ""), paste(name, "_2017", sep = ""))]
  animal <- gather(animal, "Year", "count",-region)
  animal$Year <- gsub(paste(name,"_", sep = ""), "", animal$Year)
  ggplot(animal, aes(x=region, y=count, fill = Year)) +
    geom_col(position = "dodge")+
    theme_minimal()+
    coord_flip()+
    ggtitle(paste(str_to_title(name), " in each Region in 1994 and 2017", sep = "")) +
    ylab(paste(name, " counts (000)", sep = "")) +
    xlab("Regions")
  ggsave(paste(name, "_1994_2017.svg", sep = ""))
}

graph_animals("sheep")
graph_animals("dairy")
graph_animals("beef")
graph_animals("deer")

