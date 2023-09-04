library(ggplot2)


# Reading file as a data frame

ufos <- read.delim("ufo_awesome.tsv", sep='\t', stringsAsFactors=FALSE, header=FALSE, na.strings="")

# We can have a look at the data with head function
head(ufos)

# Assigning column names
names(ufos) <- c("DateOccurred","DateReported","Location","ShortDescription", "Duration","LongDescription")

# Formatting the Date Columns

# Looking at the defect entries in the Date columns
head(ufos[which(nchar(ufos$DateOccurred) != 8 | nchar(ufos$DateReported) != 8), 1]) 

# Removing the malfunctioned rows
clean_rows <- ifelse((nchar(ufos$DateOccurred)!=8 | nchar(ufos$DateReported)!=8), FALSE, TRUE)

# Keeping only the best rows
ufos <- ufos[clean_rows,]

# Type casting to Date types
ufos$DateOccurred <- as.Date(ufos$DateOccurred, format = "%Y%m%d")
ufos$DateReported <- as.Date(ufos$DateReported, format = "%Y%m%d")

head(ufos)

# The location attribute looks a bit messy lets clean it up
# Looking at the attribute most of the location are in the format City, State
# So lets keep only those which are of that type

clean_location <- function(text)
{
  split_location <- strsplit(text,",")[[1]]
  cleaned_text <- gsub("^ ", "", split_location)
  if (length(cleaned_text) > 2)
    {return(c(NA, NA))}
  else
    {return(cleaned_text)}
}

locations <- lapply(ufos$Location, clean_location)
head(locations)

# locations is a list which has city and state, we will devide it
locs <- do.call(rbind, locations)
ufos <- transform(ufos, City=locs[, 1], State=tolower(locs[, 2]))

# Since most of the ufo visits are in US lets keep US States only, even aliens are racists

us_states <- c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il", "in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh", "nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt","wa","wi","wv","wy")

ufos$State <- us_states[match(ufos$State, us_states)]
ufos$City[is.na(ufos$State)] <- NA

# Now let us only consider those entries where no value in the attribute is NULL
cleaned_ufo <- subset(ufos, !is.na(ufos$State))

# Statistics
summary(ufos$DateOccurred)
summary(ufos$DateReported)

# Basic Plotting
histogram <- ggplot(cleaned_ufo, aes(x=DateOccurred))+geom_histogram()+scale_x_date(major="50 years")

# Since most of the sightings were reported from 1990 onward, lets take a subset of that
ufos_after_1990 <- subset(cleaned_ufo, DateOccurred>=as.Date("1990-01-01"))

# Now lets check if the sightings are Seasonal by aggregating date on year and month
cleaned_ufo$YearMonth <- strftime(cleaned_ufo$DateOccurred, format="%Y-%m")

# Lets group the data by State and YearMonth
count_of_sightings<-ddply(cleaned_ufo,.(State,YearMonth), nrow)
head(sightings.counts)

# There are several Year-Month and state combinations for which there are no sightings
date_range <- seq.Date(from = as.Date(min(cleaned_ufo$DateOccurred)), to = as.Date(max(cleaned_ufo$DateOccurred)), by = "month")
date_strings <- strftime(date_range, "%Y-%m")

# Lets fill the missing dates
states_dates <- lapply(state.abb, function(s) cbind(s, date_strings))
states_dates <- data.frame(do.call(rbind, states_dates), stringsAsFactors = FALSE)

# Using merge to take the counts we have
all_sightings <- merge(states_dates, sightings_counts, by.x = c("s", "date_strings"), by.y = c("State", "YearMonth"), all = TRUE)

# cleaning merged data
names(all_sightings) <- c("State", "YearMonth", "Sightings")

# Covert the NAs to 0's
all_sightings$Sightings[is.na(all_sightings$Sightings)] <- 0

# Reset the character Year-Month to a Date objects
all_sightings$YearMonth <- as.Date(rep(date_range, length(state.abb)))

all_sightings$State <- as.factor(all_sightings$State)

# Inspecting trends visually

state_plots <- ggplot(all_sightings, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")
