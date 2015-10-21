##### Import .xlsx file and store to 'data' variable #####
require(xlsx)
require(tm)
require(SnowballC)
require(scales)
require(ggvis)
require(dplyr)
require(readxl)

# setwd("C:/Users/Charlie/Documents/Booz/Dashboard/Shiny/words")
# library(shiny)
# library(shinyapps)
# deployApp()


# data <- read.xlsx("C:/Users/Charlie/Documents/Booz/Dashboard/Shiny/newdata/final.xlsx",
#                   sheetIndex = 3)
# data <- read.xlsx("./final.xlsx", sheetIndex = 3)
data <- read_excel("./final.xlsx", 3)

trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)
data$System <- trim.leading(data$System)
data$System <- trim.trailing(data$System)
data$System[141] <- "ArboNET-Eastern Equine Encephalitis Surveillance"

data$accessibility <- trim.trailing(data$accessibility)

hist_data <- data

##### Convert factor variables to character #####
factors <- sapply(data, is.factor)
data[factors] <- lapply(data[factors], as.character)

# Create second data set to keep in the original format
data2 <- data

##### Extract words from free text variables into lists #####
freetext_var <- c("analytics", "conditions", "data_source",
                  "data_submitted_via", "dissem", "IT_System", 
                  "visualization", "updates")

data$analytics <- strsplit(data$analytics, " ")
data$conditions <- strsplit(data$conditions, " ")
data$data_source <- strsplit(data$data_source, " ")
data$data_submitted_via <- strsplit(data$data_submitted_via, " ")
data$dissem <- strsplit(data$dissem, " ")
data$IT_system <- strsplit(data$IT_system, " ")
data$visualization <- strsplit(data$visualization, " ")
data$updates <- strsplit(data$updates, " ")
data$accessibility <- strsplit(data$accessibility, " ")

# Rename lists
names(data$analytics) <- data$System
names(data$conditions) <- data$System
names(data$data_source) <- data$System
names(data$data_submitted_via) <- data$System
names(data$dissem) <- data$System
names(data$IT_system) <- data$System
names(data$visualization) <- data$System
names(data$updates) <- data$System
names(data$accessibility) <- data$System

# Get rid of punctuation and stopwords
tmp <- data.frame()
for (i in 1:length(data$conditions)) {
    tmp <- tolower(gsub("[^[:alnum:]]|â|[0-9]", "", data$conditions[[i]]))
    tmp <- removeWords(tmp, c(stopwords("en"), "etc", "health", "data",
                              "also", "unk"))
    tmp <- tmp[nchar(tmp) > 1]
    data$conditions[[i]] <- tmp
}

tmp <- data.frame()
for (i in 1:length(data$data_source)) {
    tmp <- tolower(gsub("[^[:alnum:]]|â|[0-9]", "", data$data_source[[i]]))
    tmp <- removeWords(tmp, c(stopwords("en"), "etc", "health", "data",
                              "also", "unk"))
    tmp <- tmp[nchar(tmp) > 1]
    data$data_source[[i]] <- tmp
}

tmp <- data.frame()
for (i in 1:length(data$data_submitted_via)) {
    tmp <- tolower(gsub("[^[:alnum:]]|â|[0-9]", "", data$data_submitted_via[[i]]))
    tmp <- removeWords(tmp, c(stopwords("en"), "etc", "health", "data",
                              "also", "unk"))
    tmp <- tmp[nchar(tmp) > 1]
    data$data_submitted_via[[i]] <- tmp
}

tmp <- data.frame()
for (i in 1:length(data$IT_system)) {
    tmp <- tolower(gsub("[^[:alnum:]]|â|[0-9]", "", data$IT_system[[i]]))
    tmp <- removeWords(tmp, c(stopwords("en"), "etc", "health", "data",
                              "also", "unk"))
    tmp <- tmp[nchar(tmp) > 1]
    data$IT_system[[i]] <- tmp
}

tmp <- data.frame()
for (i in 1:length(data$visualization)) {
    tmp <- tolower(gsub("[^[:alnum:]]|â|[0-9]", "", data$visualization[[i]]))
    tmp <- removeWords(tmp, c(stopwords("en"), "etc", "health", "data",
                              "also", "unk"))
    tmp <- tmp[nchar(tmp) > 1]
    data$visualization[[i]] <- tmp
}

tmp <- data.frame()
for (i in 1:length(data$dissem)) {
    tmp <- tolower(gsub("[^[:alnum:]]|â|[0-9]", "", data$dissem[[i]]))
    tmp <- removeWords(tmp, c(stopwords("en"), "etc", "health", "data",
                              "also", "unk"))
    tmp <- tmp[nchar(tmp) > 1]
    data$dissem[[i]] <- tmp
}

tmp <- data.frame()
for (i in 1:length(data$analytics)) {
    tmp <- tolower(gsub("[^[:alnum:]]|â|[0-9]", "", data$analytics[[i]]))
    tmp <- removeWords(tmp, c(stopwords("en"), "etc", "health", "data",
                              "also", "unk"))
    tmp <- tmp[nchar(tmp) > 1]
    data$analytics[[i]] <- tmp
}

tmp <- data.frame()
for (i in 1:length(data$updates)) {
    tmp <- tolower(gsub("[^[:alnum:]]|â|[0-9]", "", data$updates[[i]]))
    tmp <- removeWords(tmp, c(stopwords("en"), "etc", "health", "data",
                              "also", "unk"))
    tmp <- tmp[nchar(tmp) > 1]
    data$updates[[i]] <- tmp
}

tmp <- data.frame()
for (i in 1:length(data$accessibility)) {
    tmp <- tolower(gsub("[^[:alnum:]]|â|[0-9]", "", data$accessibility[i]))
    tmp <- removeWords(tmp, c(stopwords("en"), "etc", "health", "data",
                              "also", "unk"))
#     tmp <- tmp[nchar(tmp) > 1]
    data$accessibility[i] <- tmp
}


##### Text Counter function #####
text.counter <- function(text, var) {
    as.numeric(lapply(names(data[, var]),
                      function(x) sum(grepl(text,
                                            data[, var][[x]],
                                            ignore.case = TRUE))))
}

##### Graph 1 #####
text.finder <- function(text, var) {
    grepl(text, data[, var], ignore.case = TRUE)
}

##### Create blank theme for pie chart #####
blank_theme <- theme_minimal() +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
    )

##### Contains keyword table #####
sys.contains <- function(text, var) {
    data[, "System"][grepl(text, data[, var],
                           ignore.case = TRUE)]
}

contains.table <- function(text, var) {
    
    # Unlist data
    key_dat <- unlist(data[, var])
    
    # Obtain names of systems which mention the keyword
    tmp <- vector()
    for (i in 1:length(key_dat)) {
        tmp <- rbind(tmp, key_dat[i] == text)
    }
    
    # Get rid of trailing numbers from system names
    names(key_dat) <- gsub("[0-9]*$", "", names(key_dat))
    
    # Generate table with system info
    freq_key.tbl <- data[data$System %in% names(key_dat)[tmp],  
                         c("System", "owner_type", "type","Species", "surv_category")]
    
    # Number of times each system mentions the keyword
    Count <- vector()
    for (i in unique(names(key_dat[tmp]))) {
        Count <- rbind(Count, sum(grepl(i, 
                                        names(key_dat)[tmp], 
                                        fixed = TRUE)))
    }
    
    # Add and sort by Count data
    freq_key.tbl <- data.frame(Count, freq_key.tbl)
    freq_key.tbl <- freq_key.tbl[order(freq_key.tbl$Count, decreasing = TRUE), ]
    
    # Change names and print
    names(freq_key.tbl) <- c("Keyword Count", "System", "Owner Type", 
                             "Type","Species", "Purpose")
    freq_key.tbl
}

# Two keywords table
sys.contains2 <- function(text1, text2, var) {
    data2[, "System"][grepl(paste(text1, text2), 
                            data2[, var], 
                            perl = TRUE,
                            ignore.case = TRUE)]
}

contains.table2 <- function(text1, text2, var) {
    contains.df <- as.data.frame(sys.contains2(text1, text2, var))
    colnames(contains.df) <- "System"
    sys.yes <- as.character(data2[, "System"]) %in% contains.df[, "System"]
    contains.df[, c("Owner Type", "Type", "Species", "Purpose")] <- 
        data2[sys.yes, c("owner_type", "type", "Species", "surv_category")]
    contains.df
}


# Three keywords table
sys.contains3 <- function(text1, text2, text3, var) {
    data2[, "System"][grepl(paste(text1, text2, text3), 
                            data2[, var], 
                            perl = TRUE,
                            ignore.case = TRUE)]
}

contains.table3 <- function(text1, text2, text3, var) {
    contains.df <- as.data.frame(sys.contains3(text1, text2, text3, var))
    colnames(contains.df) <- "System"
    sys.yes <- as.character(data2[, "System"]) %in% contains.df[, "System"]
    contains.df[, c("Owner Type", "Type", "Species", "Purpose")] <- 
        data2[sys.yes, c("owner_type", "type", "Species", "surv_category")]
    contains.df
}

# Names for categories in the table
cat.names <- c("Keyword Count", "System", "Owner Type", "Type", "Species", "Purpose")

# Multiple keyword table (Take out "Keyword Count")
cat.names2 <- cat.names[-1]

##### Variable labels #####
label.names <- c("System" = "System",
                 "status" = "Current Status",
                 "Species" = "Species",
                 "type" = "System Type",
                 "surv_category" = "Purpose",
                 "owner_type" = "Owner Type",
                 "owner_name" = "Owner Name",
                 "geo_cover" = "Geographic Coverage",
                 "conditions" = "Conditions",
                 "data_source" = "Data Source",
                 "data_submitted_via" = "Submission Mechanism",
                 "IT_system" = "IT System",
                 "visualization" = "Visualization",
                 "dissem" = "Dissemimation",
                 "analytics" = "Analytics",               
                 "updates" = "Update Frequency",
                 "est" = "Year Established",
                 "accessibility" = "Accessibility",
                 "level_doc" = "Level of Documentation")

##### Histogram #####
freq.dtf <- function(var) {
    dat <- as.data.frame(hist_data[, var])
    corpus <- Corpus(DataframeSource(dat))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, function(x) removeWords(x, c(stopwords("en"), 
                                                          "etc", "health", "data",
                                                          "also", "unk")))
    corpus <- tm_map(corpus, PlainTextDocument)
    tdm <- TermDocumentMatrix(corpus)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    assign(paste0("freq_key.", var), data.frame(word = names(v), freq = v),
           envir = .GlobalEnv)
}

# Create frequency counts for keywords in selected variables
hist.varnames <- c("conditions", "data_source", "data_submitted_via", "IT_system",
                   "visualization", "dissem", "analytics", "updates", "accessibility")

combine.rows <- function(var, outrow, gonerow) {
    if (is.na(get(paste0("freq_key.", var))[gonerow, "freq"]) == FALSE &
            is.na(get(paste0("freq_key.", var))[outrow, "freq"]) == FALSE) {
        tmp <- get(paste0("freq_key.", var))
        tmp[outrow, "freq"] <- tmp[outrow, "freq"] + tmp[gonerow, "freq"]
        if (is.na(tmp[outrow, "word"]) == FALSE) {    
            assign(paste0("freq_key.", var), tmp[row.names(tmp) != gonerow, ],
                   envir = .GlobalEnv)
        }
    }
}

for (i in hist.varnames) {
    freq.dtf(i)
}

my_hist <- function(var, n = 10) {
    ggplot(data = get(paste0("freq_key.", var))[1:n, ], 
           aes(x = reorder(word, -freq), y = freq))
}

cat.varnames <- c("owner_type", "type", "Species", "surv_category", "level_doc", 
                  "status", "owner_name", "geo_cover", "updates", "accessibility")

cat.freq <- function(var) {
    tmp.tbl <- as.data.frame(table(data2[, var]))
    
    assign(paste0("freq_cat.", var), 
           data.frame(tmp.tbl, 
                      label = paste0(tmp.tbl$Var1, " (", 
                      tmp.tbl$Freq, ", ",
                      format(round(100 * tmp.tbl$Freq / sum(tmp.tbl$Freq), 1), nsmall = 1),
                      "%)")), 
           envir = .GlobalEnv)
}

for (i in cat.varnames) {
    cat.freq(i)
}

##### Reorder updates #####
update_order <- c("near real time",
                  "8 min",
                  "10 min",
                  "15 min",
                  "60 min",
                  "12 hrs",
                  "24 hrs",
                  "48 hrs",
                  "weekly",
                  "10 days",
                  "monthly",
                  "quarterly",
                  "3 - 4 times per year",
                  "annual",
                  "2 years",
                  "5 years",
                  "as needed",
                  "varies",
                  "UNK",
                  "N/A")

freq_cat.updates[15, "Freq"] <- freq_cat.updates[15, "Freq"] + freq_cat.updates[16, "Freq"]
freq_cat.updates <- freq_cat.updates[row.names(freq_cat.updates) != 16, ]

freq_cat.updates <- freq_cat.updates %>% slice(match(update_order, Var1))
freq_cat.updates$order <- 1:nrow(freq_cat.updates)

##### Table of unknowns #####
unks <- function(var) {
    data$System[data2[, var] == "UNK"]
}

# Analytics and IT System
an <- unks("analytics")
it <- unks("IT_system")
an <- c(an, rep(NA, length(it) - length(an)))

both <- an[an %in% it]

an1 <- an[!(an %in% both)]
it1 <- it[!(it %in% both)]

an2 <- c(both, an1)
it2 <- c(both, it1)
unks.df <- data.frame(it2, an2)

colnames(unks.df) <- c("IT System", "Analytics")
