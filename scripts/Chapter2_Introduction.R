#install.packages("Lahman")
library(tidyverse)
library(dplyr)
library(Lahman)



# Create a data frame for Babe Ruth
bruth_pitch_df <- Pitching[Pitching$playerID=="ruthba01",]

# Slice and Select function
# bruth_df %>% slice(1:10) %>% select(yearID,G,AB,R,H,HR,BB,SO)

bruth_pitch_df %>% 
    summarize(
        LO = min(ERA),
        QL = quantile(ERA,.25), QU = quantile(ERA,.75),M = median(ERA),
        Hi = max(ERA)
    )

# Year of the lowerst ERA of Babe Ruth
bruth_pitch_df %>% filter(ERA==min(ERA)) %>% select(yearID)

# Adding new column "FIP", Fielding independent pitching.
bruth_pitch_df <- bruth_pitch_df %>% 
    mutate(FIP = (13 * HR + 3 * BB -2 * SO)/IPouts) 

# Sort the data by FIP(ascending)
bruth_pitch_df %>% 
    arrange(FIP) %>% 
    select(yearID,W,L,ERA,FIP) %>% 
    head(10)

bruth_pitch_df %>% 
    group_by(teamID) %>% 
    summarize(mean_W = mean(W),
              mean_L = mean(L),
              mean_ERA = mean(ERA),
              mean_FIP = mean(FIP))


# Vector
Win.Pct <- 100 * bruth_pitch_df$W / (bruth_pitch_df$W + bruth_pitch_df$L)

Age <- bruth_pitch_df$yearID - 1895
plot(Age,Win.Pct)
summary(Win.Pct)

# Character data and data frame
Year <- 2008:2017
NL <- c("PHI","PHI","SFN","SLN","SFN",
        "SLN","SFN","NYN","CHN","LAN")
AL <- c("TBA","NYA","TEX","TEX","DET",
        "BOS","KCA","KCA","CLE","HOU")
Winner <- c("NL","AL","NL","NL","NL",
            "AL","NL","AL","NL","AL")
N_Games <- c(5,6,5,7,4,7,7,5,7,7)

# Create a data frame
WS_results <- tibble(
    Year = Year, NL_Team =  NL, AL_Team = AL,
    N_Games = N_Games, Winner = Winner)

# Find patterns
grep("NY",c(AL,NL),value=TRUE)

WS <- WS_results %>% 
    group_by(Winner) %>% 
    summarize(N=n())
WS

# plot bar chart
ggplot(WS,mapping = aes(x=Winner,y=N)) +
    geom_col()

# Factors
WS_results %>% 
    group_by(NL_Team) %>% 
    summarize(N=n())

WS_results <- WS_results %>% 
    mutate(NL_Team = factor(NL_Team, levels = c("NYN","PHI","CHN",
                                                "SLN","LAN","SFN")))
str(WS_results$NL_Team)
WS_results %>% 
    group_by(NL_Team) %>% 
    summarize(N=n())

# Lists

world_series <- list(Winner=Winner, Number.Games=N_Games, Seasons="2008 to 2017")
world_series

# Frequency of number of games (less than 8) in 1903.
ws <- filter(SeriesPost, yearID >= 1903,
       round == "WS", wins+losses < 8)

ggplot(ws,mapping = aes(x=wins+losses)) + 
    geom_bar(fill="blue") +
    labs(x="Number of games", y="Frequency")


# Calculate Home run rate (Micky mantle)
hr_rates <- function(age,hr,ab){
    rates <- round(100 * hr / ab, 1)
    list(x=age, y=rates)
}

HR <- c(13,23,21,27,37,52,34,42,31,40,54)
AB <- c(341,549,461,543,517,533,474,519,541,527,514)
Age <- c(19:29)

# Scatter plot
plot(hr_rates(Age,HR,AB))
hr_rates <- hr_rates(Age,HR,AB)

# Writing csv file
Mantle <- data.frame(Age, HR,AB,Rates=hr_rates$y)
write.csv(Mantle,"csv_files/mantle.csv")


##################################################

# Splitting, Applying, and Combining data

# Batting data between 1960 and 1969.
Batting %>% 
    filter(yearID>=1960, yearID <=1969) -> Batting_60

# Total number of homeruns for each player
Batting_60 %>% 
    group_by(playerID) %>% 
    summarize(Total_HR = sum(HR)) -> hr_60


# Sort the hr_60 data in desc order
hr_60 %>% 
    arrange(desc(Total_HR))->hr_60

head(hr_60)


# Iterating using map()
hr_leader <- function(data){
    data %>% 
        group_by(playerID) %>% 
        summarize(Total_HR = sum(HR)) %>% 
        arrange(desc(Total_HR)) %>% 
        head(1)
}

# Home run leader for each decade.
Batting %>% 
    mutate(decade = 10 * floor(yearID/10)) %>% 
    split(pull(.,decade)) %>% 
    map_df(hr_leader, .id="decade") -> hr_by_decade

# Collect the career batting statistics
Batting %>% 
    group_by(playerID) %>% 
    summarize(tAB = sum(AB,na.rm = TRUE),
              tHR = sum(HR,na.rm = TRUE), 
              tSO = sum(SO,na.rm = TRUE)) -> long_careers

# filter tAB >= 5000 players 
Batting_5000 <- filter(long_careers, tAB >= 5000)
head(Batting_5000)

# Correlation between HR rates & SO rates
ggplot(Batting_5000, mapping = aes(x=tHR/tAB, y=tSO/tAB))+ 
    geom_point() + geom_smooth()

####################################################

# Exercises

## 1. Top Base Stealers in the Hall of Fame
# (a) Create a data frame
players <- c("Rickey Henderson","Lou Brock","Ty Cobb","Eddie Collins","Max Carey","Joe Morgan","Luis Aparico","Paul Molitor","Roberto Alomar")
SB <- c(1406,938,897,741,738,689,506,504,474)
CS <- c(335,307,212,195,109,162,136,131,114)
G <- c(3081,2616,3034,2826,2476,2649,2599,2683,2379)
sb_df <- data.frame(players,SB,CS,G)
sb_df

# (b) Create New column "SB.Attempt" (SB+CS)
sb_df <- sb_df %>% 
    mutate(SB.Attempt = SB + CS)

# (c) Create New column "SB.Game" (SB/G) Stolen bases per game
sb_df <- sb_df %>% 
    mutate(SB.Game = SB / G)

sb_df <- sb_df %>% 
    mutate(SB.SuccessRate = 100 * SB / SB.Attempt)

install.packages("ggrepel")
library(ggrepel)

ggplot(sb_df, mapping = aes(x=SB.Game,y=SB.SuccessRate))+
    geom_point() + geom_label_repel(aes(label = players), size = 3)


# 2. Character, Factor, and Logical Variables in R
outcomes <- c("Single","Out","Out","Single","Out","Double","Out","Walk","Out","Single")

# Construct a frequency table
table(outcomes)

# Ordered from least_successful to most-successful
f.outcomes <- factor(outcomes,levels = c("Out","Walk","Single","Double"))
table(f.outcomes)

outcomes == "Walk"
sum(outcomes == "Walk")


# 3. Pitches in the 350-Wins Club

#(a) Create vectors
p_players <- c("Pete Alexander","Roger Clements","Pud Galvin","Walter Johnson","Greg Maddux","Christy Mathewson","Kid Nichols","Warren Spahn","Cy Young")
W <- c(373,354,364,417,355,373,361,363,511)
L <- c(208,184,310,279,227,188,208,245,316)
SO <- c(2198,4672,1806,3509,3371,2502,1868,2583,2803)
BB <- c(951,1580,745,1363,999,844,1268,1434,1217)
# (b) Calculate Winning percentage
Win.PCT = (100 * W/W+L)

# (c) Create a data frame
Wins.350 <- data.frame(p_players,W,L,Win.PCT)

# (d) Sort the data by Win.PCT 
Wins.350 <- Wins.350 %>% arrange(Win.PCT)
Wins.350


# 4. Pitchers in the 350-Wins Club, Continued
# (b)Create a vecor strikeout-walk ratio
SO.BB.Ratio <- SO/BB

# (c)Create a data frame
SO.BB <- data.frame(p_players,SO,BB,SO.BB.Ratio)

# (d) filter the data who had strikeout-ratio more than 2.8.
SO.BB<- SO.BB %>% filter(SO.BB.Ratio > 2.8)

# (e) Sort by Walk
SO.BB <- SO.BB %>% arrange(desc(BB))

SO.BB               


# 5. Pitcher Strikeout/Walk Ratios
# (a) Read Pitching file
head(Pitching)

# (b) Compute the cumulative strikeouts, cumulative walks,mid career year, 
# and the total innings pitched for all pitchers on the data file.

career_pitching <- Pitching %>% 
    group_by(playerID) %>% 
    summarize(SO = sum(SO, na.rm = TRUE),
              BB = sum(BB, na.rm = TRUE),
              IPouts = sum(IPouts, na.rm = TRUE),
              midYear = median(yearID, na.rm = TRUE))

# Merge data sets
career_pitching <- inner_join(Pitching,career_pitching,by="playerID")

# (c) filter data (IPouts >= 10000)
career_pitching %>% 
    filter(IPouts.y >10000) -> career.10000

head(career.10000)

# (d) Scatter plot
ggplot(career.10000, mapping = aes(x=midYear, y=SO.y/BB.y))+
    geom_point() + geom_smooth()
