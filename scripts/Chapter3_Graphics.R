# Chapter 3 - Graphics

library(tidyverse)
library(dplyr)

hof <- read_csv("data/csv_files/hofbatting.csv")

# 3.2 Character Variable
# 3.2.1 Bar graph

# Create columns "MidCareer" and "Era".
hof <- hof %>% 
    mutate(MidCareer = (From + To) / 2,
           Era = cut(MidCareer,
                     breaks = c(1800,1900,1919,1941,1960,1976,1993,2050),
                     labels = c("19th Century","Dead Ball",
                                "Lively Ball","Integration",
                                "Expansion","Free Agency","Long Ball")))

# Frequency table of variable Era.
hof_eras <- summarize(group_by(hof,Era), N=n())
hof_eras

# Plot a bar graph
ggplot(hof,mapping = aes(x=Era)) +
    geom_bar(fill="blue")

# 3.2.2 Add axes labels and a title
ggplot(hof,aes(Era))+
    geom_bar(fill="blue")+
    xlab("Baseball Era")+
    ylab("Frequency")+
    ggtitle("Era of the Nonpitching Hall of Famers")

# 3.2.3 Other graphs of a character variable
ggplot(hof_eras,aes(Era,N))+
    geom_point() + 
    xlab("Baseball Era")+
    ylab("Frequency")+
    ggtitle("Era of the Nonpitching Hall of Famers")+
    coord_flip()


# 3.3 Saving Graphs
ggplot(hof,aes(Era))+
    geom_bar(fill="blue")+
    xlab("Baseball Era")+
    ylab("Frequency")+
    ggtitle("Era of the Nonpitching Hall of Famers")
ggsave("graphs/bargraph.png")


# Saving 2 graphs in a pdf file 
pdf("graphs/graphs.pdf")
ggplot(hof,aes(Era))+geom_bar()
ggplot(hof_eras,aes(Era,N))+geom_point()
dev.off()

# 3.4 Numeric Varible: One-Dimensional Scatterplot and Histogram

# One-Dimentional scatterplot
ggplot(hof, aes(x=OPS, y=1))+
    geom_jitter(hight = 0.6) + ylim(-1,3) + 
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) + 
    coord_fixed(ratio = 0.03)
    
# Histogram
ggplot(hof,aes(x=OPS))+
    geom_histogram()

# Setting bins with a histogram
ggplot(hof,aes(x=OPS))+
    geom_histogram(breaks = seq(0.4,1.2,by=0.1),
                   color = "blue", fill = 'white')

# 3.5 Two Numeric Variables
# 3.5.1 Scatterplot 

ggplot(hof, aes(MidCareer, OPS)) + 
    geom_point() + geom_smooth()


# identify the players with extreme values
library(ggrepel)

ggplot(hof, aes(MidCareer, OPS)) + 
    geom_point() +
    geom_smooth() + 
    geom_text_repel(data = filter(hof,OPS > 1.05 | OPS <.5),
                    aes(MidCareer,OPS,label=...2))

# 3.5.2 Building a graph, step-by-step

p <- ggplot(hof,aes(OBP,SLG))+geom_point()

p

p <- p + 
    xlim(0.25,0.50) + ylim(0.28, 0.75) +
    xlab("On Base Percentage") +
    ylab("Slugging Percentage")
p

p <- p + 
    scale_x_continuous("On Base Percentage",
                       limits = c(0.25, 0.50))+
    scale_y_continuous("Slugging Percentage",
                       limits = c(0.28, 0.75))

p

p <- p + geom_abline(slope = -1,
                     intercept = seq(0.7, 1, by=0.1))

p

# annotate the ops values
p + annotate("text",
             x = rep(.27,4),c(.42,.52,.62,.72),
             label =paste("OPS = ",
                          c(0.7,0.8,0.9,1.0)))


# Another way of creating the graph.
ops_labels <- tibble(
    OBP = rep(0.3,4),
    SLG = seq(0.4,0.7,by = 0.1),
    label = paste("OPS =", OBP + SLG)
)

p + geom_text(data = ops_labels, hjust = "right",
              aes(label=label))


# 3.6 A Numeric Variable and a Factor Variable

hof <- hof %>% 
    mutate(hr_rate = HR/AB)

# 3.6.1 Parallel stripcharts
ggplot(hof,aes(hr_rate,Era)) + 
    geom_jitter(height = 0.1)

# 3.6.2 Parallel boxplots
ggplot(hof, aes(Era,hr_rate)) + 
    geom_boxplot() + coord_flip()



## 3.7 Comparing Ruth, Aaron, Bonds, and A-Rod
# 3.7.1
library(Lahman)

# Creating a function to get birth year.
get_birthyear <- function(Name) {
    Names <- unlist(strsplit(Name," "))
    Master %>% 
        filter(nameFirst == Names[1],
               nameLast == Names[2]) %>% 
        mutate(birthyear = ifelse(birthMonth >= 7,
                                  birthYear + 1, birthYear),
               Player = paste(nameFirst, nameLast)) %>% 
        select(playerID,Player, birthyear)
}

# Run the function 
PlayerInfo <- bind_rows(get_birthyear("Babe Ruth"),
                        get_birthyear("Hank Aaron"),
                        get_birthyear("Barry Bonds"),
                        get_birthyear("Alex Rodriguez"))

PlayerInfo


# 3.7.2 Creating the player data frames

Batting %>% 
    inner_join(PlayerInfo, by = "playerID") %>% 
    mutate(Age = yearID - birthyear) %>% 
    select(Player, Age, HR) %>% 
    group_by(Player) %>% 
    mutate(CHR = cumsum(HR)) -> HRdata

HRdata


# Cumulative home run counts against age for four players.
ggplot(HRdata, aes(x=Age, y=CHR, linetype=Player))+
    geom_line()


## 3.8 The 1998 Home Run Race

# Read Data
fields <- read_csv("data/csv_files/fields.csv")
data1998 <- read_csv("data/csv_files/all1998.csv", col_names = pull(fields,Header))

# Extract playerID
sosa_id <- Master %>% 
    filter(nameFirst == "Sammy", nameLast == "Sosa") %>% 
    pull(retroID)

mac_id <- Master %>% 
    filter(nameFirst == "Mark", nameLast == "McGwire") %>% 
    pull(retroID)


hr_race <- data1998 %>% 
    filter(BAT_ID %in% c(sosa_id, mac_id))

hr_race

library(lubridate)

# Function to extract Date and Home run count.
cum_hr <- function(d){
    d %>% 
        mutate(Date = ymd(str_sub(GAME_ID, 4, 11))) %>% 
        arrange(Date) %>% 
        mutate(HR = ifelse(EVENT_CD == 23, 1, 0),
               cumHR = cumsum(HR)) %>% 
        select(Date, cumHR)
}

hr_ytd <- hr_race %>% 
    split(pull(., BAT_ID)) %>% 
    map_df(cum_hr,.id="BAT_ID") %>% 
    inner_join(Master, by = c("BAT_ID" = "retroID"))
hr_ytd

# Line plot 
ggplot(hr_ytd,aes(Date, cumHR, linetype = nameLast))+
    geom_line() +
    geom_hline(yintercept = 62, color ='blue') +
    annotate("text",ymd("1998-04-15"),65,
             label='62', color='blue') +
    ylab("Home Runs in the Season")

# 3.10 Exercises

# 1. Hall of Fame Pitching Dataset
hofpitching <- read_csv("data/csv_files/hofpitching.csv")

# BF = the number of batters faced by a pitcher in his career.
hofpitching <- hofpitching %>% 
    mutate(BF.group = cut(BF, c(0, 10000, 15000, 20000, 30000),
                          labels = c("Less than 10000","(10000,15000)",
                                     "(15000,20000)","More than 20000")))


# (a) Construct a freq table of BF.group using summarize() function.
BF_group_freq <- hofpitching %>% 
    group_by(BF.group) %>% 
    summarize(freq = n())
BF_group_freq 

# (b) Construct a bar graph of the output from summarize().
ggplot(BF_group_freq, aes(BF.group, freq))+
    geom_col(fill = 'blue')

# (c) Construct an alternative graph of the BF.group variable.
# Compare the effectiveness of the bar graph and the new graph in compare the frequencies in the four intervals.
ggplot(BF_group_freq, aes(freq, BF.group))+
    geom_point()



# 2. Hall of Fame Pitching Dataset (Continued)

# (a) Using the geom_histogram() function, construct a histogram of WAR
# for the pitchers in the HOF dataset.
ggplot(hofpitching,aes(WAR)) + 
    geom_histogram(breaks = seq(0,170,by=5),
                   color = "blue", fill = 'white')

# (b) There are two pitchers who stand out among all of the HOF on the total WAR variable.
# Identify those two pitchers.
library(ggrepel)
q <- ggplot(hofpitching,aes(WAR)) + 
    geom_histogram(breaks = seq(0,170,by=5),color = "blue", fill = 'white') + 
    geom_text_repel(data=filter(hofpitching,WAR > 140),
                        aes(WAR+15,2,label = ...2), size=3)

q

# 3. Hall of Fame Pitching Dataset (Continued)
hofpitching <- hofpitching %>% 
    mutate(WAR.Season = WAR / Yrs)

# (a)geom_point()
ggplot(hofpitching,aes(x=WAR.Season,y=BF.group))+
    geom_point()
    

# (b) geom_boxplot()
ggplot(hofpitching,aes(WAR.Season, BF.group)) +
    geom_boxplot() 


# 4. Hall of Fame Pitching Dataset (Continued)
hofpitching <- hofpitching %>% 
    mutate(midYear = (From + To) / 2)
    
hofpitching.recent <- hofpitching %>% 
    filter(midYear >= 1960)

# (a) Sort the data by WAR.Season
hofpitching.recent %>% 
    arrange(WAR.Season) -> hofpitching.recent


# (b) Construct a dot plot of the values of WAR.Season where the labels are players name.
ggplot(hofpitching.recent, aes(WAR.Season, y=1))+
    geom_jitter() + 
    geom_text_repel(aes(WAR.Season, label = ...2), max.overlaps = Inf, size=2.5)
    
# (c) Which two 1960+ pitchers stand out with respect to wins above replacement per season?

tail(hofpitching.recent,2)[2]


# 5. Hall of Fame Pitching Dataset (Continued)

# (a) Construct a scatterplot of MidYear(horizontal) 
#against WAR.Season(vertical)
ggplot(hofpitching.recent, aes(midYear, WAR.Season)) + 
    geom_point()

# (b) 
ggplot(hofpitching.recent, aes(midYear, WAR.Season)) + 
    geom_point() + geom_smooth()

# (c) There are two pitchers whose mid Careers were in the 1800s 
# who had relatively low WAR.Season values. Find out by using filter and geom_text functions.
ggplot(hofpitching.recent, aes(midYear, WAR.Season)) + 
    geom_point() +
    geom_label_repel(data=filter(hofpitching, midYear >=1980, WAR.Season < 2.0),
                    aes(midYear, WAR.Season,label=...2),size=3)

# 6. Working with the Lahman batting dataset
# (a) Read Master and Batting datasets

# (b) Collect a single dataframe the season batting statistics for the great hitters.
# Ty Cobb, Ted Williams, and Pete Rose.

cobb_id <- Master %>% filter(nameFirst=="Ty",nameLast=="Cobb") %>% 
    pull(playerID)

williams_id <- Master %>% filter(nameFirst=="Ted",nameLast=="Williams") %>% 
    pull(playerID)

rose_id <- "rosepe01"

season_batting_stats <- Batting %>% 
    filter(playerID %in% c(cobb_id,williams_id,rose_id))

season_batting_stats

# (c) Add the variable Age to each data frame 

players_info <- bind_rows(get_birthyear("Ty Cobb"),
                          get_birthyear("Ted Williams"),
                          get_birthyear("Pete Rose")
)

players_info

season_batting_stats %>% inner_join(players_info, by="playerID") %>% 
    mutate(Age = yearID - birthyear) -> season_batting_stats 
              

season_batting_stats %>% 
    group_by(playerID) %>% 
    mutate(cum_hit = cumsum(H)) -> season_batting_stats

# (d), (e) Create a line plot for three players.
ggplot(season_batting_stats, aes(Age, cum_hit, color=Player))+
    geom_line()


# 7. Working with the Retrosheet Play-by-Play Dataset

# (a) Create two data frames mac.data and sosa.data.
mac.data <- hr_race %>% filter(BAT_ID==mac_id)
sosa.data <- hr_race %>% filter(BAT_ID==sosa_id)

# (b) Filter the data frames to the plays where a batting event occurred.
mac.data <- filter(mac.data, BAT_EVENT_FL == TRUE)
sosa.data <- filter(sosa.data, BAT_EVENT_FL == TRUE)

# (c) For each data frame, create a new variable PA that 
#numbers the plate appearances 1,2, ...
mac.data <- mutate(mac.data, PA = 1:nrow(mac.data))
sosa.data <- mutate(sosa.data, PA = 1:nrow(sosa.data))

# (d) The following commands return the number of the plate appearances when the player hit home runs.
mac.HR.PA <- mac.data %>% 
    filter(EVENT_CD == 23) %>% 
    pull(PA)

sosa.HR.PA <- sosa.data %>% 
    filter(EVENT_CD == 23) %>% 
    pull(PA)

# (e) Using diff(), the following commands compute 
# the spacings between the occurrences of home runs.
mac.spacings <- diff(c(0,mac.HR.PA))
sosa.spacings <- diff(c(0,sosa.HR.PA))

# Create a new data frame HR_Spacing with two variable Player and Spacing.

Player <- c("Mark McGwire", "Sammy Sosa")
Spacings <- c(mac.spacings, sosa.spacings)

HR_Spacings <- data.frame(Player,Spacings)

ggplot(HR_Spacings) +
    geom_histogram(aes(Spacings, fill = Player))+
    scale_x_continuous(breaks = seq(0,50,5))+
    scale_y_continuous(breaks = seq(0,30,5))
