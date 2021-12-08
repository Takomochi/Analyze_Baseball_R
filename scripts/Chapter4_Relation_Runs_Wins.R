# Chapter 4 - The Relation Between Runs and Wins

# 4.2 The Teams Table in the Lahman Database
# install.packages("tidyverse")
library(tidyverse)
library(Lahman)

tail(Teams,3)

# Team performance since 2001
my_teams <- Teams %>% 
    filter(yearID>2000) %>% 
    select(teamID,yearID,lgID,G,W,L,R,RA)
my_teams %>% tail()

# Create new columns, RD and Wpct
my_teams %>% 
    mutate(RD = R - RA, Wpct = W / (W+L)) ->my_teams

ggplot(my_teams,aes(x=RD,y=Wpct))+
    geom_point() + geom_smooth()+
    scale_x_continuous("Run differential")+
    scale_y_continuous("Winning Percentage") -> run_diff
    

# 4.3 Linear Regression

linfit <- lm(Wpct ~RD, data=my_teams)
linfit

run_diff + 
    geom_smooth(method="lm", se=FALSE, color="blue")

library(broom)

my_teams_aug <- augment(linfit, data = my_teams)

base_plot <- ggplot(my_teams_aug, aes(x=RD, y=.resid)) +
    geom_point(alpha=0.3)+
    geom_hline(yintercept = 0, linetype=3)+
    xlab("Run differential")+ylab("Residual")

highlight_teams <- my_teams_aug %>% 
                        arrange(desc(abs(.resid))) %>% 
                                    head(4)

highlight_teams

library(ggrepel)

base_plot + 
    geom_point(data=highlight_teams, color = 'blue')+
    geom_text_repel(data = highlight_teams, color = 'blue',
                    aes(label=paste(teamID, yearID)))

resid_summary <- my_teams_aug %>% 
                summarize(N=n(), avg=mean(.resid),
                          RMSE = sqrt(mean(.resid^2)))

resid_summary

rmse <- resid_summary %>% 
    pull(RMSE)

# RMSE confirmation
my_teams_aug %>% 
    summarize(N=n(),
              within_one = sum(abs(.resid)< rmse),
              within_two = sum(abs(.resid)< 2 * rmse)) %>% 
    mutate(within_one_pct = within_one / N,
           within_two_pct = within_two / N)

# 4.4 The Pythagorean Formula for Winning Percentage.


my_teams <- my_teams %>% 
    mutate(Wpct_pyt = R^2 / (R^2 * RA^2))

my_teams <- my_teams %>% 
    mutate(residuals_pyt = Wpct - Wpct_pyt)

my_teams %>% 
    summarize(rmse = sqrt(mean(residuals_pyt^2)))


## 4.4.1 The Exponent in the Pythagorean model

my_teams %>% 
    mutate(logWratio = log(W/L),
           logRratio = log(R/RA)) -> my_teams

pytFit <- lm(logWratio ~ 0+logRratio, data=my_teams)
pytFit


# 4.4.2 Good and bad predictions by the Pythagorean model

# 2011, Boston Red Sox scored 875 runs, 737 allowed runs.
# 162 * 875^2 / 875^2 + 737^2 = 95 games


glheaders <- read_csv("data/csv_files/game_log_header.csv")
gl2011 <- read_csv("data/csv_files/gl2011.txt",
                   col_names = names(glheaders),
                   na = character())

BOS2011 <- gl2011 %>% 
    filter(HomeTeam == "BOS" | VisitingTeam == "BOS") %>% 
    select(VisitingTeam, HomeTeam, VisitorRunsScored,HomeRunsScore)


head(BOS2011)

BOS2011 <- BOS2011 %>% 
    mutate(ScoreDiff = ifelse(HomeTeam == "BOS",
                              HomeRunsScore - VisitorRunsScored,
                              VisitorRunsScored - HomeRunsScore),
           W = ScoreDiff > 0)


library(skimr)

BOS2011 %>% 
    group_by(W) %>% 
    skim(ScoreDiff)

results <- gl2011 %>% 
    select(VisitingTeam, HomeTeam,
           VisitorRunsScored, HomeRunsScore) %>% 
    mutate(winner = ifelse(HomeRunsScore > VisitorRunsScored,
                           HomeTeam, VisitingTeam),
           diff = abs(VisitorRunsScored -HomeRunsScore))


head(results)

one_run_wins <- results %>% 
    filter(diff == 1) %>% 
    group_by(winner) %>% 
    summarize(one_run_w = n())

one_run_wins

# relation between the Pythagorean residuals and the number of one-run victories.
teams2011 <- my_teams %>% 
    filter(yearID == 2011) %>% 
    mutate(teamID = ifelse(teamID == 'LAA', 'ANA',
                           as.character(teamID))) %>% 
    inner_join(one_run_wins, by = c('teamID' = 'winner'))


head(teams2011)

# plot
ggplot(data = teams2011,aes(x=one_run_w, y= residuals_pyt))+
    geom_point() + 
    geom_text_repel(aes(label=teamID)) + 
    xlab("One run wins") + ylab("Pythagorean residuals")


# Closers
top_closers <- Pitching %>% 
    filter(GF>50 & ERA < 2.5) %>% 
    select(playerID,yearID,teamID)

head(top_closers)

my_teams %>% 
    inner_join(top_closers) %>% 
    pull(residuals_pyt) %>% 
    summary()


# 4.5 How many Runs for a Win?
D(expression(G * R^2 / (R^2 + RA^2)),"R")

IR <- function(RS=5,RA=5){
    (RS^2+RA^2)^2/(2*RS*RA^2)
}

ir_table <- expand.grid(RS=seq(3,6,.5),
                        RA=seq(3,6,.5))
head(ir_table)
tail(ir_table)

ir_table %>% 
    mutate(IRW=IR(RS,RA)) %>% 
    spread(key = RA, value = IRW, sep = '=') %>% 
    round(1)



