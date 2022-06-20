### 00. SETUP #############################
library(cowplot)
library(tidyverse)
library(Metrics)
library(zoo)
library(glmnet)
library(HDCI)
library(gridExtra)
library(MLmetrics)
library(hms)
library(hdm)
library(bayestestR)
library(rstanarm)
library(mombf)
library(mvtnorm)
library(lme4)
library(quantreg)
library(bayesQR)
library(lmerTest)
library(merTools)
library(MuMIn)
library(cluster) #clustering
library(factoextra) # clustering algorithms & visualization

### 01. IMPORT DATA ####################
PATH_data <-  '/home/gigilovicu/Documents/masters/semester_1/statistics/project_submission/'
source(file.path(paste0(PATH_data, 'code/'),"routines (glm).R"))
set.seed(1993)
# Salaries
data_salaries_raw <- read_csv(paste0(PATH_data, 'salaries/salaries_1985to2018.csv'))
data_salaries_raw$team <- recode(data_salaries_raw$team, `Charlotte Bobcats` = "Charlotte Hornets")
data_salaries_players_raw <- read_csv(paste0(PATH_data, 'salaries/players.csv')) 

# Games data
data_games_raw <- read_csv(paste0(PATH_data, 'games_data/games_details.csv'))
data_games_summary_raw <- read_csv(paste0(PATH_data, 'games_data/games.csv'))

#Season data
data_season_large_raw <- read_csv(paste0(PATH_data, 'season_data/Seasons_Stats.csv')) %>% 
                         filter(Year >= 2005)

### 02. CLEAN DATA - SALARIES, SEASON PERFORMANCE, GAMES WON #####################
# SALARIES
#Player ids object to match with salary data 
id_players <- data_games_raw %>% dplyr::select(name = PLAYER_NAME, player_id = PLAYER_ID) %>% group_by(player_id) %>% 
              summarise(player_id = first(player_id), name = first(name))

# Team abbreviation as a key between salary data and games data
TEAM_ABBREVIATION <- data_salaries_raw %>% dplyr::select(team) %>% group_by(team) %>% summarise(team = first(team)) %>% arrange(team) %>% 
                     mutate(TEAM_ABBREVIATION = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET",
                                                  "GSW", "HOU", "IND", "KCK", "LAC", "LAL", "MEM", "MIA", "MIL",
                                                  "MIN", "NJN", "NOP", "NOP", "NOP", "NYK", "OKC", "ORL", "PHI",
                                                  "PHX", "POR", "SAC", "SAS", "OKC", "TOR", "UTA", "MEM", "WAS", "WAS", "NA"))
# Match ids initially - salary data
data_salaries <- data_salaries_raw %>% left_join(TEAM_ABBREVIATION, by = "team") %>% 
                 left_join(data_salaries_players_raw %>% dplyr::select(`_id`, name),
                           by = c('player_id' = '_id')) %>% filter(season_end >= 2004, season_end <= 2020) %>% 
                 dplyr::select(name, TEAM_ABBREVIATION, season_start, season_end, salary) %>%
                 left_join(id_players, by = "name") %>% filter(TEAM_ABBREVIATION != "NA")

#Get player ids that are NA and map names from salary data
player_id_nas <- data_salaries %>% filter(is.na(player_id)) %>% group_by(name) %>%
                 summarise(name = first(name)) %>%
                 mutate(name_repair = c("AJ Hammons", "AJ Price", "Domantas Sabonis", "Bennett Davis", "Boniface Ndong",
                                        "CJ Miles", "Yoeli Childs", "Clar. Weatherspoon", "DJ Mbenga", "DJ Stephens",
                                        "DJ Strawberry", "DJ White", "Dalibor Bagaric",
                                        "Efthimi Rentzias", "Eric Montross", "Pooh Jeter", "Evan Eschmyer",
                                        "Frank Mason", "Glen Rice", "Ha Seung-Jin", "Hakeem Olaju", "Hamady Ndiaye",
                                        "Harry Giles III", "Ibrahim Kutluay", "JJ Hickson", 
                                        "JJ O'Brien", "JJ Redick", "JR Smith", "James Ennis III", "Jason Caffey",
                                        "Jeffery Taylor", "Jason Sasser", "Johnny O'Bryant III",
                                        "Joseph Forte", "Juancho Hernangomez", "Kirk Haston", "KJ McDaniels",
                                        "Mamadou N'diaye", "Marcus Morris Sr.", "Matt Maloney", "Michael Dickerson",
                                        "Michael Sweetney", "Muggsy Bogues", "Nate Huffman", "Otto Porter Jr.",
                                        "PJ Hairston", "Orlando Sanchez", "Perry Jones III", "Roger Mason Jr.",
                                        "Ronald Murray", "Shawn Kemp", "Slava Medvedenko",
                                        "Steven Smith", "Sun Sun", "Terrell Brandon", "Todd MacCulloch",
                                        "Wang Zhi-zhi", "Wes Iwundu")) %>% 
                 dplyr::select(name, name_repair)

# Repair names - salary data
player_name_repair <- player_id_nas$name_repair
names(player_name_repair) <- player_id_nas$name
data_salaries$name <- recode(data_salaries$name, !!!player_name_repair)
# Match player_ids again - final salaries data
data_salaries <- data_salaries %>% dplyr::select(-player_id) %>% left_join(id_players, by = "name") %>% 
                 filter(!is.na(player_id))
# Filter so capture players that changed teams mid-season - keep season start and end year (i.e. the start year means they played some of the previous season with this team)
salaries_filter <- data_salaries %>% dplyr::select(name, player_id, TEAM_ABBREVIATION, season_start, season_end) %>%
                   pivot_longer(-c(name, TEAM_ABBREVIATION, player_id), names_to = "season_type", values_to = "season") %>%
                   distinct(player_id, name, TEAM_ABBREVIATION, season)
# Salaries data with player's changing team mid-season
data_salaries <- salaries_filter %>% left_join(data_salaries %>% dplyr::select(player_id, TEAM_ABBREVIATION, season = season_end, salary),
                                               by = c("player_id", "TEAM_ABBREVIATION", "season")) %>% 
                 arrange(name, TEAM_ABBREVIATION, season) %>% group_by(player_id, TEAM_ABBREVIATION) %>% mutate(salary = na.locf(salary, fromLast = TRUE)) 

#Total salary bill for each team in each year
data_salaries_totals <- data_salaries %>% group_by(TEAM_ABBREVIATION, season) %>%
                        summarise(salary_total = sum(salary)) 

#SEASON LEVEL PERFORMANCE METRICS
#Match IDs, remove unnecessary columns, rename some
data_season_large <- data_season_large_raw %>% left_join(id_players, by = c("Player" = "name")) %>% 
                     dplyr::select(-c(...1, blanl, blank2, PF),
                                   name = Player, season = Year, TEAM_ABBREVIATION = Tm, TO = TOV) %>% 
                     dplyr::select(season, name, player_id, Pos:PTS) %>% 
                     mutate(name = str_replace_all(name, '//*', '')) %>% 
                     filter(TEAM_ABBREVIATION != "TOT")

#Recode franchise name changes (except NJN which do after joining)
team_name_changes <- tibble(old = c("BRK", "CHO", "NOK", "NOH", "PHO", "SEA"),
                            new = c("BKN", "CHA", "NOP", "NOP", "PHX", "OKC"))
team_name_repair <- team_name_changes$new
names(team_name_repair) <- team_name_changes$old
data_games_raw$TEAM_ABBREVIATION <- recode(data_games_raw$TEAM_ABBREVIATION, !!!team_name_repair)
data_season_large$TEAM_ABBREVIATION <- recode(data_season_large$TEAM_ABBREVIATION, !!!team_name_repair)
#Get player ids that are NA and map names from season data
player_id_nas_season <- data_season_large %>% dplyr::select(season, name, player_id, TEAM_ABBREVIATION) %>% 
                        filter(is.na(player_id)) %>% group_by(name) %>% summarise(name = first(name)) %>% 
                        mutate(name_repair = c("AJ Hammons", "AJ Price", "Allen Iverson",
                                               "Alonzo Mourning", "Boniface Ndong",
                                               "CJ McCollum", "CJ Miles", "Clar. Weatherspoon",
                                               "DJ White", "DJ Stephens", "DJ Strawberry", "DJ White",
                                               "Danuel House Jr.", "Derrick Jones Jr.", "Didier Illunga-Mbenga",
                                               "Dikembe Mutombo", "Pooh Jeter", "Gary Payton",
                                               "Ha Seung-Jin", "Hamady Ndiaye", "Ibrahim Kutluay",
                                               "JJ Hickson", "JJ O'Brien", "JJ Redick", "JR Smith",
                                               "James Ennis III", "James Michael McAdoo", "John Lucas III",
                                               "Johnny O'Bryant III", "Juan Carlos Navarro",
                                               "Juancho Hernangomez", "KJ McDaniels", "Keith Van Horn",
                                               "Kelly Oubre Jr.", "Larry Drew II", "Larry Nance Jr.",
                                               "Luc Mbah a Moute", "Luigi Datone", "Mamadou N'diaye",
                                               "Marcus Morris Sr.", "Metta World Peace", "Nando De Colo",
                                               "Nene Hilario", "Nick Van Exel", "Otto Porter Jr.", "PJ Hairston",
                                               "Perry Jones III", "Peter John Ramos", "RJ Hunter", "Reggie Miller",
                                               "Roger Mason Jr.", "Ronald Murray", "Shaquille O'Neal", "Sheldon Mac",
                                               "Slava Medvedenko", "Steven Smith", "Sun Sun", "Taurean Prince",
                                               "Tim Hardaway Jr.", "Wade Baldwin IV", "Edy Tavares", "Wang Zhi-zhi",
                                               "Yao Ming")) %>% 
                        dplyr::select(name, name_repair)

# Repair names - season data
player_name_repair_season <- player_id_nas_season$name_repair
names(player_name_repair_season) <- player_id_nas_season$name
data_season_large$name <- recode(data_season_large$name, !!!player_name_repair_season)
# Match player_ids again - season data
data_season_large <- data_season_large %>% dplyr::select(-player_id) %>% left_join(id_players, by = "name") %>% 
                     filter(!is.na(player_id)) %>% dplyr::select(season, player_id, name:PTS)

#Recode NJN to BKN
data_season_large$TEAM_ABBREVIATION <- recode(data_season_large$TEAM_ABBREVIATION, !!!c("NJN" = "BKN"))

# GAMES WON
# Binary flag for whether a team won a game or not
data_games_id <- data_games_summary_raw %>% dplyr::select(GAME_ID, HOME_TEAM_ID, VISITOR_TEAM_ID, SEASON, HOME_TEAM_WINS) %>% 
                 mutate(winning_team = if_else(HOME_TEAM_WINS == 1, HOME_TEAM_ID, VISITOR_TEAM_ID))
# Games won in a season
data_games_won <- data_games_id %>%
                  mutate(winning_team = if_else(HOME_TEAM_WINS == 1,
                                                HOME_TEAM_ID,
                                                VISITOR_TEAM_ID)) %>%
                  dplyr::select(HOME_TEAM_ID, VISITOR_TEAM_ID, SEASON, winning_team) %>% 
                  pivot_longer(-c(SEASON, winning_team), values_to = 'team_id') %>%
                  dplyr::select(-name) %>% 
                  mutate(win_flag = if_else(winning_team == team_id, 1, 0)) %>%
                  group_by(SEASON, team_id) %>% 
                  summarise(win_flag = sum(win_flag))
#Games data - match in win flag
data_games_all <- data_games_raw %>% dplyr::select(GAME_ID:TEAM_ID, PLAYER_ID, TEAM_ABBREVIATION, PLAYER_NAME, START_POSITION, MIN:PLUS_MINUS) %>% 
                  mutate(MIN = (paste0('00:', MIN) %>% parse_hms() %>% as.numeric())/60) %>% 
                  left_join(data_games_id %>% dplyr::select(GAME_ID, season = SEASON, HOME_TEAM_ID, winning_team), by = "GAME_ID") %>% 
                  mutate(win_flag = if_else(TEAM_ID == winning_team, 1, 0),
                         home_flag = if_else(TEAM_ID == HOME_TEAM_ID, 1, 0)) %>% filter(season <= 2017, season >= 2005)
# Add team abbreviation column to data_games_won
team_id_abb <- data_games_all %>% group_by(TEAM_ID, TEAM_ABBREVIATION) %>% summarise()
data_games_won <- data_games_won %>% left_join(team_id_abb, by = c("team_id" = "TEAM_ID"))

### 03. PROCESS DATA #####################
# Performance data - top 8 players per team by minutes played in each season
data_season_summary <- data_games_all %>% dplyr::select(-FG_PCT, -FG3_PCT, -FT_PCT, -REB, -PF, -PLUS_MINUS) %>%
                       group_by(season, TEAM_ABBREVIATION, PLAYER_ID) %>% 
                       summarise(TEAM_ABBREVIATION = first(TEAM_ABBREVIATION),
                                 PLAYER_NAME = first(PLAYER_NAME),
                                 across(c(MIN:PTS, win_flag, home_flag), sum, na.rm = TRUE)) %>%
                       arrange(TEAM_ABBREVIATION, season, desc(MIN)) 
#Take top 8 players by minutes played
data_season_top8 <- data_season_summary %>% slice_max(order_by = MIN, n = 8, with_ties = FALSE)

# Share of minutes played captured by top 8
data_min <- data_season_top8 %>% group_by(season, TEAM_ABBREVIATION) %>% summarise(min8 = sum(MIN)) %>% 
            left_join(data_season_summary %>%
                      group_by(season, TEAM_ABBREVIATION) %>%
                      summarise(total_min = sum(MIN)), by = c("season", "TEAM_ABBREVIATION")) %>% 
            mutate(share = min8/total_min*100) %>% group_by(season) %>% summarise(share = mean(share))
          
# Join to salary data, remove repeated players, impute missing salary data with median per team and season
data_salaries_concentration <- data_salaries %>% right_join(data_season_top8 %>% dplyr::select(PLAYER_NAME, season, PLAYER_ID, TEAM_ABBREVIATION),
                                                                                         by = c("season", "player_id" = "PLAYER_ID", "TEAM_ABBREVIATION")) %>% dplyr::select(-name) %>%   
                               group_by(TEAM_ABBREVIATION, season) %>% distinct(player_id, TEAM_ABBREVIATION, season, .keep_all = TRUE) %>% #there are some duplicate players 
                               mutate(salary = replace_na(salary, median(salary, na.rm = TRUE)),
                                      salary_share = salary/sum(salary)*100,
                                      salary_hhi = sum(salary_share^2, na.rm = FALSE)) %>% ungroup() %>% arrange(TEAM_ABBREVIATION, season, desc(salary_share))

# Season data - add in games won and select relevant columns
data_season <- data_season_top8 %>%
               mutate(across(FGM:PTS, function(x) x/MIN*10),
                      FG_PCT = replace_na(FGM/FGA*100, 0), FG3_PCT = replace_na(FG3M/FG3A*100, 0), FT_PCT = replace_na(FTM/FTA*100, 0)) %>%
               left_join(data_salaries_concentration %>% dplyr::select(player_id, TEAM_ABBREVIATION, season, salary_share, salary_hhi),
                         by = c('PLAYER_ID' = 'player_id', 'TEAM_ABBREVIATION', 'season')) %>%
               ungroup() %>% group_by(season, TEAM_ABBREVIATION) %>%
               arrange(TEAM_ABBREVIATION, season, desc(salary_share)) %>% 
               dplyr::select(-FGM, -FGA, -FG3M, -FG3A, -FTM, -FTA, -home_flag, -MIN, -PLAYER_NAME, -win_flag) %>% 
               left_join(data_games_won, by = c('season' = 'SEASON', 'TEAM_ABBREVIATION')) %>% ungroup() %>% 
               dplyr::select(-team_id) %>% 
               left_join(data_salaries_totals, by = c("TEAM_ABBREVIATION", "season"))

#Recode NJN to BKN
data_season$TEAM_ABBREVIATION <- recode(data_season$TEAM_ABBREVIATION, !!!c("NJN" = "BKN"))
# Need to recode NJN before join in WS data
data_season <- data_season %>%
               left_join(data_season_large %>%
                         dplyr::select(player_id, season, TEAM_ABBREVIATION, OWS:WS) %>% 
                         mutate(season = season - 1),
                         by = c("PLAYER_ID" = "player_id", "season", "TEAM_ABBREVIATION")) %>% 
               distinct(PLAYER_ID, season, TEAM_ABBREVIATION, .keep_all = TRUE) 

#Scale data
data_season_scaled <- data_season %>% mutate(across(c(OREB:FT_PCT, salary_share, salary_total), function(x) scale(x)[,1])) %>%
                      mutate(salary_hhi =  (salary_hhi - mean(salary_hhi))/sd(salary_hhi)) %>% 
                      group_by(TEAM_ABBREVIATION, season) %>% mutate(rank = paste0('player_',1:8)) %>% ungroup()

# Pivot wider on players, ranked by salary (and remove win shares)
data_season_wide <- data_season_scaled %>% 
                    dplyr::select(-c(salary_share, salary_total, PLAYER_ID, TEAM_ABBREVIATION, season, OWS:WS)) %>% 
                    pivot_wider(names_from = rank, values_from = c(OREB:PTS, FG_PCT:FT_PCT))

### 04. DATA FOR REGRESSIONS #########
#Season level data - for regularisation
# y
season_wins <- data_season_wide$win_flag
#Single variables
cols_string_season <- paste0("`", colnames(data_season_wide %>% ungroup() %>%
                                             dplyr::select(-c(win_flag))), "`")
#Model
season_model <- paste("~", cols_string_season %>% paste0(collapse = " + "))
# X - full model no interactions
seasons <- model.matrix(as.formula(season_model),
                        data_season_wide %>% dplyr::select(-c(win_flag)))[,-1]

#Long data with salary_share interactions
#Interactions
cols_string_season_long <- paste0("`", colnames(data_season_scaled %>% ungroup() %>%
                                                  dplyr::select(-c(win_flag, season, TEAM_ABBREVIATION,
                                                                   rank, PLAYER_ID, OWS:WS))), "`")
interactions <- combn(cols_string_season_long, 2, FUN = paste0, simplify = FALSE, collapse = "*") %>% unlist()
interactions_salary_share <- interactions[str_detect(interactions, 'salary_share')]
interactions_salary_share <- interactions_salary_share[-c(length(interactions_salary_share)-1,
                                                          length(interactions_salary_share))]
#y
win_shares <- data_season_scaled$WS %>% na.omit()
# Model
season_model_long <- paste("~", c(cols_string_season_long) %>% paste0(collapse = " + "))
season_model_interactions <- paste("~", c(cols_string_season_long, interactions_salary_share) %>%
                                     paste0(collapse = " + "))
# X
seasons_long <- model.matrix(as.formula(season_model_interactions),
                             data_season_scaled %>% na.omit() %>% dplyr::select(-c(win_flag, PLAYER_ID, rank, OWS:WS)))[,-1]

# Set up data for OLS and quantile regression
formula_long <- paste0('WS ', season_model_interactions) %>% as.formula()
data_season_qr <- data_season_scaled %>% ungroup() %>% na.omit() %>% 
                  dplyr::select(-c(OWS:DWS, win_flag, TEAM_ABBREVIATION, season, PLAYER_ID, rank))  

### 05. OLS - WIDE DATA ##########
season_ols <- lm(formula = paste0('win_flag ', season_model) %>%  as.formula(),
                  data_season_wide)

#Out of sample predictions
season_ols_outsample <- kfoldCV.mle(model = paste0('win_flag ', season_model) %>%  as.formula(),
                                    data = data_season_wide)

# RMSEs in and out of sample
r2_season_ols <- cor(data_season_wide$win_flag, season_ols$fitted.values)^2
r2_season_out_ols <- cor(data_season_wide$win_flag, season_ols_outsample$pred)^2


rmse_season_ols <- rmse(data_season_wide$win_flag, season_ols$fitted.values)/mean(data_season_wide$win_flag)
rmse_season_out_ols <- rmse(data_season_wide$win_flag, season_ols_outsample$pred)/mean(data_season_wide$win_flag)

### 06. PART 1 - REGULARISATION ON WIDE DATA ##########
#LASSO
# CV LASSO Model
season_lasso_cv <- cv.glmnet(x = seasons, y = season_wins, K = 10, family = 'gaussian')
coef(season_lasso_cv, s='lambda.min')
#BIC LASSO Model
season_lasso_bic <- lasso.bic(x = seasons, y = season_wins, extended = FALSE)
# With bootstrapped confidence intervals
season_lasso <- lassopost(x = seasons, y = season_wins, method.lambda = 'bic')

# Predictions Outsample
pred_outsample_lasso <- kfoldCV.lasso(x = seasons, y = season_wins , K = 10 , seed=1993, criterion = "bic")
pred_outsample_lasso_cv <- kfoldCV.lasso(x = seasons, y = season_wins , K = 10 , seed=1993, criterion = "cv")

# Calculate R^2
r2_insample_lasso <-  cor(season_wins, season_lasso_bic$ypred)^2
r2_outsample_lasso <-  cor(season_wins, pred_outsample_lasso$pred)^2
#RMSE - BIC
rmse_insample_lasso <-  rmse(season_wins, season_lasso_bic$ypred)/mean(season_wins)
rmse_outsample_lasso <-  rmse(season_wins, pred_outsample_lasso$pred)/mean(season_wins)
#RMSE - CV
rmse_insample_lasso_cv <-  rmse(season_wins, predict(season_lasso_cv, newx=seasons))/mean(season_wins)
rmse_outsample_lasso_cv <-  rmse(season_wins, pred_outsample_lasso_cv$pred)/mean(season_wins)

#BAYESIAN MODEL SELECTION
#Prior elicitation
g_grid <- seq(0.001, 1, length=100)
V <-  diag(ncol(seasons))
n <- nrow(seasons)
#Generate 100 random beta-tilde vectors ~ N(0, I)
betas_sim <- rmvnorm(100, sigma= V)
betas_list <- lapply(seq_len(ncol(t(betas_sim))), function(i) t(betas_sim)[,i])
# Generate theoretical R^2 
# Function for tau
calculate_tau <- function(beta, X, g, n) {
  tau <- 1/(1 + n/(g*(beta %*% t(X) %*% X %*% beta))) 
  return(tau)
}

#Theoretical R^2
r2 <-  double(length(g_grid))
for (i in 1:length(g_grid)) {
  r2[i] <- map(betas_list, calculate_tau, seasons, g = g_grid[i], n = n) %>% unlist() %>% mean()  
}

#Match in tibble with values of g
r2_tibble <- tibble(g_grid, r2)

#Bayesian model selection
season_bayes <- modelSelection(y = season_wins, x = seasons, priorCoef = zellnerprior(taustd = 0.01),
                               priorDelta = modelbbprior(1,1))
# Check the top 10 models
head(postProb(season_bayes), 5)
#Extract credible intervals and tidy them up
season_ci <- coef(season_bayes)[-c(nrow(coef(season_bayes))),] #omit variance
season_ci[,1:3]= round(season_ci[,1:3], 3)  
season_ci[,4]= round(season_ci[,4], 4)
#Coefficients and credible intervals - BMA
season_bayes_full <-  season_ci %>% data.frame() %>% tibble() %>%
                      mutate(variable = rownames(season_ci)) %>%
                      dplyr::select(variable, estimate:margpp)
#Predictions insample
seasons_pred_insample_bayes <- cbind(1, seasons) %*% season_bayes_full$estimate
#Predictions outsample
seasons_pred_outsample_bayes <- kfoldCV.bayes(y = season_wins, x = seasons,
                                             priorCoef = zellnerprior(taustd = 0.01),
                                             priorDelta = modelbbprior(1,1), seed = 1993)
#R^2 using same formula as for LASSO
r2_insample_bayes <-  cor(season_wins, seasons_pred_insample_bayes)^2
r2_outsample_bayes <-  cor(season_wins, seasons_pred_outsample_bayes$pred)^2
#RMSE
rmse_insample_bayes <-  rmse(season_wins, seasons_pred_insample_bayes)/mean(season_wins)
rmse_outsample_bayes <-  rmse(season_wins, seasons_pred_outsample_bayes$pred)/mean(season_wins)

### 07. OLS - LONG DATA ###############
#Run model                 
season_long_ols <- lm(paste0("WS ", season_model_interactions) %>% as.formula(), data_season_qr)
#Extract coefficients and add confidence intervals
season_long_ols_coef <- tibble(variable = names(season_long_ols$coefficients),
                               estimate = season_long_ols %>% coefficients()) %>% 
                        bind_cols(season_long_ols %>% confint() %>% data.frame())
# Make CV out-of-sample predictions
season_long_ols_outsample <- kfoldCV.mle(model = paste0('WS', season_model_interactions) %>% as.formula(),
                                         data = data_season_qr)
#R2
r2_season_long_ols_out <- cor(season_long_ols_outsample$pred, data_season_qr$WS %>% na.omit())^2
#RMSEs
rmse_season_long_ols <- rmse(data_season_qr$WS %>% na.omit(), season_long_ols$fitted.values)/mean(data_season_qr$WS)
rmse_season_long_out_ols <- rmse(data_season_qr$WS %>% na.omit(), season_long_ols_outsample$pred)/mean(data_season_qr$WS)

### 08. PART 2 - QUANTILE REGRESSION ON LONG DATA ##################
# Run models
seasons_qr_median <- rq(formula_long, data_season_qr, tau = 0.5)
seasons_qr_80 <- rq(formula_long, data_season_qr, tau = 0.8)

#bootstrapped confidence intervals
seasons_qr_median_summary <- seasons_qr_median %>% summary(se = 'boot')
qr_coef <- seasons_qr_median_summary$coefficients[,1] 
qr_err <- seasons_qr_median_summary$coefficients[,2]  
qr_ci <- tibble(X1 = c(), X2 = c())
for (i in 1:length(qr_coef)){
  qr_ci <- bind_rows(qr_ci, data.frame(t(qr_coef[i] + c(-1,1)*qr_err[i]*qnorm(0.975))))}
#Store coefficients and confidence intervals
qr_full <- tibble(variable = names(qr_coef), estimate = qr_coef) %>% bind_cols(qr_ci)

# Check RMSEs across range of tau values
taus <- seq(0.05,0.95, by = .05)
rmse_in_qr <- c()
rmse_out_qr <- c()
for(i in taus) {
  qr_model_cv <- kfoldCV.qr(model = formula_long, data = data_season_qr,
                         tau = i, seed = 1993)
  qr_model <- rq(formula_long, data_season_qr, tau = i, method = 'lasso')
  rmse_insample <- rmse(data_season_qr$WS, qr_model$fitted.values)/mean(data_season_qr$WS)
  rmse_outsample <- rmse(data_season_qr$WS, qr_model_cv$pred)/mean(data_season_qr$WS)
  rmse_in_qr <- c(rmse_in_qr, rmse_insample)
  rmse_out_qr <- c(rmse_out_qr, rmse_outsample)
}
names(rmse_in_qr) <- taus
names(rmse_out_qr) <- taus

### 09. PART 3 - HIERARCHICAL MODELS ON LONG DATA #################
#Intercept - random effects
intercepts <- paste0('+ (1 | season) + (1 | TEAM_ABBREVIATION)') 
nested_intercepts <- paste0('+ (1 | season/TEAM_ABBREVIATION)')
player_intercept <- paste0('+ (1 | PLAYER_ID)')
#Slope - random effects
hhi_team_slope <- paste0('+ (1+salary_hhi|TEAM_ABBREVIATION) + (1 | season)')
hhi_season_slope <- paste0('+ (1+salary_hhi|season) + (1 | TEAM_ABBREVIATION)')

# Data
data_season_hm <- data_season_scaled %>% dplyr::select(-c(OWS:DWS, win_flag)) %>%
                  mutate(season = as.factor(season),
                         TEAM_ABBREVIATION = as.factor(TEAM_ABBREVIATION),
                         rank = as.factor(rank),
                         PLAYER_ID = as.factor(PLAYER_ID)) %>% na.omit()

#LMER model - INTERCEPTS
model_hm_ts <- paste0('WS ', season_model_interactions, nested_intercepts)
#Model
season_hm <- lmerTest::lmer(model_hm_ts %>% as.formula(),
                            data_season_hm %>% dplyr::select(-c(PLAYER_ID,rank)))

#Coefficients and confidence intervals
season_hm_full <- tibble(variable = summary(season_hm)$coefficients[,1] %>% names(),
                         estimate = summary(season_hm)$coefficients[,1]) %>%  
                  bind_cols((season_hm %>% confint() %>% data.frame() %>% tibble())[-c(1:3),])

# R2 - in-sample
r2_ts <- MuMIn::r.squaredGLMM(season_hm)
#  Out of sample predictions
season_hm_pred <- kfoldCV.lmer(model_hm_ts %>% as.formula(),
                               data_season_hm %>% dplyr::select(-c(PLAYER_ID,rank)),
                               seed = 1993, K = 10)
# R2 - out-of-sample
r2_out_hm <- cor(season_hm_pred$pred, data_season_hm$WS)^2

rmse_in_hm <- rmse(fitted(season_hm), data_season_hm$WS)/mean(data_season_hm$WS)
rmse_out_hm <- rmse(season_hm_pred$pred, data_season_hm$WS)/mean(data_season_hm$WS)

#LMER MODEL - SLOPE AND INTERCEPT
model_hm_slope <- paste0('WS ', season_model_interactions, hhi_team_slope)
#Model
season_hm_slope <- lmerTest::lmer(model_hm_slope %>% as.formula(),
                            data_season_hm %>% dplyr::select(-c(PLAYER_ID,rank)))

#Coefficients and confidence intervals
season_hm_slope_full <- tibble(variable = summary(season_hm_slope)$coefficients[,1] %>% names(),
                               estimate = summary(season_hm_slope)$coefficients[,1]) %>%  
                        bind_cols((season_hm_slope %>% confint() %>% data.frame() %>% tibble())[-c(1:5),])

# R2 - in-sample
r2_slope <- MuMIn::r.squaredGLMM(season_hm_slope)
#  Out of sample predictions
season_hm_slope_pred <- kfoldCV.lmer(model_hm_slope %>% as.formula(),
                                     data_season_hm %>% dplyr::select(-c(PLAYER_ID,rank)),
                                     seed = 1993, K = 10)
# RMSE - out-of-sample
r2_out_hm_slope <- cor(season_hm_slope_pred$pred, data_season_hm$WS)^2

rmse_in_hm_slope <- rmse(fitted(season_hm_slope), data_season_hm$WS)/mean(data_season_hm$WS)
rmse_out_hm_slope <- rmse(season_hm_slope_pred$pred, data_season_hm$WS)/mean(data_season_hm$WS)

### 10. PART 4 - EXPLORATORY CLUSTERING ###################
set.seed(666)
# Reload  data_games_season to use it for clustering adding player back
# Season data - add in games won and select relevant columns
data_season_clust <- data_season_top8 %>%
                     mutate(across(FGM:PTS, function(x) x/MIN*10),
                            FG_PCT = replace_na(FGM/FGA*100, 0), FG3_PCT = replace_na(FG3M/FG3A*100, 0), FT_PCT = replace_na(FTM/FTA*100, 0)) %>%
                     left_join(data_salaries_concentration %>% dplyr::select(player_id, TEAM_ABBREVIATION, season, salary_share, salary_hhi),
                               by = c('PLAYER_ID' = 'player_id', 'TEAM_ABBREVIATION', 'season')) %>%
                     ungroup() %>% group_by(season, TEAM_ABBREVIATION) %>%
                     arrange(TEAM_ABBREVIATION, season, desc(salary_share)) %>% 
                     dplyr::select(-FGM, -FGA, -FG3M, -FG3A, -FTM, -FTA, -home_flag, -MIN, -win_flag) %>% 
                     left_join(data_games_won, by = c('season' = 'SEASON', 'TEAM_ABBREVIATION')) %>% ungroup() %>% 
                     dplyr::select(-team_id) %>%
                     left_join(data_season_large %>%
                               dplyr::select(player_id, season, TEAM_ABBREVIATION, OWS:WS) %>% 
                               mutate(season = season - 1),
                               by = c("PLAYER_ID" = "player_id", "season", "TEAM_ABBREVIATION")) %>% 
                     distinct(PLAYER_ID, season, TEAM_ABBREVIATION, .keep_all = TRUE) %>% na.omit()

# Prework: Filter by season and set player name as row index
data_season_clust <- data_season_clust %>% ungroup()# %>% filter(season == '2016')
data_season_clust <- as.data.frame(data_season_clust)
data_season_clust <- data_season_clust %>% ungroup() %>% dplyr::select(-season)

#### OFFENSE
# Build dataframe and cluster with offensive metrics
p_offense <- data_season_clust %>% ungroup() %>% dplyr::select(PTS, FG_PCT, AST, OREB, OWS)
# Hard clustering assignation for offensive metrics
cl_p_offense <- eclust(p_offense ,FUNcluster="kmeans", k=8, hc_metric = "euclidean")
# Put cluster and salary share back on p_offense dataframe to analyze
p_offense$cluster = cl_p_offense$cluster
p_offense$salary_share = data_season_clust$salary_share
#### DEFENSE
# Build dataframe and cluster with defensive metrics
p_defense <- data_season_clust %>%ungroup() %>% dplyr::select(STL, TO, BLK, DREB, DWS)
# Hard clustering assignment for defensive metrics
cl_p_defense <- eclust(p_defense ,FUNcluster="kmeans", k=8, hc_metric = "euclidean")
# Put cluster and salary share back on p_defense dataframe to analyze
p_defense$cluster = cl_p_defense$cluster
p_defense$salary_share =data_season_clust$salary_share
#### OVERALL
# Build dataframe and cluster with all metrics
p_overall <- data_season_clust %>% ungroup() %>% dplyr::select(PTS, AST, OREB, STL, BLK, DREB, TO, WS,
                                                               FG_PCT)
# Hard clustering assignation for offensive metrics
cl_p_overall <- eclust(p_overall ,FUNcluster="kmeans", k=8, hc_metric = "euclidean")
# Put cluster and salary share back on p_offense dataframe to analyze
p_overall$cluster = cl_p_overall$cluster
p_overall$salary_share = data_season_clust$salary_share

# Data for plot
offense_cluster_data <- p_offense %>% dplyr::select(cluster, salary_share) %>% group_by(cluster) %>% 
                        summarise(salary_share_sd = sd(salary_share, na.rm = TRUE),
                                  salary_share_mean = mean(salary_share, na.rm = TRUE),
                                  salary_share_1sd = salary_share_mean + salary_share_sd,
                                  salary_share_1sdm = salary_share_mean - salary_share_sd) %>%
                        arrange(desc(salary_share_mean)) %>% dplyr::select(salary_share_mean:salary_share_1sdm)

defense_cluster_data <- p_defense %>% dplyr::select(cluster, salary_share) %>% group_by(cluster) %>% 
                        summarise(salary_share_sd = sd(salary_share, na.rm = TRUE),
                                  salary_share_mean = mean(salary_share, na.rm = TRUE),
                                  salary_share_1sd = salary_share_mean + salary_share_sd,
                                  salary_share_1sdm = salary_share_mean - salary_share_sd) %>%
                         arrange(desc(salary_share_mean)) %>% dplyr::select(salary_share_mean:salary_share_1sdm)

overall_cluster_data <- p_overall %>% dplyr::select(cluster, salary_share) %>% group_by(cluster) %>% 
                        summarise(salary_share_sd = sd(salary_share, na.rm = TRUE),
                                  salary_share_mean = mean(salary_share, na.rm = TRUE),
                                  salary_share_1sd = salary_share_mean + salary_share_sd,
                                  salary_share_1sdm = salary_share_mean - salary_share_sd) %>%
                        arrange(desc(salary_share_mean)) %>% dplyr::select(salary_share_mean:salary_share_1sdm)

# Average salary share by player rank
salary_share_avg <- data_season %>% group_by(TEAM_ABBREVIATION, season) %>%
                    mutate(rank = paste0('player_',1:8)) %>% ungroup() %>% 
                    group_by(rank) %>% summarise(avg_salary_share = mean(salary_share, na.rm = TRUE),
                                                 sd_salary_share = sd(salary_share, na.rm = TRUE),
                                                 sd1_salary_share = avg_salary_share + sd_salary_share,
                                                 sdm_salary_share = avg_salary_share - sd_salary_share) %>% 
                    dplyr::select(rank, avg_salary_share, sd1_salary_share, sdm_salary_share)

# All data
cluster_averages <- salary_share_avg %>% mutate(salary_rank = 1:8) %>% dplyr::select(-rank) %>% 
                    bind_cols(offense_cluster_data %>% dplyr::select(off_mean = salary_share_mean)) %>% 
                    bind_cols(defense_cluster_data %>% dplyr::select(def_mean = salary_share_mean)) %>% 
                    bind_cols(overall_cluster_data %>% dplyr::select(tot_mean = salary_share_mean))

### 11. RESULTS TIBBLES #################
#RMSE - season wins models (RMSE normalised by mean y)
rmse_table_wide <- tibble(nrmse_season_wins = c("OLS", "L1 Penalisation (lambda by BIC)",
                                               "L1 Penalisation (lambda by CV)",
                                               "Bayesian Model Selection"),
                     insample = c(rmse_season_ols, rmse_insample_lasso,
                                  rmse_insample_lasso_cv,
                                  rmse_insample_bayes),
                     outsample = c(rmse_season_out_ols, rmse_outsample_lasso,
                                   rmse_outsample_lasso_cv,
                                   rmse_outsample_bayes))

#RMSE - win share models (RMSE normalised by mean y)
rmse_table <- tibble(nrmse_win_shares = c("OLS", "Median Regression",
                               "Mixed Effects (nested intercepts)",
                               "Mixed Effects (varying slope - salary_hhi)"),
                     insample = c(rmse_season_long_ols, rmse_in_qr[10],
                                  rmse_in_hm, rmse_in_hm_slope),
                     outsample = c(rmse_season_long_out_ols, rmse_out_qr[10],
                                   rmse_out_hm, rmse_out_hm_slope))
