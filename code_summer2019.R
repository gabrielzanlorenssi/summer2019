
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(broom)
library(haven)


# Data --------------------------------------------------------------------

data <- read_dta("data_final.dta")
debt <- read_dta("debt.dta")

# Creating debt variable --------------------------------------------------

debt[is.na(debt)] <- 0

debt <- debt %>% 
  mutate(debt_total = encargext + encargint +
           v_3200000000) %>% 
  mutate(debt = debt_total * 100 / v_1000000000)


# Merging -----------------------------------------------------------------

data %>% 
  left_join(debt, by=c("uf", "year")) %>% 
  mutate(fiscal_laws = ifelse(year>2002, 1, 0)) %>% 
  filter(elecyear == 1) %>% 
  group_by(uf) %>% 
  mutate(l_vote = lag(elecvote),
         i1 = unempch_rel * tax,
         i2 = unempch_rel * ind_serv) -> x

# Model -------------------------------------------------------------------


model1 <- lm(data = x,
             formula = vote_share ~ l_vote + incumbent + unempch_uf +
                ind_serv  + i1 + i2 +
               (debt*fiscal_laws)) 

# Results -----------------------------------------------------------------

tidy(model1)
glance(model1)