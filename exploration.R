### explore the Shuttle set, because I don't actualy know it
library(mlbench)
data("Shuttle")
library(tidyverse)
Shuttle %>%
  summary()
ggplot(Shuttle, aes(Class))+
  geom_bar()+
  ggtitle("Counts of all category")
Shuttle %>% 
  count(Class,sort = TRUE)
# 1 Rad.Flow  45586
# 2 High       8903
# 3 Bypass     3267
# 4 Fpv.Open    171
# 5 Fpv.Close    50
# 6 Bpv.Open     13
# 7 Bpv.Close    10

# I might remove Bpv category
## unique variables per var
Shuttle %>% 
  summarise_all(~length(unique(.)))

Shuttle %>% 
  mutate(Class = fct_infreq(Class)) %>% 
  pivot_longer(cols = -Class,names_to = "Vars",values_to = "number") %>% 
  ggplot(aes(Class, number))+
  geom_jitter(alpha = 1/2)+
  facet_wrap(~Vars,scales = "free_y")+
  labs(
    title="Relation between variables and outcome",
    y = "variable value"
  )
ggsave(filename = "relation_outcome_vars.png")

# this tells me I need to scale and center all 9 variables.
# I believe v6 and v4 are probably not that predictive.

## look at distributions per variable
Shuttle %>% 
  mutate(Class = fct_infreq(Class)) %>% 
  pivot_longer(cols = -Class,names_to = "Vars",values_to = "number") %>%
  ggplot(aes(number)) +
  geom_density()+
  facet_wrap(~Vars,scales = "free_x")

Shuttle %>% 
  mutate(Class = fct_infreq(Class)) %>% 
  pivot_longer(cols = -Class,names_to = "Vars",values_to = "number") %>%
  ggplot(aes(number)) +
  geom_histogram()+
  facet_wrap(~Vars,scales = "free_x")
# I should treat the variables differently
Shuttle %>% 
  mutate(
    #V1 # many on start and dropping off, this is time related I believe so maybe exclude.
    V2sign = V2 <0,
    V2 = log1p(abs(V2)), # mostly zero, some extreme values -4821  5075, most between 5 and 12
    # leave V3
    # Leave V4 too
    V5sign = V5 <0,
    V5 = log1p(abs(V5)),
    V6sign = V6 <0,
    V6 = log1p(abs(V6)),
    V7sign = V7 <0,
    V7 = sqrt(abs(V7)),
    # V8 is fine
    V9sign = V9 < 0,
    V9 = log1p(abs(V9))
  ) %>% 
  mutate(Class = fct_infreq(Class)) %>% 
  pivot_longer(cols = -Class,names_to = "Vars",values_to = "number") %>%
  ggplot(aes(number)) +
  geom_histogram()+
  facet_wrap(~Vars,scales = "free_x")

Shuttle %>% 
  #filter(V2 !=0) %>% 
  mutate(V2 = sqrt(V2)) %>% 
  ggplot(aes(V5))+
  geom_histogram(bins = 50)
  #geom_density()

Shuttle %>% 
  #filter(V2 >0) %>% 
  mutate(
    V2sign = V2 <0,
    V2 = log1p(abs(V2)),
    #V3 = log10(V3)
    V5 = log1p(abs(V5)),
    V6 = log1p(abs(V6)),
    V7 = sqrt(abs(V7)),
    V8 = V8,
    V9sign = V9 < 0,
    V9 = log1p(abs(V9))
    ) %>% 
  ggplot(aes(V9)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~Class, scales = "free")+
  #scale_x_continuous(trans = scales::boxcox_trans())+
  NULL


# any correlations between the predictors?
# if so, I could use some pls or something to bring it back.
library(corrplot)
M <- Shuttle %>% select(-Class) %>% cor()
corrplot(M, method = "circle")
# this tells me V1 and V7 are negative correlated. V5 with both V8 and V9
#V8 V9 

# what happened after 
# inputation, yeoJohnson and scaling 
test %>% 
  mutate(Class = fct_infreq(Class)) %>% 
  pivot_longer(cols = -Class,names_to = "Vars",values_to = "number") %>% 
  ggplot(aes(Class, number))+
  geom_jitter(alpha = 1/2)+
  facet_wrap(~Vars,scales = "free_y")+
  labs(
    title="Relation between FE variables and outcome",
    y = "variable value"
  )
ggsave(filename = "relation_FE_outcome_vars.png")