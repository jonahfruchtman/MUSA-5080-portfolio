#Jonah Fruchtman#
##Week 2 In-Class Practice##

#1 Load libraries
library(tidycensus)
library(tidyverse)

#2 Get Census data
pa_income <- get_acs (
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
  )

#Look at data
dim(pa_income)
glimpse(pa_income)
head(pa_income)

#3 Look at GEOID
pa_income$GEOID

#4 filter() - picks rows
#Filter by income greater than 60,000
Income60k <- filter(pa_income, estimate > 60000)

#Filter by MOE greater than 3000
moe3k <- filter(pa_income, moe > 3000)

#Filter Counties by estiamte less than 50,000
EstBel50k <- filter(pa_income, estimate < 50000)

#5 select() - picks columns

select_col <- select(pa_income, NAME, estimate, moe)

select_col2 <- select(pa_income, GEOID, estimate)


#6 mutate() - makes a new column

pa_income <- mutate(pa_income, moe_pct = moe / estimate *100)

pa_income

#7 arrange() - Sorts

#Arrange moe_pct low to high
arrange(pa_income, moe_pct)

#Arrange moe_pct high to low
arrange(pa_income, desc(moe_pct))

#8 The pipe %>% -- "and then"

#Without pipe
step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

#With pipe
pa_income %>%
  filter(moe_pct >5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct)

worst <- pa_income %>%
  filter(moe_pct > 8) %>%
  arrange(estimate) %>%
  select(NAME, moe_pct)

worst

#9 group_by() + summarize() - MANY rows become few

#group by "reliable" MOE
pa_income <- mutate(pa_income, reliable = moe_pct <5)

reliable <- pa_income %>%
  group_by(reliable) %>%
  summarize (n = n(),
             avg_income = mean(estimate))

#10 case_when() - sorting into categories

#create 3 categories
pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct <3 ~ "High confidence",
    moe_pct <6 ~"Moderate confidence",
    TRUE ~ "Low cofidence"
  ))

#count per category
count(pa_income, reliability)

#reliability               n
#1 High confidence        26
#2 Low confidence           7
#3 Moderate confidence    34

#11 Two variables, and a shape problem
#Is the margin of error bigger in small counties?

#PA long data
pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA",
  year = 2023,
  survey = "acs5",
)

pa_two

#PA wide data
pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop = "B01003_001"),
  state = "PA",
  year = 2023,
  survey = "acs5",
  output = "wide"
)

pa_wide

#Answering the question
pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)

#NAME                           popE incomeE moe_pct
#1 Cameron County, Pennsylvania   4475   47681    9.07
#2 Sullivan County, Pennsylvania  5849   64758    7.90
#3 Montour County, Pennsylvania  18079   72926    7.68
#4 Forest County, Pennsylvania    6785   50061    7.55
#5 Union County, Pennsylvania    42570   72894    7.07
#6 Elk County, Pennsylvania      30703   64103    6.47
#7 Greene County, Pennsylvania   35265   66870    6.31
#8 Clinton County, Pennsylvania  37707   58842    5.45
#9 Fulton County, Pennsylvania   14545   64798    5.31
#10 Carbon County, Pennsylvania   65191   67877    5.27
