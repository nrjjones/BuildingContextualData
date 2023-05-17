library(tidyverse)
library(haven)

uwp <- read_spss("./raw/P1XXX - UW-Parkside_April 21.sav") %>% as_factor()
uno <- read_spss("./raw/P1XXX - UN-Omaha_April 21.sav") %>% as_factor()
umn <- read_spss("./raw/P1XXX - UMN_April 21.sav") %>% as_factor()
pit <- read_spss("./raw/P1XXX - Pitt_April 21.sav") %>% as_factor()

# Uncomment to show update

#uwp <- read_spss("./raw/P1XXX - UW-Parkside_May 12.sav") %>% as_factor()
#uno <- read_spss("./raw/P1XXX - UN-Omaha_May 12.sav") %>% as_factor()
#umn <- read_spss("./raw/P1XXX - UMN_May 12.sav") %>% as_factor()
#pit <- read_spss("./raw/P1XXX - Pitt_May 12.sav") %>% as_factor()


uwp <- uwp %>%
  select(EndDate) %>%
  mutate(school = "UW-Parkside")

uno <- uno %>%
  select(EndDate) %>%
  mutate(school = "UN-Omaha")

umn <- umn %>%
  select(EndDate) %>%
  mutate(school = "U Minnesota")

pit <- pit %>%
  select(EndDate) %>%
  mutate(school = "Pitt")

df <- bind_rows(uwp, uno, umn, pit) %>%
  mutate(
    EndDate = format(EndDate, "%Y-%m-%d"),
    EndDate = as.Date(EndDate)
  ) %>%
  group_by(school, EndDate) %>%
  summarize(
    n = n()
  ) %>%
  mutate(Responses = cumsum(n))

#ggplot(df) +
#  geom_line(aes(x=EndDate, y=Responses, group=school, color=school)) +
#  theme_minimal()
#
