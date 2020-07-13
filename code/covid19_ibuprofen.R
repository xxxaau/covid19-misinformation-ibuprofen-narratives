# Coronavirus (COVID-19) paper for Harvard Review
# days of this study from 11 to 25 march 2020
# countries ("Spain", "France", "Germany", "Italy", "Netherlands", "Catalonia")
# 19 April 2020
# Sergi Xaudiera

options(scipen = 999) # human rediable
library(tidyverse)
library(lubridate)

# Gather ----
library(rtweet)
# tweets
tw <- search_tweets("ibuprofen OR ibuprofeno OR ibuprofène", 
                    n = 18000000, include_rts = TRUE, retryonratelimit = TRUE)
save_as_csv(tw, "data/202003_coronavirus_ibuprofen_0.csv",prepend_ids = FALSE)
# followers
fw <- get_followers("salutcat", n = 400000, retryonratelimit = T)
save_as_csv(fw, "data/202003_corona_followers_salut.csv", prepend_ids = FALSE)

# Merge ----
ds0 <- read_csv("~/Downloads/ibu_1.csv")
ds1 <- read_csv("~/Downloads/ibu_2.csv")
ds2 <- read_csv("~/Downloads/ibu_3.csv")
ds <- full_join(ds0,ds1) %>%
  full_join(., ds2) %>%
  distinct(., status_id, .keep_all = TRUE)
write_csv(ds, "data/202003_corona_ibruprofen.csv")

# Clean ----
ds <- read_csv("data/202003_corona_ibruprofen_raw.csv")
# cases <- read_csv("data/202003_corona_cases.csv")
# explore
# top <- ds %>%
#   group_by(lang) %>%
#   summarize(value = n()) %>%
#   arrange(desc(value)) %>%
#   top_n(150)
# select
x <- select(ds,
            status_id, screen_name, text, created_at, is_retweet, retweet_count, favorite_count,
            hashtags, urls_expanded_url, media_type, status_url,
            user_id, mentions_user_id, lang, followers_count, friends_count, country,
            statuses_count, favourites_count, account_created_at, verified,
            retweet_status_id, retweet_created_at, retweet_user_id, retweet_screen_name,
            retweet_retweet_count, retweet_favorite_count,
            retweet_followers_count, retweet_friends_count,retweet_statuses_count,
            retweet_location)
# languages to study
lang_ds <- c("es", "fr", "ca", "de", "it", "nl")
y <- filter(x, lang %in% lang_ds)
rm(lang_ds)
# exclude users from some locations
excl_rt_loc <- c("Caracas, Venezuela", "Leon, Mexico","Johannesburg, South Africa", "Venezuela",
                 "México","Montevideo, Uruguay", "Santiago, Chile", "Chile", "Asuncion, Paraguay", 
                 "Ecuador","Argentina","Buenos Aires, Argentina", "Guayaquil, Ecuador", 
                 "Ciudad de México, México", "Colombia", "Lima, Perú", "Panama", "Miami",
                 "Caracas - Venezuela", "RiyadLand", "Miranda, Venezuela", "Dominican Republic",
                 "Buenos Aires", "Ciudad de Panamá, Panamá", "Ciudad de México",
                 "República Dominicana", "Centro de la Ciudad de México", "El Salvador",
                 "Mérida, Venezuela", "Bogotá, D.C., Colombia", "Santiago de Chile", 
                 "Córdoba, Argentina","Lima, Peru")
excl_country <- c("Argentina", "Chile", "Mexico", "Colombia", "Ecuador", "Venezuela",
                 "Paraguay", "Dominican Republic", "Panama", "Brazil", "Guatemala", "Peru")
y <- y %>%
  filter(!retweet_location %in% excl_rt_loc) %>%
  filter(!country %in% excl_country) %>%
  filter(created_at > ymd("2020-03-11") & created_at < ymd("2020-03-25"))
rm(excl_rt_loc, excl_country)
rm(ds, x)
write_csv(y,"data/202003_corona_ibruprofen.csv")

# Calculate
y <- read_csv("data/202003_corona_ibuprofen_clean.csv")
# count reach by tweet
reach <- y %>%
  filter(!is.na(retweet_status_id)) %>%
  group_by(retweet_status_id) %>%
  summarise(tweet_reach = sum(followers_count)) %>%
  rename(status_id = retweet_status_id)
y <- left_join(y,reach, by = "status_id")
rm(reach)
y$tweet_reach <- y$tweet_reach %>% replace_na(0)

# top 25 rt_count by lang
top <- y %>%
  filter(!is_retweet & !is.na(lang)) %>%
  group_by(lang) %>%
  top_n(25, retweet_count) %>%
  arrange(created_at) %>%
  select(lang, status_id, screen_name, text,retweet_count,
         created_at,tweet_reach, verified)
table(top$lang)
# write_csv(top,"data/202003_corona_ibruprofen_top25.csv")

ds0 <- read_csv("data/202003_corona_followers_emergenciescat.csv")
ds1 <- read_csv("data/202003_corona_followers_gencat.csv")
ds2 <- read_csv("data/202003_corona_followers_salutcat.csv")
ds3 <- read_csv("data/202003_corona_followers_sanidad.csv")
list_official_followers <- full_join(ds0,ds1) %>%
  full_join(., ds2) %>%
  full_join(., ds3) %>%
  distinct(., user_id, .keep_all = TRUE)
rm(ds0,ds1,ds2,ds3)
list_no_infected_terms <- c("fake","canard","fakenews", "MedUni_Wien","Falschmeldungen",
                            "wien","AEMPSGOB","Agencia Española","evidencias", "#fakenews",
                            "#FakeNews", "fake news", "FakeNews","#FakeNews","#FakeNews,",
                            "#fakenewsvirus","#fakenewsvirus.")

y <- y %>%
  mutate(
    is_infected = ifelse(!str_detect(y$text, list_no_infected_terms), TRUE, FALSE),
    is_followed = ifelse((y$user_id %in% list_official_followers$user_id), TRUE, FALSE)
  )
table(y$is_infected)
table(y$is_followed)
rm(list_no_infected_terms, list_official_followers)

list_no_infected_tw <- as_tibble(c("1238782938344554496","1239200928231829504",
                         "1238843505885265920", "1238805916608315392",
                         "1238815009989328896", "1241481734241296384",
                         "1239197585455407104", "1239198403508174848",
                         "1238843505885265920", "1238814043550486528",
                         "1239589798572146688", "1238805916608315392",
                         "1238815009989328896", "1238908034082095104",
                         "1238832055665995776", "1240305439419531264",
                         "1240058603245047808", "1239198106316673024",
                         "1239889781968035840", "1239898632813453312",
                         "1238914960622845952", "1238801610555502592",
                         "1239224206098923520", "1239084545871355904",
                         "1239082153037742080", "1239055149856886784",
                         "1239079099001430016", "1239078992625483776",
                         "1239078676244893696", "1238955700711239680",
                         "1239297820919619584", "1239545152454168576",
                         "1239618136204337152", "1239546920936378368",
                         "1240618426835644416","1240005603361198080",
                         "1238913028348948480"))
list_no_infected_users <-  as_tibble(c("171299971","5552712","23745534",
                                       "5494392", "5553302", "2400260346", "5553082",
                                       "5580672", "5553592", "5550562", "5552802",
                                       "5560422", "5580452", "27007409", "5635572",
                                       "5559962", "969656894301368320"
))
y <- y %>%
  mutate(
    is_infected_2 = case_when(
      y$status_id %in% list_no_infected_tw$value ~ FALSE,
      y$retweet_status_id %in% list_no_infected_tw$value ~ FALSE,
      y$user_id %in% list_no_infected_users$value ~ FALSE,
      y$retweet_user_id %in% list_no_infected_users$value ~ FALSE
      )
  )

rm(list_no_infected_tw,list_no_infected_users)
table(y$is_infected)
table(y$is_infected_2)

r <- select(y, status_id, is_infected, is_infected_2) %>%
  mutate(
    is_infected_3 = case_when(
      is_infected & !is_infected_2 ~ FALSE,
      is_infected & is.na(is_infected_2) ~ TRUE,
      !is_infected & is.na(is_infected_2) ~ FALSE,
      !is_infected & !is_infected_2 ~ FALSE
    )
  )

y <- select(y,-is_infected)
r <- select(r, is_infected_3, status_id) %>%
  rename(is_infected = is_infected_3)
y <- left_join(y,r, by = "status_id")
rm(r)
table(y$is_infected)

# count false reach
top <- read_csv("data/202003_corona_ibruprofen_top25.csv")
y_top <- y %>%
  filter(
    status_id %in% top$status_id | retweet_status_id %in% top$status_id
  ) %>%
  select(status_id,retweet_status_id,lang, created_at, screen_name, is_retweet, 
         retweet_count,followers_count, account_created_at, verified, retweet_created_at,
         tweet_reach, is_followed, is_infected) %>%
  mutate(
    user_infected = case_when(
      status_id %in% top$status_id[top$is_infected == "FALSE"] ~ "FALSE",
      status_id %in% top$status_id[top$is_infected == "TRUE"] ~ "TRUE",
      retweet_status_id %in% top$status_id[top$is_infected == "FALSE"] ~ "FALSE",
      retweet_status_id %in% top$status_id[top$is_infected == "TRUE"] ~ "TRUE",
    )
  )
table(y_top$is_infected)
table(y_top$user_infected)
  
y_top <- y_top %>%
  mutate(
    user_type = case_when(
      is_infected & is_followed ~ "a",
      is_infected & !is_followed ~ "b",
      !is_infected & is_followed ~ "c",
      !is_infected & !is_followed ~ "d"
    )
  ) 
table(y_top$user_type)
# user_a: follow TRUE; infected TRUE  # user_b: follow FALSE; infected TRUE
# user_c: follow TRUE; infected FALSE # user_d: follow FALSE, infected FALSE
y_ca <- y_top %>%
  filter(lang == "ca")
table(y_ca$user_type)

# Count ----
# count tweets per day by language
y <- filter(y, !is.na(lang))
day_lang <- y %>%
  mutate(day = day(created_at)) %>%
  group_by(day,lang) %>%
  count() %>%
  mutate(date = paste0("2020-03-",day))

# count infected per day y_top
day_infec <- y_top %>%
  mutate(day = day(created_at)) %>%
  group_by(day,user_infected) %>%
  count() %>%
  mutate(date = paste0("2020-03-",day))
day_infec_lang <- y_top %>%
  mutate(day = day(created_at)) %>%
  group_by(day,lang,user_infected) %>%
  count() %>%
  mutate(date = paste0("2020-03-",day))
# count reach infected per day
day_infec_reach <- y_top %>%
  mutate(day = day(created_at)) %>%
  group_by(day,user_infected) %>%
  summarise(reach = sum(tweet_reach)) %>%
  mutate(date = paste0("2020-03-",day))
infec_lang <- y_top %>%
  mutate(day = day(created_at)) %>%
  group_by(lang,user_infected) %>%
  count() 
infec_lang_reach <- y_top %>%
  mutate(day = day(created_at)) %>%
  group_by(lang,user_infected) %>%
  summarise(reach = sum(tweet_reach))

y_ca_cor <- y_ca %>%
  select(is_infected,is_followed) %>%
  mutate(
    user_type = case_when(
      is_infected & is_followed ~ 1,
      is_infected & !is_followed ~ 2,
      !is_infected & is_followed ~ 3,
      !is_infected & !is_followed ~ 4
    )
  )

x_regressio <- glm(formula= is_infected ~ is_followed, data = y_ca, family="binomial")
summary(x_regressio)
confint(x_regressio)
mean(y_ca$is_infected)
sd(y_ca$is_infected)
mean(y_ca$is_followed)
sd(y_ca$is_followed)
write_csv(y_ca, "tuits_catalans.csv")
# tw_top <- y %>%
#   filter(!is_retweet & retweet_count > 100 &
#            created_at < ymd("2020-03-15") & created_at > ymd("2020-03-14")) %>% 
#   select(status_id,created_at,retweet_count,lang,tweet_reach,user_type) %>%
#   arrange(desc(retweet_count))
# 
# user_tweets <- y %>%
#   group_by(user_id) %>%
#   count() %>%
#   rename(user_tweets = n)
# 
# user <- y %>%
#   left_join(., user_tweets) %>%
#   select(user_id,lang, followers_count, friends_count,user_tweets,user_type) %>%
#   distinct(user_id, .keep_all = TRUE)
# rm(user_tweets)


# Plot ----
library(ggplot2)
library(ggthemes)
library(ggpubr)

p1 <- ggplot(day_lang, aes(x = day, y= n, group = lang, fill = lang)) +
  geom_area() +
  scale_x_continuous(
    breaks = day_lang$day,
    label = day_lang$day
  ) +
  labs(x = "March 2020", y ="", fill = "language") +
  ggtitle("Panel A. Daily tweets (by language)")  +
  theme_tufte() +
  theme(legend.position="top")

p2 <- ggplot(day_lang, aes(fill=lang, y=n, x=day)) + 
  geom_bar(position = "fill", stat ="identity") +
  scale_x_continuous(
    breaks = day_lang$day,
    label = day_lang$day
  ) +
  labs(x = "March 2020", y ="%", fill = "language") +
  ggtitle("Panel B. Daily tweets (% by language)")  +
  theme_tufte() +
  theme(legend.position="top")

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

p3 <- ggplot(day_infec_lang, aes(x=day, y=n/1000, fill=user_infected)) + 
  geom_bar(position="stack", stat ="identity") +
  scale_x_continuous(
    breaks = day_lang$day,
    label = day_lang$day
  ) +
  labs(x = "march 2020", y ="tweets (x,1000)", fill = "Infected") +
  #facet_grid(~lang) +
  ggtitle("Tweets posted") +
  theme_tufte()

p4 <- ggplot(day_infec_reach, aes(x=day, y=reach/1000000, fill=user_infected)) + 
  geom_bar(position="stack", stat ="identity") +
  scale_x_continuous(
    breaks = day_lang$day,
    label = day_lang$day
  ) +
  labs(x = "march 2020", y ="reach (x1,000,000)", fill = "Infected") +
  #facet_grid(~lang) +
  ggtitle("Tweets reach") +
  theme_tufte() 

ggarrange(p3, p4, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

ggplot(top, aes(x=created_at, y=log(retweet_count))) +
  geom_point(aes(shape=is_infected, color=lang)) +
  labs(x = "", y ="log(retweets)") +
  ggtitle("Top Tweets") +
  theme_tufte() +
  theme(legend.position="bottom")

# type_author (manual)
# 1. generic
# 2. credible sources. 1/involuntàriament  2/intencionadament
# 3. fake credible. Canals verificats per altres temes
# 4. profiteers. volen treure profit de la situació, alguns venen cures no provades

top <- top %>%
  mutate(
    type_author_label = case_when(
      type_author == 1 ~ "generic",
      type_author == 2 ~ "credible",
      type_author == 3 ~ "fake credible",
      type_author == 4 ~ "profiteers"
    )
  )
top <- top %>%
  mutate(
    is_verified = ifelse(!is_infected,T,F)
  )
df2 <- select(top, -lang)
supp.lang <- c("France", "Germany", "Netherlands", "Spain", "Catalonia", "Italy")
names(supp.lang) <- c("1_fr", "2_de", "3_nl", "4_es", "5_ca", "6_it")
top <- top %>%
  mutate(
    lang = case_when(
      lang == "fr" ~ "1_fr",
      lang == "de" ~ "2_de",
      lang == "nl" ~ "3_nl",
      lang == "es" ~ "4_es",
      lang == "ca" ~ "5_ca",
      lang == "it" ~ "6_it"
    )
  )

ggplot(top, aes(x=created_at, y=log(retweet_count))) +
  geom_point(data = df2, colour = "grey85") +
  geom_point(aes(shape=type_author_label, color=is_verified, size = 2)) +
  geom_point(alpha = 1/10) +
  labs(x = "", y ="log(retweets)") +
  guides(fill = guide_legend(reverse=TRUE),size = FALSE)+
  ggtitle("Top Tweets (by language)") +
  theme_stata() +
  theme(legend.position="bottom") +
  #facet_grid(rows = vars(lang), scales ="fixed")
  facet_wrap(vars(lang),
             labeller = labeller(lang = supp.lang))


ggplot(top, aes(x=created_at, y=log(retweet_count))) +
  geom_point(aes(shape=is_infected, color=type_author_label)) +
  labs(x = "", y ="log(retweets)") +
  ggtitle("Top Tweets (by Author)") +
  theme_tufte() +
  theme(legend.position="bottom")

a <- top %>%
  filter(is_infected) %>%
  group_by(type_author_label) %>%
  count()

c <- y_ca %>%
  group_by(user_type) %>%
  count()
