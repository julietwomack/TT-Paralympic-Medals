# Libraries
library(tidyverse)
library(tidytuesdayR)

# Get data from TidyTuesdayR
data <- tidytuesdayR::tt_load('2021-08-03')
medals <- data$athletes

#############################
# Cleaning Code Starts Here #
#############################

# Column names
colnames(medals)

# Adding a Unique ID to each row for joins later if needed
medals_ID <- medals %>% 
  mutate(ID = row_number()) %>% 
  relocate(ID, .before = gender)

# Count by Gender: there are 144 NA's
medals_ID %>% group_by(gender) %>% count()

# Find where the NA's are for gender: All NA's are for Basketball events in 2012 or 2016.
filter_view <- medals_ID %>% filter(is.na(gender))

# Fix Gender NA's: filter_view$ID contains the ID numbers of athletes whose gender is NA, but the actual gender value was put int he event column. I will also change event to "Wheelchair Basketball" since every NA gender "type" is Basketball.
medals_gender_fix <- medals_ID %>%
  mutate(
    gender = if_else(ID %in% filter_view$ID, event, gender),
    event = if_else(ID %in% filter_view$ID, "Wheelchair Basketball",
                    event)
  )

# Check if fix corrected issue: There are no more NA's
medals_gender_fix %>% group_by(gender) %>% count()

# Group by Event: appears to be no abnormalities
view(medals_gender_fix %>% group_by(event) %>% count())

# Group by Medal: appears to be no abnormalities
medals_gender_fix %>% group_by(medal) %>% count()

# Find athlete rows with non-alphabet names: there are 10 athletes with a dash (-) as their name.
filter_view2 <- medals_gender_fix %>% 
  filter(!str_detect(athlete, "[A-Z]"))

#####  NOTES  #####

# Womens Double FITA Round Novice Paraplegic (1980): Only one competitor, Chiyoko Ohmae (won gold).

# Womens Short Metric Round Paraplegic (1980): Only two competitors during this event, Rosa Schweizer (gold) and Valerie Williamson (silver)

# Mens Double FITA Round C1-C2 (1984): Only one competitor, David Barefoot (gold).

# Mens Double FITA Round C3,C6 (1984): Only one competitor Thorne Philip (gold)

# Mens Double Short Metric Round Tetraplegi (1984): Only one competitor, Kenneth Holm (gold)

# Womens Double FITA Round Div. 3 (1984): Only two competitors Helen Hilderley (gold) and Beverley Leaper (silver)

##########

# Making athletes who are inputted as a dash into NOCOMPETITOR. filter_view2$ID contains the ID's who fit that scenario.
medals_athlete_fix <- medals_gender_fix %>%
  mutate(athlete = 
           if_else(ID %in% filter_view2$ID, 
                   "NOCOMPETITOR", athlete))

# Checking to see if the dashes were corrected.
view(medals_athlete_fix %>% 
       filter(!str_detect(athlete, "[A-Z]")))

# Separating the "athlete" column to find abnormal names since in "athlete" column all athlete's first names are all uppercase.
medals_athlete_sep <- medals_athlete_fix %>%
  separate(
    athlete,
    into = "athlete_one",
    sep = " ",
    convert = TRUE,
    remove = FALSE
  )


# Fix athlete names who are misspelled or duplicated

#####  NOTES #####

# ID 3545 and 3574 are redundant. There were only four members of the 4 x 100 and 4 x 200 team. So they will be deleted, the group ID numbers will be fixed for that team, and the incorrect spelling of HONG DukHo will be fixed.
medals_athlete_fix2 <- medals_athlete_sep %>%
  filter(!ID %in% c(3545, 3574)) %>%
  mutate(athlete =
           if_else(
             athlete == "HONG Duk",
             str_replace(
               athlete,
               pattern = "HONG Duk", "HONG DukHo"),
             athlete),
         grp_id = if_else(ID == 3544 | ID == 3573,1, grp_id),
         grp_id = if_else(ID == 3546 | ID == 3575,2, grp_id),
         grp_id = if_else(ID == 3547 | ID == 3576,3, grp_id),
         grp_id = if_else(ID == 3548 | ID == 3577,4, grp_id))

# Fix incorrect spelling of COAN McKenzie, remove the incorrect spelling, and find group_id number for that team
medals_athlete_fix3 <- medals_athlete_fix2 %>%
  filter(!ID == 16152) %>%
  mutate(athlete =
           if_else(
             athlete == "COAN Mc",
             str_replace(
               athlete,
               pattern = "COAN Mc", "COAN McKenzie"),
             athlete),
         grp_id = if_else(ID == 16150, 1, grp_id),
         grp_id = if_else(ID == 16151, 2, grp_id),
         grp_id = if_else(ID == 16153, 3, grp_id),
         grp_id = if_else(ID == 16154, 4, grp_id))

# Find cases where athlete_one is title case
filter_view3 <- medals_athlete_fix3 %>%
  filter(str_detect(athlete_one, "[A-Z][a-z]")
         & is.na(country))

# filter_view3$ID contains ID numbers where a country won the event, not a single individual. Athlete will be changed to TEAM.
medals_athlete_fix4 <- medals_athlete_fix3 %>%
  mutate(country = if_else(ID %in% filter_view3$ID,
                           athlete,
                           country),
         athlete = if_else(ID %in% filter_view$ID, 
                           "TEAM",
                           athlete))

# filter_view4$ID contains ID numbers where a country tied with another country for Bronze. Athlete will be changed to TEAM. And a new column Country2 will be created, then combined into country_name, and country and country2 will be dropped (e.g. United States of America / China)

filter_view4 <- medals_athlete_fix4 %>%
  filter(
    str_detect(athlete_one, "[A-Z][a-z]") &
      athlete != "TEAM" &
      athlete != country
    )

medals_athlete_fix5 <- medals_athlete_fix4 %>%
  mutate(
    country2 = 
      if_else(
        ID %in% filter_view4$ID, athlete, NULL),
    athlete = if_else(ID %in% filter_view4$ID, "TEAM", athlete)) %>%
  unite(country_name,
        country,
        country2,
        sep = " / ",
        na.rm = TRUE) %>%
  select(-athlete_one)

# Find other instances where athlete is abnormal name: everything looks ok

view(medals_athlete_fix5 %>% group_by(athlete) %>% count())

# Find any abnormal ABB (country abbreviation): 49 NA and 11 with a dash (-)
view(medals_athlete_fix5 %>% group_by(abb) %>% count())

# Find ID's where ABB is a dash
filter_view5 <- medals_athlete_fix5 %>% filter(abb == "-")

# ABB == "-" are instances where there was no competitor. So athlete's with ID's from filter_view5$ID will be changed from NA to "NOCOMPETITOR" and ABB will be changed to NC
medals_abb_fix <- medals_athlete_fix5 %>%
  mutate(
    athlete = if_else(ID %in% filter_view5$ID,
                      "NOCOMPETITOR",
                      athlete),
    abb = if_else(ID %in% filter_view5$ID,
                  "NC",
                  abb)
  )
  
# Find where abb is NA
filter_view6 <- medals_abb_fix %>% 
  filter(is.na(abb))

# filter_view6$ID contains the ID's where abb is NA. If it is NA then change abb to "NC"
medals_abb_fix2 <- medals_abb_fix %>%
  mutate(abb = 
           if_else(ID %in% filter_view6$ID, "NC", abb))


# Find any abnormalities with country_name: 14393 with blank value, 39 with a dash, and multiple ones where the country name is an athlete's name.
view(medals_abb_fix2 %>% group_by(country_name) %>% count())

# Instances where country_name is a dash: these instances are when abb = NC, thus we will change country_name to NOCOUNTRY
view(medals_abb_fix2 %>% filter(country_name == "-"))

medals_country_name_fix <- medals_abb_fix2 %>%
  mutate(country_name = if_else(country_name == "-",
                                "NOCOUNTRY", country_name))



# Find instances where an athlete's name is a country
filter_view7 <-  medals_country_name_fix %>% 
       group_by(country_name) %>% 
       count() %>%
       filter(n < 10 & !str_detect(country_name, " / "))

# List of countries in filter_view7
country_vals <- c("Azerbaijan", "Colombia", "Croatia",
                  "Cuba", "Jamaica", "Nigeria",
                  "Portugal", "Serbia", "South Africa",
                  "Tunisia", "USSR", "Venezuela")

# Remove countries in country_vals from filter_view7
filter_view7a <- filter_view7 %>% 
  filter(!country_name %in% country_vals)

# If country_name has an athlete's name, then put that value into the athlete name column and change country_name to blank.
medals_country_name_fix2 <- medals_country_name_fix %>%
  mutate(athlete = 
           if_else(country_name %in% filter_view7a$country_name,
                   country_name, athlete),
         country_name = 
           if_else(country_name %in% filter_view7a$country_name,
                   "", country_name))


# See if there are more abnormal country_name instances: there are 14488 blanks that need to be filled in. Find where they are.
view(medals_country_name_fix2 %>% 
       group_by(country_name) %>% 
       count())

# Appears that every instance where country_name is blank, abb is not NA or blank, so we can use the abbreviations to fill in the country_name.
view(medals_country_name_fix2 %>%
       filter(country_name == ""))

# Add the correct country to each row where country_name is blank using case when
medals_country_name_fix3 <- medals_country_name_fix2 %>%
  mutate(
    country_name =
      case_when(
        abb == "ALG" ~ "Algeria",
        abb == "ANG" ~ "Angola",
        abb == "ARG" ~ "Argentina",
        abb == "AUS" ~ "Australia",
        abb == "AUT" ~ "Austria",
        abb == "AZE" ~ "Azerbaijan",
        abb == "BAH" ~ "The Bahamas",
        abb == "BEL" ~ "Belgium",
        abb == "BIH" ~ "Bosnia and Herzegovina",
        abb == "BIR" ~ "Myanmar",
        abb == "BLR" ~ "Belarus",
        abb == "BOT" ~ "Botswana",
        abb == "BRA" ~ "Brazil",
        abb == "BRN" ~ "Brunei",
        abb == "BUL" ~ "Bulgaria",
        abb == "CAN" ~ "Canada",
        abb == "CHI" ~ "Chile",
        abb == "CHN" ~ "China",
        abb == "CIV" ~ "Ivory Coast",
        abb == "COL" ~ "Columbia",
        abb == "CPV" ~ "Cape Verde",
        abb == "CRO" ~ "Croatia",
        abb == "CUB" ~ "Cuba",
        abb == "CYP" ~ "Cyprus",
        abb == "CZE" ~ "Czech Republic",
        abb == "DEN" ~ "Denmark",
        abb == "DOM" ~ "Dominican Republic",
        abb == "EGY" ~ "Egypt",
        abb == "ESP" ~ "Spain",
        abb == "EST" ~ "Estonia",
        abb == "ETH" ~ "Ethiopia",
        abb == "EUN" ~ "Unified Team",
        abb == "FIJ" ~ "FIJI",
        abb == "FIN" ~ "Finland",
        abb == "FRA" ~ "France",
        abb == "FRG" ~ "FR Germany",
        abb == "FRO" ~ "Faroe Islands",
        abb == "GBR" ~ "Great Britain",
        abb == "GDR" ~ "East Germany",
        abb == "GER" ~ "Germany",
        abb == "GRE" ~ "Greece",
        abb == "HKG" ~ "Hong Kong",
        abb == "HUN" ~ "Hungary",
        abb == "INA" ~ "Indonesia",
        abb == "IND" ~ "India",
        abb == "IPP" ~ "Serbia and Montenegro",
        abb == "IRI" ~ "Iran",
        abb == "IRL" ~ "Ireland",
        abb == "IRQ" ~ "Iraq",
        abb == "ISL" ~ "Iceland",
        abb == "ISR" ~ "Israel",
        abb == "ITA" ~ "Italy",
        abb == "JAM" ~ "Jamaica",
        abb == "JOR" ~ "Jordan",
        abb == "JPN" ~ "Japan",
        abb == "KAZ" ~ "Kazakhstan",
        abb == "KEN" ~ "Kenya",
        abb == "KOR" ~ "Korea",
        abb == "KSA" ~ "Saudi Arabia",
        abb == "KUW" ~ "Kuwait",
        abb == "LAO" ~ "Laos",
        abb == "LAT" ~ "Latvia",
        abb == "LBA" ~ "Libya",
        abb == "LTU" ~ "Lithuania",
        abb == "LUX" ~ "Luxembourg",
        abb == "MAR" ~ "Morocco",
        abb == "MAS" ~ "Malaysia",
        abb == "MDA" ~ "Moldovo",
        abb == "MEX" ~ "Mexico",
        abb == "MGL" ~ "Mongolia",
        abb == "MOZ" ~ "Mozambique",
        abb == "NAM" ~ "Namibia",
        abb == "NC" ~ "NOCOUNTRY",
        abb == "NED" ~ "Netherlands",
        abb == "NGR" ~ "Nigeria",
        abb == "NOR" ~ "Norway",
        abb == "NZL" ~ "New Zealand",
        abb == "PAK" ~ "Pakistan",
        abb == "PAN" ~ "Panama",
        abb == "PER" ~ "Peru",
        abb == "PHI" ~ "Philippines",
        abb == "PLE" ~ "Palestine",
        abb == "PNG" ~ "Papua New Guinea",
        abb == "POL" ~ "Poland",
        abb == "POR" ~ "Portugal",
        abb == "PUR" ~ "Puerto Rico",
        abb == "QAT" ~ "Qatar",
        abb == "RSA" ~ "South Africa",
        abb == "RUS" ~ "Russia",
        abb == "RWA" ~ "Rwanda",
        abb == "SCG" ~ "Serbia",
        abb == "SGP" ~ "Singapore",
        abb == "SLO" ~ "Slovenia",
        abb == "SRB" ~ "Serbia",
        abb == "SRI" ~ "Sri Lanka",
        abb == "SUD" ~ "Sudan",
        abb == "SUI" ~ "Switzerland",
        abb == "SVK" ~ "Slovakia",
        abb == "SWE" ~ "Sweden",
        abb == "SYR" ~ "Syria",
        abb == "TCH" ~ "Czechoslovakia",
        abb == "THA" ~ "Thailand",
        abb == "TPE" ~ "Chinese Taipei",
        abb == "TTO" ~ "Trinidad and Tobago",
        abb == "TUN" ~ "Tunisia",
        abb == "TUR" ~ "Turkey",
        abb == "UAE" ~ "United Arab Emirates",
        abb == "UGA" ~ "Uganda",
        abb == "UKR" ~ "Ukraine",
        abb == "URS" ~ "USSR",
        abb == "URU" ~ "Uruguay",
        abb == "USA" ~ "United States of America",
        abb == "UZB" ~ "Uzbekistan",
        abb == "VEN" ~ "Venezuela",
        abb == "VIE" ~ "Vietnam",
        abb == "YUG" ~ "Yugoslavia",
        abb == "ZIM" ~ "Zimbabwe",
        TRUE ~ country_name
      )
  )

# Find instances where country_name may still be blank: 0 instances
view(medals_country_name_fix3 %>% 
       group_by(country_name) %>% 
  count())

# See if there are any abnormal columns in grp_id: there are 14428 instances of NA but that is because it was not a team event
view(medals_country_name_fix3 %>% group_by(grp_id) %>% count())

# See if there are any abnormal instances of "type": Nothing/looks good
view(medals_country_name_fix3 %>%
  group_by(type) %>% count())

# See if there are any abnormal instances of "year": No, looks good
view(medals_country_name_fix3 %>%
       group_by(year) %>% count())

# See if there are any abnormal instances of "guide": there are some instances where the name begins with the word "Guide:", so I will remove that from those strings for more consistency.
view(medals_country_name_fix3 %>%
       group_by(guide) %>% count())

# Obtain the ID numbers where the string contains "Guide:"
filter_view9 <- medals_country_name_fix3 %>% 
  filter(str_detect(guide, "Guide: "))

# Remove "Guide:" string from guide names and then trim off any white space
medals_guide_fix <- medals_country_name_fix3 %>%
  mutate(guide = str_replace(guide, "Guide:*", ""),
         guide = str_trim(guide, side = "both"))

# Find any existing NA's: These are instances where a TEAM won and not just a single individual.
filter_view10 <- medals_guide_fix %>% 
  filter(is.na(athlete) & country_name != "NOCOUNTRY")

# Change any NA athletes to "TEAM"
medals_athlete_fix6 <- medals_guide_fix %>%
  mutate(athlete = 
           if_else(ID %in% filter_view10$ID, "TEAM", athlete))

# Remove any rows where country_name = NOCOUNTRY, change name of abb to country_code
medals_clean <- medals_athlete_fix6 %>%
  filter(!country_name == "NOCOUNTRY") %>%
  mutate(country_code = abb) %>%
  select(-abb) %>% 
  relocate(country_code, .before = country_name)

###########################
# Cleaning Code ENDS Here #
###########################

#########################################
# Exploratory Data Analysis STARTS Here #
#########################################

# Which individual athlete has won the most medals at the Paralympics (1980 - 2016)?: ZORN Trischa with 46 medals for Women and KIM Kyung Mook for MEN
medals_clean %>% 
  filter(athlete != "TEAM") %>% 
  group_by(gender) %>%
  count(athlete) %>% 
  arrange(desc(n)) %>%
  head(5) %>% ggplot2::ggplot() + 
  geom_bar(mapping = aes(x = athlete, y = n, fill = gender), 
           stat = "identity") +
  labs(x = "Athlete", y = "Medal Count") + 
  ggtitle("Paralympic Athletes with the Most Medals (1980 -2016)") +
  guides(fill = guide_legend(title = "Gender", reverse = T)) +
  scale_fill_manual(values = c("#a0c4ff", "#ffadad")) + 
  scale_y_continuous(expand = c(0,0)) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.50))

# Which country has won the most medals for TEAM events (1980 - 2016)?: The USA and then Great Britain
medals_clean %>% 
  filter(athlete == "TEAM") %>%
  count(country_name) %>% 
  arrange(desc(n)) %>%
  head(5) %>% ggplot() + 
  geom_bar(mapping = aes(x = country_name, y = n, 
                         fill = country_name), stat = "identity") +
  scale_y_continuous(expand = c(0,0), limits = c(0,80)) +
  labs(x = "Country", y = "Medal Count") +
  ggtitle("Top 5 Countries with the Most Paralympic Medals for Team Sports (1980 - 2016)") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.50), 
        legend.position = "none")

# Has medals won by the top 5 countries increased or decreased overtime? Only Australia has seen a decrease in Paralympic Medals overtime and then FR Germany (West Germany) stopped competing in the Olympics after reunification with East Germany in 1990.
medals_clean %>% 
  filter(country_name %in% c("Australia", "FR Germany",
                             "Great Britain", "Netherlands",
                             "United States of America")) %>%
  group_by(year, country_name) %>% 
  count() %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = n, color = country_name)) +
  ggtitle("Medals Over Time for Top 5 Countries (1980 - 2016)") +
  labs(x = "Year", y = "Medal Count") + 
  guides(color = guide_legend(title = "Country")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.50)) 

# For the United States, what type of events have they won the most gold medals in? ATHLETICS
medals_clean %>% 
  filter(medal == "Gold" & 
           country_name == "United States of America") %>%
  group_by(type) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  head(5)

# Which ATHLETIC events have the United States won the most gold medals in? Looks like track related events.
medals_clean %>% 
  filter(medal == "Gold" & 
           country_name == "United States of America" &
           type == "Athletics") %>%
  group_by(event) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  head(5)

# Which United States Paralympic Athlete has won the most medals? ZORN Trischa followed by LONG Jessica, both are swimmers.
medals_clean %>% 
  filter(country_name == "United States of America" &
           athlete != "TEAM") %>%
  group_by(gender, athlete, type) %>% 
  count() %>% arrange(desc(n))

# What type of event has the United States won the least number of medals in at the Paralympics? FENCING
medals_clean %>% 
  filter(country_name == "United States of America") %>%
  group_by(type) %>% count() %>% arrange(n) %>% head(5)

#######################################
# Exploratory Data Analysis ENDS Here #
#######################################

# Export Cleaned Data into CSV for Tableau Dashboard Creation
write_excel_csv(medals_clean, "medals_clean.csv")
