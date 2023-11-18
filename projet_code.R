to_be_loaded <- c("devtools",
                  "tidyverse", 
                  "ggforce",
                  "nycflights13",
                  "patchwork", 
                  "glue",
                  "DT", 
                  #    "gt",
                  "kableExtra",
                  "viridis",
                  "skimr",
                  "knitr",
                  "unilur",
                  "restatapi",
                  "SparseM",
                  "MatrixModels",
                  "ggpubr",
                  "cowplot")

for (pck in to_be_loaded) {
  if (!require(pck, character.only = TRUE)) {
    install.packages(pck)
    stopifnot(require(pck, character.only = T))
  }  
}

Pop_by_emp_by_sex_age_edu<- get_eurostat_data("cens_hnelev")
emp_by_emp_by_sex_age <- get_eurostat_data("lfsi_emp_a")
emp_by_emp_by_sex <- get_eurostat_data("tesem010")
#emp_by_sex <- emp_by_emp_by_sex[-which(emp_by_emp_by_sex$geo %in% c("EA20","EU27_2020")),]
df <- emp_by_emp_by_sex %>%
  select(sex,age,geo,time,values) %>%
  filter(!geo %in% c("EA20","EU27_2020")) # supprimer les lignes qui ont EA20 ou EU27_2020 dans la variable geo

glimpse(df) 
skimr :: skim(df) # better than summary()

df <- df %>%
  filter(!is.na(values)) # exclure missing values
# boxplot
df %>%
  ggplot(aes(x = time, y = values)) +
  geom_boxplot() +
  labs(title = "Boxplot de taux d'emploi en fonction d'année")

# scatterplot
df %>%
  ggplot(aes(x = time, y = values)) +
  geom_point(stat = "summary", fun = "mean", color = "blue")

# violin
df %>%
  ggplot(aes( x = df$sex,
              y = values)) +
  geom_violin(outlier.shape=2,
              outlier.colour = 'red', 
              outlier.alpha=.5, 
              coef=1.5)

#density
df %>% tibble(x=values) %>%
  ggplot() +
  aes(x=x) +
  geom_rug(alpha=.1) +
  geom_histogram(aes(y= after_stat(density)),
                 alpha=.5,
                 fill="white",
                 colour="black",
                 bins=30)  +
  geom_density()+
  ggtitle("Histogram") +
  labs(caption="Sample size=1000, 30 bins") 

emp_by_niv_edu <- get_eurostat_data("lfsi_educ_a")

emp_by_niv_edu %>%
  skimr :: skim()
levels(emp_by_niv_edu$isced11)

emp_by_emp_by_sex_age %>%
  ggplot(aes(x = age, y = values)) +
  geom_point(stat = "summary", fun = "mean", color = "blue") 

#install.packages("restatapi")
get_eurostat_toc()
search_eurostat_toc("employment")
df %>%
  pivot_longer(cols = colnames(df)[2:ncol(df)],
               names_to = "year",
               values_to = "value") %>%
  filter(!is.na(as.numeric(value))) %>%
  mutate(value = as.numeric(value)) %>%
  #  group_by(year)%>%
  #  summarise(.f - )
  ggplot(aes(x = year,
             y = value)) + geom_boxplot()

#search_eurostat_toc("Employment and activity by sex and age - annual data")
emp_by_sex_age %>%
  glimpse()

edu <- get_eurostat_data("educ_uoe_enra03")
result <- edu %>%
  group_by(time) 

by_sex <- get_eurostat_data("tesem010")
by_sex %>%
  glimpse()

group_by_sex1 <- by_sex %>%
  group_by(sex) %>%
  summarise(mean_employment_by_sex = mean(values))
group_by_sex1 %>%
  glimpse()

group_by_year <- by_sex %>%
  group_by(time) %>%
  summarise(mean_employment_by_time = mean(values))

emp_by_emp_by_sex %>%
  ggplot(aes( x = emp_by_emp_by_sex$time,
              y = values)) +
  geom_boxplot(outlier.shape=2,
               outlier.colour = 'red', 
               outlier.alpha=.5, 
               coef=1.5)

emp_by_emp_by_sex %>%
  ggplot(aes( x = emp_by_emp_by_sex$sex,
              y = values)) +
  geom_violin(outlier.shape=2,
              outlier.colour = 'red', 
              outlier.alpha=.5, 
              coef=1.5)
```

```{r}
emp_by_emp_by_sex_age %>% tibble(x=values) %>%
  ggplot() +
  aes(x=x) +
  geom_rug(alpha=.1) +
  geom_histogram(aes(y= after_stat(density)),
                 alpha=.5,
                 fill="white",
                 colour="black",
                 bins=30)  +
  ggtitle("Histogram") +
  labs(caption="Sample size=1000, 30 bins") 

summary(by_sex$values)

by_sex %>% ggplot(aes(x = time, y = values)) + 
  geom_point() 