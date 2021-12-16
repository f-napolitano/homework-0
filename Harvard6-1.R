# Modulo 6 Harvard - Wrangling
# Seccion 2 - Tidy Data

library(tidyverse)
library(dslabs)
data(gapminder)

# ----- 2.1.2. Reshaping Data  -------------------
# crear e inspeccionar un tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country,year,fertility)
head(tidy_data)

# hacer la grafica tidy
tidy_data %>%
  ggplot(aes(year,fertility,color=country)) +
  geom_point()

# importar e inspeccionar el gapminder original
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

# uso de gather() para pasar de wide a tidy
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

# lo mismo que antes pero ahora no gather country
new_tidy_data <- wide_data %>% gather(year, fertility, -country)

# convertir nombre de columnas a integer y graficar
new_tidy_data <- wide_data %>% gather(year, fertility, -country, convert = TRUE)
new_tidy_data %>% ggplot(aes(year, fertility, color = country)) +
  geom_point()

# uso de la funcion spread (inverso de gather) para pasar de tidy a wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

# ----- 2.1.3. Separate and Unite -------------------
# importar datos
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather todas las columnas menos country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# usar separate para separar variables que estan en una misma columna
# pero manteniendo "life_expectancy" entero
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# ahora crear una columna por cada variable --> spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value)

# ----- 2.2.1. Combining Tables  -------------------
# combinar 2 tablas con distinta informacion pero que tienen filas en comun
# por ejemplo, los estados
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data("murders")
head(murders)

data("polls_us_election_2016")
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# graficar los votos respecto a la poblacion
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() + geom_text_repel() + scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") + geom_smooth(method = "lm", se = FALSE)

# que pasa si en las 2 tablas no todas las filas estan en ambas. Crear 2 tablas asi
tab1 <- slice(murders, 1:6) %>% select(state,population)
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2
# y ver distintas formas de unir las dos tablas
left_join(tab1,tab2)
right_join(tab1,tab2)
inner_join(tab1,tab2)
full_join(tab1,tab2)
semi_join(tab1,tab2) #deja en tab1 lo que tambien hay en tab2 pero no junta
anti_join(tab1,tab2) #opuesto a semi, deja en tab1 lo que no hay en tab2 y no junta

# ----- 2.2.2. Binding  -------------------
# para pegar 2 datasets con igual cantidad de filas --> bind_cols()
tab1 <- tab[,1:3]
tab2 <- tab[,4:6]
tab3 <- tab[,7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

# para pegar 2 datasets con la misma cantidad de columnas --> bind_rows()
tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

# ----- 2.2.3. Set Operators  -------------------
# interseccion de 2 tablas por filas
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)
# union de 2 tablas por filas
union(tab1, tab2)
# quedarse con lo que hay en tabla1 pero no esta en tabla 2
setdiff(tab1, tab2)
# setequal() compara dos conjuntos son iguales independientemente del orden
setequal(tab1, tab2)

# ----- 2.2. Ejercicios  -------------------
# instalar libraria con estadistica de bateos
library(Lahman)
top <- Batting %>% filter(yearID == 2016) %>% arrange(desc(HR)) %>% slice(1:10)
top %>% as_tibble()
Master %>% as_tibble() #estadistica demografica historica de jugadores
# hacer una tabla combinada con los 10 mejores del 2016
top_names <- top %>% left_join(Master) %>% select(playerID, nameFirst, nameLast, HR)

top_salary <- Salaries %>% filter(yearID == 2016) %>% right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

# top 10 HR que recibieron premio en 2016
awards <- AwardsPlayers %>% filter(yearID == 2016) %>% select(playerID)
top %>% select(playerID) %>% intersect(awards)

# los que recibieron premios pero no son top 10
awards %>% setdiff(select(top, playerID))

# ----- 2.3. Web Scrapping  -------------------
library(tidyverse)
# importar pagina web en R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)

tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab

tab <- tab %>% html_table()
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

# ----- 2.3.2 CSS Selectors  -------------------
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

guacamole <- list(recipe)
guacamole
# MLB players payroll exercise
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
num_table <- function(numero){
  return(html_table(nodes[[numero]]))
}
num_table(19)
tab_1 <- html_table(nodes[[10]], header = TRUE) %>% select(2:4)
tab_2 <- html_table(nodes[[19]], header = TRUE)
tab_general <- full_join(tab_1, tab_2, by = "Team")
tab_general
# brexit poll exercise
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- html_nodes(h, "table")
num_table <- function(numero){
  return(html_table(tab[[numero]], fill = TRUE))
}
num_table(6)
