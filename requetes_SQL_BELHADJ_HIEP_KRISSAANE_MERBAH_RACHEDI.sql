-- Création de la base de données 
create database projet_FAO;

-- Importation des bdd (script R) 

-- library(RMySQL)
-- con <- dbConnect(RMySQL::MySQL(),
--               dbname = "projet_FAO",
--  			 host = "localhost",
--               port = 3306,
--               user = "root",
--               password = "12345678",
-- )

-- dbSendQuery(con, "SET GLOBAL local_infile = true;") # <--- Added this
-- dbWriteTable(con, name= "ani_and_veg", value= ani_and_veg, append= TRUE, temporary= FALSE, row.names=FALSE)
-- dbWriteTable(con, name= "food_security_indicators", value= food_security_indicators, append= TRUE, temporary= FALSE, row.names=FALSE)
-- dbWriteTable(con, name= "food_balance", value= food_balance, append= TRUE, temporary= FALSE, row.names=FALSE)
-- dbWriteTable(con, name= "population", value= population, append= TRUE, temporary= FALSE, row.names=FALSE)

-- dbDisconnect(con)
 
use projet_FAO;

-- 1. Les 10 pays ayant le plus haut ratio disponibilité alimentaire/habitant en termes de protéines (en kg) par habitant, puis en termes de kcal par habitant.

select country, protein_supply_quantity_gcapitaday/1000 as dispo_alim_prot
from ani_and_veg
group by country
limit 10;

select country, food_supply_kcalcapitaday as kcal_habitant
from ani_and_veg
group by country
limit 10;

-- 2. Pour chaque année disponible, les 10 pays ayant le plus faible ratio disponibilité alimentaire/habitant en termes de protéines (en kg) par habitant. Le nombre de lignes de la table renvoyée sera donc égal à 10 fois le nombre d'années disponibles.

select country, year, protein_supply_quantity_gcapitaday/1000 as dispo_alim_prot
from ani_and_veg 
where country is in (select country, protein_supply_quantity_gcapitaday/1000 as dispo_alim_prot
from ani_and_veg
group by country
order by dispo_alim_prot desc
limit 10)
group by year;

-- 3. La quantité totale (en kg) de produits perdus par pays et par année. La table renvoyée contiendra donc une ligne par couple (pays, année).

select losses, year, country 
from ani_and_veg
group by year, country
having losses is not null

-- 4. Les 10 pays pour lesquels la proportion de personnes sous-alimentées est la plus forte.

select Area,(sum(Value)/6*1000000) as val_sous_nutritition
from projet_FAO.food_security_indicators
group by Area
order by val_sous_nutritition desc
limit 10;
 
-- 5. Les 10 produits pour lesquels le ratio Autres utilisations/Disponibilité intérieure est le plus élevé.

select item_code, other_uses/stock_variation as ratio, country
from ani_and_veg
group by item_code
having ratio is not null
order by ratio desc
limit 10;

-- 5.1 

-- Combien de personnes décèdent des causes de la faim ?
-- plus de 9 millions de morts par an en 2000 selon la FAO 

-- Quelles sont les prévisions de population mondiale en 2050 ? 
-- 9,1 milliards 

