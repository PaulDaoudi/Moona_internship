# Travail de Paul Daoudi pour Moona

install.packages("mongolite")
library(mongolite)
db = mongo("sleeps", url = "mongodb://ds_exo:moonads2017@ds129720.mlab.com:29720/heroku_pfr804n7")
db.get.databases(db)
mydata <- db$find()
View(mydata)


# Question 1 : 


View(mydata$properties)
View(as.POSIXct(mydata$properties$primary_sleep_period_end_timestamp, origin="1970-01-01"))

user <- unique(mydata$user)

### Il n'y a que 4 utilisateurs dans la base de données. Pour chaque utilisateur, nous allons faire une analyse des caractéristiques pendant la nuit.

user1 <- subset(mydata, user == 320417)
user2 <- subset(mydata, user == 220732)
user3 <- subset(mydata, user == 305652)
user4 <- subset(mydata, user == 320126)

### On prend les propriétés de chaque client. Chaque ligne de ce data frame correspond à une nuit.

user1_prop <- as.data.frame(user1$properties)
user2_prop <- as.data.frame(user2$properties)
user3_prop <- as.data.frame(user3$properties)
user4_prop <- as.data.frame(user4$properties)

### Les clients 1 et 4 n'ont qu'une seule donnée. On les visualise, mais il n'y a pas besoin d'étude statistique pour les analyser.

# Client 1 : Il n'y a pas beaucoup d'informations. Sleep_latency, sleep_time_target, resting_time_heart n'ont pas de valeur donc le total sleep_score est à 0.

# Client 4 : Même s'il n'y a qu'une seule nuite, les informations sont plus complètes :
  ## - sleep_latency : 960
  ## - sleep_time_target : 28800 secondes (8 heures)
  ## - primary_sleep_period_total_sleep_duration : 18330 (5 heures). Il a donc dormi 5 heures alors que son objectif était de dormir 8 heures.
  ## - sleep_efficiency : 0.7883366 
  ## - resting_heart_rate : 53.32031
  ## - average_respiration_rate : 16.67347
  ## - total_sleep_score : 66

# Client 2 : Il y a beaucoup d'information (103 nuits). Faisons donc une étude statistique. Grâce à R, il existe une fonction qui nous sort une étude statistique pour chaque caractéristique.
summary(user2_prop)
  ## sleep_latency : 
    ### moyenne : 644.7
    ### min : 0
    ### max : 3480
  ## sleep_time_target : 
    ### moyenne : 27 883 (environ 7h45)
    ### min : 27 000 (7h30)
    ### max : 27 900 (7h45)
  ## primary_sleep_period_total_sleep_duration : 
    ### moyenne : 24 268 (environ 6h45)
    ### min : 0 (nuit blanche ou bien valeur non renseignée ?)
    ### max : 37 447 (environ 10h20)
      ### Remarque : les valeurs 0, s'il s'agit de valeurs non renseignées, baissent la moyenne donc il se peut que la moyenne ne soit pas très pertinente.
  ## sleep_efficiency : 
    ### moyenne : 0.9466
    ### min : 0.7308
    ### max : 1
      ### Son sommeil est donc très efficace : lorsqu'il passe du temps sur son lit, il est très souvent endormi.
  ## resting_heart_rate : 
    ### moyenne : 54.67
    ### min : 39.99
    ### max : 105.87
  ## average_respiration_rate :
    ### moyenne : 14.40 
    ### min : 17.94
    ### max : 23.29
  ## total_sleep_score : 
    ### moyenne : 75.85
    ### min : 0
    ### max : 100
      ### Si les valeurs correspondant à 0 dans primary_sleep_period_total_sleep_duration sont des valeurs non-renseignées, alors la moyenne du score devrait donc être supérieure. Il s'agit d'un grand dormeur, dont le sommeil est réparateur.

# Client 3 : de même, beaucoup de nuits sont renseignées.
summary(user3_prop)
  ## sleep_latency : 
    ### moyenne : 390
    ### min : 0
    ### max : 1080
  ## sleep_time_target :
    ### moyenne : 27 000 secondes (7h30)
    ### min : 7h30
    ### max : 7h30
  ## primary_sleep_period_total_sleep_duration :
    ### moyenne : 10912 (3 heures)
    ### min : 23032 (6h25)
    ### max : 32226 (9h)
  ## sleep_efficiency :
    ### moyenne : 0.9740
    ### min : 0.8566
    ### max : 1.0000
  ## resting_heart_rate : 
    ### moyenne : 59.58 
    ### min : 52.81
    ### max : 74.34
  ## average_respiration_rate :
    ### moyenne : 17.08 
    ### min : 18.49
    ### max : 21.13
  ## total_sleep_score : 
    ### moyenne : 72.59
    ### min : 21.00
    ### max : 100.00  

summary(mydata$properties)


# Question 2 : on visualise les données de time_value_tracks.


View(mydata$time_value_tracks)

user1_tvt <- as.data.frame(user1$time_value_tracks)
user2_tvt <- as.data.frame(user2$time_value_tracks)
user3_tvt <- as.data.frame(user3$time_value_tracks)
user4_tvt <- as.data.frame(user4$time_value_tracks)

# Sleep_cycles :

### On commence par prendre l'exemple de la 2ème et la 3ème nuit :

View(mydata$time_value_tracks$sleep_cycles);
nuit1_sc <- as.data.frame(mydata$time_value_tracks$sleep_cycles[2,1][[1]]);
nuit2_sc <- as.data.frame(mydata$time_value_tracks$sleep_cycles[3,1][[1]]);

# Actigram_epochwise :

View(mydata$time_value_tracks$actigram_epochwise);
nuit1_ae <- as.data.frame(mydata$time_value_tracks$actigram_epochwise[2,1][[1]]);
nuit2_ae <- as.data.frame(mydata$time_value_tracks$actigram_epochwise[3,1][[1]]);

# Heart Rate Curve

View(mydata$time_value_tracks$heart_rate_curve);
nuit1_hr <- as.data.frame(mydata$time_value_tracks$heart_rate_curve[2,1][[1]]);
nuit2_hr <- as.data.frame(mydata$time_value_tracks$heart_rate_curve[3,1][[1]]);

# Sleep Stages

View(mydata$time_value_tracks$sleep_stages);
nuit1_ss <- as.data.frame(mydata$time_value_tracks$sleep_stages[2,1][[1]]);
nuit2_ss <- as.data.frame(mydata$time_value_tracks$sleep_stages[3,1][[1]]);

# Le sleep_cycle :

## Voici les résultats du sleep_cycle pour la 1ère nuit :

fnuit1_sc <- function() {
  x <- list();
  for (k in 1:length(nuit1_sc[1][[1]])){
    x <- append(x,as.POSIXct(nuit1_sc[k,1][[1]], origin="1970-01-01"));
  }
  print(x);
  plot(x,nuit1_sc[2][[1]],main="Sleep_cycles pour la nuit 1");
}

fnuit1_sc()

## Pour la 2ème nuit :

fnuit2_sc <- function() {
  x <- list();
  for (k in 1:length(nuit2_sc[1][[1]])){
    x <- append(x,as.POSIXct(nuit2_sc[k,1][[1]], origin="1970-01-01"));
  }
  print(x);
  plot(x,nuit2_sc[2][[1]],main="Sleep_cycles pour la nuit 2");
}

fnuit2_sc()

### Remarque préliminaire : l'énoncé nous dit que le sommeil est le plus profond pour sleep_cycle = 1, alors que d'après les courbes, c'est pour sleep_cycle = 1. Nous avons donc considéré cela pour la suite de l'exercice.

### La profondeur du sommeil dépend de l'horaire, et des nuits mais on peut observer plusieurs phénomènes commun à tous les clients.
### Il existe 4 cycles de sommeils. Le premier est très court : lorsque le client s'endort, son sommeil devient rapidement très prodond, pour atteindre un stade où le sommeil est le plus profond.
### Ensuite, la profondeur du sommeil augmente pour atteindre des valeurs aléatoires (il arrive parfois, comme on peut le voir dans la nuit 2 que le patient se réveille pendant ce cycle). Ensuite, il redescend, et le client est dans un état de profond sommeil.
### Lors du 3ème cycle, cette profondeur remonte et réveille le client. Puis, le client retombe dans un sommeil profond. 
### Enfin, le client se réveille.

### Ainsi, durant la nuit, la profondeur du sommeil varie dans 3 cycles. Dans chaque cylcle, elle augmente, puis diminue pour atteindre souvent la valeur d'un profond sommeil.

### On peut penser que lorsque la profondeur du sommeil devient très faible, voire nulle, pendant un instant, le client se réveille. Pourtant, comme il retombe dans un sommeil profond, on considère qu'il ne s'est pas réveillé (on le verra dans les courbes des stades du sommeil).
### Remarque sur la nuit 2 : la profondeur du sommeil est nulle de 4h30 à 5h30. On en déduit que le client s'est réveillé pendant cet intervalle.

### Visualisons le sleep_cycle en fonction de l'heure. On prend tous les éléments dans un même data frame, puis on fait une régression linéaire pour avoir la valeur moyenne du sleep_cycle selon l'horaire. Pour l'instant, on ne prend aucune information en compte concernant le client.

# Actigram_epochwise :

### 1ère nuit :

fnuit1_ae <- function() {
  x <- list();
  for (k in 1:length(nuit1_ae[1][[1]])){
    x <- append(x,as.POSIXct(nuit1_ae[k,1][[1]], origin="1970-01-01"));
  }
  print(x);
  plot(x,nuit1_ae[2][[1]],main="Actigram_epochwise pour la nuit 1");
}

fnuit1_ae()

### Pour la 2ème nuit :

fnuit2_ae <- function() {
  x <- list();
  for (k in 1:length(nuit2_ae[1][[1]])){
    x <- append(x,as.POSIXct(nuit2_ae[k,1][[1]], origin="1970-01-01"));
  }
  print(x);
  plot(x,nuit2_ae[2][[1]],main="Actigram_epochwise pour la nuit 2");
}

fnuit2_ae()

# Heart Rate Curve

## 1ère nuit :

fnuit1_hr <- function() {
  x <- list();
  for (k in 1:length(nuit1_hr[1][[1]])){
    x <- append(x,as.POSIXct(nuit1_hr[k,1][[1]], origin="1970-01-01"));
  }
  print(x);
  plot(x,nuit1_hr[2][[1]],main="Heart Rate Curve pour la nuit 1");
}

fnuit1_hr()

## Pour la 2ème nuit :

fnuit2_hr <- function() {
  x <- list();
  for (k in 1:length(nuit2_hr[1][[1]])){
    x <- append(x,as.POSIXct(nuit2_hr[k,1][[1]], origin="1970-01-01"));
  }
  print(x);
  plot(x,nuit2_hr[2][[1]],main="Heart Rate Curve pour la nuit 2");
}

fnuit2_hr()

### Le rythme cardiaque suit le même chemin que le cycle du sommeil : il diminue pour atteindre sa valeur minimale lorsque le sommeil est le plus profond, et il augmente lorsque le client se réveille.

# Sleep Stages

## 1ère nuit :

fnuit1_ss <- function() {
  x <- list();
  for (k in 1:length(nuit1_ss[1][[1]])){
    x <- append(x,as.POSIXct(nuit1_ss[k,1][[1]], origin="1970-01-01"));
  }
  print(x);
  plot(x,nuit1_ss[2][[1]],main="Sleep Stages pour la nuit 1");
}

fnuit1_ss()

### Le client a dormi tout la nuit : il ne s'est pas levé de son lit, et ne s'est pas réveillé.
### On aurait pu croire d'après le sleep_cycles que le patient se serait réveillé vers 4h30, mais il est probable que le client dormais, mais à un degré très faible.

## Pour la 2ème nuit :

fnuit2_ss <- function() {
  x <- list();
  for (k in 1:length(nuit2_ss[1][[1]])){
    x <- append(x,as.POSIXct(nuit2_ss[k,1][[1]], origin="1970-01-01"));
  }
  print(x);
  plot(x,nuit2_ss[2][[1]],main="Sleep Stages pour la nuit 2");
}

fnuit2_ss()

### Le client s'est réveillé dans la nuit, et s'est même levé. Cela correspond bien aux autres courbes puisque le sleep_cycles nous indique que lors de la nuit, vers 5h du matin, la profondeur du sommeil était assez faible pour que l'on considère que le patient était réveillé.

### Toutes les données sont donc cohérentes puisqu'elles semblent indiquer un même cycle de sommeil.


## Essayons maintenant de visualiser la moyenne de ces données, et en les retranscrivant sur une seule nuit. On créé donc des tables contenant toutes les informations sur une seule propriété.

## Sleep_cycles

sleep_cycles <- as.data.frame(mydata$time_value_tracks$sleep_cycles[2,1][[1]])
sleep_cycles$nuit=2

for (k in 3:dim(mydata)[1]) { 
  toappend <- as.data.frame(mydata$time_value_tracks$sleep_cycles[k,1][[1]])
  toappend = toappend %>% mutate(nuit=k) 
  sleep_cycles <- rbind(sleep_cycles,toappend)
}

sleep_cycles$V1=as.POSIXct(sleep_cycles$V1,origin="1970-01-01")

### La table est crée. Cependant, nous voulons comparer ces valeurs dans une seule et même nuit. 
### On créé donc une colonne avec l'heure (convertie en minutes), une colonne avec les minutes, et une colonne avec l'heure exacte convertie en minute.

sleep_cycles$hour=as.numeric(substr(sleep_cycles$V1, 12, 13))*60
sleep_cycles$minute=as.numeric(substr(sleep_cycles$V1, 15, 16))
sleep_cycles$hour_minute=sleep_cycles$hour+sleep_cycles$minute

### Cependant, il faut que l'on rajoute une journée à partir de minuit.

for (i in 1:dim(sleep_cycles)[1]){
  if (sleep_cycles$hour_minute[i] < 1414) {
    sleep_cycles$hour_minute[i] <- 24*60 + sleep_cycles$hour_minute[i]
  }
}

sc_ae <- sleep_cycles %>% group_by(hour_minute) %>% summarise(avg_eopch=mean(V2,na.rm=T))

plot(sc_ae$hour_minute,sc_ae$avg_eopch,main="Moyenne sleep_cycles")

### On observe qu'en moyenne, le sleep_cycle est entre 0.4 et 0.6. De plus, on voit qu'il est fréquent pour une personne de se réveiller en plein milieu de la nuit.

## actigram_epochwise

actigram_epochwise <- as.data.frame(mydata$time_value_tracks$actigram_epochwise[2,1][[1]])
actigram_epochwise$nuit=2

for (k in 3:dim(mydata)[1]) { 
  toappend <- as.data.frame(mydata$time_value_tracks$actigram_epochwise[k,1][[1]])
  toappend = toappend %>% mutate(nuit=k) 
  actigram_epochwise <- rbind(actigram_epochwise,toappend)
}

actigram_epochwise$V1=as.POSIXct(actigram_epochwise$V1,origin="1970-01-01")
actigram_epochwise$hour=as.numeric(substr(actigram_epochwise$V1, 12, 13))*60
actigram_epochwise$minute=as.numeric(substr(actigram_epochwise$V1, 15, 16))

actigram_epochwise$hour_minute=actigram_epochwise$hour+actigram_epochwise$minute

for (i in 1:dim(actigram_epochwise)[1]){
  if (actigram_epochwise$hour_minute[i] < 1414) {
    actigram_epochwise$hour_minute[i] <- 24*60 + actigram_epochwise$hour_minute[i]
  }
}

avg_ae <- actigram_epochwise %>% group_by(hour) %>% summarise(avg_eopch=mean(V2,na.rm=T))

plot(avg_ae$hour,avg_ae$avg_eopch,main="Moyenne actigram_epochwise")

### On observe qu'il y a plus de mouvement en plein milieu de la nuit. Cela correspond aux données de sleep_cycles puisque le client se réveille à cette période.

## heart_rate_curve

heart_rate_curve <- as.data.frame(mydata$time_value_tracks$heart_rate_curve[2,1][[1]])
heart_rate_curve$nuit=2

for (k in 3:dim(mydata)[1]) { 
  toappend <- as.data.frame(mydata$time_value_tracks$heart_rate_curve[k,1][[1]])
  toappend = toappend %>% mutate(nuit=k) 
  heart_rate_curve <- rbind(heart_rate_curve,toappend)
}

heart_rate_curve$V1=as.POSIXct(heart_rate_curve$V1,origin="1970-01-01")
heart_rate_curve$hour=as.numeric(substr(heart_rate_curve$V1, 12, 13))*60
heart_rate_curve$minute=as.numeric(substr(heart_rate_curve$V1, 15, 16))

heart_rate_curve$hour_minute=heart_rate_curve$hour+heart_rate_curve$minute

for (i in 1:dim(heart_rate_curve)[1]){
  if (heart_rate_curve$hour_minute[i] < 1414) {
    heart_rate_curve$hour_minute[i] <- 24*60 + heart_rate_curve$hour_minute[i]
  }
}

avg_hrc <- heart_rate_curve %>% group_by(hour) %>% summarise(avg_eopch=mean(V2,na.rm=T))

plot(avg_hrc$hour,avg_hrc$avg_eopch,main="Moyenne heart_rate_curve")

### Le rythme cardiaque suit l'idée d'un sommeil très peu profond au milieu de la nuit : il y est maximum. Ensuite, il diminue puisque la profondeur du sommeil augmente.

## sleep_stages

sleep_stages <- as.data.frame(mydata$time_value_tracks$sleep_stages[2,1][[1]])
sleep_stages$nuit=2

for (k in 3:dim(mydata)[1]) { 
  toappend <- as.data.frame(mydata$time_value_tracks$sleep_stages[k,1][[1]])
  toappend = toappend %>% mutate(nuit=k) 
  sleep_stages <- rbind(sleep_stages,toappend)
}

sleep_stages$V1=as.POSIXct(sleep_stages$V1,origin="1970-01-01")
sleep_stages$hour=as.numeric(substr(sleep_stages$V1, 12, 13))*60
sleep_stages$minute=as.numeric(substr(sleep_stages$V1, 15, 16))

sleep_stages$hour_minute=sleep_stages$hour+sleep_stages$minute

for (i in 1:dim(sleep_stages)[1]){
  if (sleep_stages$hour_minute[i] < 1414) {
    sleep_stages$hour_minute[i] <- 24*60 + sleep_stages$hour_minute[i]
  }
}

avg_ss <- sleep_stages %>% group_by(hour) %>% summarise(avg_eopch=mean(V2,na.rm=T))

plot(avg_ss$hour,avg_ss$avg_eopch,main="Moyenne sleep_stages")

### Cette courbe nous prouve qu'en moyenne, au milieu de la nuit, le client est réveillé puisqu'il se lève.


# Question 3 : Quels sont les facteurs les plus importants pour calculer le sleep score ? 


master = as.data.frame(mydata$properties$total_sleep_score)
names(master)[1]='total_sleep_score'

master = master %>% 
  mutate(sleep_latency = mydata$properties$sleep_latency) %>%
  mutate(sleep_time_target = mydata$properties$sleep_time_target) %>%
  mutate(primary_sleep_period_total_sleep_duration = mydata$properties$primary_sleep_period_total_sleep_duration) %>%
  mutate(sleep_efficiency = mydata$properties$sleep_efficiency) %>%
  mutate(resting_heart_rate = mydata$properties$resting_heart_rate) %>%
  mutate(average_respiration_rate = mydata$properties$average_respiration_rate)

master=master[-1,]

correlation_matrix=as.data.frame(cor(master,use = "complete.obs"))

### On peut déduire de la matrice plusieurs choses. 
### Tout d'abord, les deux principaux facteurs qui calculent le sleep score sont tout d'abord, le primary_sleep_period_total_sleep_duration puis sleep_efficiency.
### Ensuite, il faut expliquer le signe des valeurs du tableau. Lorsqu'ils sont positifs, cela signifie que lorsque la valeur augmente, alors la valeur du sleep_score augmente et inversement pour les signes négatifs.
### Cela soulève une question : pourquoi le sleep score diminue lorsque le taux de respiration augmente ? En effet, cela ne fonctionne que pour un taux de respiration élevé. En effet, il est normal que le client dorme mal s'il a un taux de respiration élevé. Par contre, il dort mal aussi lorsqu'il a un taux de respirations très bas, et on ne voit pas cela dans les données.

