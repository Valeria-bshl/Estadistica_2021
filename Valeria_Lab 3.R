# Salma Valeria García Yáñez
# Matrícula: 1972562
# Fecha: 04/03/21


conjunto <-  read.csv("DBH_1.csv", header = TRUE)

head(conjunto)
tail(conjunto)

hist(conjunto$Altura)
hist(conjunto$Diametro)

mean(conjunto$Diametro)
mean(conjunto$Vecinos)

range(conjunto$Vecinos)



dbh<- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1,
        14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3, 9.7, 6.5,
        23.4, 8.2, 28.5, 10.4, 11.5, 14.3, 17.2, 16.8)


sum(dbh)
prod(dbh)

hist(dbh)


# Importar datos desde una url --------------------------------------------

prof_url <- "https://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa <- read.csv(prof_url)  
head(profepa)

sum(profepa$Inspecciones)
sum(profepa$Operativos)
sum(profepa$Recorridos.de.vigilancia)


prof_url_2 <- pasteθ("https://www.profepa.gob.mx/innovaportal/",
                     "file/7635/1/accionesInspeccionfoanp.csv")

profepa2 <- read.csv(prof_url_2)



# Parte II ----------------------------------------------------------------

# Selección mediante restricciones

dbh
mean(dbh)

dbh < 10

sum(dbh < 10)

which(dbh < 10)

dbh.url <- "https://raw.githubusercontent.com/mgtagle/PrincipiosEstadistica2021/main/DBH_1.csv"

parcelas <- read.csv(dbh.url)


tree.13 <- parcelas[!(parcelas$parcela == "2"),]

tree.23 <- parcelas[!(parcelas$parcela == "1"),]

tree.12 <- parcelas[!(parcelas$parcela == "3"),]

# Revisar las medias del dbh en cada combinación de parcelas

mean(tree.12$dbh); mean(tree.13$dbh); mean(tree.23$dbh)


# Seleccion de submuestras ------------------------------------------------

tree_mean <- subset(parcelas, dbh <= mean(parcelas$dbh))

tree.up <- subset(parcelas, dbh >= mean(parcelas$dbh))

mean(tree_mean$dbh); mean(tree.up$dbh) 

# Representación gráfica de los dos subconjuntos

boxplot(tree_mean$dbh, main = "DBH <= media", col = "skyblue", horizontal = TRUE)
boxplot(tree.up$dbh, main = "DBH >= media", col =  "lightpink", horizontal = F)

quantile(tree_mean$dbh, 0.5)
quantile(tree_mean$dbh, 0.75)


# Parte 3: Representación gráfica -----------------------------------------

mamiferos <- read.csv("https://www.openintro.org/data/csv/mammals.csv")

mean(mamiferos$total_sleep)

hist(mamiferos$total_sleep, col = "lightgreen", 
     ylim = c(8, 12), xlim = c(8, 22),
     las = 1)

mean(mamiferos$body_wt)
mean(mamiferos$brain_wt)
boxplot(mamiferos$body_wt)

fivenum(mamiferos$brain_wt)

data("chickwts")
head(chickwts)

alimentación <- table(chickwts$feed)
alimentación

barplot(alimentación)

barplot(alimentación[order(alimentación, decreasing = TRUE)])

orig.par <- par() # Originales de las gráficas
par(oma=c(1,1,1,1))
par(mar=c(4,5,2,1))

barplot(alimentación[order(alimentación)],
        horiz = TRUE,
        las = 1,
        col = c("#58FAAC", "#F7819F", "#BCA9F5", "#5882FA", "#F78181", "#F7FE2E"),
        main = "Frecuencias por tipos de alimentación",
        xlab = "Número de pollos")

orig.par
