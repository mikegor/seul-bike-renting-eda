if (!require("naniar")) install.packages("naniar")
library("naniar")
#biblioteki do rysowania corrplota
if (!require("corrplot")) install.packages("corrplot")
library("corrplot")
#biblioteka do list comprehension
if (!require("comprehenr")) install.packages("comprehenr")
library("comprehenr")
if (!require("ggplot2")) install.packages("ggplot2")
library("ggplot2")
if (!require("ggmosaic")) install.packages("ggmosaic")
library("ggmosaic")
if (!require("grid")) install.packages("grid")
library("grid")
if (!require("gridExtra")) install.packages("gridExtra")
library("gridExtra")
if (!require("e1071")) install.packages("e1071")
library("e1071")

#nalezy wpisac sciezke do datasetu
setwd("sciezka do pliku")

df_original <- read.csv('data/SeoulBikeData.csv')

print(names(df_original))

str(df_original)

unique(df_original$Holiday)


print(colnames(df_original))
#stworzenie grup ilosci wypozyczonych rowerow
df_original[,"Bike.Count.Group"] <- NA
df_original$Bike.Count.Group[df_original$Rented.Bike.Count <= 400] <- 'S'
df_original$Bike.Count.Group[df_original$Rented.Bike.Count > 400 & df_original$Rented.Bike.Count <= 1000] <- 'M'
df_original$Bike.Count.Group[df_original$Rented.Bike.Count > 1000] <- 'L'

#zamiana wartosci kategorycznych na liczbowe dla mozliwosci wizualizacji
df <- df_original
df$Seasons <- replace(df$Seasons, df$Seasons == "Winter", "1")
df$Seasons <- replace(df$Seasons, df$Seasons == "Spring", "2")
df$Seasons <- replace(df$Seasons, df$Seasons == "Summer", "3")
df$Seasons <- replace(df$Seasons, df$Seasons == "Autumn", "4")
print(colnames(df))

df$Functioning.Day <- replace(df$Functioning.Day, df$Functioning.Day == "No", "0")
df$Functioning.Day <- replace(df$Functioning.Day, df$Functioning.Day == "Yes", "1")

df$Holiday <- replace(df$Holiday, df$Holiday == "No Holiday", "0")
df$Holiday <- replace(df$Holiday, df$Holiday == "Holiday", "1")
str(df)
unique(df$Seasons)

df <- transform(df, Seasons = as.numeric(Seasons), Functioning.Day = as.numeric(Functioning.Day), Holiday = as.numeric(Holiday))

#usuniecie dni w których wypo¿yczalnia nie dzialala ze wzgledu na to, ze w tych dniach ilosc wypozyczonych rowerow zawsze wynosi zero co moze zaklamywac niektore wizualizacje
df <- df[df$Functioning.Day == 1,]
df <- subset(df, select=-Functioning.Day)

#sprawdzenie wartosci brakujacych za pomoca biblioteki naniar
vis_miss(df)

#funkcja rysujaca wykresy wizualizacyjne dla danych liczbowych
show_number_plots <- function(xData, xName) {
  par(mfrow=c(2,2))
  par(mar=c(4,4,2,2))
  hist(xData, main='Histogram', xlab=xName)
  plot(density(xData), main='Density', xlab=xName)
  boxplot(xData, main='Boxplot', xlab=xName)
}

#narysowanie wykresow dotyczacych danych liczbowych
for (i in colnames(df)){
  if(typeof(df[ , i]) == 'integer' || typeof(df[ , i]) == 'double'){
    show_number_plots(df[ , i], i)
  }
}

#narysowanie braplotow dla danych napisowych
for(i in colnames(df)){
  if(typeof(df[ ,i]) == 'character' && i!='Date'){
    barplot(table(df[ ,i]), main='Barplot', xlab=i)
  }
}



#wyciagniecie kolumn liczbowych
numberCols = to_vec(for(i in colnames(df)) if(typeof(df[ ,i]) == 'integer' || typeof(df[ ,i]) == 'double') i)
numberCols_original = to_vec(for(i in colnames(df_original)) if(typeof(df_original[ ,i]) == 'integer' || typeof(df_original[ ,i]) == 'double') i)
print(df[numberCols])

#obliczenie i narysowanie korelacji pomiedzy parami atrybutow
M <- cor(df[numberCols])
plot.new()
dev.off()
par(mfrow=c(1,1))
par(mar=c(2,2,2,2))
corrplot(M, method='color')

#wykres pokazujacy zaleznosci miedzy wszytkimi atrybutami liczbowymi
pairs(df[numberCols], col=rgb(0, 0, 0, 0.05), cex.labels=1)

#stworzenie tego samego wykresu jednak z pokolorowaniem go wg ilosci rowerow
cols <- to_vec(for(i in 1:nrow(df)) rgb(0, 0, 1, 0.1))

cols[df$Bike.Count.Group == 'M'] <- rgb(0, 1, 0, 0.1)
cols[df$Bike.Count.Group == 'L'] <- rgb(1, 0, 0, 0.1)
pairs(df[numberCols], col=cols, cex.labels=1)



#wykresy gestosci z podzialem na klasy
for(col in colnames(df_original[numberCols_original])) {
  p1 <- ggplot(data=df_original, aes_string(x=col, group='Bike.Count.Group', fill='Bike.Count.Group')) + geom_density(adjust=1.5, alpha=.4)
  
  print(p1)
}

#boxploty z pokolorowaniem na klasy
for(col in colnames(df_original[numberCols_original])) {
  p2 <- ggplot(data=df_original, aes_string(x=col, group='Bike.Count.Group', fill='Bike.Count.Group')) + geom_boxplot(alpha=.4)
  
  print(p2)
}


#liczba obserwacji z wylaczeniem brakujacych

for(col in colnames(df_original)) {
  is_na = is.na(df_original[col])
  number_of_nans = sum(is_na)
  total = length(is_na)
  print(sprintf("%-25s: %i", col, total - number_of_nans))
}

#liczba brakujacych

for(col in colnames(df_original)) {
  is_na = is.na(df_original[col])
  number_of_nans = sum(is_na)
  print(sprintf("%-25s: %i", col, number_of_nans))
}

for(col in colnames(df_original[numberCols_original])) {
  print(summary(df_original[col]))
  print(sd(df_original[col]))
  print(sprintf("Std: %f", sd(df_original[col])))
  print(sprintf("Variance: %f", var(df_original[col])))
}

#miary polozenia i rozrzutu
summary = data.frame(unclass(summary(df_original[numberCols_original])), check.names = FALSE, stringsAsFactors = FALSE)

std <- c()
variance <- c()
skewness <- c()
kurtosis <- c()
for(col in colnames(df_original[numberCols_original])) {
  std <- c(std, sprintf("Std: %f", sd(df_original[,col])))
  variance <- c(variance, sprintf("Variance: %f", var(df_original[,col])))
  skewness <- c(skewness, sprintf("Skewness: %f", skewness(df_original[,col])))
  kurtosis <- c(kurtosis, sprintf("Kurtosis: %f", kurtosis(df_original[,col])))
}

print(std)
summary <- rbind(summary, std)
summary <- rbind(summary, variance)
summary <- rbind(summary, skewness)
summary <- rbind(summary, kurtosis)
print(summary)

grid.table(summary)


# Q-Q plots i histogramy (original data)

for(col in colnames(df_original[numberCols_original])) {
  par(mfrow=c(1,2))
  hist(df_original[,col], main='Histogram', xlab=col)
  qqnorm(df_original[,col], main='Q-Q plot', frame=FALSE, xlab=col)
  qqline(df_original[,col], col='steelblue')
}

# Q-Q plots i histogramy z przeksztalceniem danych logarytm oraz pierwiastek kwadratowy

print(min(df_original['Wind.speed..m.s.']))

for(col in colnames(df_original[numberCols_original])) {
  min_value = min(df_original[,col])
  log_data = log(df_original[,col] - min_value + 0.01)
  sqrt_data = sqrt(df_original[,col] - min_value + 0.01)
  par(mfrow=c(2,2))
  par(mar=c(4,4,2,2))
  
  hist(log_data, main='Histogram Log', xlab=col)
  qqnorm(log_data, main='Q-Q plot Log', frame=FALSE, xlab=col)
  qqline(log_data, col='steelblue')
  
  hist(sqrt_data, main='Histogram Sqrt', xlab=col)
  qqnorm(sqrt_data, main='Q-Q plot Sqrt', frame=FALSE, xlab=col)
  qqline(sqrt_data, col='steelblue')
}

#testy normalnosci
for(col in colnames((df_original[numberCols_original]))){
  print(col)
  x <- sample(df_original[,col], size=30)
  print(shapiro.test(x))
}

#wykresy mozaikowe
ggplot(data=df_original) + geom_mosaic(aes(x = product(Holiday, Seasons), fill=Holiday))
ggplot(data=df_original) + geom_mosaic(aes(x = product(Functioning.Day, Seasons), fill=Functioning.Day))
