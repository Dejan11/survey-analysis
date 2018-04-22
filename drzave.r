library(countrycode)
library(treemap)
library(translate)
data = read.csv("countries_count.csv")
View(data)
data=data[,-1]
data$Var1 = as.character(data$Var1)

#first transalte the country names

translated = apply(data, 1, function(x) translate(x, 'sr', 'en', key = "AIzaSyDZ22pr6VqfBDsOuZ8anm_iRNI-fs6k-Gw"))

data$translated = as.vector(unlist(translated))
data = edit(data)

#fix the ones that didn't translate 

#data$translated[26] = "United States"
#data$translated[28] = "Bosnia and Herzegovina"
#data$translated[4] = "Czech Republic"

data = edit(data)
#get country acronyms
data$acronym = countrycode(data$translated, 'country.name', 'iso3c')
data$sum = rowSums(data[,2:6], na.rm = TRUE)
data$label <- paste(data$Var1, data$sum, sep = " - ")

treemap(data, #Your data frame object
        index="label",  #A list of your categorical variables
        vSize = "sum",
        vColor="sum", #This is your quantitative variable
        type="manual", #Type sets the organization and color scheme of your treemap
        palette ="RdYlBu",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Државе у којима су студенти или алумнисти провели мобилност", #Customize your title
        fontsize.title = 14
        
)
#theme(legend.position="none")