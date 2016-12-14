# Part I: There are 20 files with .dat extention. You have to read all the files in to single dataframe.
folderpath <- "C:\\BigData\\Analytics\\R\\datasets\\iris_dat\\iris"
combineFiles <- function(filelist){
  for(file in filelist){
    if(!exists("dataset")){
      dataset <- read.table(file,skip = 9,sep = ',')
    } else{
      tempdataset <- read.table(file,skip = 9,sep = ',')
      dataset <- rbind(dataset,tempdataset)
      rm(tempdataset)
    }
  }
  return(dataset)
}
combinedDataset <- combineFiles(list.files(folderpath))

#Part II : The data is present in xml format, with file name, iris.xml. Your task is to read the XML data and store it in the data frame df.
install.packages("XML")
library("XML")
folderpath <- "C:\\BigData\\Analytics\\R\\datasets\\iris.xml"
xmlDF <- xmlToDataFrame(folderpath)
xmlDF

#Part III: Convert the iris data into the JSON format and read the data in JSON format and convert it into dataframe "iris_data". 
install.packages("rjson")
library("rjson")
iris
fromDFToJSON <- toJSON(iris)
iris_data <- data.frame(fromJSON(fromDFToJSON))
iris_data

#Part IV: Use dplyr function on the data iris_data. Implement select, match, filter, arrange, rename, and mutate function on the iris_data.
library(dplyr)
iris_data%>%
  select(matches("S"))%>%
  arrange(Species,Sepal.Length,desc(Sepal.Width))%>%
  mutate(Sepal.Length1 = Sepal.Length+0.5)%>%
  filter(Sepal.Length > 6)

rename(iris_data, petal.len = Petal.Length)

#Part V: Print the summary of iris_data 
summary(iris_data)
