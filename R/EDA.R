#'Plot graphs to do Exploratory Data Analysis
#'
#'Provide Dataframe along with list of column number in the form of vector and also provide directory if you want to save plots in specific directory
#'(x,vect=c(1,2,3,..), dir="../EDAplots")
#'@param data= dataframe
#'@param var= list of column number to be pass(if not provided, whole dataset will be taken into consideration)
#'@param path= "provide valid path to save plots" (if not provided, plots will be saved in working directory)
#'@author Vikas Yadav and Sunny Kumar
#'@return plots will be directly saved in the given directory

#Exploratory data analysis
graph_var = function(data,var = c(), path='')
{
  if (!is.data.frame(data)) # if passed data is not dataframe then stop
    stop("the given object is not a dataframe")

  if (is.null(var))
    var = 1:ncol(data)

  df = data
  nums= unlist(lapply(df, is.numeric))
  data_sum = df[ ,nums]

  for (i in var) #for all variables whose plots are required
  {
    if (!is.numeric(data[,i]) & length(unique(data[,i])) < 10) #for categorical variables
    {
      png(paste(path,'\\',names(data)[i],".png",sep="")) #naming graphs with extension png
      par(mfrow=c(2,1)) #dividing page in 2 halfs for two graphs on same page

      bartable = table(data[,i]) #taking count of unique values
      b_labels = names(bartable) #labelling unique values
      barplot(bartable, main = paste("Barplot of", names(data)[i]), col = rainbow(length(b_labels))) #plotting bargraph

      pietable = table(data[,i])
      pct = round(pietable/sum(pietable)*100)
      lbls = paste(names(pietable), pct) # add percents to labels
      lbls = paste(lbls,"%",sep="") # add % to labels
      pie(pietable, main = paste("Pie chart of",names(data)[i]),  #plotting pie chart
          labels = lbls , col = rainbow(length(lbls)))


      dev.off() #saving graphs
    }


    else if (is.numeric(data[,i])) #graphs for numerical varaibles
    {
      png(paste(path,'\\',names(data)[i],".png",sep = ""))
      par(mfrow=c(2,1))

      #plotting boxplot
      boxplot(data[,i], main = paste("Boxplot of",names(data)[i]),
              ylab = names(data)[i], col="maroon", border = "grey5",
              horizontal = TRUE)
      #plotting histogram
      hist(data[,i], main = paste("Histogram of",names(data)[i]),
           xlab = names(data)[i],col = "lightgreen", border = F)


      dev.off()
    }
    require(corrplot)
    png(filename = paste(path,"\\Correlation_plot.png", sep=""))
    par(mfrow = c(1,1))
    # Correlation Plot
    corrplot(cor(data_sum), method="color",
             diag=FALSE, # tl.pos="d",
             type="upper", order="hclust",
             title = 'Correlation plot of Data',
             addCoef.col = "black",
             mar=c(0,0,1,0)
    )
    dev.off()
  }

}

