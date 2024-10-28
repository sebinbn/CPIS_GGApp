#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(ggplot2)
library(reshape2)

CPIS_GGData = readRDS("CPIS_GG.rds")
country_liab =  sort(unique(CPIS_GGData$Counterpart.Country.Name))
country_asset =  sort(unique(CPIS_GGData$Country.Name))




# Fun1: Extract country data and return in long format --------------------
# add a no data message
#' Extract country data and return a dataframe in long form from CPIS
#' 
#' Function uses CPIS bulkdownloaded data to return a dataframes in long form for 
#' country specified; It can either extract GG Liabilities or GG Assets
#' 
#' @param cntry a string representing Country name as in CPIS.
#' @param Liab a logical if true extracts liability data else asset.
#' @param Annual a logical if true extracts annual data else semi-annual.
Cntry_GG_L_S_Extract = function(cntry,CPIS_GGData, Liab, Annual = F){
  
  # Which column to use as basis to extract changes depending on assets or 
  # liabilities and also which columns to drop 
  
  
  if ( Liab == T){ # difference in procedure for extracting liabilities
    Cntry_GG_Index = CPIS_GGData$Counterpart.Country.Name == cntry                    
    CPIS_GGDataCols= c(1,2,12:74)
    
  }else{ #difference in procedure for extracting assets
    Cntry_GG_Index = CPIS_GGData$Country.Name == cntry                     
    CPIS_GGDataCols = c(5,6,12:74)
  }
  
  # Procedure common b/w asset/liability extraction
  Cntry_GG_Index  = Cntry_GG_Index & 
    CPIS_GGData$Indicator.Code ==  'I_A_D_T_T_BP6_USD' & 
    CPIS_GGData$Counterpart.Sector.Code == 'GG' &
    CPIS_GGData$Sector.Code == 'T'
  
  Cntry = CPIS_GGData[Cntry_GG_Index, CPIS_GGDataCols]
 
  # Subsetting and removing columns with info redundant after subsetting.Also, 
  # original data has annual and semi-annual rows of which annual is removed.
  
  if (Annual == T){
    Cntry = Cntry[Cntry$X2023S2 == "" & Cntry$X2022S2 == "",]                                                  
    Cntry = Cntry[,c(T,T,!grepl("S", colnames(Cntry)[-c(1:2)]) ) ]
  }else{
    Cntry = Cntry[Cntry$X2023S2 != "" | Cntry$X2022S2 != "" | Cntry$X2021S2 != "",]                                                  
    Cntry = Cntry[,c(T,T,grepl("S", colnames(Cntry)[-c(1:2)]) ) ]
  }
  
  if (nrow(Cntry) == 0){
    print(paste("No data for",cntry))
    return()
  }
  
  # putting data in long form which is originally in wide form, then creating 
  # date column from dates strings and converting flow values in characters to
  # numeric
  Cntry_t = data.frame(t(Cntry[,-c(1,2)]))
  if(Liab == T){
    colnames(Cntry_t) = Cntry$Country.Name
  }else{
    colnames(Cntry_t) = Cntry$Counterpart.Country.Name 
  }
  
  dates = rep("", nrow(Cntry_t) )
  if (Annual == T){ #procedure for creating Annual dates
    
    dates = paste(substr(rownames(Cntry_t),2,5),"-12-31", sep = "")
    
  }else{ #procedure for creating semi-annual dates
    
    sem_indx = substr(rownames(Cntry_t),6,7) == "S2"
    dates[sem_indx] = paste(substr(rownames(Cntry_t)[sem_indx],2,5),
                            "-12-31", sep = "")
    dates[!sem_indx] = paste(substr(rownames(Cntry_t)[!sem_indx],2,5),
                             "-06-30", sep = "")
  }
  
  Cntry_Export = cbind(Date = as.Date(dates), 
                       data.frame(lapply(Cntry_t, as.numeric)) ) 
  if(sum(colSums(Cntry_Export[-1], na.rm = T)) == 0){return()}
  return(Cntry_Export)
} #end of function


# Fun2: Add Others, Total column ---------------------------------------

#' Adds 'Others' and 'Totals' columns to country dataframe
#' 
#' Function uses country dataframe as input to return a dataframe and a vector
#' of names of top5 countries. The dataframe will have added 'Others' (not Top
#' 5) and 'Totals' columns added. It also removes rows with no data.
#' 
#' @param cntryData a dataframe with country data.Dataframes are to be in long 
#' form 
#' @param exUS a logical indicating whether to include/exclude US data when 
#' Top 5 countries, Others and Totals column. The default is exUS = F 
#' form 

Create_OtherTot = function(cntryData, exUS = F){
  
  if (exUS == T){
    cntryData = cntryData[colnames(cntryData) != 'United.States']
  }
  
  t10 = order(as.double(cntryData[nrow(cntryData),-1]),decreasing = T)[1:10]         # captures index of top10 values.
  notTop10 = colnames(cntryData)[-c(1,t10+1)]
  
  
  cntryLiab = cbind(cntryData, 
                    Others = rowSums(cntryData[,notTop10], na.rm = T),
                    Total = rowSums(cntryData[,-1], na.rm = T))
  cntryLiab = cntryLiab[cntryLiab$Total != 0,]                           # removing rows with no data
  Top10 = colnames(cntryData)[t10+1]
  return(list('Data' = cntryLiab, 'Top10' = Top10))
}

# Fun3: Add Others column in Assets table ---------------------------------------

#' Adds 'Others' column to country assets dataframe
#' 
#' Function uses country asset dataframe as input to return a dataframe with 
#' non-Top10 added and a vector of names of top 10 countries. It also removes 
#' rows with no data.
#' 
#' @param cntryData a dataframe with country data.Dataframes are to be in long 
#' form 

Create_Other = function(cntryData){
  cntryData = cntryData[rowSums(is.na(cntryData[-1])) != (ncol(cntryData)-1),]              # condition checks if all values in a row are NA and then removes that row
  
  t10 = order(as.double(cntryData[nrow(cntryData),-1]),decreasing = T)[2:11]      # captures index of top 10 values.
  Top10 = colnames(cntryData)[t10+1]                                              # converts index to column names
  notTop10 = !colnames(cntryData) %in% Top10 
  notTop10[1] = F                                                                 # changing date to False
  
  
  cntryData = cbind(cntryData, 
                    Others = rowSums(cntryData[,notTop10], na.rm = T))
  
  return(list('Data' = cntryData, 'Top10' = Top10))
}

# Fun4: Create AreaPlot ---------------------------------------

#' Plots area chart of country data
#' 
#' Function uses list of data and top10 index returned by Create_OtherTot as input
#'  to return a ggplot object 
#' 
#' @param List_DatT10 a list with 'Data', a dataframe  and 'Top10' a vector.
#' @param cntry a string of country name.

AreaPlot = function(List_DatT10, cntry){
  cntry_long = melt(List_DatT10$Data[,c('Date',List_DatT10$Top10)],
                    id.vars = "Date")  
  cntry_plot = ggplot(data = cntry_long, 
                      aes(x = Date, y = value/1000000000, color = variable)) +
    geom_area(aes(fill = variable))+
    guides(color = "none") +
    labs(y = 'Billions of USD', x = element_blank(), title = cntry)+ 
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
          legend.title = element_blank(),legend.text = element_text(size = 14))
}


# Fun6: Container function ---------------------------------------

#' Calls all other functions to do necessary manipulations
#' 
#' Function uses input of country name to return two ggplot objects 
#' 
#' @param cntry a string of country name.
#' @param exUS a logical, if true excludes US ownership data from analysis.                                          
PlotCountry_L_S = function(cntry, CPIS_GGData, L = T, exUS = F){
  cntry_GG_S = Cntry_GG_L_S_Extract(cntry,CPIS_GGData, Liab = L, Annual = F)
  
  if (is.null(cntry_GG_S)){ return(paste("No data for",cntry))}
 
  if (L) {
    AreaPlotData = Create_OtherTot(cntry_GG_S, exUS)
  }else{
    AreaPlotData = Create_Other(cntry_GG_S)
  }
  
  cntry_GG_AreaPlot = AreaPlot(AreaPlotData,cntry)
 
}




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Nationalities and Government bond investments"),
    card(markdown(paste(
      "This application uses data from IMF's",
      "[CPIS](https://data.imf.org/?sk=b981b4e3-4e58-467e-9b90-9de0c3367363&sid=1481577756129)",
      "database to visualise international capital flows into government bonds.",
      "As such, limitations of this database apply. Data for some countries are",
      "not available in all years. Any sudden jumps are most likely new data becoming",
      "available."))
    ),
   card(
      card_header(h3("Portfolio of Government bonds")),
      h5("Choose country to see top 10 countries whose bonds are held by investors of that country"),
      selectInput("AssetCntry","Select a country:",choices = country_asset),
      textOutput("AssetErr"),
      plotOutput("AssetPlot")
    ),
   card(
     card_header(h3("Nationality of Investor")),
     markdown("Choose country to see the Top 10 nationalities of investors who invest in that country"),
     selectInput("LiabCntry", "Select a Country:",choices = country_liab),
     textOutput("LiabErr"),
     plotOutput("LiabPlot")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$LiabPlot <- renderPlot({
      out = PlotCountry_L_S(input$LiabCntry,CPIS_GGData, L = T)
      if (is.character(out)){
        output$LiabErr <- renderText(out)
        return(NULL)
      }else{
        output$LiabErr <- renderText(NULL)
        out
        }
      
    })
    output$AssetPlot <- renderPlot({
      out = PlotCountry_L_S(input$AssetCntry,CPIS_GGData, L = F)
      if (is.character(out)){
        output$AssetErr <- renderText(out)
        return(NULL)
      }else{
        output$AssetErr <- renderText(NULL)
        out
      }
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
