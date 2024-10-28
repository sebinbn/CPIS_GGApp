# This file uses .csv data downloaded from IMF and converts it to .rds data after
# some cleaning and subsetting. Keeping as .rds saves future data load times.

CPISData = read.csv('../CPIS_bulk/CPIS_09-17-2024 17-10-09-84_timeSeries.csv') 

# column names in R cannot have space and comma. So these are replaced with '.'. 
# Not doing this will automatically convert to '.'. But when I go back to extract
# from CPISData based on columns in an already extracted table, this will miss
# county data
for (symb_replace in c(' ',',')){
  CPISData$Country.Name = gsub(symb_replace, '.',CPISData$Country.Name)
  CPISData$Counterpart.Country.Name = gsub(symb_replace, '.',
                                           CPISData$Counterpart.Country.Name)
}

CPISsub = CPISData[CPISData$Indicator.Code ==  'I_A_D_T_T_BP6_USD' &
                     CPISData$Counterpart.Sector.Code == 'GG',]
saveRDS(CPISsub, file = "CPIS_GG.rds")
