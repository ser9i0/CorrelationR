#!/usr/bin/env Rscript

library(tseries)

# TEST DE CORRELACIÓN

# Test que obtiene el coeficiente de correlación utilizando varios métodos

# USAGE
# ./CorrelationTest.R datafile_path init_col end_col target_col
# eg: ./CorrelationTest.R data.csv 2 -1 3

# OUTPUT
# - Screen log, showing the flow and info about the analysis procedure

args<-commandArgs(trailingOnly=TRUE)

if(length(args)==0){
    stop("Invalid arguments: datafile_path init_col (2) end_col (-1) target_col (1).")
}

dataset<-read.csv( args[1] )
init_col<-as.numeric(args[2])
end_col<-as.numeric(args[3])
target_col<-as.numeric(args[4])
target_name<-names(dataset)[target_col]

if( end_col == -1 )
{
  end_col<-ncol(dataset)
}

if(init_col>end_col)
{
  stop("Wrong INIT (",init_col,") and/or END (",end_col,") columns value.")
}

message("# CORRELATION COEFFICIENTS #####################")

for( i in init_col:end_col )
{
  if( i == target_col )
  {
    next
  }

  col_name<-names(dataset)[i]

  cor_coef_pearson<-cor(dataset[,target_col], dataset[,i], method="pearson")
  cor_coef_spearman<-cor(dataset[,target_col], dataset[,i], method="spearman")
  cor_coef_kendall<-cor(dataset[,target_col], dataset[,i], method="kendall")

  message("... ",target_name," - ",col_name,": ",cor_coef_pearson," (Pearson), ",cor_coef_spearman," (Spearman), ",cor_coef_kendall," (Kendall)")
}

