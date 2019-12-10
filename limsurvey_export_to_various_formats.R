########################################
## Marc Schwenzer, m.schwenzer@uni=tuebingen.de
## 2019, GPL-2
##
## Export Limsurvey files to various formats
# 1. IN LS select: Responses (and statistics) -> Export Responses
# - select Export Format: R(data file) , Option `Full Answers` , select all columns
# - click "Export"
# - Save or copy to directory '/path/to/limesurvey_files/dir'
# 2. IN LS select: Responses (and Statistics) -> Export Responses
# - select Export Format: R(syntax file)
# - click 'Export"
# - Save or copy both files to directory  '/path/to/limesurvey_files/dir/'
# 3. Set path '/path/to/limesurvey_files/dir/' here:
'/path/to/limesurvey_files/dir/' -> the_dir


## Load packages: Guarantee necessary packages are installed
## and loaded
## https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
using<-function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))
    need<-libs[req==FALSE]
    if(length(need)>0){ 
        install.packages(need)
        lapply(need,require,character.only=TRUE)
    }
}
using('dplyr','purrr','stringr','janitor','rio','foreign')

## Set dir
setwd (the_dir)
## Code file
# exectute the LS R-source file to import labels and modes of variables
dir(the_dir,pattern='syntax_file.R$')%>% source(file=.)


# Correct variables names for e.g. stata limitations
data %>% clean_names %>% names %>% str_replace_all('\\.(\\.)+','_')  %>% abbreviate(minlength=48,strict=TRUE)-> names(data)

data -> data_v12
attr(data_v12,'variable.labels')%>% str_trunc(79,'right')  -> v12_var_labels


attr(data,'variable.labels')  %>% str_trunc(79,'right') %>% iwalk(~{
    data[,.y] -> z
    .x -> attr(z,'label')
    z->> data[,.y]
})
NULL -> attr(data,'variable.labels')

# Export to R-Data, Stata, Excel, SPSS-File and feather (to python)

c('rda','dta','xlsx','sav','feather') %>% map(~{dir(the_dir,pattern='data_file.csv$') %>% str_replace('csv$',.x) -> outputfile
    try(export(data,file=outputfile))
})

# Preprocess for Stata12
# Detect problems : Long strings
check_max_string_length<- function(x){x %>% nchar %>% max(.,na.rm=1)}
data_v12  %>% summarize_if(is.character,check_max_string_length)  %>% gather(var,maxlength) %>% arrange(-maxlength)


data_v12 %>% clean_names %>% names %>% str_replace_all('\\.(\\.)+','_')  %>% abbreviate(minlength=20,strict=TRUE)-> names(data_v12)

data_v12 %>% mutate_if(is.character,function(x){str_conv(x,encoding='ISO-8859-1')}) -> data_v12
data_v12 %>% mutate_if(is.character,function(x){x %>% stringi::stri_trans_general( "latin-ascii") %>% abbreviate(40)}) -> data_v12
data_v12 %>% mutate_if(is.factor,function(x){x -> y
    levels(y) %>% stringi::stri_trans_general( "latin-ascii") %>% abbreviate(24) -> levels(y)
    return(y)
}) -> data_v12
data_v12  %>% summarize_if(is.character,check_max_string_length)  %>% gather(var,maxlength) %>% arrange(-maxlength)
# v12 variable labels
v12_var_labels  %>% str_trunc(79,'right') %>% iwalk(~{
    data_v12[,.y] -> z
    .x -> attr(z,'label')
    z->> data_v12[,.y]
})
NULL -> attr(data_v12,'variable.labels')

# write to Stata12 using 'haven-package
try(data_v12 %>% haven::write_dta(dir(the_dir,pattern='data_file.csv') %>% str_replace('\\.csv','_v12.dta'),version=12))
# ... and foreign-package.
try(data_v12 %>% foreign::write.dta(file=dir(the_dir,pattern='data_file.csv') %>% str_replace('\\.csv','_v12_2.dta'),version=12))
# Signal the export is finished
cat(paste0('All Files exported... You will find them in this directory:\n',the_dir,'\n'))
