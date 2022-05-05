
library("tabulizer")
library("shiny")
library("miniUI")
library("tidyverse")
library("stringr")
library("rvest")
library("XML")
library("stringi")

setwd("C:\\data\\election_security")

#download all the PDFs into the directory
page<-read_html("https://www.eac.gov/payments-and-grants/election-security-funds")

########################################
#Define functions
########################################

#define function to scrape the links to a desired report 
#and save copies for records
prep_reports<-function(rep){
  #grab links for the target report for each state
  links<-page %>% 
    html_nodes(xpath=  paste("//a[contains(text(),'",rep,"')]/@href",sep = "")) %>%
    html_text()
  
  #download the report for reference into the wd
  #for (link in links){
  #  download.file(link, destfile=basename(link), quiet = TRUE, mode="wb")
  #}
  links
}

#function to get expenditure data from PDF urls and append to df here
get_data<-function(yr,site){
  
  #grab PDF from site and convert table in the PDF into a dataframe
  #2021 and 2020 have nicely formatted SF425s so don't need to manually define table dimensions
  if (yr==2021 | yr==2020) {
    df_results<-extract_tables(site, page=1,
                               output="data.frame",
                               area=list(c(205,24,489,590)))
  } else {
    try(df_results<-extract_areas(site, page=1, output="data.frame"),silent=FALSE)
  }
  
  d<-data.frame()
  try(d<-df_results[[1]])
  
  #check if second column was incorrectly inserted
  try(if (NCOL(d)>2) {
    d<-cbind(d[[1]],d[[3]])
  })
  
  if (NCOL(d)==2) {
    colnames(d)<-c("Item","Amount")
  }
  
  #get state name
  start<-stri_locate_last(site, regex = "\\/")[1]+1
  st<-toupper(substr(site,start,start+1))
  
  #get name of the source pdf
  substr(site,start,nchar(site))
  
  #add state name, year, and source website as columns
  new_dat<-cbind(State=st,Year=yr,Source=site,d)
  
  return(new_dat)
}

#function to grab the site URls and compile the expenditure data
errors<-list()
scrape_SF425<-function(rep_,yr_){
  print(yr_)
  print(rep_)
  
  #generate empty df to store the data
  SF<<-data.frame(State=character(),Year=character(),Source=character(),Item=character(),Amount=character())
  SF_messy<<-data.frame(State=character(),Year=character(),Source=character(),Item=character(),Amount=character())
  
  #generate the list of urls containing the target report
  sites_<-prep_reports(rep_)
  
  #if you want to only loop through some sites, specify range
  #sites_<-sites_[15:18]
  
  #loop through the urls and grab pdf from each url
  for (site_ in sites_){
    print(paste("Getting SF425 data for: ",site_))
    
    #define new empty df to store the data if scraped properly
    newdat<-data.frame()
    newdat<-try(get_data(yr_,site_))
    newdat<-as.data.frame(newdat)
    
    #try to get it again if didn't scrape properly
    if (NCOL(newdat)!=5 | nrow(newdat)<5) {
      print(paste("Error scraping SF425 data from",site_))
      print("Please try again to select the table from the PDF")
        
      #try again to select the table:
      try(newdat<-get_data(yr_,site_),silent = T)
      
      #run quick cleaning script on the messy data
      try(newdat<-clean_SF425(data.frame(newdat)),silent = T)
      
      #add to messy df if scraped with error
      try(SF_messy<<-bind_rows(SF_messy,as.data.frame(newdat)))
      
      errors[[length(errors)+1]]<<-site_
    } else{
      #add new data to SF425 dataframe
      SF<<-rbind(SF,newdat)
      
      print("Appended successfully")
    }
  }
  
  #clean the SF data that processed properly
  SF<-clean_SF425(SF) %>% select(-c("Item"))
  
  #save the csvs for reference
  write.csv(SF_messy,file = "SF425_data_messy.csv")
  
  return(SF)
}

#should have about 616 rows after cleaning
clean_SF425<-function(SF425_){
  #cleaning the SF425__ data
  SF425_$Amount<-gsub(" ","",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("l","",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("j","",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("\\$","",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("o","0",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("O","0",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("'","",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("i","",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("J","",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("I","",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("s","5",as.character(SF425_$Amount))
  SF425_$Amount<-gsub("n","",as.character(SF425_$Amount))
  SF425_$Amount<-gsub(",","",as.character(SF425_$Amount))
  SF425_$Amount<-gsub(",","",as.character(SF425_$Amount))
  SF425_$Item<-gsub(",",".",as.character(SF425_$Item))
  
  if (SF425_$Year[1]=="2020" | SF425_$Year[1]=="2021") {
    #delete rows without line item data
    pattern <- paste(c('a\\. ','b\\. ','c\\. ','d\\. ','e\\. ','f\\. ','g\\. ','h\\. ','i\\. ','j\\. ','k\\. ','l\\. ','m\\. ','n\\. ','o\\. ','p\\. '),collapse="|")
    SF425_ <- SF425_[grepl(pattern, SF425_$Item)==1,]
  } else{
    SF425_ <- SF425_[!grepl("10. Transact",SF425_$Item),] 
    SF425_ <- SF425_[!grepl("\\(Use",SF425_$Item),] 
    SF425_ <- SF425_[!grepl("Federal Cash",SF425_$Item),] 
    SF425_ <- SF425_[!grepl("Federal Expenditures and",SF425_$Item),]
    SF425_ <- SF425_[!grepl("Recipient Share:",SF425_$Item),]
    SF425_ <- SF425_[!grepl("Program Income:",SF425_$Item),]
  }
  
  #add simplified line item alphabetical labels
  SF425_$ItemNum<-substr(SF425_$Item,0,1)
  
  #add simplified line item titles
  SF425_$LineItem[SF425_$ItemNum=="a"]="Cash Receipts"
  SF425_$LineItem[SF425_$ItemNum=="b"]="Cash Disbursements"                                                  
  SF425_$LineItem[SF425_$ItemNum=="c"]="Cash on Hand"                                       
  SF425_$LineItem[SF425_$ItemNum=="d"]="Total Federal funds authorized"                                      
  SF425_$LineItem[SF425_$ItemNum=="e"]="Federal share of expenditures"                                       
  SF425_$LineItem[SF425_$ItemNum=="f"]="Federal share of unliquidated obligations"                           
  SF425_$LineItem[SF425_$ItemNum=="g"]="Total Federal share"                          
  SF425_$LineItem[SF425_$ItemNum=="h"]="Unobligated balance of Federal funds"               
  SF425_$LineItem[SF425_$ItemNum=="i"]="Total recipient share required"                                      
  SF425_$LineItem[SF425_$ItemNum=="j"]="Recipient share of expenditures"                                     
  SF425_$LineItem[SF425_$ItemNum=="k"]="Remaining recipient share to be provided"           
  SF425_$ItemNum[SF425_$ItemNum=="I"]="l"
  SF425_$LineItem[SF425_$ItemNum=="l"]="Total Federal share of program income earned"                        
  SF425_$LineItem[SF425_$ItemNum=="I"]="Total Federal program income earned"
  SF425_$LineItem[SF425_$ItemNum=="m"]="Program income expended in accordance with the deduction alternative"
  SF425_$LineItem[SF425_$ItemNum=="n"]="Program income expended in accordance with the addition alternative" 
  SF425_$LineItem[SF425_$ItemNum=="o"]="Unexpended program income" 
  
  return(SF425_)
}

########################################
#2018 State Narratives with projected budgets
########################################

#define function to get 2018 budget data from tables
##a lot of cleaning is required becaues the budget table formatting
##doesn't read consistently into R
get_narratives<-function(site_,yr_){
  
  #get filename
  filenam<-substr(site,start,nchar(site))
  
  #get budget tables from PDFs
  narrative<-extract_areas(filenam, output="data.frame")
  budget<-narrative[[length(narrative)]]
  
  #try again if failed
  if (narrative[[length(narrative)]]=="") {
    print("please try to select table again")
    narrative<-extract_areas(site_, output="data.frame")
    budget<-narrative[[length(narrative)]]
  }
  
  #get state name
  start<-stri_locate_last(site_, regex = "\\/")[1]+1
  st<-substr(site_,start,start+1)
  
  #delete all instances of "$" and "-" and NA and double spaces
  budget<-lapply(budget, gsub, pattern='\\$', replacement='')
  budget<-lapply(budget, gsub, pattern='\\-', replacement='')
  budget<-lapply(budget, gsub, pattern='\\_', replacement='')
  budget<-lapply(budget, gsub, pattern='  ', replacement=' ')
  budget<-as.data.frame(budget)
  
  #remove last line because we don't need it
  budget<-budget[1:(nrow(budget)-1),]
  
  #delete empty columns
  blanks<-colSums(is.na(budget) | budget == "")==nrow(budget)
  budget<-budget[blanks==0]

  #copy row 2 to row 1 if row 1 blank
  budget[1,which(budget[1,]=="" | is.na(budget[1,])==1)]<-budget[2,which(budget[1,]=="" | is.na(budget[1,])==1)]

  #add to running csv
  write.csv(budget,file=paste(st,"1_2018 budget narratives.csv"))
  
  #try to find and add column names
  try(colnames(budget)[which(budget[1,]=="(a) Voting")]<-"Voting_Equipment")
  try(colnames(budget)[which(budget[1,]=="(b) Election")]<-"Election_Auditing")
  try(colnames(budget)[which(budget[1,]=="Election")]<-"Election_Auditing")
  try(colnames(budget)[which(budget[1,]=="Registration")]<-"Voter_Registration_Systems")
  try(colnames(budget)[which(budget[1,]=="(d) Cyber Security")]<-"Cyber_Security")
  try(colnames(budget)[which(budget[1,]=="Cyber Security")]<-"Cyber_Security")
  try(colnames(budget)[which(budget[1,]=="(e) Communications")]<-"Communications")
  try(colnames(budget)[which(budget[1,]=="Communications")]<-"Communications")
  try(colnames(budget)[which(budget[1,]=="(f) Other")]<-"Other_F")
  try(colnames(budget)[which(budget[1,]=="(g) Other")]<-"Other_G")
  try(colnames(budget)[which(budget[1,]=="TOTALS")]<-"Totals")
  try(colnames(budget)[which(budget[1,]=="% Fed Tota")]<-"Pct_Fed_Total")
  try(colnames(budget)[which(budget[1,]=="% Fed Total")]<-"Pct_Fed_Total")
  
  #find column and row where PERSONNEL is located and delete preceeding rows
  ##PERSONNEL is the first row of the data so we will use it as anchor
  if (sum(budget[[1]]=="1. PERSONNEL (including fringe)")==1){
    budget<-budget[which(budget[[1]]=="1. PERSONNEL (including fringe)"):NROW(budget),]
    colnames(budget)[1]<-"Item"
  }
  if (sum(budget[[2]]=="PERSONNEL (including fringe)")==1){
    budget<-budget[which(budget[[2]]=="PERSONNEL (including fringe)"):NROW(budget),]
    budget[[2]]<-paste(budget[[1]],budget[[2]], sep=" ")
    #delete first column since the data starts at column 2
    budget<-select(budget,-c(colnames(budget[1])))
    colnames(budget)[1]<-"Item"
  }
  if (sum(budget[[3]]=="PERSONNEL (including fringe)")==1){
    budget<-budget[which(budget[[3]]=="PERSONNEL (including fringe)"):NROW(budget),]
    budget[[3]]<-paste(budget[[1]],budget[[2]],budget[[3]], sep=" ")
    #delete first column since the data starts at column 3
    budget<-select(budget,-c(colnames(budget[1]),colnames(budget[2])))
    colnames(budget)[1]<-"Item"
  }
  
  #remove columns after pct total because they're not needed
  budget<-budget[1:(which(colnames(budget)=="Pct_Fed_Total")-1)]
  
  #add state
  budget$State<-st
  
  #add year column
  budget$Year<-yr_
  
  #add source url
  budget$Source<-site_
  
  #reorder columns to match the budgets df
  budget <- budget %>% select(State,Year,Source,everything())
  
  #add to running csv
  write.csv(budget,file=paste(st,"2_2018 budget narratives.csv"))
  
  #print error if empty budget
  if (ncol(budget)<4) {
    print(paste("Table not scraped for ",st))
  }
  
  #append to budgets df
  return(budget)
}

#get urls for 2018 state narratives 
sites<-prep_reports('State Narrative - 2018')

#create empty dataframe to store the budgets
budgets<-data.frame(State=character(),Year=numeric(),Source=character(),Item=character(),
                    Voting_Equipment=character(),Election_Auditing=character(),
                    Voter_Registration_Systems=character(),Cyber_Security=character(),
                    Communications=character(),Other_F=character(),Other_G=character(),
                    Totals=character())
budgets_messy<-data.frame()
year=2018

#loop through urls for pdfs containing the budget narratives
for (site in sites){
  budget_<-data.frame()
  start<-stri_locate_last(site, regex = "\\/")[1]+1
  print("Getting 2018 state narrative for:")
  print(substr(site,start,start+1))
  
  #scrape narratives
  try(budget_<-get_narratives(site,year),silent = T)
  
  #add to error list if failed
  if (ncol(budget_)<=2){
    errors[[length(errors)+1]]<-substr(site,start,start+1)
    print("could not scrape data")
  }
  
  if (ncol(budget_)==ncol(budgets)){
    #append narratives to global budgets dataframe
    budgets<-rbind(budgets,budget_)
    print("reformatted successfully")
  } else{
    #if the data didn't process right, append to messy
    budgets_messy<-bind_rows(budgets_messy,budget_)
    print("reformatted with errors")
  }
}

#For those with errors, have to hand-enter their data.
write.csv(budgets,file="budgets_neat.csv")
write.csv(budgets_messy,file="budgets_messy.csv")

#melt and transform into long form
budgets2018<-read.csv("2018_budgets_edited_CSV.csv")

#replace NAs with 0s
budgets2018[is.na(budgets2018)] = 0
budgets2018[budgets2018=="  "] = 0
budgets2018[budgets2018==""] = 0

#convert numbers stored as characters to numbers
vars<-list("Year", "Voting_Equipment", "Election_Auditing", "Voter_Registration_Systems",
"Cyber_Security", "Communications", "Other_F", "Other_G", "Totals")
lapply(vars, function(var) budgets2018[[var]]<-as.numeric(budgets2018[[var]]))
write.csv(budgets2018,"2018_budgets_edited_CSV.csv")


########################################
#2020 State Narratives with projected budgets
########################################

#have to transcribe 2020 narratives by hand sadly
sites<-prep_reports("State Narrative - 2020")


########################################
#Get SF425 expenditure data
########################################

SF425<-data.frame(State=character(),Year=character(),Source=character(),ItemNum=character(),LineItem=character(),Amount=character())

#get 2018 SF425s
#will need to check 2018 entries by hand due to low-res scanning
report<-"Financial Report - Fiscal Year 2018"
year<-"2018"
SF425_2018<-scrape_SF425(report,year)
write.csv(SF425_2018,file = "SF425_data_2018_unedited.csv")
#need to further clean this data by hand and then reload cleaned data
SF425_2018<-read.csv(file = ".\\cleaned_data\\SF425_data_2018_edited_CSV.csv")

#get 2019 SF425s
report="Financial Report - Fiscal Year 2019"
year="2019"
SF425_2019<-scrape_SF425(report,year)
write.csv("SF425_data_2019_unedited.csv")
#then manually revise and re-save under this file name:
SF425_2019<-read.csv(file = ".\\cleaned_data\\SF425_data_2019_edited_CSV.csv")

#get 2020 SF425s
report<-"Financial and Progress Reports - Fiscal Year 2020 Annual"
year<-"2020"
SF425_2020<-scrape_SF425(report,year)
write.csv(SF425_2020,file = "SF425_data_2020_unedited.csv")
#then manually revise and re-upload the revised data  under this file name:
SF425_2020<-read.csv(file = ".\\cleaned_data\\SF425_data_2020_edited_CSV.csv")

#get 2021 SF425s
report<-"Financial and Progress Reports - Fiscal Year 2021 Annual"
year="2021"
SF425_2021<-scrape_SF425(report,year)
write.csv(SF425_2021,file = "SF425_data_2021_unedited.csv")
#then manually revise and re-save under this file name:
SF425_2021<-read.csv(file = ".\\cleaned_data\\SF425_data_2021_edited_CSV.csv")

#combine into big dataset
SF425<-rbind(SF425_2018,SF425_2019,SF425_2020,SF425_2021)

#redefine amount as integer
SF425$Amount<-as.numeric(SF425$Amount)

#print to CSV
SF425 %>% select(State,Year,Source,ItemNum,LineItem,Amount) %>% write.csv(.,file="SF425 data_all years_edited_CSV.csv")
########################################
#Get text of narratives
#######################################

#get text of reports for anticipated spending
links<-prep_reports()

#foreach pdf
###read and clean text of pdf
###lemmatize text into tokens
###count vectorization to determine frequency of key words
###check if key words in text 

#2020 narratives
"State Narrative - 2018"
"Request Letter - 2018"

#get text of reports that they actually 
"State Narrative - 2020"
"Request Letter - 2020" 
