library(RSQLite)
library(xlsx)

##save a data frame to sqlite table

#write a data frame to targeted table of a sqlite database
#If the table preexist,then asked to overwrite or not
#if the table not perexist, then create a new table harboring the data frame
d2table<-function(db,table_name,df){
  table_list<-dbListTables(db)
  if(table_name %in% table_list){
    print("databse has a same table!!!")
    replace<-readline("Do you want to replace the table?\nPlease enter y or n\n")
    if(replace=="y"){
      dbRemoveTable(db,table_name)
      dbWriteTable(db,table_name,as.data.frame(df))
    }
    else{
      stop("no operation happened on data base")
    }
  }
  else{
    dbWriteTable(db,table_name,as.data.frame(df))
  }
}

# save a data frame to a sqlite
# using name of data frame as the table name
# if the sqlite database preexists, then append  data frame in sqlite database
# if not, create a new sqlite database, then put data frame in it
# finally, will show the all tables name in the data base
push_d2sql<-function(df,dbname){
  df_name<-deparse(substitute(df))
  dbfullpath<-file.path(getwd(),paste0(dbname,".db",sep=""))
  dr<-dbDriver("SQLite")
  try(
    if(file.exists(dbfullpath)){
      mydb<-dbConnect(dr,paste0(dbname,".db",sep=""))
      d2table(mydb,df_name,df)


    }
    else{
      print("In current dir, there is no targeted database\n")
      print("A new database is created...")
      mydb<-dbConnect(dr,paste0(dbname,".db",sep=""))
      d2table(mydb,df_name,df)
    }
  )
  print("Now,database contains the following tables")
  print(dbListTables(mydb))
  dbDisconnect(mydb)
}


#get a table from a sqlite database
#in a data frame
get_d5sql<-function(db_name,table_name){
  try(
    dr<-dbDriver("SQLite"),
    db<-dbConnect(dr,db_name),
    df<-dbReadTable(db,table_name)
  )
  dbDisconnect(db)
  return(df)
}


# save a data frame to a excel file
# using name of data frame as the sheet name
# if the excel workbook preexists, if not, create a new sqlite database,
# write a data frame to targeted sheet of a excel workbook
# If the sheet preexist,then asked to overwrite or not
# if the sheet not perexist, then create a new sheet harboring the data frame
# finally, will show the all tables name in the data base
push_d2xls<-function(df,workbook_name){

  sheet_name<-deparse(substitute(df))
  file_path<-file.path(getwd(),workbook_name)
  if(!is.data.frame(df)){
    df<-as.data.frame(df)
  }

  if(file.exists(file_path)){
    wb<-loadWorkbook(file_path)
    wb_sheets<-getSheets(wb)

    if(sheet_name %in% names(wb_sheets)){
      print("Database already has a sheet with the same name")
      print("Do you want override it?")
      response<-readline("please type y or n:  \n")
      if(response=="n"){
        stop("no further operation conducted")
      }
      removeSheet(wb,sheet_name)
      sheet<-createSheet(wb,sheetName = sheet_name)
      wb_sheets<-getSheets(wb)
      print(names(wb_sheets))
      df<-as.data.frame(df)
      addDataFrame(df,sheet)
      saveWorkbook(wb,workbook_name)
    }
    else{
      sheet<-createSheet(wb,sheetName = sheet_name)
      df<-as.data.frame(df)
      addDataFrame(df,sheet)
      saveWorkbook(wb,workbook_name)
    }

  }
  else{
    wb<-createWorkbook()
    sheet<-createSheet(wb,sheetName = sheet_name)
    df<-as.data.frame(df)
    addDataFrame(df,sheet)
    saveWorkbook(wb,workbook_name)
  }
}

#get a sheet from a excel file
#in a data frame
get_d5xls<-function(workbook_name,sheet_name){
  df<-read.xlsx(workbook_name,sheet_name)
  return(df)
}



