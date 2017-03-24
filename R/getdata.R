require(gdata)
get_extension<-function(file_name){
  ext<-unlist(strsplit(file_name,"\\."))
  ext<-ext[length(ext)]
  return (ext)
}
get_data<-function(...){
  src=file.choose()
  file_name=basename(src)

  switch(get_extension(file_name),
         txt=read.table(file=src,...),
         csv=read.csv(file=src,...),
         xls=read.xls(xls=src,...),
         xlsx=read.xls(xls=src,...))

}
