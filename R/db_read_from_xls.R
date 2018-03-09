read_db <- function(filenames, slo_path)
{

  dic<-readxl::read_excel(slo_path)

  dt<-integer(0)
  filename<-filenames[[4]]
  #  filename<-filenames[[2]]
  #  filenames<-filenames[filenames!='../arc/db/Birth95_2006 full num.xlsx']
  for(filename in filenames)
  {
    partial_dt<-read_source_file(filename, dic)
    if(length(dt)==0)
    {
      dt<-partial_dt
    } else {
      dt<-rbind(dt, partial_dt)
    }
  }
  gc()
  return(dt)
}

read_source_file<-function(filename, dic)
{
  cat(paste0('Reading in file ', filename, '...\n' ))
  df<-readxl::read_excel(filename)
  dt<-integer(0)

  duplicates<-list()
  for(varnr in seq_along(colnames(df)))
  {
    dt_oldname <- colnames(df)[[varnr]]
    dic_row <- which(dic[['Short name']]==dt_oldname)
    #    cat(paste0(varnr, ": ", dt_oldname,'\n'))
    if(length(dic_row)==0) #Zmiennej nie ma w słowniku, więc pewnie ma suffix '__n'
    {
      dt_oldname_stripped <- stringr::str_match(dt_oldname, '^(.*)__\\d+$')[[2]]
      if(! is.na(dt_oldname_stripped))
      {
        dic_row <- which(dic[['Short name']]==dt_oldname)
        if(length(dic_row)!=1)
        {
          browser() #Nie znaleziono danej zmiennej w słowniku lub w słowniku są duplikaty
        }
        dt_oldname <- dt_oldname_stripped
      }
    }

    if(length(dic_row)==1)
    {
      if(length(duplicates[[dt_oldname]])==0)
      {
        duplicates[dt_oldname]<-1
      } else {
        duplicates[dt_oldname]<-duplicates[dt_oldname] + 1
      }

      dt_newname <- dic[['New short name']][[dic_row]]
      if (duplicates[[dt_oldname]] > 1)
      {
        if (duplicates[[dt_oldname]]==2)
        {
          #We need to retrospectively
          #fix the name of the first variable
          data.table::setnames(dt, dt_newname, paste0(dt_newname,'_1'))
        }
        dt_newname<-paste0(dt_newname,'_',duplicates[[dt_oldname]])
      }
      var<-df[[varnr]]
      if (length(dt)==0)
      {
        dt<-data.table(var)
        setnames(dt, 'var', dt_newname)
      } else {
        dt[,(dt_newname):=var]
      }
    }
  }
  return(dt)
}

