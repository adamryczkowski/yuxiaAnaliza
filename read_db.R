readxls<-function() {
  source<-getOption('yuxiaAnaliza.dbPath')
  filenames <- file.path(source, list.files(source,pattern='^[^~].*\\.xlsx$'))
  slo_path<-system.file('slo.xlsx', package = 'yuxiaAnaliza')

  rawdt<-read_db(filenames=filenames, slo_path = slo_path)

}

readdb<-function() {
  rawdt<-readxlsx()
  #kodowanieECpath<-'shared/kodowanieEC.xlsx'
  kodowanieECpath<-system.file('kodowanieEC.xlsx', package = 'yuxiaAnaliza')
  debugonce(annotate_db)
  ans<-annotate_db(rawdt, kodowanieECpath=kodowanieECpath)
  dt_fixed <- ans$dt
  SlownikNazwRegionow <- ans$SlownikNazwRegionow

}
