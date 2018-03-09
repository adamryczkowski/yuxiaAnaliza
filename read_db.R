readxls<-function() {
  source<-getOption('yuxiaAnaliza.dbPath')
  filenames <- file.path(source, list.files(source,pattern='^[^~].*\\.xlsx$'))
  slo_path<-system.file('slo.xlsx', package = 'yuxiaAnaliza')

  rawdt<-read_db(filenames=filenames, slo_path = slo_path)

}

readdb<-function() {
  rawdt<-readxlsx()
  ans<-annotate_db(rawdt)
  dt_fixed <- ans$dt
  SlownikNazwRegionow <- ans$SlownikNazwRegionow

}
