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
  zamiany_zmiennych_path<-system.file('Zamiany_zmiennych.ods', package = 'yuxiaAnaliza')
  debugonce(annotate_db)
  ans<-annotate_db(rawdt, kodowanieECpath=kodowanieECpath, zamiany_zmiennych_path=zamiany_zmiennych_path)
  dt_fixed <- ans$dt
  SlownikNazwRegionow <- ans$SlownikNazwRegionow

}
