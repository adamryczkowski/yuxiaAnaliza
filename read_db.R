readxls<-function() {
  source<-getOption('yuxiaAnaliza.dbPath')
  filenames <- file.path(source, list.files(source,pattern='^[^~].*\\.xlsx$'))
  slo_path<-system.file('slo.xlsx', package = 'yuxiaAnaliza')

  rawdt<-read_db(filenames=filenames, slo_path = slo_path)

}

readdb<-function() {
  rawdt<-readxls()
  #kodowanieECpath<-'shared/kodowanieEC.xlsx'
  kodowanieECpath<-system.file('kodowanieEC.xlsx', package = 'yuxiaAnaliza')
  zamiany_zmiennych_path<-system.file('Zamiany_zmiennych.ods', package = 'yuxiaAnaliza')
  #debugonce(annotate_db)
  ans<-annotate_db(rawdt, kodowanieECpath=kodowanieECpath, zamiany_zmiennych_path=zamiany_zmiennych_path)
  dt_fixed <- ans$dt
  SlownikNazwRegionow <- ans$SlownikNazwRegionow
  return(dt_fixed)
}

readsgalga<-function() {
  dt_fixed<-readdb()
  dt_lgasga<-apply_sga_lga(dt_fixed)
}

db<-readRDS('db.rds')
db<-readsgalga()
saveRDS(db, 'db.rds', compress='xz')
