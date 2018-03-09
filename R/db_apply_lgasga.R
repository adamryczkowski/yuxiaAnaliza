sga_lga<-function(flag_male, flag_one, prefix='m3_') {
  filters<-c('filtr_ciaze_pojedyncze_chlopcy','filtr_ciaze_pojedyncze_dziewczynki','filtr_wieloraka_chlopcy', 'filtr_wieloraka_dziewczynki')

  filtr<-filters[as.numeric(!flag_male) * 1 + as.numeric(!flag_one)*2 + 1]
  filtr_fn <- eval(parse(text = paste0(filtr)))()
  filter_str<-filtr_fn$filtr

  filename<-paste0("q/", prefix, if(flag_male) 'ch' else 'dz', if(flag_one) '1' else '2', '.rds')
  gc<-readRDS(filename)
  varx<-'preg_weeks'
  vary<-'preg_weight'
  mydt<-db %>% filter_(filter_str) %>% select_(.dots=c(varx, vary))


  df<-decorate_quantiles(m4 = readRDS(filename), mydt = mydt, varx = varx, vary = vary)$df
  dflo <- df %>% filter(perc==0.03) %>% select(-perc)
  dfhi <- df %>% filter(perc==0.97) %>% select(-perc)

  flags_lo<-as.numeric(approxfun(dflo$zn, dflo$zz)(mydt$preg_weeks)>mydt$preg_weight)
  sga<-factor(flags_lo+1,  labels = c("Noworodki nie-SGA", "Noworodki SGA"))
  attr(sga, "label") <- "Przynależność do kategorii SGA"
  flags_hi<-as.numeric(approxfun(dfhi$zn, dfhi$zz)(mydt$preg_weeks)<mydt$preg_weight)
  lga<-factor(flags_hi+1,  labels = c("Noworodki nie-LGA", "Noworodki LGA"))
  attr(lga, "label") <- "Przynależność do kategorii LGA"
  which_<-which(eval(parse(text=filter_str), envir = db))

  return(list(lga=lga, sga=sga, which=which_))
}

apply_sga_lga<-function(db, prefix='m3_') {
  lgasga_11<-sga_lga(FALSE, FALSE)
  lgasga_12<-sga_lga(FALSE, TRUE)
  lgasga_21<-sga_lga(TRUE, FALSE)
  lgasga_22<-sga_lga(TRUE, TRUE)
  #db$sga<-NULL
  db$sga<-as.integer(NA)
  db[lgasga_11$which,'sga']<-lgasga_11$sga
  db[lgasga_12$which,'sga']<-lgasga_12$sga
  db[lgasga_21$which,'sga']<-lgasga_21$sga
  db[lgasga_22$which,'sga']<-lgasga_22$sga
  attributes(db$sga)<-attributes(lgasga_22$sga)
  attr(db$sga, 'f.o.b')<-3

  db$lga<-as.integer(NA)
  db[lgasga_11$which,'lga']<-lgasga_11$lga
  db[lgasga_12$which,'lga']<-lgasga_12$lga
  db[lgasga_21$which,'lga']<-lgasga_21$lga
  db[lgasga_22$which,'lga']<-lgasga_22$lga
  attributes(db$lga)<-attributes(lgasga_22$lga)
  attr(db$lga, 'f.o.b')<-3
  return(db)
}
