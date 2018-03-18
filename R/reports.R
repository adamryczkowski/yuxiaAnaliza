prepare_report<-function(title="Analiza statystyczna") {
  doc<-pander::Pandoc$new()
  doc$author<-"Adam Ryczkowski"
  doc$format<-"docx"
  doc$title<-title
  options(OutDec=',')
  return(doc)
}

save_report<-function(report, filename='/tmp/report', flag_open = TRUE, template='') {
  pander::panderOptions('big.mark', '\uA0')
  pander::panderOptions('missing', 'b/d')
  pander::panderOptions('date', '%Y-%m-%d\uA0%X')
  pander::panderOptions('use.hyphening', TRUE)
  pander::panderOptions('decimal.mark', getOption("OutDec"))
  tmpfile <- tempfile(pattern='report_', tmpdir = getwd(), fileext = '')
  if(template!='') {
    template<-pathcat::path.cat(getwd(), template)
    if(!file.exists(template)) {
      browser()
    }
    opts<-paste0(' --template "', template, '"')
  }
  report$export(tmpfile, open=FALSE,
                options=paste0('+RTS -K100000000 -RTS --filter pandoc-fignos --filter pandoc-tablenos ',
                               '-M "tablenos-caption-name:Tabela" -M "fignos-caption-name:Rycina"', opts ))
  file.rename(paste0(tmpfile,'.md'), paste0(filename, '.md'))
  file.rename(paste0(tmpfile,'.', report$format), paste0(filename, '.', report$format))
}
