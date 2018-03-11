
# nocov start
.onLoad	<-	function(libname,	pkgname)	{
  op	<-	options()
  op.yuxiaAnaliza	<-	list(
    yuxiaAnaliza.dbPath	=	'/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/Yu Xia/arc/db',
    yuxiaAnaliza.lsgsga_data	=	'/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/Yu Xia/yuxia-local/q',
    yuxiaAnaliza.db_cache	=	'/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/Yu Xia/yuxia-local/q'
  )
  toset	<-	!(names(op.yuxiaAnaliza)	%in%	names(op))
  if(any(toset))	options(op.yuxiaAnaliza[toset])
  invisible()
}
# nocov end

.datatable.aware=TRUE
