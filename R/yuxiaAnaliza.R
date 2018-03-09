
# nocov start
.onLoad	<-	function(libname,	pkgname)	{
  op	<-	options()
  op.yuxiaAnaliza	<-	list(
    yuxiaAnaliza.dbPath	=	'/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/Yu Xia/arc/db'
  )
  toset	<-	!(names(op.yuxiaAnaliza)	%in%	names(op))
  if(any(toset))	options(op.yuxiaAnaliza[toset])
  invisible()
}
# nocov end

