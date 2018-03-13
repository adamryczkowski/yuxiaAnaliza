#TODO
#OK 1. Przekazywanie parametrów do renderowanego dokumentu. Jednym z nich będzie flaga włączająca dodatkowe informacje dot. wklejonego wykresu, takie jak nazwa pliku. Inną będzie flaga dodająca wewnętrzne nazwy porównywanych zmiennych i lokalizację komórki w macierzach zależności. Obie ułatwią nam debugowanie
#2. Kod łączący kilka policzonych raportów w jeden.
#3. Błąd z lokalizacjami rozdziałów. Nie powinno być tak, że wszystkie analizy zeszły do jednego rozdziału
#4. Problem z niektórymi wykresami mozaikowymi, gdzie wyszły same zera. Ale dopóki nie rozwiążę #1, trudno będzie ten wykres znaleźć
#5. duplikacja mozajkowych. Powinny być 2 mozaikowe, ale nie identyczne, ale jeden ma być transpozycją drugiego


library(data.table)
dt<-readRDS('db.rds')
dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
aggrt<-yuxiaAnaliza::allAggregates()
macierze_path<-system.file('macierze_analiz.xlsx', package='yuxiaAnaliza')
m<-relationshipMatrix::read_matrix(macierze_path, data.table(dt_structure), aggregate_types = aggrt)
cl<-yuxiaCharts::classify_analyses(m)
tododf<-cl$tododf
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot_wyliczany', 'crosstab', 'boxplot')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot')))
doc<-relationshipMatrix::render_matrix(cellsdf=subset_df[1:100,], author="Adam", title="analiza",
                                       stats_dispatchers=cl$dispatchers,
                                       report_dispatchers=list(),
                                       report_functions=list(),
                                       aggregates=aggrt, filters=get_filters(), df_task=dt)
doc<-relationshipMatrix::render_matrix(cellsdf=subset_df[132:133,], author="Adam", title="analiza",
                                       stats_dispatchers=cl$dispatchers,
                                       report_dispatchers=list(),
                                       report_functions=list(),
                                       aggregates=aggrt, filters=yuxiaAnaliza::get_filters(), df_task=dt)
doc$set_property('chart_debug', TRUE)
doc$set_property('chart_postprocess', FALSE)
doc$pre_render()
saveRDS(doc, file='doc2.rds', compress='xz')
pandoc<-pander::Pandoc$new()
doc<-readRDS('doc.rds')
rm(dt)
rm(cl)
gc()
#  debugonce(doc$render)
doc$render(pandoc)
unlink('/tmp/cos.docx')
unlink('/tmp/cos.md')
save_report(pandoc, filename = '/tmp/cos')
