
locateSymbol<-function(symbol, startenv=globalenv(), nest_level=1) {
  if(environmentName(startenv)=='R_EmptyEnv') {
    return(list(nest_level=nest_level, env_name = NA))
  }
  if(class(startenv)!='environment') {
    browser()
  }
  if(exists(symbol, envir=startenv, inherits = FALSE)) {
    return(list(nest_level=nest_level,env_name = environmentName(startenv)))
  } else {
    return(locateSymbol(symbol, parent.env(startenv), nest_level = nest_level +1))
  }
}

get_missing_symbols_from_expr<-function(expr, package_name='yuxia', env=.GlobalEnv) {
  symbols<-utils::getParseData(parse(text=expr)) %>% filter(token=='SYMBOL_FUNCTION_CALL') %>% select(text) %>% unique %>% `[[`(1)
  env_names<-symbols %>% map(locateSymbol, startenv = env) %>% map_chr('env_name')
  valid_env_names <- env_names %in% c('R_GlobalEnv', package_name)
  return(symbols[valid_env_names])
}

get_all_symbols_from_expr<-function(expr, package_name='yuxia', env=.GlobalEnv) {
  symbols<-utils::getParseData(parse(text=expr)) %>% filter(token %in% c('SYMBOL', 'SYMBOL_FUNCTION_CALL')) %>% select(text) %>% unique %>% `[[`(1)
  env_names<-symbols %>% map(locateSymbol, startenv = env) %>% map_chr('env_name')
  fn_names <- symbols %>% map_lgl(function(smb) exists(smb) && is.function(eval(parse(text=smb), envir = env)))
  valid_env_names <- env_names %in% c('R_GlobalEnv', paste0('package:',package_name))
  return(symbols[valid_env_names & fn_names])
}

get_missing_symbols_list<-function(expr, package_name='yuxia', env=.GlobalEnv) {
  symbols <- get_all_symbols_from_expr(expr, package_name = package_name, env=env)
  all_symbols <- symbols

  while(length(symbols)>0){
    exprs <- symbols %>% map(function(s) list(symbol=s, body=paste0(capture.output(suppressWarnings(dump(s,file='' ))), collapse='\n')))
    new_symbols<-exprs %>% transpose %>%
      pmap(.f=function(symbol, body) get_missing_symbols_from_expr(body,
                                                                   package_name=package_name,
                                                                   env=environment(as.function(eval(parse(text=symbol), envir = env))))) %>%
      flatten_chr
    symbols <- setdiff(new_symbols, all_symbols)
    all_symbols <- unique(c(symbols, all_symbols))
  }
  return(sort(all_symbols))
}

get_complete_source_of_expression_<-function(expr, packages_to_include=getPackageName(), env=.GlobalEnv) {
  if(packages_to_include=='.GlobalEnv'){
    packages_to_include<-NULL
  }

  if(!is.character(expr)){
    expr <- as.character(deparse(expr))
  }

  all_symbols<-get_missing_symbols_list(expr, package_name = packages_to_include, env = env)
  sources <- all_symbols %>% map(function(smb) paste0(capture.output(dump(smb,file='' )), collapse='\n')) %>% flatten_chr

  return(paste0(paste0(sources, collapse='\n\n'), '\n\n', expr))
}


get_complete_source_of_expression<-function(expr, packages_to_include=getPackageName()) {
  a <- lazyeval::lazy(expr)
  str_expr<-as.character(deparse(a$expr))
  ret <- get_complete_source_of_expression_(expr=str_expr, packages_to_include=packages_to_include, env=a$env)
  return(ret)
}

get_digest_of_expression_<-function(expr, packages_to_include=getPackageName(), env=.GlobalEnv) {
  expr_str <- get_complete_source_of_expression_(expr, packages_to_include, env)
  return(list(hash=digest::digest(expr_str, ascii=FALSE), source=expr_str))
}

get_digest_of_expression_m<-memoise::memoise(get_digest_of_expression_)

get_digest_of_expression<-function(expr, packages_to_include=getPackageName()) {
  browser()
  a <- lazyeval::lazy(expr)
  str_expr<-as.character(deparse(a$expr))
  ret <- get_digest_of_expression_(expr=str_expr, packages_to_include=packages_to_include, env=a$env)$hash
  return(ret)
}

get_digest_of_function<-function(fn, packages_to_include=NULL) {
  ret <- get_digest_of_expression_m(paste0(fn, '()'), packages_to_include = c(getPackageName(), packages_to_include))$hash
  #  ret <- get_digest_of_expression_(expr=fn)
  return(ret)
}

get_digest_of_function_ex<-function(fn, packages_to_include=NULL) {
  ret <- get_digest_of_expression_m(paste0(fn, '()'), packages_to_include = c(getPackageName(), packages_to_include))
  #  ret <- get_digest_of_expression_(expr=fn)
  return(ret)
}
