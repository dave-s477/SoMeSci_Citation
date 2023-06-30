# This function was adapted from https://github.com/erblast/easyalluvial
alluvial = function( data
                     , key
                     , value
                     , id
                     , fill = NULL
                     , fill_right = T
                     , bins = 5
                     , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                     , NA_label = 'NA'
                     , order_levels_value = NULL
                     , order_levels_key = NULL
                     , order_levels_fill = NULL
                     , complete = TRUE
                     , fill_by = 'first_variable'
                     , col_vector_flow = palette_qualitative() %>% palette_filter( greys = F)
                     , col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(3,6,4,7,5)]
                     , verbose = F
                     , stratum_labels = F
                     , stratum_label_size = 4.5
                     , stratum_width = 1/4
                     , auto_rotate_xlabs = T
                     , labels = NULL
                     , title = NULL
                     , facet_name = NULL
                     , ...
){
  
  # quosures
  
  key = enquo( key )
  value = enquo( value )
  id = enquo( id )
  fill = enquo( fill )
  
  key_str = quo_name(key)
  value_str = quo_name(value)
  id_str = quo_name(id)
  
  # in order to make the function parameters compatible with strings we convert the strings
  # extracted from the quosures to symbols and override the quosures
  
  key = as.name( key_str )
  value = as.name( value_str )
  id = as.name( id_str )
  
  # fill
  
  if( rlang::quo_is_null(fill) ){
    fill_str = NULL
  }else{
    fill_str = quo_name(fill)
    fill = as.name( fill_str )
  }
  
  
  # store params to attach to plot
  
  params = list(
    key = key_str
    , value = value_str
    , id = id_str
    , fill = fill_str
    , fill_right = fill_right
    , bins = bins
    , bin_labels = bin_labels
    , NA_label = NA_label
    , order_levels_value = order_levels_value
    , order_levels_key = order_levels_key
    , order_levels_fill = order_levels_fill
    , complete = complete
    , fill_by = fill_by
    , col_vector_flow = col_vector_flow
    , col_vector_value =  col_vector_value
    , verbose = verbose
    , stratum_labels = stratum_labels
    , stratum_width = stratum_width
    , auto_rotate_xlabs = auto_rotate_xlabs
  )
  
  # ungroup
  
  data = ungroup(data)
  
  # transform numerical variables for binning
  
  data = data %>%
    ungroup() %>%
    select( one_of( c(key_str, value_str, id_str, fill_str) ) ) %>%
    mutate( !! key_str := as.factor( !! key )
            , !! id_str := as.factor( !! id )
    )
  
  
  data_trans = data %>%
    manip_bin_numerics( bins, bin_labels, NA_label = NA_label, ... ) %>%
    mutate( !! value_str := as.factor( !! value ) )
  
  if( ! is_null(fill_str) ){
    if( fill_str %in% levels(data_trans[[key_str]]) ){
      stop( paste( 'Name of fill variable/column:', fill_str, ', cannot be one of'
                   , levels(data_trans[[key_str]]) ) )
    }
  }
  
  #complete data
  
  if( is.null(fill_str) ){
    
    data_trans = data_trans %>%
      complete( !! key , !! id )
    
  }else{
    
    id_2_fill_keys = data_trans %>%
      group_by( !! id, !! fill) %>%
      summarise()
    
    data_trans = data_trans %>%
      complete( !! key , !! id ) %>% ## leaves NA values for fill
      select( - !! fill ) %>%     ## deselect and rejoin fill
      left_join( id_2_fill_keys, by = id_str )
    
  }
  
  # preserve order of categorical variables
  
  ordered_levels_x = c( order_levels_key, levels( select(data_trans, !! key)[[1]] ) ) %>% unique()
  ordered_levels_y = c( order_levels_value, levels( select(data_trans, !! value)[[1]] ) ) %>% unique()
  
  if( ! is.null(fill_str) ){
    ordered_levels_fill = c( order_levels_fill, levels( select(data_trans, !! fill)[[1]] ) ) %>% unique()
    ordered_levels_y = c( ordered_levels_y, ordered_levels_fill) %>% unique()
    
    if(fill_right){
      ordered_levels_x = c( ordered_levels_x, fill_str )
    }else{
      ordered_levels_x = c( fill_str, ordered_levels_x )
    }
    
  }else{
    ordered_levels_fill = NULL
  }
  
  # convert NA values in value to NA_label
  
  data_trans = data_trans %>%
    mutate(   !! value_str := as.character( !! value )
              ,  !! value_str := ifelse( is.na( !!  value ), NA_label, !! value )
              ,  !! value_str := as.factor( !! value) ) ##factor label will be restored further down
  
  
  suppressWarnings({
    
    # add alluvial ids
    data_spread = data_trans %>%
      spread( key = !! key, value = !! value )
    
    # to ensure dbplyr 0.8.0. compatibility we 
    # transform factors to character before grouping
    # and back after grouping. The default behaviour has
    # changed so we comment out the to character transformation
    
    factor_cols = names( select_if(data_spread, is.factor) )
    
    data_alluvial_id = data_spread %>%
      select( - !! id ) %>%
      # mutate_at( .vars = vars( one_of(factor_cols) ), as.character ) %>%
      group_by_all() %>%
      count() %>%
      ungroup() %>%
      mutate( alluvial_id = row_number() ) %>%
      mutate_at( .vars = vars( one_of(factor_cols) ), as.factor )
    
    
    data_new = data_alluvial_id %>%
      gather( key = 'x', value = 'value'
              , - one_of(c('alluvial_id','n', fill_str))  ) %>%
      mutate( x = as.factor(x)
              , x = forcats::fct_relevel(x, ordered_levels_x))
  })
  
  # attach alluvial_id to id keys
  # will be attached to plot later
  
  join_by = names(data_spread)[names(data_spread) %in% names(data_alluvial_id)]
  
  data_key = data_spread %>%
    left_join( data_alluvial_id, by =  join_by) %>%
    mutate_if( is.factor, fct_drop)
  
  # compose fill columns
  
  last_x = levels(data_new$x) %>%
    .[ length(.) ]
  
  first_x = levels(data_new$x)[1]
  
  if( ! is.null(fill_str) ){
    
    data_fill = data_new %>%
      filter( x == last_x ) %>%
      mutate( value = !! fill
              , x = fill_str )
    
    suppressWarnings({
      
      data_new = data_new %>%
        bind_rows( data_fill )
    })
    
    data_new$fill = select( data_new, !! fill)[[1]]
    
  }else if( fill_by %in% c( 'first_variable', 'last_variable') ) {
    
    data_fill = data_new
    
    if( fill_by == 'first_variable') data_fill = data_fill %>%
        filter( x == first_x )
    
    
    if(fill_by == 'last_variable') data_fill = data_fill %>%
        filter( x == last_x )
    
    data_fill = data_fill %>%
      select( alluvial_id, value ) %>%
      rename( fill = value )
    
    join_by = names(data_new)[names(data_new) %in% names(data_fill)]
    
    data_new = data_new #%>%
    left_join( data_fill, by = join_by )
    
    
  } else if( fill_by == 'all_flows'){
    
    data_new$fill = data_new$alluvial_id %>%
      as.factor(.)
    
  }else if( fill_by == 'value'){
    
    data_new$fill = data_new$value
    
  }else{
    warning( 'no valid fill option selected')
    
    data_new$fill = 'a'
    
  }
  
  # reformat factors
  data_new = data_new %>%
    mutate_if( is.character, as.factor ) %>%
    mutate( x =  forcats::fct_relevel( x, ordered_levels_x )
            , value =  forcats::fct_relevel( value, ordered_levels_y )
            , fill =  forcats::fct_relevel( fill , ordered_levels_fill )
    ) %>%
    mutate( value =  forcats::fct_rev(value) )
  
  n_flows    = max( manip_factor_2_numeric( data_new$alluvial_id) )
  reduced_to = round( n_flows/ nrow(data_key) * 100, 1 )
  max_weight = max( data_new$n )
  max_weight_perc = round( max_weight/nrow(data_key) * 100, 1 )
  
  line1 = paste('Number of flows:', n_flows)
  line2 = paste('Original Dataframe reduced to', reduced_to, '%' )
  line3 = paste('Maximum weight of a singfle flow', max_weight_perc, '%')
  
  if( verbose ){
    print( line1 )
    print( line2 )
    print( line3 )
  }
  
  caption = paste( line1, line2, line3, sep = '\n' )
  
  if(n_flows >= 1500){
    
    print( line1 )
    print( line2 )
    print( line3 )
    
    warning( paste( n_flows, ' flows are a lot and the plot will take a long time to render') )
  }
  
  #adjust col_vector length fill flows
  
  n_colors_needed = length( unique(data_new$fill) )
  
  col_vector_flow = palette_increase_length( col_vector_flow, n_colors_needed  )
  
  df_fill_flow = tibble( fill = unique(data_new$fill)
                         , fill_flow = col_vector_flow )
  
  data_new = data_new %>%
    left_join( df_fill_flow, by = 'fill' )
  
  # adjust col_vector length fill value
  
  n_colors_needed = length( unique(data_new$value) )
  
  col_vector_value = palette_increase_length( col_vector_value, n_colors_needed  )
  
  d_fill_value = tibble( value = unique(data_new$value)
                         , fill_value = col_vector_value )
  
  data_new = data_new %>%
    left_join( d_fill_value, by = 'value' )
  
  
  if( ! is.null(fill_str) ){
    
    data_new = data_new %>%
      mutate( fill_value = ifelse( as.character(value) == as.character(!!fill)
                                   & x == fill_str
                                   , fill_flow, fill_value ) )
  }
  
  colbla <- function(col_vector, value){
    names(col_vector[value])
  }
  
  data_new %<>%
    filter(x != 'fill') %>%
    mutate(facet=facet_name)
  
  
  p <- ggplot(data_new,
              aes(x = x
                  , stratum = value
                  , alluvium = alluvial_id
                  , y = n
                  , label = value)) +
    ggalluvial::geom_flow(stat = "alluvium"
                          , lode.guidance = "leftright"
                          , aes( fill = fill_flow
                                 , color = fill_flow )
                          , width = stratum_width
    ) +
    ggalluvial::geom_stratum(  aes(fill = fill_value
                                   , color = fill_value)
                               , width = stratum_width
    ) +
    labs( x = '', y = 'count', title = title)
  
  if (!is.null(labels)) {
    p <- p + theme(legend.position = 'top' ) +
      scale_fill_identity(guide = "legend", label=labels) +
      scale_color_identity(guide = "legend", label=labels) +
      guides(color = guide_legend(nrow=1, title="Metadata status"), fill=guide_legend(nrow=1, title="Metadata status"))
  }else{
    p <- p + scale_fill_identity() +
      scale_color_identity()
  }
  
  if (!is.null(facet_name)){
    p <- p + facet_grid(~facet)
  }
  
  if(stratum_labels){
    p = p + geom_text( stat = ggalluvial::StatStratum
                       , size = stratum_label_size )
  }
  
  
  
  # angle x labels------------------------------------
  
  max_length_x_level = levels( data_new$x ) %>%
    map_int( nchar ) %>%
    max()
  
  
  if( max_length_x_level > 5 & auto_rotate_xlabs ){
    p = p +
      theme( axis.text.x = element_text( angle = 90, vjust = 0.5 , hjust = 0 ) )
  }
  
  p$data_key = data_key
  p$alluvial_type = 'long'
  p$alluvial_params = params
  
  return(p)
}

software_name = function(data_long, df){
  s <- "The software name is commonly included by publishers in both \\emph{Direct Citation} ("
  data_long %>%
    group_by(software_citation_type, source) %>%
    mutate(rel=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, rel) %>%
    summarize(n=n()) %>%
    mutate(rel=n/rel) -> df_tmp
  s <- paste0(
    s,
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Direct')$rel) * 100, 1),
    "\\%) and \\emph{Manual} citations (",
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\%), with only some information represented in a structured manner with ",
    round((filter(df_tmp, source == 'JATS' & value == 'S' & software_citation_type == 'Direct')$rel) * 100, 1), 
    "\\% of \\emph{Direct Citations} and ",
    round((filter(df_tmp, source == 'JATS' & value == 'S' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\% of \\emph{Manuals}. 
"
  )
  
  data_long %>%
    filter(source == 'JATS' & value %in% c('US', 'S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(value == 'NA') %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Crossref and Semantic Scholar only loose information on software names in rare cases with ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO')$rel) * 100, 1), 
    "\\% and ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\% for \\emph{Direct} and ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'CRO')$rel) * 100, 1), 
    "0\\% and ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1), 
    "\\% for \\emph{Manual}. 
"
  )
  
  data_long %>%
    filter(source == 'JATS' & value %in% c('NA')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(value %in% c('S', 'US')) %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total) -> df_tmp
  
  s <- paste0(
    s,
    "In turn, information is added by Semantic Scholar in ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\% of \\emph{Direct} citations. 
"
  )
  
  # Overall structured
  data_long %>%
    filter(source %in% c("JATS", "SEM", "CRO")) %>%
    filter(!value %in% c('M')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Semantic Scholar manages to increase the ratio of structured information, for both \\emph{Direct} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM', value == 'S')$rel) * 100, 1), 
    "\\%) and \\emph{Manual} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM', value == 'S')$rel) * 100, 1),
    "\\%) citations, with structured samples outweighing unstructured samples for \\emph{Manuals}, while Crossref directly reflects publisher structure, when information is not lost. 
"
  )
  
  # Loosing structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value %in% c('US')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total)  -> df_tmp_lost
  
  # Adding structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('US')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value == 'S') %>% 
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n())  %>%
    mutate(rel = n / n_total) -> df_tmp_added

  s <- paste0(
    s,
    "Notably, Semantic Scholar does not retain structure for all \\emph{Direct} references, but instead looses structure for ",
    round((filter(df_tmp_lost, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1),
    "\\%, and adds structure for ",
    round((filter(df_tmp_added, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\% of references. 
Regarding \\emph{Manuals}, Semantic Scholar never looses structure, but adds it in ",
    round((filter(df_tmp_added, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1),  
    "\\% of cases.
"
  )
  
  # Overall ratio of error
  data_long %>%
    filter(source %in% c("SEM_ERR", "CRO_ERR")) %>%
    filter(!value %in% c('M', 'NA')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp

  s <- paste0(
    s, 
    "All information on software names contained in Crossref is correct, while Semantic Scholar introduces a small amount of errors in both \\emph{Direct} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\%) and \\emph{Manual} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\%) references. 
For \\emph{Manuals} all errors result from unstructured names, in \\emph{Direct} they result from both structured and unstructured names. 
"
  )
  
  # reason for errors..
  df %>%
    filter(software_citation_type %in% c('Direct', 'Manual')) %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    select(source, software_citation_type, starts_with('Name')) %>%
    select(-ends_with('unstructured')) %>% 
    filter(Name) %>%
    mutate(error = `Name<>incomplete_content` | `Name<>wrong_content` | `Name<>wrong_place`) %>%
    filter(error) %>%
    group_by(source, software_citation_type) %>%
    mutate(n_total = n()) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n_in = sum(`Name<>incomplete_content`), n_wrong = sum(`Name<>wrong_content`), n_place = sum(`Name<>wrong_place`)) %>%
    mutate(n_in_rel = n_in / n_total, n_wrong_rel = n_wrong / n_total, n_place_rel = n_place / n_total) -> df_tmp
  
  s <- paste0(
    s, 
    "In \\emph{Manual} references all errors are due to misrepresentation of software name as other information, while in \\emph{Direct} citations ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_place_rel) * 100, 1),
    "\\% of errors are due to misrepresentation and ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_wrong_rel) * 100, 1),
    "\\% of errors contain the wrong information." 
  )
  
  return(s)
}

software_creator = function(data_long, df){
  s <- "The software creator is commonly included by publishers in both \\emph{Direct Citation} ("
  data_long %>%
    group_by(software_citation_type, source) %>%
    mutate(rel=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, rel) %>%
    summarize(n=n()) %>%
    mutate(rel=n/rel) -> df_tmp
  s <- paste0(
    s,
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Direct')$rel) * 100, 1),
    "\\%) and \\emph{Manual} citations (",
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\%). 
It is structured in a majority of \\emph{Manuals} (",
    round((filter(df_tmp, source == 'JATS' & value == 'S' & software_citation_type == 'Manual')$rel) * 100, 1), 
    "\\%) but less often in \\emph{Direct Citations} (",
    round((filter(df_tmp, source == 'JATS' & value == 'S' & software_citation_type == 'Direct')$rel) * 100, 1),
    "\\%). 
"
  )
  
  data_long %>%
    filter(source == 'JATS' & value %in% c('US', 'S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(value == 'NA') %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Crossref and Semantic Scholar both loose information on software creator in a notable amount of cases for \\emph{Direct} (", # here
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO')$rel) * 100, 1), 
    "\\% and ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\%) and \\emph{Manual} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'CRO')$rel) * 100, 1), 
    "\\% and ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1),
    "\\%).
"
  )
  
  data_long %>%
    filter(source == 'JATS' & value %in% c('NA')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(value %in% c('S', 'US')) %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total) -> df_tmp
  
  # Nothing to report here .. as not info is gained
  
  # Overall structured
  data_long %>%
    filter(source %in% c("JATS", "SEM", "CRO")) %>%
    filter(!value %in% c('M')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Semantic Scholar manages to increase the ratio of structured information slightly for \\emph{Direct} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM', value == 'S')$rel) * 100, 1), 
    "\\%) and strongly for \\emph{Manual} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM', value == 'S')$rel) * 100, 1),
    "\\%) citations, with structured samples clearly outweighing unstructured samples for \\emph{Manuals}.
Same as for software name, Crossref mostly reflects publisher structure for creators, when information is not lost. 
"
  )
  
  # Loosing structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value %in% c('US')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total)  -> df_tmp_lost
  
  # Adding structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('US')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value == 'S') %>% 
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n())  %>%
    mutate(rel = n / n_total) -> df_tmp_added
  
  s <- paste0(
    s,
    "Again, Semantic Scholar does not retain structure for all \\emph{Direct} references, but instead looses structure for ",
    round((filter(df_tmp_lost, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1),
    "\\%, and adds structure for ",
    round((filter(df_tmp_added, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\% of references. 
Regarding \\emph{Manuals}, Semantic Scholar also looses structure in ",
    round((filter(df_tmp_lost, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1), 
    "\\% but adds it in ",
    round((filter(df_tmp_added, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1),  
    "\\% of references.
"
  )
  
  # Overall ratio of error
  data_long %>%
    filter(source %in% c("SEM_ERR", "CRO_ERR")) %>%
    filter(!value %in% c('M', 'NA')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp
  
  s <- paste0(
    s, 
    "Semantic Scholar introduces a notable amount of errors in both \\emph{Direct} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\%) and \\emph{Manual} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\%) references, with most \\emph{Direct} errors resulting from structured samples, and a balanced distribution for \\emph{Manuals}. ",
"Crossref also introduces a notable amount of errors in both \\emph{Direct} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO_ERR', value == 'E')$rel) * 100, 1),
    "\\%) and \\emph{Manual} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'CRO_ERR', value == 'E')$rel) * 100, 1),
    "\\%) references, where all result from structured samples.
"
  )
  
  # reason for errors..
  df %>%
    filter(software_citation_type %in% c('Direct', 'Manual')) %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    select(source, software_citation_type, starts_with('Creator')) %>%
    select(-ends_with('unstructured')) %>% 
    filter(Creator) %>%
    mutate(error = `Creator<>incomplete_content` | `Creator<>wrong_content` | `Creator<>wrong_place`) %>%
    filter(error) %>%
    group_by(source, software_citation_type) %>%
    mutate(n_total = n()) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n_in = sum(`Creator<>incomplete_content`), n_wrong = sum(`Creator<>wrong_content`), n_place = sum(`Creator<>wrong_place`)) %>%
    mutate(n_in_rel = n_in / n_total, n_wrong_rel = n_wrong / n_total, n_place_rel = n_place / n_total) -> df_tmp
  
  s <- paste0(
    s, 
    "In Crossref almost all of these errors (\\emph{Direct} ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO')$n_in_rel) * 100, 1),
    "\\%, \\emph{Manual} ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'CRO')$n_in_rel) * 100, 1),
    "\\%) are due to systematically incomplete entries when multipe authors are stated, as only the first one is included. 
In Semantic Scholar they are more equally distributed for \\emph{Direct Citations} between wrong information (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_wrong_rel) * 100, 1),
    "\\%, incomplete entries ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_in_rel) * 100, 1),
    "\\%, and misrepresentation ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_place_rel) * 100, 1),
    "\\% and misrepresentation ", 
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$n_place_rel) * 100, 1), 
    "\\% and imcomplete entries ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$n_in_rel) * 100, 1),
    "\\% for \\emp{Manuals}."
  )
  
  return(s)
}

software_identifier = function(data_long, df){
  s <- "A software identifier is included in almost half of references in both \\emph{Direct Citation} ("
  data_long %>%
    group_by(software_citation_type, source) %>%
    mutate(rel=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, rel) %>%
    summarize(n=n()) %>%
    mutate(rel=n/rel) -> df_tmp
  s <- paste0(
    s,
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Direct')$rel) * 100, 1),
    "\\%) and \\emph{Manual} citations (",
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\%), always in a structured manner.
"
  )
  
  data_long %>%
    filter(source == 'JATS' & value %in% c('US', 'S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(value == 'NA') %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Semantic Scholar looses information of identifier in a high amount of cases for \\emph{Direct Citations} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\%) and always looses it for \\emph{Manuals}.
Crossref looses information in fewer cases with ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO')$rel) * 100, 1), 
    "\\% for \\emph{Direct} and ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'CRO')$rel) * 100, 1), 
    "\\% for \\emph{Manual}. 
"
  )
  
  # Loosing structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value %in% c('US')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total)  -> df_tmp_lost
  
  s <- paste0(
    s,
    "Further, Semantic Scholar looses structure for ",
    round((filter(df_tmp_lost, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1),
    "\\% of \\emph{Direct Citations}, while Crossref looses structure for ",
    round((filter(df_tmp_lost, software_citation_type == 'Direct' & source == 'CRO')$rel) * 100, 1), 
    "\\% of \\emph{Direct} and ",
    round((filter(df_tmp_lost, software_citation_type == 'Manual' & source == 'CRO')$rel) * 100, 1),  
    "\\% of \\emph{Manuals}.
"
  )
  
  # Overall ratio of error
  data_long %>%
    filter(source %in% c("SEM_ERR", "CRO_ERR")) %>%
    filter(!value %in% c('M', 'NA')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp
  
  s <- paste0(
    s, 
    "Errors are only present in rare cases concerning Semantic Scholar and \\emph{Direct Citations} within structured information, affecting ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\% of covered references. 
"
  )
  
  # reason for errors..
  df %>%
    filter(software_citation_type %in% c('Direct', 'Manual')) %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    select(source, software_citation_type, starts_with('URL')) %>%
    select(-ends_with('unstructured')) %>% 
    filter(URL) %>%
    mutate(error = `URL<>incomplete_content` | `URL<>wrong_content` | `URL<>wrong_place`) %>%
    filter(error) %>%
    group_by(source, software_citation_type) %>%
    mutate(n_total = n()) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n_in = sum(`URL<>incomplete_content`), n_wrong = sum(`URL<>wrong_content`), n_place = sum(`URL<>wrong_place`)) %>%
    mutate(n_in_rel = n_in / n_total, n_wrong_rel = n_wrong / n_total, n_place_rel = n_place / n_total) -> df_tmp
  
  # There are no errors in IDs and Archives..
  
  s <- paste0(
    s, 
    "The errors are due to misrepresentation ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_place_rel) * 100, 1),
    "\\% and wrong information ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_wrong_rel) * 100, 1),
    "\\%." 
  )
  
  return(s)
}

software_version = function(data_long, df){
  s <- "The software version is commonly included by publishers in \\emph{Direct Citations} ("
  data_long %>%
    group_by(software_citation_type, source) %>%
    mutate(rel=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, rel) %>%
    summarize(n=n()) %>%
    mutate(rel=n/rel) -> df_tmp
  s <- paste0(
    s,
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Direct')$rel) * 100, 1),
    "\\%), but less frequently in \\emph{Manual} citations (",
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\%).
For both citation types, versions are rarely represented in a structured manner with ",
    round((filter(df_tmp, source == 'JATS' & value == 'S' & software_citation_type == 'Direct')$rel) * 100, 1), 
    "\\% in \\emph{Direct Citations} and ",
    round((filter(df_tmp, source == 'JATS' & value == 'S' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\% in \\emph{Manuals}. 
"
  )
  
  data_long %>%
    filter(source == 'JATS' & value %in% c('US', 'S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(value == 'NA') %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Crossref rarely looses information on software names in ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO')$rel) * 100, 1), 
    "\\% of \\emph{Direct} and ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'CRO')$rel) * 100, 1), 
    "\\% of \\emph{Manuals}. 
Semantic Scholar, on the other hand, looses version information in a considerable amount of \\emph{Direct Citations} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\%), but never in \\emph{Manuals}.
"
  )
  
  # Loosing structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value %in% c('US')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total)  -> df_tmp_lost
  
  # Adding structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('US')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value == 'S') %>% 
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n())  %>%
    mutate(rel = n / n_total) -> df_tmp_added
  
  s <- paste0(
    s,
    "Crossref does not loose structure information when samples are represented, but adds structure for ",
    round((filter(df_tmp_added, software_citation_type == 'Direct' & source == 'CRO')$rel) * 100, 1),
    "\\% \\emph{Direct Citations}.
Semantic Scholar looses structure for ",
    round((filter(df_tmp_lost, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1),
    "\\% \\emph{Direct Citations} and ",
    round((filter(df_tmp_lost, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1),
    "\\% \\emph{Manuals}, and adds structure for ",
    round((filter(df_tmp_added, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1),
    "\\% of \\emph{Direct Citations}. 
"
    )
  
  # Overall ratio of error
  data_long %>%
    filter(source %in% c("SEM_ERR", "CRO_ERR")) %>%
    filter(!value %in% c('M', 'NA')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp
  
  s <- paste0(
    s, 
    "No errors are present in Crossref for \\emph{Manuals} and only few for \\emph{Direct Citations} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO_ERR', value == 'E')$rel) * 100, 1),
    "\\%, all due to misrepresentation of the version as other information. 
For Semantic Scholar a notable amount of errors is present in \\emph{Direct Citations} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\% and \\emph{Manuals} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\%). 
"
  )
  
  # reason for errors..
  df %>%
    filter(software_citation_type %in% c('Direct', 'Manual')) %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    select(source, software_citation_type, starts_with('Version')) %>%
    select(-ends_with('unstructured')) %>% 
    filter(Version) %>%
    mutate(error = `Version<>incomplete_content` | `Version<>wrong_content` | `Version<>wrong_place`) %>%
    filter(error) %>%
    group_by(source, software_citation_type) %>%
    mutate(n_total = n()) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n_in = sum(`Version<>incomplete_content`), n_wrong = sum(`Version<>wrong_content`), n_place = sum(`Version<>wrong_place`)) %>%
    mutate(n_in_rel = n_in / n_total, n_wrong_rel = n_wrong / n_total, n_place_rel = n_place / n_total) -> df_tmp
  
  s <- paste0(
    s, 
    "The errors in Semantic Scholar for \\emph{Direct Citation} are mainly due to wrong information (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_wrong_rel) * 100, 1),
    "\\%=, followed by incomplete information (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_in_rel) * 100, 1),
    "\\%), and misrepresentation (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_place_rel) * 100, 1),
    "\\%), while for \\emph{Manual} they are due to incomplete information (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$n_in_rel) * 100, 1),
    "\\%) and misrepresentation (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$n_place_rel) * 100, 1),
    "\\%)."
  )
  
  return(s)
}

software_year = function(data_long, df){
  s <- "The publication date is commonly included by publishers in both \\emph{Direct Citation} ("
  data_long %>%
    group_by(software_citation_type, source) %>%
    mutate(rel=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, rel) %>%
    summarize(n=n()) %>%
    mutate(rel=n/rel) -> df_tmp
  s <- paste0(
    s,
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Direct')$rel) * 100, 1),
    "\\%) and \\emph{Manual} citations (",
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\%). 
It is often represented in a structured manner for \\emph{Manuals} (", 
    round((filter(df_tmp, source == 'JATS' & value == 'S' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\%) but less frequently for \\emph{Direct Citations} (",
    round((filter(df_tmp, source == 'JATS' & value == 'S' & software_citation_type == 'Direct')$rel) * 100, 1), 
    "\\%). 
"
  )
  
  data_long %>%
    filter(source == 'JATS' & value %in% c('US', 'S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(value == 'NA') %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Crossref and Semantic Scholar only loose information on publication date in few references with ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO')$rel) * 100, 1), 
    "\\% and ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\% for \\emph{Direct} and ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'CRO')$rel) * 100, 1), 
    "0\\% and ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1), 
    "\\% for \\emph{Manual}. 
"
  )
  
  data_long %>%
    filter(source == 'JATS' & value %in% c('NA')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(value %in% c('S', 'US')) %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total) -> df_tmp
  
  s <- paste0(
    s,
    "In turn, information is added by Semantic Scholar in ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\% of \\emph{Direct} citations. 
"
  )
  
  # Overall structured
  data_long %>%
    filter(source %in% c("JATS", "SEM", "CRO")) %>%
    filter(!value %in% c('M')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Semantic Scholar manages to strongly increase the ratio of structured information, for both \\emph{Direct} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM', value == 'S')$rel) * 100, 1), 
    "\\%) and \\emph{Manual} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM', value == 'S')$rel) * 100, 1),
    "\\%) citations, while Crossref mainly reflects the structure of the publisher. 
"
  )
  
  # Loosing structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value %in% c('US')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total)  -> df_tmp_lost
  
  # Adding structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('US')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value == 'S') %>% 
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n())  %>%
    mutate(rel = n / n_total) -> df_tmp_added
  
  s <- paste0(
    s,
    "Notably, Semantic Scholar retains structure in all cases except for ",
    round((filter(df_tmp_lost, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1),
    "\\% \\emph{Direct Citations}, and adds structure for ",
    round((filter(df_tmp_added, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\% of \\emph{Direct Citations} and ",
    round((filter(df_tmp_added, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1), 
    "\\% of \\emph{Manuals}. 
Crossref adds structure in ", 
    round((filter(df_tmp_added, software_citation_type == 'Direct' & source == 'CRO')$rel) * 100, 1), 
    "\\% of \\emph{Direct Citations}.
"
  )
  
  # Overall ratio of error
  data_long %>%
    filter(source %in% c("SEM_ERR", "CRO_ERR")) %>%
    filter(!value %in% c('M', 'NA')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp
  
  s <- paste0(
    s, 
    "High numbers of errors are present in Semantic Scholar for \\emph{Direct Citations} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\%) and \\emph{Manuals} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\%). 
For Crossref, few errors are present in \\emph{Manuals} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'CRO_ERR', value == 'E')$rel) * 100, 1),
    "\\%).
"
  )
  
  # reason for errors..
  # No errors are present in exact publication dates
  
  df %>%
    filter(software_citation_type %in% c('Direct', 'Manual')) %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    select(source, software_citation_type, starts_with('Date_of_access')) %>%
    select(-ends_with('unstructured')) %>% 
    filter(Date_of_access) %>%
    mutate(error = `Date_of_access<>incomplete_content` | `Date_of_access<>wrong_content` | `Date_of_access<>wrong_place`) %>%
    filter(error) %>%
    group_by(source, software_citation_type) %>%
    mutate(n_total = n()) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n_in = sum(`Date_of_access<>incomplete_content`), n_wrong = sum(`Date_of_access<>wrong_content`), n_place = sum(`Date_of_access<>wrong_place`))  -> df_tmp_1
  # 1 error in SEM direct: incomplete
  
  df %>%
    filter(software_citation_type %in% c('Direct', 'Manual')) %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    select(source, software_citation_type, starts_with('PubDate_year')) %>%
    select(-ends_with('unstructured')) %>% 
    filter(PubDate_year) %>%
    mutate(error = `PubDate_year<>incomplete_content` | `PubDate_year<>wrong_content` | `PubDate_year<>wrong_place`) %>%
    filter(error) %>%
    group_by(source, software_citation_type) %>%
    mutate(n_total = n()) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n_in = sum(`PubDate_year<>incomplete_content`), n_wrong = sum(`PubDate_year<>wrong_content`), n_place = sum(`PubDate_year<>wrong_place`))  -> df_tmp
  
  df_tmp[2,3] <- df_tmp[2,3] + 1
  df_tmp[2,4] <- df_tmp[2,4] + 1
  df_tmp %<>%
    mutate(n_in_rel = n_in / n_total, n_wrong_rel = n_wrong / n_total, n_place_rel = n_place / n_total)
  
  s <- paste0(
    s, 
    "The errors in Crossref are all due to incomplete information, while the errors in Semantic Scholar for \\emph{Direct Citation} are distributed between wrong information ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_wrong_rel) * 100, 1),
    "\\% incomplete information ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_in_rel) * 100, 1),
    "\\% and misrepresentation ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$n_place_rel) * 100, 1),
    "\\%, and between wrong information ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$n_wrong_rel) * 100, 1),
    "\\% and incomplete information ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$n_in_rel) * 100, 1),
    "\\% for \\emph{Manuals}."
  )
  
  return(s)
}

software_description = function(data_long){
  s <- "A software description is included by publishers in about half of references for \\emph{Direct Citation} ("
  data_long %>%
    group_by(software_citation_type, source) %>%
    mutate(rel=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, rel) %>%
    summarize(n=n()) %>%
    mutate(rel=n/rel) -> df_tmp
  s <- paste0(
    s,
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Direct')$rel) * 100, 1),
    "\\%) and \\emph{Manual} citations (",
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\%), with only some information represented in a structured manner with ",
    round((filter(df_tmp, source == 'JATS' & value == 'S' & software_citation_type == 'Direct')$rel) * 100, 1), 
    "\\% of \\emph{Direct Citations} and ",
    round((filter(df_tmp, source == 'JATS' & value == 'S' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\% of \\emph{Manuals}. 
"
  )
  
  data_long %>%
    filter(source == 'JATS' & value %in% c('US', 'S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(value == 'NA') %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Crossref and Semantic Scholar only loose information on software names in rare cases with ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO')$rel) * 100, 1), 
    "\\% and ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\% for \\emph{Direct} and ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'CRO')$rel) * 100, 1), 
    "0\\% and ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1), 
    "\\% for \\emph{Manual}. 
"
  )
  
  
  # Overall structured
  data_long %>%
    filter(source %in% c("JATS", "SEM", "CRO")) %>%
    filter(!value %in% c('M')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Semantic Scholar manages to increase the ratio of structured information, for both \\emph{Direct} (",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM', value == 'S')$rel) * 100, 1), 
    "\\%) and \\emph{Manual} (",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM', value == 'S')$rel) * 100, 1),
    "\\%) citations, while Crossref directly reflects publisher structure, when information is not lost. 
"
  )
  
  # Loosing structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value %in% c('US')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total)  -> df_tmp_lost
  
  # Adding structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('US')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value == 'S') %>% 
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n())  %>%
    mutate(rel = n / n_total) -> df_tmp_added
  
  s <- paste0(
    s,
    "Semantic Scholar adds structure for ",
    round((filter(df_tmp_added, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\% of references. 
Regarding \\emph{Manuals}, Semantic Scholar adds it in ",
    round((filter(df_tmp_added, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1),  
    "\\% of cases.
"
  )
  
  # Overall ratio of error
  data_long %>%
    filter(source %in% c("SEM_ERR", "CRO_ERR")) %>%
    filter(!value %in% c('M', 'NA')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp
  
  s <- paste0(
    s, 
    "Errors are rare for description an only appear in \\emph{Direct Citation} with ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\% in Semantic Scholar ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO_ERR', value == 'E')$rel) * 100, 1),
    "\\% in Crossref."
  )
  
  return(s)
}

software_type_of_citatin = function(data_long){
  s <- "The type of citation not commonly included in both \\emph{Direct Citation} ("
  data_long %>%
    group_by(software_citation_type, source) %>%
    mutate(rel=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, rel) %>%
    summarize(n=n()) %>%
    mutate(rel=n/rel) -> df_tmp
  s <- paste0(
    s,
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Direct')$rel) * 100, 1),
    "\\%) and \\emph{Manual} citations (",
    round((1 - filter(df_tmp, source == 'JATS' & value == 'NA' & software_citation_type == 'Manual')$rel) * 100, 1),
    "\\%), and never represented in a structured manner. 
"
  )
  
  data_long %>%
    filter(source == 'JATS' & value %in% c('US', 'S')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(value == 'NA') %>%
    filter(source %in% c('SEM', 'CRO')) %>%
    inner_join(df_i, by=c('id')) %>%
    group_by(source, software_citation_type, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel = n / n_total) -> df_tmp
  
  s <- paste0(
    s,
    "Information is in some cases lost for \\emph{Direct Citations} (Semantic Scholar ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\%, Crossref ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO')$rel) * 100, 1), 
    "\\%) and \\emph{Manual} (Semantic Scholar ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'SEM')$rel) * 100, 1), 
    "\\% , Crossref ",
    round((filter(df_tmp, software_citation_type == 'Manual' & source == 'CRO')$rel) * 100, 1), 
    "\\%).. 
"
  )
  
  # Adding structure
  data_long %>%
    filter(source == 'JATS' & value %in% c('US')) %>%
    select(id, value) -> df_i
  data_long %>%
    group_by(source, software_citation_type) %>%
    filter(value != 'M') %>%
    mutate(n_total = n()) %>%
    filter(source %in% c('CRO', 'SEM') & value == 'S') %>% 
    inner_join(df_i, by=c('id')) %>%
    group_by(software_citation_type, source, n_total) %>%
    summarize(n=n())  %>%
    mutate(rel = n / n_total) -> df_tmp_added
  
  s <- paste0(
    s,
    "Structure is mainly represented as by the publisher for both database with Semantic Scholar adding some structure to \\emph{Direct Citations} (", 
    round((filter(df_tmp_added, software_citation_type == 'Direct' & source == 'SEM')$rel) * 100, 1), 
    "\\%). 
"
  )
  
  # Overall ratio of error
  data_long %>%
    filter(source %in% c("SEM_ERR", "CRO_ERR")) %>%
    filter(!value %in% c('M', 'NA')) %>%
    group_by(software_citation_type, source) %>%
    mutate(n_total=n()) %>%
    ungroup() %>%
    group_by(software_citation_type, source, value, n_total) %>%
    summarize(n=n()) %>%
    mutate(rel=n/n_total) -> df_tmp
  
  s <- paste0(
    s, 
    "Errors are rare for type of citation an only appear in \\emph{Direct Citation} with ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'SEM_ERR', value == 'E')$rel) * 100, 1),
    "\\% in Semantic Scholar ",
    round((filter(df_tmp, software_citation_type == 'Direct' & source == 'CRO_ERR', value == 'E')$rel) * 100, 1),
    "\\% in Crossref."
  )
  
  return(s)
}