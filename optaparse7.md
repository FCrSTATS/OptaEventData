
OPTA f24 Data Parse (Updated)
=============================

This script allows us to import a OPTA f24 file and twist it into a manageable dataframe object. This is am updated version and all my own work.

``` r
## Enter the location of the events XML for the match you want to process 
events.xml <- "LOCATON_OF_XML_FILE_ON_YOUR_COMPUTER"
```
### Grab The Qualifiers Function

We need to define some functions that will help us grab the information from the 'qualifiers' of each event

``` r
grab.the.qualifiers <- function(xlm.2.spread) {
  
  
  Value <- ifelse(is.na(Qualifiers.List["value"]), 1, Qualifiers.List["value"])
  temp <- data.frame(Q = as.character(Value), stringsAsFactors = F)
  colnames(temp) <- Qualifiers.List["qualifier_id"]

  return(bind_rows(results.temp, temp))
 
}
```

### Pick the Maximum (non-NA) Values

Once we have grabbed each qualifier we need a way of selecting just the values for each column from the qualifiers results dataframe. Again another little helper function.

``` r
pick.out.the.maximum.values <- function(qualifier.values){
  
  max.values <- list()
  for (c in 1:NCOL(qualifier.values)) {
  col.2.test <- qualifier.values[,c]
  max.val <- col.2.test[!is.na(col.2.test)][1]
  max.values <- append(unlist(max.values), max.val)
  }
  results.Q <- t(as.data.frame(max.values))
  colnames(results.Q) <- colnames(qualifier.values)
  return(results.Q)
  }
```

### The Main Unpacking Function

We need to define the main function that will take one event and return all of the event information as well as the qualifier information.

``` r
convert.event.node.row <- function(xml.2.spread){
  
    ## convert the info in the event node header into a dataframe 
    results <- as.data.frame(t(as.data.frame((xml.2.spread$attrs))))
    rownames(results) <- NULL

    ## find the number of qualifiers for this event 
    no.of.qualifiers <- lengths(xml.2.spread$value)
    
    if(no.of.qualifiers > 0){
    ## create a list of qualifiers 
    Qualifier.Unpacked.Step1 <- data.frame(stringsAsFactors = F)
  
    ## loop through each qualifer and pull out the info then bind it to the results .. above 
    for (Q in 1:no.of.qualifiers) {
    Qualifier.unpacked <- unlist(xml.2.spread$value[1][[1]][Q])
    Value <- ifelse(is.na(Qualifier.unpacked["value"]), 1, Qualifier.unpacked["value"])
    temp <- data.frame(Q = as.character(Value), stringsAsFactors = F)
    colnames(temp) <- Qualifier.unpacked["qualifier_id"]
    Qualifier.Unpacked.Step1 <- bind_rows(Qualifier.Unpacked.Step1, temp)
    }
    
    ## keep the maximum values in the dataframe (the only none NA values) return as a 
    ## dataframe for use 
    Qualifier.unpacked.df <- pick.out.the.maximum.values(Qualifier.Unpacked.Step1)
    rownames(Qualifier.unpacked.df) <- NULL 
    
    ## bind the results together 
    results <- cbind(results, Qualifier.unpacked.df)}
    
    ## return the dataframe 
    return(results)
} # end of function 
```

### Load the Libraries

We will be utilising a few libraries which we need to load.

``` r
library(dplyr); library(purrr); library(xml2)
```

### Parse the XML

We need to load the events xml file from it's location

``` r
events.xml <- paste0("Database/Events/Raw Data/" , Match.Select, "_f24.xml")
pbpParse <- read_xml(events.xml, encoding = "", as_html = TRUE, options = "NOERROR")
```

### Spilt the Loaded XML Tree

We want to grab all of the nodes that are named "event" and store their attributes and their qualifiers into lists within a dataframe. We use our friend map\_df() from the purrr package to do this.

``` r
all.event.nodes <- pbpParse %>% 
    xml_find_all('//event') %>% 
     map_df(~list(attrs = list(xml_attrs(.x)), 
                 value = list(map(xml_children(.x), xml_attrs))))
```

### Put Every Event into Our Main Function

These few lines seem simple but will pass the list of events, take one row at a time, pass it through our function and then bind all of the results into one dataframe names full.unpack! Powerful stuff :)

``` r
    full.unpack <- all.event.nodes %>% 
    split(1:nrow(.)) %>% 
    purrr::map(convert.event.node.row) %>% 
    dplyr::bind_rows()
```

Job done! you are now left with a dataframe of event information for all events! 

## Complete Code 
``` r
## Enter the location of the events XML for the match you want to process 
events.xml <- "LOCATON_OF_XML_FILE_ON_YOUR_COMPUTER"

## Grab The Qualifiers Function

grab.the.qualifiers <- function(xlm.2.spread) {
  
  Value <- ifelse(is.na(Qualifiers.List["value"]), 1, Qualifiers.List["value"])
  temp <- data.frame(Q = as.character(Value), stringsAsFactors = F)
  colnames(temp) <- Qualifiers.List["qualifier_id"]

  return(bind_rows(results.temp, temp))
 
}

## Pick the Maximum (non-NA) Values

pick.out.the.maximum.values <- function(qualifier.values){
  
  max.values <- list()
  for (c in 1:NCOL(qualifier.values)) {
  col.2.test <- qualifier.values[,c]
  max.val <- col.2.test[!is.na(col.2.test)][1]
  max.values <- append(unlist(max.values), max.val)
  }
  results.Q <- t(as.data.frame(max.values))
  colnames(results.Q) <- colnames(qualifier.values)
  return(results.Q)
}

## The Main Unpacking Function


convert.event.node.row <- function(xml.2.spread){
  
    ## convert the info in the event node header into a dataframe 
    results <- as.data.frame(t(as.data.frame((xml.2.spread$attrs))))
    rownames(results) <- NULL

    ## find the number of qualifiers for this event 
    no.of.qualifiers <- lengths(xml.2.spread$value)
    
    if(no.of.qualifiers > 0){
    ## create a list of qualifiers 
    Qualifier.Unpacked.Step1 <- data.frame(stringsAsFactors = F)
  
    ## loop through each qualifer and pull out the info then bind it to the results .. above 
    for (Q in 1:no.of.qualifiers) {
    Qualifier.unpacked <- unlist(xml.2.spread$value[1][[1]][Q])
    Value <- ifelse(is.na(Qualifier.unpacked["value"]), 1, Qualifier.unpacked["value"])
    temp <- data.frame(Q = as.character(Value), stringsAsFactors = F)
    colnames(temp) <- Qualifier.unpacked["qualifier_id"]
    Qualifier.Unpacked.Step1 <- bind_rows(Qualifier.Unpacked.Step1, temp)
    }
    
    ## keep the maximum values in the dataframe (the only none NA values) return as a 
    ## dataframe for use 
    Qualifier.unpacked.df <- pick.out.the.maximum.values(Qualifier.Unpacked.Step1)
    rownames(Qualifier.unpacked.df) <- NULL 
    
    results <- cbind(results, Qualifier.unpacked.df)}
    
    return(results)
} # end of function 



## Load the libaries 
library(dplyr); library(purrr); library(xml2)

## Parse the XML
pbpParse <- read_xml(events.xml, encoding = "", as_html = TRUE, options = "NOERROR")

## Spilt the Loaded XML Tree
all.event.nodes <- pbpParse %>% 
    xml_find_all('//event') %>% 
     map_df(~list(attrs = list(xml_attrs(.x)), 
                 value = list(map(xml_children(.x), xml_attrs))))

## Put Every Event into Our Main Function

    full.unpack <- all.event.nodes %>% 
    split(1:nrow(.)) %>% 
    purrr::map(convert.event.node.row) %>% 
    dplyr::bind_rows()
    
    


```
