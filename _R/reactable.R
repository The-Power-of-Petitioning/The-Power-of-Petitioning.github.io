## will need to ensure you load the usual packages + reactable + htmltools.




## add tooltip (to header in colDef)
## https://glin.github.io/reactable/articles/cookbook/cookbook.html?q=tooltip#tooltips
## renders the header as an <abbr> element with a title attribute
with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
            title = tooltip, value)
}
## see also https://github.com/glin/reactable/issues/220 



## https://glin.github.io/reactable/articles/custom-rendering.html#headers  
## plus https://glin.github.io/reactable/articles/building-twitter-followers.html?q=replace#finishing-touches for replace function
## js function to replace _ in header names with space. often wouldn't need anything else.
## but only works once. not sure if there's an equiv of str_replace_all ??

headerMakeSpace <- JS("function(column, state) {
      return column.name.replace('_', ' ')
    }")




# https://glin.github.io/reactable/articles/sticky-columns-selection.html

# more exmaples... all property names should be camelCased

# style = list(
#     display = "inline-block",
#     marginRight = "0.5rem",
#     width = width,
#     height = height,
#     backgroundColor = color,
#     borderRadius = "50%"
#   ))

# Background style to visually distinguish sticky columns.  backgroundColor = "#f7f7f7", 
sticky_style <- list(borderRight = "2px solid rgba(0, 0, 0, 0.1)", fontWeight = 600) # , fontSize="110%"  bold doesn't work here... will need to add it to the col. maybe can just add inline to the <a> ? or give the a a class. bold is a bit heavy; try 500 or 600. guess what, 500 is indistingushable from normal and 600 indistinguishable from bold. :< ok make it a bit bigger instead. and it won't let you use % without fannying about with %s. ffs. will larger font-size work in sticky style? yes. try weight again in mindseye?

sticky_header_style <- list(borderRight = "2px solid rgba(0, 0, 0, 0.1)", borderTop = "1px solid rgba(0,0,0,0.1") 

group_left_style <- list(borderLeft = "1px solid rgba(0, 0, 0, 0.1)") 





## filterMethod ####

## JS functions: if already made JS() here they don't need to be wrapped in JS("") in the reactable

# Filter method that filters numeric columns by minimum value
filterMinValue <- JS("function(rows, columnId, filterValue) {
  return rows.filter(function(row) {
    return row.values[columnId] >= filterValue
  })
}")

## same thing when used directly in reactable, for comparison when making other functions...
# numerical Filter by minimum value
# filterMethod = JS("function(rows, columnId, filterValue) {
#                         return rows.filter(function(row) {
#                         return row.values[columnId] >= filterValue
#                       })
#                     }")


# Filter method that filters numeric columns by maximum value
filterMaxValue <- JS("function(rows, columnId, filterValue) {
  return rows.filter(function(row) {
    return row.values[columnId] >= filterValue
  })
}")


## filter single number >= or range (using slash separator). 
## based on https://github.com/glin/reactable/issues/318
## should work with any numerical values, including decimal places.
## but as-is, it has no min/max limits (and won't exclude NAs?) so it's perhaps slightly less ideal for year filters. (just use it?)

# -Infinity in min value includes NAs. seems 0 generally treated same as NA
# odd behaviour that 0-0 doesn't filter anything? but it would be a slightly silly thing to do...! 
# 1-1 behaves as expected. I wonder if it's anything to do with Number() function, don't think i've used that in previous version. or maybe it's just the equals part.
# make min value 1 and it excludes NAs (but also excludes 0s. is that a problem? sometimes 0 might actually be a thing, not just an error.)
## original split on " " but  "-" or "/" seems more intuitive to me

# Filter by a single number (greater than or equal to)
# 0.4 filters value greater than or equal to 0.4
# filter by a range
# 0.4-0.5 filters values between 0.4 and 0.5 (inclusive)

filterNumRange <-
  JS("function(rows, columnId, filterValue) {
        var range = filterValue.split('/');
        range[0] = Number(range[0]) || -Infinity; 
        range[1] = Number(range[1]) || Infinity;
        return rows.filter(function(row) {
          return row.values[columnId] >= range[0] &&
            row.values[columnId] <= range[1]
        })
      }")



filterNumPosRange <-
  JS("function(rows, columnId, filterValue) {
        var range = filterValue.split('/');
        range[0] = Number(range[0]) || 1; 
        range[1] = Number(range[1]) || Infinity;
        return rows.filter(function(row) {
          return row.values[columnId] >= range[0] &&
            row.values[columnId] <= range[1]
        })
      }")




## cf version you'd need to wrap in JS("")...
# // Filter method that filters numeric columns by minimum value
# function filterMinValueJS(rows, columnId, filterValue) {
#   return rows.filter(function(row) {
#     return row.values[columnId] >= filterValue
#   })
# }


## date range filtering ####

## this is *almost* a default: the range of 1000 to 1999 will work for most projects. (no idea what will happen with BCE though)
## IMPORTANT: ALWAYS USE THIS WITH A FILTERINPUT THAT INCLUDES HANDLING NULL!
## most likely TextPlaceholder input below which is designed to work with the method.
## also you'll still need to calc the min and max dates for placeholder/hints.

filterMostDateRanges <- 
  JS("function(rows, columnId, filterValue) {
  
        var range = filterValue.split('/');
          range[0] = range[0] || '1000-01-01';
          range[1] = range[1] || '1999-12-31'; 
          
        // if range1 length < 4 use 4 else use range1length.
        
         var range1length = range[1].length;
         
        // TODO rewrite if-else more concisely like wot a proper programmer would
         
         if(range1length < 4) { 
             var range1end =  4;
            } else { 
            var range1end = range1length;
          } 

          return rows.filter(function(row) {
            return row.values[columnId] >= range[0] &&
              row.values[columnId].substring(0, range1end) <= range[1]
        })
      }")

# eg of what's needed in the filterInput
# // onchange = sprintf("Reactable.setFilter('hcr-table', '%s', event.target.value || undefined)", name),



## when you already have ranges in the data, you need to split that as well!
## IMPORTANT: YOU CAN'T HAVE ANY NAs IN THE DATA [at present]
## there aren't any NAs or part dates in this data, hoorah
## it's better to use number defaults than infinity, i think.
## inclusive filter so that eg "1620-1640" will be matched by the input "1637-1639":
## start_year <= end_input && end_year >= start_input 
## cf. exclusive filter
## start_year >= start_input && end_year <= end_input

## even without NAs it will still need cell rendering in the reactable, and a filterInput if you're using datalist default.

filterYearFuzzyRange <-
  JS("function(rows, columnId, filterValue) {
        
        var range = filterValue.split('/'); 
        range[0] = Number(range[0]) || 1; 
        range[1] = Number(range[1]) || 2000;

        return rows.filter(function(row) {
          return (
            Number(row.values[columnId].split('-')[0]) <= range[1] && Number(row.values[columnId].split('-')[1]) >= range[0]  
            )
        }
      )
   }")

## example cell rendering in reactable colDef 
# simple version if all yyyy-yyyy and no NAs.
# if (substr(value, 1,4) == substr(value, 6, 9)) {substr(value, 1, 4)} else {value}

# after fixes to NAs/part ranges.
## fake_start and fake_end will change to match the data.

# cell = function(value) {
#   fake_start <- "1500-"
#   fake_end <- "-1900"
#   case_when(
#     {value}=="-" ~ "n.d.",
#     str_detect({value}, fake_start) ~  paste0("?", substr({value}, 5, 9)),
#     str_detect({value}, fake_end) ~ str_replace({value}, fake_end, "-?"),
#     substr({value}, 1, 4) == substr({value}, 6, 9) ~ substr({value}, 1, 4),
#     .default = {value}
#   )
# }







## Filter by case-sensitive exact text match. 
# if you don't want case-sensitive you'll probably need regex.
# see also https://github.com/glin/reactable/issues/318#issuecomment-2107704839
# in some examples the final line doesn't have String() - idk whether it makes a difference    
# return row.values[columnId] === filterValue; 

filterTextExact <- JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return String(row.values[columnId]) === filterValue
        })
      }")



## filterInput functions ####

## you can probably compare the internal filter functions to the external ones if you want to make adaptations...

# (internal) data list column filter for a table with the given ID
# based on https://glin.github.io/reactable/articles/custom-filtering.html#data-list-filter
# additions:
# sort values alphabetically (default seems to be order of appearance in data)
# placeholder text (default empty)
# might be possible to extend to change type? or better to have separate functions for numeric etc?

dataListFilterInput <- function(tableId, placeholder=NULL, style = "width: 100%; height: 28px;") {
  function(values, name) {
    dataListId <- sprintf("%s-%s-list", tableId, name)
    tagList(
      tags$input(
        type = "text",
        list = dataListId,
        placeholder = placeholder,
        oninput = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", tableId, name),
        "aria-label" = sprintf("Filter %s", name),
        style = style 
      ),
      tags$datalist(
        id = dataListId,
        # added sort here
        lapply(sort(unique(values)), function(value) tags$option(value = value))
      )
    )
  }
}

## eg to use data list filter as the default for all factor columns
#  defaultColDef = colDef(
#    filterInput = function(values, name) {
#      if (is.factor(values)) {
#        dataListFilterInput("cars-list")(values, name)
#      }
#    }
#  ),


# # Use data list filter for a specific column
# Manufacturer = colDef(
#   filterInput = dataListFilterInput("cars-list")
# )



# dropdown select with "All" as default.

selectFilterInput <- function(tableId, style = "width: 100%; height: 28px;") {
  function(values, name) {
    tags$select(
      # Set to undefined to clear the filter
      onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", tableId, name),
      # "All" has an empty value to clear the filter, and is the default option
      tags$option(value = "", "All"),
      # added sort here (but what if you have a value alphabetically < All?)
      lapply(sort(unique(values)), tags$option),
      "aria-label" = sprintf("Filter %s", name),
      style = style
    )
  }
}

# Manufacturer = colDef(
#   filterInput = selectFilterInput("cars-list")
# )



## fairly standard text dropdown, but includes a "placeholder" so you can easily add a hint
# and handles undefined
# could add style to args as well (as above) if you want to be able to adjust those

filterTextPlaceholderInput <-
  function(tableId, placeholder) {
    function(values, name) {
      tags$input(
        type = "text",
        placeholder = placeholder,
        # Set to undefined to clear the filter
        onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", tableId, name),
        "aria-label" = sprintf("Filter %s", name),
        style = "width: 100%; height: 28px;"
      )
    }
  }



## Custom (single) slider with label and value
## https://glin.github.io/reactable/articles/custom-filtering.html#external-range-filter

rangeFilterInput <- function(tableId, columnId, label, min, max, value = NULL, step = NULL, width = "200px") {
  value <- if (!is.null(value)) value else min
  inputId <- sprintf("filter_%s_%s", tableId, columnId)
  valueId <- sprintf("filter_%s_%s__value", tableId, columnId)
  oninput <- paste(
    sprintf("document.getElementById('%s').textContent = this.value;", valueId),
    sprintf("Reactable.setFilter('%s', '%s', this.value)", tableId, columnId)
  )
  div(
    tags$label(`for` = inputId, label),
    div(
      style = sprintf("display: flex; align-items: center; width: %s", validateCssUnit(width)),
      tags$input(
        id = inputId,
        type = "range",
        min = min, max = max,
        step = step, value = value,
        oninput = oninput,
        onchange = oninput, # For IE11 support
        style = "width: 100%;"
      ),
      span(id = valueId, style = "margin-left: 8px;", value) # number next to the slider
    )
  )
}


## eg use

# tagList(
#   rangeFilterInput(
#     "causes-range-extra",
#     "year",
#     "Filter from year",
#     floor(min(ycp_causes_table$year)),
#     ceiling(max(ycp_causes_table$year))
#   ),
#   reactable(
#     ycp_causes_table,
#     columns = list(
#       year = colDef(show=TRUE, maxWidth = 100,
#                     filterMethod = filterMinValue 
#       ), 
#     ),
#     elementId = "causes-range-extra"
#   )
# )


## external numerical filter 
## based on rangeFilter
## works but it's a bit hacky because i don't fully understand what i'm doing!

numFilterInput <- function(tableId, columnId, label, min, max, value = NULL,  width = "200px") {
  value <- if (!is.null(value)) value else min # else max for a To filter.
  inputId <- sprintf("filter_%s_%s", tableId, columnId)
  valueId <- sprintf("filter_%s_%s__value", tableId, columnId)
  oninput <- paste(
    sprintf("document.getElementById('%s').textContent = this.value;", valueId),
    sprintf("Reactable.setFilter('%s', '%s', this.value)", tableId, columnId)
  )
  div(
    tags$label(`for` = inputId, label),
    div(
      style = sprintf("display: flex; align-items: center; width: %s", validateCssUnit(width)),
      tags$input(
        id = inputId,
        type = "number",
        min = min,
        max = max,
        value = value,
        oninput = oninput,
        onchange = oninput, # For IE11 support
        style = "width: 100%;"
      ) ,
      span(id = valueId, style = "margin-left: 8px; display: none;", value) # doesn't work w/o this line. makes sense in a slider, but you don't want the number here. so what is it you need to do instead? display:none does work but i'm fairly sure not "correct" approach. FIXME!
    )
  )
}

