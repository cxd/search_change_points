require(dplyr)
require(RevoScaleR)
require(reshape2)

# initialise the sql context
init_sql <- function(connStr, shareDir) {
  
  sqlcon <- RxInSqlServer(
    connectionString=connStr,
    shareDir=shareDir,
    wait=TRUE,
    consoleOUtput=FALSE
  )
  sqlcon
}

# load the exitpoint data for the supplied application.
load_exitpoint_data <- function(connStr, sqlcon, var_column="Transfer_Percentage", application="REI") {
  
  query <- paste0("select
  convert(VARCHAR(10),
                  convert(Date,
                  convert(varchar(10), DATE) --converting Dataype int to varchar then 
                  ,10)                        --varchar to date and then
                  ,111) as Date,                          --date to 'YYYY/MM/DD' Format 
                  isnull(EXITREF.Category,'NoMappingFound') as Category,
                  sum(Transfer_Count) as Transfer_Count ,
                  sum(Transfer_Count)*100.0/  SUM(Count) as Transfer_Percentage,
                  sum(Hangup_Count) as Hangup_Count,
                  sum(Hangup_Count)*100.0/  SUM(Count) as Hangup_Percentage
                  from [DHS_Daily_Customer_Report].[dbo].[Daily_DHS_All_Exit_Points_hdfs] EXITHDFS
                  left  join  [DHS_Daily_Customer_Report].[dbo].[Daily_DHS_ExitPoint_Ref] EXITREF
                  on EXITREF.ExitPointName = EXITHDFS.ExitPointName
where 
Date between CONVERT(char(8),dateadd(d,-90,GETDATE()), 112) and CONVERT(char(8), GETDATE(), 112)
and upper(EXITHDFS.ExitPointName) like '",application, "%'
group by EXITREF.Category,Date
ORDER BY [DATE] ASC, Category")
  
  
  data <- RxSqlServerData(
    sqlQuery = query,
    connectionString = connStr,
    rowsPerRead=10000)
  
  
  rxSetComputeContext(sqlcon)
  
  # note in general we would not pull all available data back in memory
  # instead a subset of that data would be read hence
  # a date range query would be more effective.
  df <- rxImport(data)
  
  # convert to wide format.
  df2 <- dcast(df, Date ~ Category, value.var=var_column)
  
  df2$application <- rep(application, nrow(df2))
  
  df2
}

ensure_columns <- function(data, columns, default_val) {
  cols <- colnames(data)
  idx <- which(columns %in% cols)
  others <- columns[-idx]
  
  if (length(others) > 0) {
    sapply(others, function(col) {
      data[,col] <- rep(default_val, nrow(data))
      data <<- data
    })
  }
  data
}

targetCols <- c("BusinessRule", "CustomerRequest", "NoMappingFound", "RecognitionFailure", "TechnicalFailure",
                "Success", "Unknown")

# load exit point data for the set of applications

load_exitpoints_for_applications <- function(connStr, sqlCon, var_column="Transfer_Percentage", applications=c("REI", "FAO", "YAS", "EMS", "RET")) {
  data <- data.frame()
  rows <- sapply(applications, function(app) {
    df <- load_exitpoint_data(connStr, sqlcon, var_column, app)
    df <- ensure_columns(df, targetCols, 0)
    str(df)
    if (nrow(data) == 0) {
      data <<- df
    } else {
      data <<- rbind(data, df)
    }
  })
  data
}