# pgtools
Standardized workflow for writing tables to PostgreSQL. This package contains tools to provide a consistent workflow for
writing `data.frames` existing in an `R` session to a PostgreSQL database connection. The tools are built around the [`DBI`](https://github.com/r-dbi)
and [`RPostgres`](https://github.com/r-dbi/RPostgres) packages.

**What this package provides?**
 - Convenient connection to PostgreSQL with credentials
 - An automated assignment of PostgreSQL field type variable lengths based on a basic assessment of element lengths
 - Schema specification for database writing
 - Simple genration of a `SQL` `CREATE TABLE` statements
 - Vectorized to accept a `list` of `data.frames` to write to PostgreSQL

This is created for primary use among the Data/Programming team at CGHR to standardize and optimize the data storage and management workflow.

## Installation
```
library(devtools)
devtools::install_github("eugejoh/pgtools")
```

## Database Connection
[Best practices](https://db.rstudio.com/best-practices/managing-credentials/) with storing and using credentials, steps here should be followed to access the `.REnviron` file. By default `pgtools::connect_pg()` searches your `.REnviron` file to retrieve the appropriate information.  

The `.REnviron` file should have the following variables:  
```
db_ip='my ip here'

db_user='my username here'

db_pw='my password here'

db_name='my database name here'
```

## Typical Workflow
This section outlines a typical workflow of writing a data frame from a R session to a PostgreSQL database connection.

<b>Example for single data frame using the `iris` dataset:</b>
```
# Single data frame
data(iris)

# Connect to database
my_conn <- connect_pg(getenv = FALSE,
   host = DBI::dbDriver("Postgres"),
   port = 5432,
   dbname = "mydb",
   user = "myusername",
   password = "mypw"
 )

# Element lengths
my_nchar <- get_nchar(iris)

# Postgres Field Types
my_fields <- set_pgfields(nchar_df, conn = local_con_test)

# Write to Postgres
write_pgtable(input = iris,
   field.types = my_fields,
   conn = my_conn,
   tbl_name = "iris")

```

<b>Example for a list of data frames:</b>
```
# Mulitple data frames
data(iris)
data(swiss)
data(mtcars)
data(cars)

# Named list of data frames
my_list <- list(iris, swiss, mtcars, cars)
names(my_list) <- c("iris", "swiss", "mtcars", "cars")

# Connect to database
my_conn <- connect_pg(getenv = FALSE,
   host = DBI::dbDriver("Postgres"),
   port = 5432,
   dbname = "mydb",
   user = "myusername",
   password = "mypw"
 )

# Element lengths
my_nchar <- get_nchar(my_list)

# Postgres Field Types
my_fields <- set_pgfields(nchar_df, conn = my_conn)

# Write to Postgres
write_pgtable(input = iris,
   field.types = my_fields,
   conn = my_conn)

```

<b>Example obtaining the `SQL` statement for `CREATE TABLE` with added primary key.  </b>
```
data(iris)

# Connect to database
my_conn <- connect_pg(getenv = FALSE,
   host = DBI::dbDriver("Postgres"),
   port = 5432,
   dbname = "mydb",
   user = "myusername",
   password = "mypw"
 )

# Add ID for Primary Key
iris$id <- seq_along(1:nrow(iris))

# Element lengths
my_nchar <- get_nchar(iris)

# Postgres Field Types
my_fields <- set_pgfields(nchar_df, conn = my_conn)

get_sql_create(my_pg_fields, pkey = "id", tbl_name = "iris")
```
