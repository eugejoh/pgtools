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

## Typical Workflow
This section outlines a typical workflow of writing a data frame from a R session to a PostgreSQL database connection.

```
# Single data frame
data(iris)

my_conn <- connect_pg(getenv = FALSE,
   host = DBI::dbDriver("Postgres"),
   port = 5432,
   dbname = "mydb",
   user = "myusername",
   password = "mypw"
 )

my_nchar <- get_nchar(iris)

my_fields <- set_pgfields(nchar_df, conn = local_con_test)

write_pgtable(input = iris,
   field.types = my_fields,
   conn = my_conn,
   tbl_name = "iris")

```

```
Mulitple data frames
data(iris)
data(swiss)
data(mtcars)
data(cars)

my_list <- list(iris, swiss, mtcars, cars)
names(my_list) <- c("iris", "swiss", "mtcars", "cars")

my_conn <- connect_pg(getenv = FALSE,
   host = DBI::dbDriver("Postgres"),
   port = 5432,
   dbname = "mydb",
   user = "myusername",
   password = "mypw"
 )

my_nchar <- get_nchar(my_list)

my_fields <- set_pgfields(nchar_df, conn = local_con_test)

write_pgtable(input = iris,
   field.types = my_fields,
   conn = my_conn,
   tbl_name = "iris")

```