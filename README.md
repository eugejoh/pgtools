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

More details to follow...

