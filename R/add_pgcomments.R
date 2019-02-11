add_pgcomments <- function(
  conn = NULL,
  schema = "public",
  tbl_name = NULL,
  tbl.comments = NULL,
  field.comments = NULL,
  override = FALSE,
  ...) {

  # check if schema exists in db
  if (nrow(DBI::dbGetQuery(conn,
                       DBI::sqlInterpolate(conn,
                                      "SELECT nspname
                                 FROM pg_catalog.pg_namespace
                                 WHERE nspname = ?schema_name",
                                      schema_name = schema))) < 1)
    stop("schema does not exist")

  # check if table exists in db
  if (!DBI::dbExistsTable(conn, DBI::Id(schema = schema, table = tbl_name)))
    stop("table does not exist")

  # check if comments exist (for both table and fields)

  if (!override) {
    if (!is.na(DBI::sqlInterpolate(conn,
    "SELECT relname, obj_description(oid)
      FROM pg_class
      WHERE relname = ?table;"
    table = tbl_name)$obj_description)) {   # if !override and comments exists = stop
      stop("comments exists and override = FALSE")

    } else {

      #write comments

      if (!missing(tbl.comments)) {

        if (!is.character(tbl.comments)) stop("tbl.comments must be character")

        tbl_cl <- DBI::sqlInterpolate(conn = conn,
                                      sql = "COMMENT ON TABLE ?schema.?table IS ?comment",
                                      schema = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(schema)),
                                      table = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(tbl_name)),
                                      comment = tbl.comments)

        qry <- DBI::dbSendQuery(
          conn = conn,
          statement = tbl_cl)
        qry
        DBI::dbClearResult(qry)
        message(paste0("COMMENT ON TABLE completed"))

      }

      if (!missing(field.comments)) {

        if (length(field.comments != dplyr::pull(
          DBI::dbGetQuery(conn,
                          DBI::sqlInterpolate(
                            conn,
                            sql = "SELECT COUNT(*) AS n
                                    FROM INFORMATION_SCHEMA.COLUMNS
                                    WHERE table_catalog = 'sl_phc'
                                    AND table_schema = ?schema_name
                                    AND table_name = ?table;",
                            schema_name = schema,
                            table = tbl_name))))
        ) stop("number of 'field.comments' and fields do not match")

        field_cl <- purrr::map_chr(names(field.comments), function(field) {
          DBI::sqlInterpolate(conn = conn,
                              sql = "COMMENT ON COLUMN ?schema.?table.?fields IS ?comment",
                              schema = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(schema)),
                              table = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(tbl_name)),
                              fields = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(field)),
                              comment = field.comments[[field]])
        })

      }


    }
  } else if (override) {   # if override TRUE, just write comments
    #write comments, THIS IS REPEATED ABOVE, make function?

    if (!missing(tbl.comments)) {

      if (!is.character(tbl.comments)) stop("tbl.comments must be character")

      tbl_cl <- DBI::sqlInterpolate(conn = conn,
                                    sql = "COMMENT ON TABLE ?schema.?table IS ?comment",
                                    schema = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(schema)),
                                    table = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(tbl_name)),
                                    comment = tbl.comments)

      qry <- DBI::dbSendQuery(
        conn = conn,
        statement = tbl_cl)
      qry
      DBI::dbClearResult(qry)
      message(paste0("COMMENT ON TABLE completed"))

    }

    if (!missing(field.comments)) {

      if (length(field.comments != dplyr::pull(
        DBI::dbGetQuery(conn,
                        DBI::sqlInterpolate(
                          conn,
                          sql = "SELECT COUNT(*) AS n
                                  FROM INFORMATION_SCHEMA.COLUMNS
                                  WHERE table_catalog = 'sl_phc'
                                  AND table_schema = ?schema_name
                          AND table_name = ?table;",
                          schema_name = schema,
                          table = tbl_name))))
      ) stop("number of 'field.comments' and fields do not match")

      field_cl <- purrr::map_chr(names(field.comments), function(field) {
        DBI::sqlInterpolate(conn = conn,
                            sql = "COMMENT ON COLUMN ?schema.?table.?fields IS ?comment",
                            schema = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(schema)),
                            table = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(tbl_name)),
                            fields = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(field)),
                            comment = field.comments[[field]])
      })

    }


  }




}