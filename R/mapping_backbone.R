mapping_backbone <- function(fun, spec_path) {
  analyze_functions_packages <- function(file) {
    funs_all <- NCmisc::list.functions.in.file(file)
    funs_no_pkg <- funs_all$`character(0)`

    pkg_functions <- list()
    code <- parse(file)

    detect_packages_calls <- function(fun_name, code_expr) {
      pkgs <- character(0)

      traverse <- function(x) {
        if (is.call(x)) {
          # Chamadas com :: ou :::
          if (as.character(x[[1]])[1] %in% c("::", ":::") && length(x) >= 3) {
            fn <- as.character(x[[3]])[1]
            if (!is.null(fn) && fn == fun_name) {
              pkg <- as.character(x[[2]])[1]
              if (!is.null(pkg)) pkgs <<- c(pkgs, pkg)
            }
          }
          for (i in seq_along(x)) traverse(x[[i]])
        } else if (is.expression(x)) {
          for (i in seq_along(x)) traverse(x[[i]])
        } else if (is.pairlist(x)) {
          traverse(as.list(x))
        }
      }

      traverse(code_expr)
      unique(pkgs)
    }

    for (f in funs_no_pkg) {
      pkgs <- detect_packages_calls(f, code)
      if (length(pkgs) > 0) {
        for (p in pkgs) {
          pkg_functions[[paste0("package:", p)]] <- unique(c(pkg_functions[[paste0("package:", p)]], f))
        }
      }
    }

    funs_no_pkg_final <- setdiff(funs_no_pkg, unlist(pkg_functions))

    result <- list("character(0)" = funs_no_pkg_final)

    for (pkg in names(pkg_functions)) {
      result[[pkg]] <- pkg_functions[[pkg]]
    }

    combine_lists <- function(x, y) {
      all_names <- union(names(x), names(y))
      result <- vector("list", length(all_names))
      names(result) <- all_names

      for (nm in all_names) {
        vals_x <- if (nm %in% names(x)) x[[nm]] else NULL
        vals_y <- if (nm %in% names(y)) y[[nm]] else NULL
        result[[nm]] <- unique(c(vals_x, vals_y))
      }

      result
    }

    result <- combine_lists(result, funs_all[setdiff(names(funs_all), "character(0)")])

    return(result)
  }

  format_pkg_functions <- function(lst) {
    names(lst)[names(lst) == "character(0)"] <- "User Defined Functions Or Package Not Identified"

    s <- sapply(names(lst), function(pkg) {
      funs <- sort(unique(lst[[pkg]]))
      paste0("{", pkg, "} ", paste(funs, collapse = ", "))
    })

    paste(s, collapse = "\n")
  }

  ##

  tmp <- tempfile(fileext = ".R")
  writeLines(deparse(fun), tmp)

  res <- analyze_functions_packages(tmp)
  res_text <- format_pkg_functions(res)

  ##

  all_funs <- unique(unlist(res))
  all_pkg <- unique(names(res))
  all_pkg <- setdiff(all_pkg, c("package:base", "character(0)"))
  all_pkg <- sub("^package:", "", all_pkg)
  all_names <- all.names(body(fun), functions = TRUE, unique = TRUE)
  used_vars <- setdiff(all_names, all_funs)
  used_vars <- setdiff(used_vars, all_pkg)
  reserved <- c("if", "else", "for", "while", "repeat", "function", "break", "next", "return", "switch", "try", "tryCatch", "...")
  used_vars <- setdiff(used_vars, reserved)
  used_vars <- used_vars[grepl("^[A-Za-z\\.][A-Za-z0-9_\\.]*$", used_vars)]
  used_vars <- sort(unique(used_vars))
  used_vars <- sort(used_vars)

  spec_var <- unique(readxl::read_xlsx(spec_path, sheet = 'Variables')$Variable)

  used_vars <- sort(used_vars[used_vars %in% spec_var])
  used_vars <- paste(used_vars, collapse = ", ")

  all_pkg_text <- paste0(R.version$version.string, '; Used Packages: ', paste(all_pkg, collapse = ", "))

  res_text <- paste0(res_text, '\n\n{Spec Used Variables:}\n', used_vars)

 result <- tibble::tibble(
   Variables = used_vars,
   `Programming Context` = all_pkg_text,
   `Programming Code` = res_text
 )

 return(result)
}
