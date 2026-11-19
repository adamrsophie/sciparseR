# JATS XML backend. Semantic structure is taken from the markup; no page
# numbers or coordinates are invented. Every element keeps its XPath.

xtext <- function(node, xpath = NULL) {
  if (!is.null(xpath)) node <- xml2::xml_find_first(node, xpath)
  if (inherits(node, "xml_missing") || length(node) == 0L) return(NA_character_)
  t <- normalize_text(xml2::xml_text(node))
  if (!nzchar(t)) NA_character_ else t
}

# Text of a node with one representation of each formula and the meaning
# of inline markup kept: paragraphs, list items and line breaks inside the
# node are separated by a space (two cell paragraphs "10" and "20" never
# become "1020"); superscripts and subscripts that are not citation or
# footnote links become Unicode superscript or subscript characters, or
# "^(...)" and "_(...)" when no such character exists ("10<sup>3</sup>" is
# "10\u00b3", not "103"); MathML is written with its structure ("(1)/(2)",
# "x^(2)"); TeX is used only when no MathML is present. Table footnote
# markers are removed when requested.
node_text <- function(node, drop_fn_markers = FALSE) {
  normalize_text(paste(serialize_items(node, drop_fn_markers)$text, collapse = ""))
}

# These queries name no namespace prefix, so they need no namespace map;
# without one xml2 does not rebuild the map of the whole document per call.
no_ns <- stats::setNames(character(), character())

text_nodes <- function(node, drop_fn_markers = FALSE) {
  q <- ".//text()[not(ancestor::tex-math[parent::*/*[local-name()='math']]) and not(ancestor::*[local-name()='math'])"
  if (drop_fn_markers) q <- paste0(q, " and not(ancestor::xref[@ref-type='table-fn' or @ref-type='fn'])")
  xml2::xml_find_all(node, paste0(q, "] | .//*[local-name()='math'][not(ancestor::*[local-name()='math'])]"), ns = no_ns)
}

sup_map <- c("0" = "\u2070", "1" = "\u00b9", "2" = "\u00b2", "3" = "\u00b3", "4" = "\u2074", "5" = "\u2075",
             "6" = "\u2076", "7" = "\u2077", "8" = "\u2078", "9" = "\u2079", "+" = "\u207a", "-" = "\u207b",
             "\u2212" = "\u207b", "=" = "\u207c", "(" = "\u207d", ")" = "\u207e", "n" = "\u207f", "i" = "\u2071")
sub_map <- c("0" = "\u2080", "1" = "\u2081", "2" = "\u2082", "3" = "\u2083", "4" = "\u2084", "5" = "\u2085",
             "6" = "\u2086", "7" = "\u2087", "8" = "\u2088", "9" = "\u2089", "+" = "\u208a", "-" = "\u208b",
             "\u2212" = "\u208b", "=" = "\u208c", "(" = "\u208d", ")" = "\u208e")

script_text <- function(x, map, mark) {
  t <- trimws(x)
  if (!nzchar(t)) return(x)
  ch <- strsplit(t, "")[[1]]
  if (all(ch %in% names(map))) return(paste(map[ch], collapse = ""))
  paste0(mark, "(", t, ")")
}

mathml_text <- function(node) {
  nm <- xml2::xml_name(node)
  kids <- xml2::xml_children(node)
  part <- function(k) {
    t <- mathml_text(k)
    if (length(xml2::xml_children(k)) || nchar(t) > 1L) paste0("(", t, ")") else t
  }
  if (nm %in% c("mi", "mn", "mo", "mtext", "ms")) return(normalize_text(xml2::xml_text(node)))
  if (nm == "annotation" || nm == "annotation-xml") return("")
  if (nm == "semantics" && length(kids)) return(mathml_text(kids[[1]]))
  if (nm == "mfrac" && length(kids) == 2L) return(paste0(part(kids[[1]]), "/", part(kids[[2]])))
  if (nm == "msup" && length(kids) == 2L) return(paste0(mathml_text(kids[[1]]), "^", part(kids[[2]])))
  if (nm == "msub" && length(kids) == 2L) return(paste0(mathml_text(kids[[1]]), "_", part(kids[[2]])))
  if (nm == "msubsup" && length(kids) == 3L) {
    return(paste0(mathml_text(kids[[1]]), "_", part(kids[[2]]), "^", part(kids[[3]])))
  }
  if (nm == "msqrt") return(paste0("sqrt(", paste(vapply(kids, mathml_text, ""), collapse = ""), ")"))
  if (nm == "mroot" && length(kids) == 2L) return(paste0("root(", mathml_text(kids[[2]]), ", ", mathml_text(kids[[1]]), ")"))
  if (!length(kids)) return(normalize_text(xml2::xml_text(node)))
  paste(vapply(kids, mathml_text, ""), collapse = "")
}

# The pieces of a node's text in document order, one per text node or
# top-level MathML element, each already serialized and with a leading
# space where a block boundary falls before it.
serialize_items <- function(node, drop_fn_markers = FALSE) {
  it <- text_nodes(node, drop_fn_markers)
  if (!length(it)) return(list(items = it, text = character()))
  is_math <- xml2::xml_type(it) == "element"
  txt <- xml2::xml_text(it)
  # ancestors looked up once for the whole node set, not per text node
  linked <- !vapply(xml2::xml_find_first(it, "ancestor::xref", ns = no_ns), inherits, logical(1), "xml_missing")
  sc <- xml2::xml_find_first(it, "ancestor::*[self::sup or self::sub][1]", ns = no_ns)
  sc_name <- vapply(sc, function(z) if (inherits(z, "xml_missing")) NA_character_ else xml2::xml_name(z), "")
  for (k in seq_along(it)) {
    if (is_math[k]) {
      txt[k] <- paste0(" ", mathml_text(it[[k]]), " ")
    } else if (!linked[k] && !is.na(sc_name[k])) {
      txt[k] <- if (sc_name[k] == "sup") script_text(txt[k], sup_map, "^") else script_text(txt[k], sub_map, "_")
    }
  }
  # block boundaries: a different paragraph, list item, title or label, or a
  # line break between two pieces
  blk <- xml2::xml_find_first(it, "ancestor-or-self::*[self::p or self::list-item or self::title or self::label or self::def or self::term][1]", ns = no_ns)
  bp <- vapply(seq_along(blk), function(k) if (inherits(blk[[k]], "xml_missing")) "" else xml2::xml_path(blk[[k]]), "")
  br <- xml2::xml_find_num(it, "count(preceding::break)", ns = no_ns)
  if (length(it) > 1L) {
    edge <- c(FALSE, bp[-1] != bp[-length(bp)] | br[-1] != br[-length(br)])
    txt[edge] <- paste0(" ", txt[edge])
  }
  list(items = it, text = txt)
}

jats_root <- function(x) {
  if (xml2::xml_name(x) == "article") return(x)
  a <- xml2::xml_find_first(x, ".//*[local-name()='article']")
  if (inherits(a, "xml_missing")) abort("No <article> element found; not a JATS document.")
  a
}

parse_jats <- function(path, cfg, log) {
  raw <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ex <- expand_entities(raw)
  # unchanged files are read from disk so their declared encoding is used
  txt <- if (identical(ex$text, raw)) path else ex$text
  undefined <- ex$unknown
  x <- withCallingHandlers(
    tryCatch(xml2::read_xml(txt, options = c("NOBLANKS", "NONET")),
             error = function(e) abort("Cannot read XML: ", conditionMessage(e))),
    warning = function(w) {
      undefined <<- c(undefined, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  # an entity the parser cannot resolve disappears from the text, which can
  # drop a minus sign: say so in the document warnings
  for (u in unique(undefined)) log$add("parse", "xml_entity", paste0("XML entity not resolved (not a known named entity or a simple internal declaration) and replaced by U+FFFD: ", trimws(u)))
  x <- xml2::xml_ns_strip(x)
  art <- jats_root(x)
  meta <- jats_metadata(art)
  blocks <- list()
  sections <- list()
  tables <- list()
  cells <- list()
  figures <- list()
  equations <- list()
  captions <- list()
  footnotes <- list()
  supp <- list()
  ord <- 0L
  add_block <- function(type, text, node, section_id, parent_id = NA_character_,
                        method = "jats_markup") {
    ord <<- ord + 1L
    bid <- make_id("b", NA, ord)
    blocks[[length(blocks) + 1L]] <<- list(
      block_id = bid, page = NA_integer_, type = type, text_raw = xml2::xml_text(node),
      text = text, section_id = section_id, reading_order = ord, parent_id = parent_id,
      method = method, status = "extracted", xpath = xml2::xml_path(node))
    bid
  }
  # abstract as a section
  sec_counter <- 0L
  new_section <- function(node, title, level, parent, label) {
    sec_counter <<- sec_counter + 1L
    sid <- sprintf("s-%03d", sec_counter)
    sections[[length(sections) + 1L]] <<- list(
      section_id = sid, parent_id = parent, level = level, number = heading_number(title %||% ""),
      title = title, label = label, reading_order = ord + 1L, method = "jats_markup",
      xpath = xml2::xml_path(node))
    sid
  }
  abs <- xml2::xml_find_first(art, "./front/article-meta/abstract")
  if (!inherits(abs, "xml_missing")) {
    sid <- new_section(abs, "Abstract", 1L, NA_character_, "abstract")
    hb <- add_block("heading", "Abstract", abs, sid)
    sections[[length(sections)]]$block_id <- hb
    # a structured abstract keeps its subsections ("Methods", "Results")
    # as child sections of the abstract
    abs_sec <- function(node, parent, level) {
      for (k in xml2::xml_children(node)) {
        nm <- xml2::xml_name(k)
        if (nm == "sec") {
          ttl <- xtext(k, "./title")
          sub_id <- new_section(k, ttl %|NA|% NA_character_, level, parent, "abstract")
          if (!is.na(ttl)) sections[[length(sections)]]$block_id <<- add_block("heading", ttl, xml2::xml_find_first(k, "./title"), sub_id)
          abs_sec(k, sub_id, level + 1L)
        } else if (nm == "p") {
          add_block("paragraph", node_text(k), k, parent)
        }
      }
    }
    abs_sec(abs, sid, 2L)
    # an abstract with bare text and no paragraph keeps that text
    if (!length(xml2::xml_find_all(abs, ".//p")) && nzchar(node_text(abs))) add_block("paragraph", node_text(abs), abs, sid)
  }
  body <- xml2::xml_find_first(art, "./body")
  q <- paste(".//sec", ".//p[not(ancestor::caption) and not(ancestor::table-wrap-foot) and not(ancestor::fn) and not(ancestor::table) and not(ancestor::supplementary-material)]",
             ".//table-wrap", ".//fig", ".//disp-formula", ".//supplementary-material", sep = " | ")
  nodes <- if (inherits(body, "xml_missing")) list() else as.list(xml2::xml_find_all(body, q))
  # appendices are sections of the back matter; tables and figures in a
  # floats-group belong to no section
  app_q <- paste("./back//app", "./back//app//sec", "./back//app//p[not(ancestor::caption) and not(ancestor::table-wrap-foot) and not(ancestor::fn) and not(ancestor::table) and not(ancestor::supplementary-material)]",
                 "./back//app//table-wrap", "./back//app//fig", "./back//app//disp-formula", sep = " | ")
  nodes <- c(nodes, as.list(xml2::xml_find_all(art, app_q)),
             as.list(xml2::xml_find_all(art, "./floats-group/table-wrap | ./floats-group/fig | ./floats-group/disp-formula")))
  # supplementary material listed in the back matter
  back_supp <- xml2::xml_find_all(art, "./back//supplementary-material")
  if (length(back_supp)) nodes <- c(as.list(nodes), as.list(back_supp))
  sec_ids <- character()
  sec_paths <- character()
  owner_section <- function(node) {
    anc <- xml2::xml_find_all(node, "ancestor::*[self::sec or self::app]")
    if (!length(anc)) return(NA_character_)
    p <- xml2::xml_path(anc[[length(anc)]])
    sec_ids[match(p, sec_paths)]
  }
  for (node in nodes) {
    nm <- xml2::xml_name(node)
    if (nm %in% c("sec", "app")) {
      title <- xtext(node, "./title")
      level <- length(xml2::xml_find_all(node, "ancestor::*[self::sec or self::app]")) + 1L
      parent <- owner_section(node)
      lab <- section_label(title %|NA|% "")
      if (nm == "app" && is.na(lab)) lab <- "appendix"
      st <- xml2::xml_attr(node, "sec-type")
      if (is.na(lab) && !is.na(st)) lab <- section_label(gsub("[|-]", " ", st))
      if (is.na(lab) && !is.na(parent)) {
        pi <- which(vapply(sections, function(z) z$section_id, character(1)) == parent)
        lab <- sections[[pi]]$label
      }
      sid <- new_section(node, title %||% NA_character_, level, parent, lab)
      sec_ids <- c(sec_ids, sid)
      sec_paths <- c(sec_paths, xml2::xml_path(node))
      if (!is.na(title)) {
        hb <- add_block("heading", title, xml2::xml_find_first(node, "./title"), sid)
        sections[[length(sections)]]$block_id <- hb
      }
    } else if (nm == "p") {
      add_block("paragraph", node_text(node), node, owner_section(node))
    } else if (nm == "table-wrap") {
      r <- jats_table(node, length(tables) + 1L, log)
      sid <- owner_section(node)
      cid <- sprintf("cap-%03d", length(captions) + 1L)
      ctext <- xtext(node, "./caption")
      lab <- xtext(node, "./label")
      if (!is.na(ctext) || !is.na(lab)) {
        bid <- add_block("caption", normalize_text(paste(c(lab, ctext)[!is.na(c(lab, ctext))], collapse = ". ")),
                         xml2::xml_find_first(node, "./caption") %||% node, sid)
        captions[[length(captions) + 1L]] <- list(caption_id = cid, target_type = "table",
                                                  target_id = r$table$table_id, label = lab,
                                                  text = ctext, block_id = bid,
                                                  xpath = xml2::xml_path(node))
        r$table$caption_id <- cid
      }
      add_block("table", NA_character_, node, sid, r$table$table_id)
      tables[[length(tables) + 1L]] <- r$table
      cells[[length(cells) + 1L]] <- r$cells
      footnotes[[length(footnotes) + 1L]] <- r$footnotes
    } else if (nm == "fig") {
      fid <- make_id("f", NA, length(figures) + 1L)
      sid <- owner_section(node)
      lab <- xtext(node, "./label")
      ctext <- xtext(node, "./caption")
      cid <- NA_character_
      if (!is.na(lab) || !is.na(ctext)) {
        cid <- sprintf("cap-%03d", length(captions) + 1L)
        bid <- add_block("caption", normalize_text(paste(c(lab, ctext)[!is.na(c(lab, ctext))], collapse = ". ")), node, sid)
        captions[[length(captions) + 1L]] <- list(caption_id = cid, target_type = "figure", target_id = fid,
                                                  label = lab, text = ctext, block_id = bid,
                                                  xpath = xml2::xml_path(node))
      }
      g <- xml2::xml_find_first(node, ".//graphic")
      figures[[length(figures) + 1L]] <- list(
        figure_id = fid, label = lab, caption_id = cid, method = "jats_markup",
        status = "extracted",
        graphic = if (inherits(g, "xml_missing")) NA_character_ else xml_href(g),
        xpath = xml2::xml_path(node))
      add_block("figure", NA_character_, node, sid, fid)
    } else if (nm == "disp-formula") {
      eid <- make_id("eq", NA, length(equations) + 1L)
      tex <- xtext(node, ".//tex-math")
      mm <- xml2::xml_find_first(node, ".//*[local-name()='math']")
      mtxt <- if (inherits(mm, "xml_missing")) NA_character_ else normalize_text(mathml_text(mm))
      inline_p <- length(xml2::xml_find_all(node, "ancestor::p")) > 0L
      bid <- if (inline_p) NA_character_ else add_block("equation", if (!is.na(mtxt)) mtxt else tex, node, owner_section(node))
      equations[[length(equations) + 1L]] <- list(
        equation_id = eid, label = xtext(node, "./label"), text = if (!is.na(mtxt)) mtxt else tex,
        latex = tex, mathml = if (inherits(mm, "xml_missing")) NA_character_ else as.character(mm),
        latex_status = if (!is.na(tex)) "source_markup" else "not_available",
        block_id = bid, method = "jats_markup", xpath = xml2::xml_path(node))
    } else if (nm == "supplementary-material") {
      mt <- xml2::xml_attr(node, "mimetype")
      ms <- xml2::xml_attr(node, "mime-subtype")
      supp[[length(supp) + 1L]] <- list(
        supp_id = sprintf("sm-%03d", length(supp) + 1L), label = xtext(node, "./label"),
        caption = xtext(node, "./caption"), href = xml_href(node),
        mimetype = if (!is.na(mt) && !is.na(ms)) paste(mt, ms, sep = "/") else mt,
        xpath = xml2::xml_path(node))
    }
  }
  blocks <- bind_entity(blocks, "blocks")
  sections <- bind_entity(sections, "sections")
  references <- jats_references(art)
  citations <- jats_citations(art, blocks, references)
  list(
    pages = empty_entity("pages"), spans = empty_entity("spans"), lines = empty_entity("lines"),
    blocks = blocks, sections = sections,
    tables = bind_entity(tables, "tables"),
    cells = bind_entity(cells, "cells"),
    figures = bind_entity(figures, "figures"),
    equations = bind_entity(equations, "equations"),
    captions = bind_entity(captions, "captions"),
    footnotes = bind_entity(c(footnotes, list(jats_document_footnotes(art))), "footnotes"),
    references = references, citations = citations,
    supplementary = bind_entity(supp, "supplementary"),
    metadata = meta)
}

xml_href <- function(node) {
  h <- xml2::xml_attr(node, "xlink:href")
  if (is.na(h)) h <- xml2::xml_attr(node, "href")
  if (is.na(h)) {
    a <- xml2::xml_attrs(node)
    k <- grep("href$", names(a))
    if (length(k)) h <- unname(a[k[1]])
  }
  h
}

jats_metadata <- function(art) {
  am <- xml2::xml_find_first(art, "./front/article-meta")
  id_of <- function(type) {
    v <- xtext(am, sprintf("./article-id[@pub-id-type='%s']", type))
    v
  }
  pmcid <- id_of("pmcid")
  if (is.na(pmcid)) {
    v <- id_of("pmc")
    if (!is.na(v)) pmcid <- if (grepl("^PMC", v)) v else paste0("PMC", v)
  }
  authors <- vapply(xml2::xml_find_all(am, "./contrib-group/contrib[@contrib-type='author']"), function(cn) {
    nm <- xml2::xml_find_first(cn, "./name")
    if (inherits(nm, "xml_missing")) return(xtext(cn, ".//collab|.//string-name"))
    normalize_text(paste(xtext(nm, "./given-names") %|NA|% "", xtext(nm, "./surname") %|NA|% ""))
  }, character(1))
  yr <- xtext(am, "./pub-date[@pub-type='epub' or @date-type='pub']/year")
  if (is.na(yr)) yr <- xtext(am, "./pub-date/year")
  lic <- xml2::xml_find_first(am, ".//permissions/license")
  ab <- xml2::xml_find_first(am, "./abstract")
  src <- "jats"
  list(title = xtext(am, "./title-group/article-title"),
       authors = authors[!is.na(authors)],
       doi = id_of("doi"), pmid = id_of("pmid"), pmcid = pmcid,
       journal = xtext(art, "./front/journal-meta//journal-title"),
       year = suppressWarnings(as.integer(yr)),
       abstract = if (inherits(ab, "xml_missing")) NA_character_ else node_text(ab),
       keywords = vapply(xml2::xml_find_all(am, ".//kwd-group/kwd"), function(k) normalize_text(xml2::xml_text(k)), character(1)),
       license = if (inherits(lic, "xml_missing")) NA_character_ else (xml_href(lic) %|NA|% xtext(lic)),
       license_text = if (inherits(lic, "xml_missing")) NA_character_ else xtext(lic),
       n_pages = NA_integer_,
       sources = list(title = src, authors = src, doi = src, pmid = src, pmcid = src,
                      journal = src, year = src, abstract = src, keywords = src, license = src))
}

# Grid reconstruction from HTML style table markup with rowspan/colspan.
jats_table <- function(node, k, log) {
  tid <- make_id("t", NA, k)
  rows <- xml2::xml_find_all(node, ".//table//tr")
  lab <- xtext(node, "./label")
  fn_nodes <- xml2::xml_find_all(node, "./table-wrap-foot//fn | ./table-wrap-foot/p")
  fns <- bind_entity(lapply(seq_along(fn_nodes), function(i) {
    f <- fn_nodes[[i]]
    data.frame(footnote_id = sprintf("%s-fn%02d", tid, i), scope = "table", target_id = tid,
               marker = xtext(f, "./label"),
               text = normalize_text(paste(xml2::xml_text(xml2::xml_find_all(f, "./*[not(self::label)]|./text()")), collapse = " ")),
               xpath = xml2::xml_path(f), stringsAsFactors = FALSE)
  }), "footnotes")
  table <- list(table_id = tid, label = lab, n_rows = 0L, n_cols = 0L, n_header_rows = 0L,
                method = "jats_markup", status = "extracted",
                footnote_ids = collapse_ids(fns$footnote_id), xpath = xml2::xml_path(node))
  if (!length(rows)) {
    table$status <- if (length(xml2::xml_find_all(node, ".//graphic"))) "image_only" else "empty"
    log$add("jats", "table_without_markup",
            paste0("Table ", lab %||% tid, " has no row markup (", table$status, ")."), NA, tid)
    return(list(table = table, cells = empty_entity("cells"), footnotes = fns))
  }
  occ <- list()
  out <- list()
  n_header <- 0L
  has_thead <- length(xml2::xml_find_all(node, ".//thead")) > 0L
  # Without <thead>, leading rows made only of column headers (<th> without
  # scope="row") are the header. A <th scope="row"> is a row label, which
  # stays a data cell so that its row keeps its values.
  col_header <- function(cell) {
    xml2::xml_name(cell) == "th" && !xml2::xml_attr(cell, "scope") %in% c("row", "rowgroup")
  }
  lead <- TRUE
  for (r in seq_along(rows)) {
    tr <- rows[[r]]
    tcs <- xml2::xml_find_all(tr, "./th|./td", ns = no_ns)
    in_head <- if (has_thead) {
      length(xml2::xml_find_all(tr, "ancestor::thead", ns = no_ns)) > 0L
    } else {
      lead <- lead && length(tcs) > 0L && all(vapply(tcs, col_header, logical(1)))
      lead
    }
    if (in_head) n_header <- r
    col <- 1L
    for (cell in tcs) {
      while (isTRUE(occ[[paste(r, col)]])) col <- col + 1L
      rs <- suppressWarnings(as.integer(xml2::xml_attr(cell, "rowspan")))
      cs <- suppressWarnings(as.integer(xml2::xml_attr(cell, "colspan")))
      rs <- if (is.na(rs) || rs < 1L) 1L else rs
      cs <- if (is.na(cs) || cs < 1L) 1L else cs
      for (i in r:(r + rs - 1L)) for (j in col:(col + cs - 1L)) occ[[paste(i, j)]] <- TRUE
      mk <- xml2::xml_find_all(cell, ".//xref[@ref-type='table-fn' or @ref-type='fn']", ns = no_ns)
      txt <- node_text(cell, drop_fn_markers = TRUE)
      out[[length(out) + 1L]] <- list(
        cell_id = sprintf("%s-r%03dc%03d", tid, r, col), table_id = tid, row = r, col = col,
        row_span = rs, col_span = cs, is_header = in_head,
        text_raw = node_text(cell), text = txt,
        markers = collapse_ids(normalize_text(xml2::xml_text(mk))),
        missing_token = grepl(missing_token_regex, txt),
        status = if (!nzchar(txt)) "empty" else if (grepl(missing_token_regex, txt)) "missing_token" else "extracted",
        xpath = xml2::xml_path(cell))
      col <- col + cs
    }
  }
  if (!length(occ)) {
    table$status <- "empty"
    log$add("jats", "table_without_cells", paste0("Table ", lab %||% tid, " has rows but no cells."), NA, tid)
    return(list(table = table, cells = empty_entity("cells"), footnotes = fns))
  }
  cells <- bind_entity(out, "cells")
  keys <- do.call(rbind, strsplit(names(occ), " "))
  n_rows <- max(as.integer(keys[, 1]))
  n_cols <- max(as.integer(keys[, 2]))
  # positions never covered by any cell are explicit empty cells
  have <- matrix(FALSE, n_rows, n_cols)
  have[cbind(as.integer(keys[, 1]), as.integer(keys[, 2]))] <- TRUE
  miss <- which(!have, arr.ind = TRUE)
  if (nrow(miss)) {
    extra <- as_entity(list(cell_id = sprintf("%s-r%03dc%03d", tid, miss[, 1], miss[, 2]), table_id = tid,
                            row = miss[, 1], col = miss[, 2], row_span = 1L, col_span = 1L,
                            is_header = miss[, 1] <= n_header, text_raw = "", text = "",
                            missing_token = FALSE, status = "absent_in_markup"), "cells")
    cells <- rbind(cells, extra)
    cells <- cells[order(cells$row, cells$col), , drop = FALSE]
    log$add("jats", "ragged_table", "Table rows have fewer cells than the grid; absent positions recorded.", NA, tid)
  }
  table$n_rows <- n_rows
  table$n_cols <- n_cols
  table$n_header_rows <- n_header
  rownames(cells) <- NULL
  list(table = table, cells = cells, footnotes = fns)
}

jats_references <- function(art) {
  refs <- xml2::xml_find_all(art, ".//ref-list/ref")
  if (!length(refs)) return(empty_entity("references"))
  rows <- lapply(seq_along(refs), function(i) {
    r <- refs[[i]]
    # fields from an element-citation when there is one; the printed text
    # from a mixed-citation when there is one
    cit <- xml2::xml_find_first(r, "./element-citation")
    if (inherits(cit, "xml_missing")) cit <- xml2::xml_find_first(r, "./mixed-citation|./citation|./nlm-citation")
    if (inherits(cit, "xml_missing")) cit <- r
    mixed <- xml2::xml_find_first(r, "./mixed-citation")
    txt_node <- if (inherits(mixed, "xml_missing")) cit else mixed
    nm <- xml2::xml_find_all(cit, ".//person-group[@person-group-type='author']/name | .//person-group[not(@person-group-type)]/name | ./name | .//string-name")
    au <- vapply(nm, function(n) normalize_text(paste(xtext(n, "./surname") %|NA|% xtext(n), xtext(n, "./given-names") %|NA|% "")), character(1))
    pid <- function(type) {
      v <- xtext(cit, sprintf(".//pub-id[@pub-id-type='%s']", type))
      if (type == "pmcid" && is.na(v)) v <- xtext(cit, ".//pub-id[@pub-id-type='pmc']")
      v
    }
    fp <- xtext(cit, ".//fpage")
    lp <- xtext(cit, ".//lpage")
    el <- xtext(cit, ".//elocation-id")
    pm <- pid("pmcid")
    if (!is.na(pm) && !grepl("^PMC", pm)) pm <- paste0("PMC", pm)
    txt <- normalize_text(xml2::xml_text(txt_node))
    h <- parse_reference(txt)
    au <- if (length(au)) paste(au, collapse = ", ") else h$authors
    method <- if (length(nm)) "jats_markup" else "jats_markup+heuristic"
    data.frame(ref_id = xml2::xml_attr(r, "id") %||% sprintf("r-%03d", i),
               label = xtext(r, "./label"), text = txt,
               authors = au,
               title = xtext(cit, ".//article-title|.//chapter-title") %|NA|% h$title,
               journal = xtext(cit, ".//source") %|NA|% h$journal,
               year = suppressWarnings(as.integer(sub("[^0-9].*$", "", xtext(cit, ".//year") %||% NA))) %|NA|% h$year,
               volume = xtext(cit, ".//volume") %|NA|% h$volume,
               pages = (if (!is.na(fp)) paste(c(fp, lp)[!is.na(c(fp, lp))], collapse = "-") else el) %|NA|% h$pages,
               doi = pid("doi") %|NA|% h$doi, pmid = pid("pmid") %|NA|% h$pmid, pmcid = pm %|NA|% h$pmcid,
               method = method, xpath = xml2::xml_path(r), stringsAsFactors = FALSE)
  })
  out <- bind_entity(rows, "references")
  if (anyNA(out$ref_id)) out$ref_id[is.na(out$ref_id)] <- sprintf("r-%03d", which(is.na(out$ref_id)))
  out
}

jats_citations <- function(art, blocks, references) {
  xs <- xml2::xml_find_all(art, ".//body//xref[@ref-type='bibr'] | .//front//abstract//xref[@ref-type='bibr']")
  if (!length(xs)) return(empty_entity("citations"))
  out <- list()
  cursor <- list()
  for (x in xs) {
    p <- xml2::xml_find_first(x, "ancestor::p[1]")
    pp <- if (inherits(p, "xml_missing")) NA_character_ else xml2::xml_path(p)
    bi <- match(pp, blocks$xpath)
    bid <- if (is.na(bi)) NA_character_ else blocks$block_id[bi]
    mk <- normalize_text(xml2::xml_text(x))
    st <- NA_integer_
    if (!is.na(bi) && nzchar(mk)) {
      # offset from the text that precedes this xref in the paragraph, so a
      # number that also occurs earlier in the text is never picked
      si <- serialize_items(p)
      tn <- si$items
      inside <- vapply(seq_along(tn), function(k) xml2::xml_type(tn[[k]]) == "text" &&
                         length(xml2::xml_find_all(tn[[k]], "ancestor::xref")) > 0L &&
                         identical(xml2::xml_path(xml2::xml_find_first(tn[[k]], "ancestor::xref[1]")), xml2::xml_path(x)), logical(1))
      if (any(inside)) {
        k <- which(inside)[1]
        pre <- paste(c(si$text[seq_len(k - 1L)], si$text[inside]), collapse = "")
        cand <- nchar(normalize_text(pre)) - nchar(mk) + 1L
        if (cand >= 1L && identical(substr(blocks$text[bi], cand, cand + nchar(mk) - 1L), mk)) st <- cand
      }
    }
    if (is.na(st) && !is.na(bi) && nzchar(mk)) {
      from <- cursor[[bid]] %||% 1L
      sub_txt <- substring(blocks$text[bi], from)
      pos <- regexpr(mk, sub_txt, fixed = TRUE)
      if (pos > 0) {
        st <- from + pos - 1L
        cursor[[bid]] <- st + nchar(mk)
      }
    }
    rids <- strsplit(xml2::xml_attr(x, "rid") %||% "", "\\s+")[[1]]
    rids <- rids[nzchar(rids)]
    if (!length(rids)) rids <- NA_character_
    for (rid in rids) {
      out[[length(out) + 1L]] <- data.frame(
        block_id = bid, ref_id = if (!is.na(rid) && rid %in% references$ref_id) rid else NA_character_,
        marker = mk, char_start = st, char_end = if (is.na(st)) NA_integer_ else st + nchar(mk) - 1L,
        page = NA_integer_, style = "xref",
        status = if (!is.na(rid) && rid %in% references$ref_id) "linked" else "unlinked",
        xpath = xml2::xml_path(x), stringsAsFactors = FALSE)
    }
  }
  cit <- bind_entity(out, "citations")
  cit$citation_id <- sprintf("c-%04d", seq_len(nrow(cit)))
  cit
}

#' Retrieve a JATS article from PubMed Central
#'
#' Downloads the JATS XML of an article in the PMC Open Access subset.
#' Network access is opt-in: nothing is requested unless
#' `allow_network = TRUE`. The license reported by the PMC Open Access web
#' service is returned with the file; PMC content is not all licensed for
#' unrestricted reuse, and the caller is responsible for respecting the
#' license.
#'
#' The function first queries the PMC OA web service
#' (`https://www.ncbi.nlm.nih.gov/pmc/utils/oa/oa.fcgi`) to check that the
#' article is in the Open Access subset and to read its license, then
#' downloads the XML through NCBI E-utilities `efetch` (`db = pmc`).
#'
#' @param pmcid PMC identifier such as `"PMC1234567"`.
#' @param dest Destination file path.
#' @param allow_network Must be `TRUE` to allow network access.
#' @param require_open_access Refuse articles that the OA service does not
#'   list as Open Access.
#' @param email Optional contact address passed to E-utilities as NCBI
#'   requests; never filled in automatically.
#' @param api_key Optional NCBI API key.
#' @return The destination path, invisibly, with attributes `license` and
#'   `pmcid`.
#' @examples
#' \dontrun{
#' f <- sc_fetch_pmc("PMC1234567", tempfile(fileext = ".xml"), allow_network = TRUE)
#' attr(f, "license")
#' doc <- sc_parse(f)
#' }
#' @export
sc_fetch_pmc <- function(pmcid, dest, allow_network = FALSE,
                         require_open_access = TRUE, email = NULL,
                         api_key = NULL) {
  check_string(pmcid, "pmcid")
  if (!isTRUE(allow_network)) {
    abort("Network access is opt-in. Set `allow_network = TRUE` to contact NCBI.")
  }
  check_string(dest, "dest")
  if (dir.exists(dest)) abort("`dest` is a directory; give the path of the XML file to write.")
  pmcid <- toupper(pmcid)
  if (!grepl("^PMC[0-9]+$", pmcid)) abort("`pmcid` must look like 'PMC1234567'.")
  oa_url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/utils/oa/oa.fcgi?id=", pmcid)
  oa_xml <- tryCatch(paste(readLines(oa_url, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                     error = function(e) abort("PMC OA service request failed: ", conditionMessage(e)))
  oa <- parse_oa_response(oa_xml)
  if (require_open_access && !oa$open_access) {
    abort(pmcid, " is not in the PMC Open Access subset (", oa$message,
          "). Set `require_open_access = FALSE` only if you hold rights to the text.")
  }
  q <- paste0("db=pmc&id=", sub("^PMC", "", pmcid), "&tool=sciparseR")
  if (!is.null(email)) q <- paste0(q, "&email=", utils::URLencode(email, reserved = TRUE))
  if (!is.null(api_key)) q <- paste0(q, "&api_key=", utils::URLencode(api_key, reserved = TRUE))
  url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?", q)
  # download to a temporary file and keep it only if it is a JATS article
  tmp <- tempfile(fileext = ".xml")
  on.exit(unlink(tmp), add = TRUE)
  status <- tryCatch(utils::download.file(url, tmp, quiet = TRUE, mode = "wb"),
                     error = function(e) abort("E-utilities download failed: ", conditionMessage(e)))
  if (status != 0L) abort("E-utilities download failed with status ", status, ".")
  ok <- tryCatch({
    x <- xml2::xml_ns_strip(xml2::read_xml(tmp, options = c("NOBLANKS", "NONET")))
    !inherits(xml2::xml_find_first(x, "//article"), "xml_missing")
  }, error = function(e) FALSE)
  if (!ok) abort("NCBI did not return a JATS article for ", pmcid, ".")
  if (!file.copy(tmp, dest, overwrite = TRUE)) abort("Could not write ", dest, ".")
  structure(dest, license = oa$license, pmcid = pmcid, class = "character") |> invisible()
}

parse_oa_response <- function(xml_string) {
  x <- xml2::read_xml(xml_string)
  err <- xml2::xml_find_first(x, ".//error")
  if (!inherits(err, "xml_missing")) {
    return(list(open_access = FALSE, license = NA_character_,
                message = paste(xml2::xml_attr(err, "code"), xml2::xml_text(err))))
  }
  rec <- xml2::xml_find_first(x, ".//record")
  if (inherits(rec, "xml_missing")) {
    return(list(open_access = FALSE, license = NA_character_, message = "no record"))
  }
  list(open_access = TRUE, license = xml2::xml_attr(rec, "license"),
       message = "open access")
}

# Footnotes outside tables (body and back matter <fn>), scope "document".
jats_document_footnotes <- function(art) {
  fn <- xml2::xml_find_all(art, "./body//fn[not(ancestor::table-wrap)] | ./back//fn[not(ancestor::table-wrap)]")
  if (!length(fn)) return(empty_entity("footnotes"))
  bind_entity(lapply(seq_along(fn), function(i) {
    f <- fn[[i]]
    data.frame(footnote_id = sprintf("fn-%03d", i), scope = "document",
               target_id = xml2::xml_attr(f, "id"), marker = xtext(f, "./label"),
               text = normalize_text(paste(xml2::xml_text(xml2::xml_find_all(f, "./*[not(self::label)]|./text()")), collapse = " ")),
               xpath = xml2::xml_path(f), stringsAsFactors = FALSE)
  }), "footnotes")
}

# Named entities that publishers use with an external DTD (which is never
# loaded) and simple entities declared in the internal subset are replaced by
# their characters before parsing, so a "&minus;7.2" keeps its sign.
html_entities <- c(
  minus = "\u2212", plusmn = "\u00b1", times = "\u00d7", le = "\u2264", ge = "\u2265",
  lt = NA, gt = NA, amp = NA, quot = NA, apos = NA,
  ndash = "\u2013", mdash = "\u2014", nbsp = "\u00a0", thinsp = "\u2009", middot = "\u00b7",
  deg = "\u00b0", micro = "\u00b5", alpha = "\u03b1", beta = "\u03b2", gamma = "\u03b3",
  delta = "\u03b4", Delta = "\u0394", kappa = "\u03ba", lambda = "\u03bb", mu = "\u03bc",
  sigma = "\u03c3", chi = "\u03c7", tau = "\u03c4", rho = "\u03c1", ne = "\u2260",
  asymp = "\u2248", approx = "\u2248", hellip = "\u2026", lsquo = "\u2018", rsquo = "\u2019",
  ldquo = "\u201c", rdquo = "\u201d", dagger = "\u2020", Dagger = "\u2021", sect = "\u00a7",
  para = "\u00b6", infin = "\u221e", rarr = "\u2192", larr = "\u2190")

expand_entities <- function(txt) {
  ents <- html_entities[!is.na(html_entities)]
  # simple internal declarations, in double or single quotes
  decl <- regmatches(txt, gregexpr("<!ENTITY\\s+([A-Za-z][A-Za-z0-9._-]*)\\s+(\"[^\"&<%]*\"|'[^'&<%]*')\\s*>", txt, perl = TRUE))[[1]]
  if (length(decl)) {
    nm <- sub("<!ENTITY\\s+([A-Za-z][A-Za-z0-9._-]*).*", "\\1", decl, perl = TRUE)
    val <- sub("^<!ENTITY\\s+[^\\s]+\\s+[\"'](.*)[\"']\\s*>$", "\\1", decl, perl = TRUE)
    ents[nm] <- val
  }
  # CDATA sections and comments are literal text: entities are expanded
  # only between them
  prot <- gregexpr("(?s)<!\\[CDATA\\[.*?\\]\\]>|<!--.*?-->", txt, perl = TRUE)[[1]]
  if (prot[1] > 0) {
    st <- as.integer(prot)
    en <- st + attr(prot, "match.length") - 1L
    gap_st <- c(1L, en + 1L)
    gap_en <- c(st - 1L, nchar(txt))
    gaps <- substring(txt, gap_st, gap_en)
    kept <- substring(txt, st, en)
  } else {
    gaps <- txt
    kept <- character()
  }
  unk <- character()
  for (g in seq_along(gaps)) {
    for (nm in names(ents)) {
      rep <- paste(sprintf("&#%d;", utf8ToInt(ents[[nm]])), collapse = "")
      gaps[g] <- gsub(paste0("&", nm, ";"), rep, gaps[g], fixed = TRUE)
    }
    # any other named entity is unknown: mark it visibly instead of failing
    # or silently dropping it
    u <- regmatches(gaps[g], gregexpr("&(?!(?:lt|gt|amp|quot|apos);)[A-Za-z][A-Za-z0-9._-]*;", gaps[g], perl = TRUE))[[1]]
    if (length(u)) gaps[g] <- gsub("&(?!(?:lt|gt|amp|quot|apos);)[A-Za-z][A-Za-z0-9._-]*;", "&#65533;", gaps[g], perl = TRUE)
    unk <- c(unk, u)
  }
  out <- gaps[1]
  for (k in seq_along(kept)) out <- paste0(out, kept[k], gaps[k + 1L])
  list(text = out, unknown = unique(unk))
}
