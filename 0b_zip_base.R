## =====================================================================
## 0b_zip_base.R  --  ZIP writing in base R only
##
## Source AFTER 0_xlsx_helpers.R. It redefines xlsx_write() to use a
## pure base-R zip writer instead of zip::zipr / utils::zip / PowerShell.
## 0_xlsx_helpers.R itself is not modified, so scripts 14 and 16 are
## unaffected.
##
## WHY THIS EXISTS
##   The three zip methods in the helper all need something the machine
##   may not have: the `zip` package (blocked by IT), zip.exe on the PATH
##   (absent on a stock Windows install -- "the system cannot find the
##   file specified"), or PowerShell resolvable by name.
##
## HOW IT WORKS WITHOUT ANY OF THEM
##   A ZIP entry needs the deflate-compressed bytes and a CRC32 of the
##   uncompressed bytes. Both come free from a gzip stream: 10-byte
##   header, raw deflate payload, then a trailer holding the CRC32 and
##   the original size. Strip header and trailer and you have exactly
##   what a ZIP entry stores, computed in C rather than in an R loop.
##
##   NOTE, AND THIS COST A ROUND OF DEBUGGING: use gzfile(), NOT
##   memCompress(x, "gzip"). Despite the argument name, memCompress uses
##   zlib compress2 and emits a ZLIB stream -- 2-byte header, deflate
##   data, 4-byte Adler-32 -- not a gzip container. Treating it as gzip
##   gives the wrong payload boundaries and an Adler checksum where a
##   CRC32 belongs, and Excel rejects the workbook as corrupt.
##
## The output is a normal ZIP. Excel does not care which tool made it.
## =====================================================================

## ---------------------------------------------------------------------
## Little-endian integer writers
## ---------------------------------------------------------------------
le2 <- function(x) as.raw(c(bitwAnd(x, 255L), bitwAnd(bitwShiftR(x, 8), 255L)))

le4 <- function(x) {
  x <- as.numeric(x)                       # sizes can exceed .Machine integer
  b <- numeric(4)
  for (i in 1:4) { b[i] <- x %% 256; x <- x %/% 256 }
  as.raw(b)
}

## ---------------------------------------------------------------------
## deflate_and_crc -- raw deflate payload and CRC32, via a real gzip file
##
## The header length is derived from the FLG byte rather than assumed to
## be 10, so an implementation that writes a filename or extra field into
## the header cannot silently shift every entry by a few bytes.
## ---------------------------------------------------------------------
deflate_and_crc <- function(dat) {
  tmp <- tempfile(fileext = ".gz")
  on.exit(unlink(tmp), add = TRUE)

  zz <- gzfile(tmp, "wb", compression = 6)
  writeBin(dat, zz)
  close(zz)

  gz <- readBin(tmp, "raw", file.info(tmp)$size)
  n  <- length(gz)
  if (n < 18 || gz[1] != as.raw(0x1f) || gz[2] != as.raw(0x8b) ||
      gz[3] != as.raw(0x08))
    stop("gzfile did not produce a gzip stream")

  flg <- as.integer(gz[4])
  pos <- 11L
  if (bitwAnd(flg, 4L)) {                       # FEXTRA
    xlen <- as.integer(gz[pos]) + 256L * as.integer(gz[pos + 1L])
    pos <- pos + 2L + xlen
  }
  if (bitwAnd(flg, 8L)) {                       # FNAME
    while (gz[pos] != as.raw(0)) pos <- pos + 1L
    pos <- pos + 1L
  }
  if (bitwAnd(flg, 16L)) {                      # FCOMMENT
    while (gz[pos] != as.raw(0)) pos <- pos + 1L
    pos <- pos + 1L
  }
  if (bitwAnd(flg, 2L)) pos <- pos + 2L         # FHCRC

  list(comp = if (n - 8L >= pos) gz[pos:(n - 8L)] else raw(0),
       crc  = gz[(n - 7L):(n - 4L)])
}

## ---------------------------------------------------------------------
## zip_base -- write `files` (paths relative to `root`) into `out`
##
## Stored (method 0) for empty files, deflate (method 8) otherwise.
## ---------------------------------------------------------------------
zip_base <- function(files, root, out) {
  if (file.exists(out)) unlink(out)
  con <- file(out, "wb")
  on.exit(close(con), add = TRUE)

  central <- raw(0)
  offset  <- 0
  n_ok    <- 0

  for (f in files) {
    path <- file.path(root, f)
    sz   <- file.info(path)$size
    dat  <- if (sz > 0) readBin(path, "raw", sz) else raw(0)

    if (length(dat) > 0) {
      dc     <- deflate_and_crc(dat)
      comp   <- dc$comp
      crc    <- dc$crc
      method <- 8L
    } else {
      comp   <- raw(0)
      crc    <- as.raw(c(0, 0, 0, 0))
      method <- 0L
    }

    ## ZIP paths always use forward slashes regardless of platform
    nm  <- charToRaw(gsub("\\\\", "/", f))
    csz <- length(comp); usz <- length(dat)

    local <- c(as.raw(c(0x50, 0x4b, 0x03, 0x04)),  # local file header sig
               le2(20L),            # version needed
               le2(0L),             # flags
               le2(method),
               le2(0L), le2(0L),    # mod time, mod date
               crc, le4(csz), le4(usz),
               le2(length(nm)), le2(0L),
               nm)
    writeBin(local, con); writeBin(comp, con)

    central <- c(central,
                 as.raw(c(0x50, 0x4b, 0x01, 0x02)),  # central dir sig
                 le2(20L), le2(20L),
                 le2(0L), le2(method),
                 le2(0L), le2(0L),
                 crc, le4(csz), le4(usz),
                 le2(length(nm)),
                 le2(0L), le2(0L),   # extra, comment
                 le2(0L),            # disk number
                 le2(0L), le4(0L),   # internal, external attrs
                 le4(offset),
                 nm)

    offset <- offset + length(local) + csz
    n_ok   <- n_ok + 1
  }

  writeBin(central, con)
  writeBin(c(as.raw(c(0x50, 0x4b, 0x05, 0x06)),      # end of central dir
             le2(0L), le2(0L),
             le2(n_ok), le2(n_ok),
             le4(length(central)), le4(offset),
             le2(0L)), con)

  invisible(n_ok)
}

## ---------------------------------------------------------------------
## xlsx_write -- same part building as 0_xlsx_helpers.R, base-R zip
##
## The body below is the helper's, unchanged, up to the point where it
## zips. Only the final section differs. If the helper is ever revised,
## this function has to be revised with it.
## ---------------------------------------------------------------------
xlsx_write <- function(SH, out_path) {

  stopifnot(length(SH) > 0)
  nms <- vapply(SH, function(s) s$name, "")
  if (any(duplicated(nms))) stop("Duplicate sheet names: ",
                                 paste(nms[duplicated(nms)], collapse = ", "))
  if (any(nchar(nms) > 31)) stop("Sheet name over 31 chars: ",
                                 paste(nms[nchar(nms) > 31], collapse = ", "))

  BUILD <- file.path(tempdir(), paste0("xlsxbuild_",
                                       format(Sys.time(), "%H%M%OS3")))
  unlink(BUILD, recursive = TRUE)
  dir.create(file.path(BUILD, "_rels"), recursive = TRUE)
  dir.create(file.path(BUILD, "xl", "_rels"), recursive = TRUE)
  dir.create(file.path(BUILD, "xl", "worksheets", "_rels"), recursive = TRUE)

  has_img <- vapply(SH, function(s) length(s$images) > 0, logical(1))
  if (any(has_img)) {
    dir.create(file.path(BUILD, "xl", "media"), recursive = TRUE)
    dir.create(file.path(BUILD, "xl", "drawings", "_rels"), recursive = TRUE)
  }

  writeLines(styles_xml, file.path(BUILD, "xl", "styles.xml"), useBytes = TRUE)

  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
    '</Relationships>'), file.path(BUILD, "_rels", ".rels"), useBytes = TRUE)

  sheet_tags <- vapply(seq_along(SH), function(k)
    sprintf('<sheet name="%s" sheetId="%d" r:id="rId%d"/>',
            xl_esc(SH[[k]]$name), k, k), "")
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    '<sheets>', paste(sheet_tags, collapse = ""), '</sheets></workbook>'),
    file.path(BUILD, "xl", "workbook.xml"), useBytes = TRUE)

  wb_rels <- c(vapply(seq_along(SH), function(k) sprintf(
    '<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet%d.xml"/>', k, k), ""),
    sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>',
            length(SH) + 1))
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    paste(wb_rels, collapse = ""), '</Relationships>'),
    file.path(BUILD, "xl", "_rels", "workbook.xml.rels"), useBytes = TRUE)

  EMU <- 914400
  img_seq <- 0

  for (k in seq_along(SH)) {
    s <- SH[[k]]

    pane <- ""
    if (!is.null(s$freeze)) {
      tl <- paste0(xl_col(s$freeze$x + 1), s$freeze$y + 1)
      pane <- sprintf(paste0('<pane xSplit="%d" ySplit="%d" topLeftCell="%s" ',
                             'activePane="bottomRight" state="frozen"/>',
                             '<selection pane="bottomRight" activeCell="%s" sqref="%s"/>'),
                      s$freeze$x, s$freeze$y, tl, tl, tl)
    }

    drawing_tag <- ""
    if (length(s$images) > 0) {
      img_seq <- img_seq + 1
      anchors <- character(0); rel_lines <- character(0)
      for (m in seq_along(s$images)) {
        im <- s$images[[m]]
        media_name <- sprintf("image%d_%d.png", img_seq, m)
        file.copy(im$file, file.path(BUILD, "xl", "media", media_name),
                  overwrite = TRUE)
        rel_lines <- c(rel_lines, sprintf(
          '<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="../media/%s"/>',
          m, media_name))
        anchors <- c(anchors, sprintf(paste0(
          '<xdr:oneCellAnchor>',
          '<xdr:from><xdr:col>%d</xdr:col><xdr:colOff>0</xdr:colOff>',
          '<xdr:row>%d</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>',
          '<xdr:ext cx="%.0f" cy="%.0f"/>',
          '<xdr:pic><xdr:nvPicPr><xdr:cNvPr id="%d" name="Picture %d"/>',
          '<xdr:cNvPicPr><a:picLocks noChangeAspect="1"/></xdr:cNvPicPr></xdr:nvPicPr>',
          '<xdr:blipFill><a:blip xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" r:embed="rId%d"/>',
          '<a:stretch><a:fillRect/></a:stretch></xdr:blipFill>',
          '<xdr:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="%.0f" cy="%.0f"/></a:xfrm>',
          '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom></xdr:spPr></xdr:pic>',
          '<xdr:clientData/></xdr:oneCellAnchor>'),
          im$col, im$row, im$w * EMU, im$h * EMU, m + 1, m, m,
          im$w * EMU, im$h * EMU))
      }
      writeLines(paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<xdr:wsDr xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing" ',
        'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
        paste(anchors, collapse = ""), '</xdr:wsDr>'),
        file.path(BUILD, "xl", "drawings", sprintf("drawing%d.xml", img_seq)),
        useBytes = TRUE)
      writeLines(paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
        paste(rel_lines, collapse = ""), '</Relationships>'),
        file.path(BUILD, "xl", "drawings", "_rels",
                  sprintf("drawing%d.xml.rels", img_seq)), useBytes = TRUE)
      writeLines(paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
        sprintf('<Relationship Id="rIdDr" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing" Target="../drawings/drawing%d.xml"/>', img_seq),
        '</Relationships>'),
        file.path(BUILD, "xl", "worksheets", "_rels", sprintf("sheet%d.xml.rels", k)),
        useBytes = TRUE)
      drawing_tag <- '<drawing r:id="rIdDr"/>'
    }

    af_tag <- if (is.null(s$autofilter)) "" else
      sprintf('<autoFilter ref="%s"/>', s$autofilter)

    writeLines(paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
      'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
      '<sheetViews><sheetView workbookViewId="0">', pane, '</sheetView></sheetViews>',
      '<sheetFormatPr defaultRowHeight="12.75"/>',
      if (is.null(s$cols)) "" else s$cols,
      '<sheetData>', paste(s$rows, collapse = ""), '</sheetData>',
      af_tag,
      '<pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75" header="0.3" footer="0.3"/>',
      drawing_tag,
      '</worksheet>'),
      file.path(BUILD, "xl", "worksheets", sprintf("sheet%d.xml", k)),
      useBytes = TRUE)
  }

  ct <- c('<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
          '<Default Extension="xml" ContentType="application/xml"/>')
  if (any(has_img)) ct <- c(ct, '<Default Extension="png" ContentType="image/png"/>')
  ct <- c(ct,
    '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
    '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
    vapply(seq_along(SH), function(k) sprintf(
      '<Override PartName="/xl/worksheets/sheet%d.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>', k), ""))
  if (img_seq > 0) ct <- c(ct, vapply(seq_len(img_seq), function(k) sprintf(
    '<Override PartName="/xl/drawings/drawing%d.xml" ContentType="application/vnd.openxmlformats-officedocument.drawing+xml"/>', k), ""))
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    paste(ct, collapse = ""), '</Types>'),
    file.path(BUILD, "[Content_Types].xml"), useBytes = TRUE)

  ## ---- zip, base R only -----------------------------------------------
  ## [Content_Types].xml first. Not strictly required by the spec, but
  ## some readers are happier with it at the front and it costs nothing.
  parts <- list.files(BUILD, recursive = TRUE, all.files = TRUE,
                      no.. = TRUE)
  parts <- c("[Content_Types].xml", setdiff(parts, "[Content_Types].xml"))

  n <- zip_base(parts, BUILD, out_path)

  if (!file.exists(out_path))
    stop("zip_base wrote nothing. Parts are in: ", BUILD)

  ## Self-check. R's internal unzip reads the central directory and
  ## verifies CRCs on extract, so if this round-trips the archive is
  ## structurally sound and Excel will not reject it as corrupt. Far
  ## better to fail here than in front of the field team.
  chk <- tryCatch(utils::unzip(out_path, list = TRUE),
                  error = function(e) NULL, warning = function(w) NULL)
  if (is.null(chk) || nrow(chk) != n)
    stop("zip_base produced an archive R cannot read back. Parts are in: ",
         BUILD)

  tdir <- file.path(tempdir(), "xlsxverify")
  unlink(tdir, recursive = TRUE); dir.create(tdir)
  ok <- tryCatch({
    utils::unzip(out_path, files = "xl/workbook.xml", exdir = tdir)
    identical(readBin(file.path(tdir, "xl", "workbook.xml"), "raw",
                      file.info(file.path(tdir, "xl", "workbook.xml"))$size),
              readBin(file.path(BUILD, "xl", "workbook.xml"), "raw",
                      file.info(file.path(BUILD, "xl", "workbook.xml"))$size))
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok))
    stop("zip_base entries do not round-trip. Parts are in: ", BUILD)

  cat("  zip_base ->", n, "parts, verified\n")
  cat("  wrote", normalizePath(out_path), "-",
      round(file.size(out_path) / 1024^2, 2), "MB\n")
  invisible(out_path)
}
