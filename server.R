port <- as.integer(Sys.getenv("PORT", unset="8080"))
srv <- serverSocket(port)
cat(sprintf("Listening on port %d\n", port))

reply <- function(con, status, ctype, body, head_only=FALSE) {
  payload <- if (head_only) "" else body
  hdr <- paste0(
    "HTTP/1.1 ", status, "\r\n",
    "Content-Type: ", ctype, "\r\n",
    "Cache-Control: no-store\r\n",
    "X-Content-Type-Options: nosniff\r\n",
    "Content-Length: ", nchar(payload, type="bytes"), "\r\n",
    "Connection: close\r\n\r\n",
    payload
  )
  writeChar(hdr, con, eos=NULL, useBytes=TRUE)
}

repeat {
  con <- socketAccept(srv, blocking=TRUE, open="r+b", timeout=5)
  tryCatch({
    line <- readLines(con, n=1, warn=FALSE)
    parts <- strsplit(ifelse(length(line), line, ""), " +")[[1]]
    method <- if (length(parts)>=1) parts[1] else ""
    path <- if (length(parts)>=2) parts[2] else ""
    repeat {
      h <- readLines(con, n=1, warn=FALSE)
      if (length(h)==0 || nchar(h)==0 || h=="\r") break
    }
    if (method %in% c("GET","HEAD") && path=="/healthz") {
      commit <- Sys.getenv("RAILWAY_GIT_COMMIT_SHA", unset="")
      body <- sprintf('{"status":"ok","runtime":"r","commit":"%s"}\n', commit)
      reply(con,"200 OK","application/json; charset=utf-8",body,method=="HEAD")
    } else if (method %in% c("GET","HEAD") && path=="/") {
      reply(con,"200 OK","text/plain; charset=utf-8","R Mini Receptor\n",method=="HEAD")
    } else if (!(method %in% c("GET","HEAD"))) {
      reply(con,"405 Method Not Allowed","text/plain; charset=utf-8","method not allowed\n")
    } else {
      reply(con,"404 Not Found","text/plain; charset=utf-8","not found\n")
    }
  }, error=function(e) {})
  close(con)
}
