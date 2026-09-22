port <- as.integer(Sys.getenv("PORT", unset="8080"))
if (is.na(port) || port < 1 || port > 65535) stop("invalid PORT")

page <- paste0(
'<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
'<title>R Experiment Analyzer</title><style>',
':root{font-family:Inter,system-ui,sans-serif;color:#f4f8ff;background:#0c1320}*{box-sizing:border-box}',
'body{margin:0;min-height:100vh;background:linear-gradient(145deg,#0c1320,#25385a);padding:32px}',
'main{max-width:1000px;margin:auto}.tag{display:inline-block;background:#314d78;color:#c5dbff;padding:7px 11px;border-radius:999px;font-size:12px;font-weight:800;text-transform:uppercase;letter-spacing:.06em}',
'h1{font-size:clamp(34px,5vw,58px);margin:16px 0 8px}.sub{color:#afbed5;max-width:760px;line-height:1.6}',
'.panel{margin-top:28px;background:#152139;border:1px solid #405982;border-radius:20px;padding:22px}',
'textarea{width:100%;min-height:160px;background:#0b1424;color:#fff;border:1px solid #405982;border-radius:14px;padding:16px;font:14px/1.5 ui-monospace,monospace;margin-top:10px}',
'button{margin-top:14px;border:0;border-radius:12px;padding:12px 18px;background:#a9caff;color:#091321;font-weight:800;cursor:pointer}',
'.grid{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-top:18px}.card{background:#0b1424;border:1px solid #405982;border-radius:14px;padding:16px}.card b{font-size:27px;display:block}.card span{font-size:12px;color:#93a6c0;text-transform:uppercase}.note{margin-top:14px;color:#9eafc6;font-size:13px}',
'@media(max-width:700px){body{padding:18px}.grid{grid-template-columns:repeat(2,1fr)}}</style></head><body><main>',
'<span class="tag">R 4.5 • GovernEvo target</span><h1>Experiment Analyzer</h1>',
'<p class="sub">Compare a baseline and candidate experiment with descriptive statistics and a simple effect-size signal.</p>',
'<section class="panel"><textarea id="baseline">98\n101\n99\n100\n102</textarea><textarea id="candidate">103\n105\n104\n102\n106</textarea>',
'<button onclick="analyze()">Compare experiments</button><div class="grid">',
'<div class="card"><b id="meanA">—</b><span>Baseline mean</span></div><div class="card"><b id="meanB">—</b><span>Candidate mean</span></div>',
'<div class="card"><b id="delta">—</b><span>Delta %</span></div><div class="card"><b id="effect">—</b><span>Effect size</span></div></div>',
'<div class="note" id="status">Ready.</div></section></main>',
'<script>
async function analyze(){
  status.textContent = "Analyzing…";
  try {
    const q = new URLSearchParams({baseline: baseline.value, candidate: candidate.value});
    const r = await fetch("/api/compare?" + q);

    // Best-effort response parsing: errors may not be JSON, and network/proxy
    // failures may produce empty bodies.
    let d = null;
    try {
      d = await r.json();
    } catch (e) {
      d = null;
    }

    if(!r.ok){
      const serverMsg = (d && (d.error || d.message)) ? (d.error || d.message) : null;
      status.textContent = `Request failed (HTTP ${r.status}).` + (serverMsg ? ` ${serverMsg}` : "");
      return;
    }

    if(!d){
      status.textContent = `Request failed (HTTP ${r.status}). Invalid JSON response.`;
      return;
    }

    meanA.textContent = d.baseline_mean.toFixed(2);
    meanB.textContent = d.candidate_mean.toFixed(2);
    delta.textContent = d.delta_percent.toFixed(2) + "%";
    effect.textContent = d.effect_size.toFixed(2);
    status.textContent = d.message;
  } catch (e) {
    status.textContent = "Request failed. Please check your connection and try again.";
  }
}
analyze();
</script>',
'</body></html>'
)

reply <- function(con, status, ctype, body) {
  hdr <- paste0(
    "HTTP/1.1 ", status, "\r\n",
    "Content-Type: ", ctype, "\r\n",
    "Cache-Control: no-store\r\n",
    "X-Content-Type-Options: nosniff\r\n",
    "Content-Length: ", nchar(body, type="bytes"), "\r\n",
    "Connection: close\r\n\r\n",
    body
  )
  writeChar(hdr, con, eos=NULL, useBytes=TRUE)
}

urldecode <- function(x) {
  x <- gsub("+", " ", x, fixed=TRUE)
  raw <- charToRaw(x)
  out <- raw()
  i <- 1
  while (i <= length(raw)) {
    if (raw[i] == as.raw(0x25) && i + 2 <= length(raw)) {
      h <- rawToChar(raw[(i+1):(i+2)])
      v <- suppressWarnings(strtoi(h, base=16L))
      if (!is.na(v)) {
        out <- c(out, as.raw(v))
        i <- i + 3
        next
      }
    }
    out <- c(out, raw[i]); i <- i + 1
  }
  rawToChar(out)
}

query_params <- function(target) {
  parts <- strsplit(target, "?", fixed=TRUE)[[1]]
  if (length(parts) < 2) return(list())
  pairs <- strsplit(parts[2], "&", fixed=TRUE)[[1]]
  result <- list()
  for (pair in pairs) {
    kv <- strsplit(pair, "=", fixed=TRUE)[[1]]
    key <- urldecode(kv[1])
    value <- if (length(kv) > 1) urldecode(paste(kv[-1], collapse="=")) else ""
    result[[key]] <- value
  }
  result
}

parse_series <- function(text) {
  items <- unlist(strsplit(gsub(",", "\n", text, fixed=TRUE), "\n", fixed=TRUE))
  items <- trimws(items)
  items <- items[nzchar(items)]
  values <- suppressWarnings(as.numeric(items))
  if (!length(values) || any(is.na(values)) || length(values) > 1000) return(NULL)
  values
}

srv <- serverSocket(port)
cat(sprintf("Listening on port %d\n", port))

repeat {
  con <- tryCatch(
    socketAccept(srv, blocking=TRUE, open="r+b", timeout=8),
    error=function(e) NULL
  )
  if (is.null(con)) next

  tryCatch({
    line <- readLines(con, n=1, warn=FALSE)
    parts <- strsplit(ifelse(length(line), line, ""), " +")[[1]]
    method <- if (length(parts)>=1) parts[1] else ""
    target <- if (length(parts)>=2) parts[2] else "/"
    path <- strsplit(target, "?", fixed=TRUE)[[1]][1]
    repeat {
      h <- readLines(con, n=1, warn=FALSE)
      if (length(h)==0 || nchar(h)==0 || h=="\r") break
    }

    if (method != "GET") {
      reply(con,"405 Method Not Allowed","application/json; charset=utf-8",'{"error":"method_not_allowed"}')
    } else if (path == "/healthz") {
      reply(con,"200 OK","application/json; charset=utf-8",'{"status":"ok","service":"r-experiment-analyzer"}')
    } else if (path == "/") {
      reply(con,"200 OK","text/html; charset=utf-8",page)
    } else if (path == "/api/compare") {
      q <- query_params(target)
      a <- parse_series(if (is.null(q$baseline)) "" else q$baseline)
      b <- parse_series(if (is.null(q$candidate)) "" else q$candidate)
      if (is.null(a) || is.null(b) || length(a) < 2 || length(b) < 2) {
        reply(con,"400 Bad Request","application/json; charset=utf-8",'{"error":"provide at least two valid numbers in each series"}')
      } else {
        ma <- mean(a); mb <- mean(b)
        pooled <- sqrt(((length(a)-1)*var(a)+(length(b)-1)*var(b))/(length(a)+length(b)-2))
        effect <- if (is.finite(pooled) && pooled > 0) (mb-ma)/pooled else 0
        delta <- if (ma != 0) (mb-ma)/abs(ma)*100 else 0
        message <- if (abs(effect) >= 0.8) "Large observed effect; validate with a larger sample before production decisions." else if (abs(effect) >= 0.5) "Moderate observed effect; continue controlled validation." else "Observed effect is small under the current sample."
        body <- sprintf('{"baseline_mean":%.8f,"candidate_mean":%.8f,"delta_percent":%.8f,"effect_size":%.8f,"message":"%s"}',ma,mb,delta,effect,message)
        reply(con,"200 OK","application/json; charset=utf-8",body)
      }
    } else {
      reply(con,"404 Not Found","application/json; charset=utf-8",'{"error":"not_found"}')
    }
  }, error=function(e) {
    try(reply(con,"500 Internal Server Error","application/json; charset=utf-8",'{"error":"internal_error"}'), silent=TRUE)
  })
  close(con)
}
