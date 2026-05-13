$appFile = ".\app.R"

if (!(Test-Path $appFile)) {
  Write-Host "app.R not found. Run this script inside your R-System folder." -ForegroundColor Red
  exit
}

Copy-Item $appFile "$appFile.backup" -Force
Write-Host "Backup created: app.R.backup"

$password = Read-Host "Enter your Supabase database password"

[Environment]::SetEnvironmentVariable("SUPABASE_DB_PASSWORD", $password, "User")
$env:SUPABASE_DB_PASSWORD = $password

$content = Get-Content $appFile -Raw

if ($content -notmatch "library\(DBI\)") {
  $content = $content -replace "library\(jsonlite\)", "library(jsonlite)`r`nlibrary(DBI)`r`nlibrary(RPostgres)"
}

$dbCode = @'

db_connect <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = "aws-1-ap-southeast-2.pooler.supabase.com",
    port = 5432,
    dbname = "postgres",
    user = "postgres.qriopzhdbnkxbwtiingz",
    password = Sys.getenv("SUPABASE_DB_PASSWORD"),
    sslmode = "require"
  )
}

load_data = function() {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  q <- DBI::dbGetQuery(con, "SELECT data::text AS data FROM app_state WHERE id = 'main'")

  if (nrow(q) == 0) return(list())

  jsonlite::fromJSON(q$data[[1]], simplifyVector = FALSE)
}

save_data = function(users, all_transactions, promos, promo_id_ctr,
                     store_open_hour, store_close_hour,
                     store_force_close, force_close_note, menu_items) {

  payload <- list(
    users = users,
    all_transactions = all_transactions,
    promos = promos,
    promo_id_ctr = promo_id_ctr,
    store_open_hour = store_open_hour,
    store_close_hour = store_close_hour,
    store_force_close = store_force_close,
    force_close_note = force_close_note,
    menu_items = menu_items
  )

  json_data <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")

  con <- db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
    con,
    "UPDATE app_state SET data = $1::jsonb, updated_at = now() WHERE id = 'main'",
    params = list(json_data)
  )
}

'@

function Replace-FunctionBlock {
  param (
    [string[]]$Lines,
    [string]$StartPattern,
    [string]$Replacement
  )

  $start = -1
  for ($i = 0; $i -lt $Lines.Count; $i++) {
    if ($Lines[$i] -match $StartPattern) {
      $start = $i
      break
    }
  }

  if ($start -eq -1) {
    throw "Could not find function: $StartPattern"
  }

  $braceCount = 0
  $end = -1

  for ($i = $start; $i -lt $Lines.Count; $i++) {
    $braceCount += ([regex]::Matches($Lines[$i], "\{")).Count
    $braceCount -= ([regex]::Matches($Lines[$i], "\}")).Count

    if ($braceCount -eq 0 -and $i -gt $start) {
      $end = $i
      break
    }
  }

  if ($end -eq -1) {
    throw "Could not find end of function: $StartPattern"
  }

  $before = if ($start -gt 0) { $Lines[0..($start - 1)] } else { @() }
  $after = if ($end -lt ($Lines.Count - 1)) { $Lines[($end + 1)..($Lines.Count - 1)] } else { @() }

  return @($before + ($Replacement -split "`r?`n") + $after)
}

$lines = $content -split "`r?`n"

$lines = Replace-FunctionBlock $lines "^\s*save_data\s*=" $dbCode
$lines = Replace-FunctionBlock $lines "^\s*load_data\s*=" ""

Set-Content -Path $appFile -Value ($lines -join "`r`n") -Encoding UTF8

Write-Host "Done. app.R now uses Supabase." -ForegroundColor Green
Write-Host "Next: install DB packages and run the app."