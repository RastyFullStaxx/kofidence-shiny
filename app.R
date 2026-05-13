library(shiny)
library(jsonlite)
library(DBI)
library(RPostgres)
source("secrets.R")

DATA_DIR  = "kofidence_data"
DATA_FILE = file.path(DATA_DIR, "kofidence_data.json")
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)


db_connect <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = "aws-1-ap-southeast-2.pooler.supabase.com",
    port = 5432,
    dbname = "postgres",
    user = "postgres.qriopzhdbnkxbwtiingz",
    password = SUPABASE_DB_PASSWORD,
    sslmode = "require"
  )
}

load_data = function() {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  q <- DBI::dbGetQuery(
    con,
    "SELECT data::text AS data FROM app_state WHERE id = 'main'"
  )

  if (nrow(q) == 0) return(NULL)

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
ph_time = function() format(Sys.time(), tz = "Asia/Manila", "%B %d, %Y %I:%M %p")

FEEDBACK_CSV_URL   = "https://docs.google.com/spreadsheets/d/e/2PACX-1vS-5WLGPmosQJFSUCGThfKXzfB0fWXr0JfOLSZF-ZD7O4JjOBc5ww1NC-3wI3Gors0STulOoZcUTU7e/pub?output=csv"
FEEDBACK_SHEET_URL = "https://docs.google.com/spreadsheets/d/1zyO9Rnw94eYgcswOwFOhEkzD7TRrorQGiVcNe3j52BQ/edit?resourcekey=&gid=1441804297#gid=1441804297"
CUSTOMER_FEEDBACK_URL = "https://forms.gle/9z1sxDkyZJVTtvaJA"

`%||%` = function(a, b) if (!is.null(a)) a else b

default_menu_items = list(
  list(id=1,  cat="Espresso Iced",        name="Americano",              price=90,  avail=TRUE),
  list(id=2,  cat="Espresso Iced",        name="Cafe Latte",             price=100, avail=TRUE),
  list(id=3,  cat="Starred Drinks",       name="Spanish Latte (Iced)",   price=110, avail=TRUE),
  list(id=4,  cat="Espresso Iced",        name="Cafe Mocha",             price=120, avail=TRUE),
  list(id=5,  cat="Espresso Iced",        name="White Mocha",            price=130, avail=TRUE),
  list(id=6,  cat="Espresso Iced",        name="Caramel Latte",          price=130, avail=TRUE),
  list(id=7,  cat="Espresso Iced",        name="Shaken Cinnamon",        price=130, avail=TRUE),
  list(id=8,  cat="Espresso Iced",        name="Dirty Matcha",           price=130, avail=TRUE),
  list(id=9,  cat="Starred Drinks",       name="Salted Caramel (Iced)",  price=140, avail=TRUE),
  list(id=10, cat="Starred Drinks",       name="Seasalt Latte (Iced)",   price=140, avail=TRUE),
  list(id=11, cat="Espresso Iced",        name="Nutella Latte",          price=150, avail=TRUE),
  list(id=12, cat="Starred Drinks",       name="Cafedence (Iced)",       price=160, avail=TRUE),
  list(id=13, cat="Espresso Iced",        name="Biscoffee Latte",        price=170, avail=TRUE),
  list(id=14, cat="Espresso Hot",         name="Americano",              price=80,  avail=TRUE),
  list(id=15, cat="Espresso Hot",         name="Cafe Latte",             price=90,  avail=TRUE),
  list(id=16, cat="Espresso Hot",         name="Spanish Latte",          price=100, avail=TRUE),
  list(id=17, cat="Espresso Hot",         name="Cafe Mocha",             price=100, avail=TRUE),
  list(id=18, cat="Espresso Hot",         name="White Choco",            price=110, avail=TRUE),
  list(id=19, cat="Espresso Hot",         name="Hot Choco",              price=110, avail=TRUE),
  list(id=20, cat="Espresso Hot",         name="Salted Caramel",         price=120, avail=TRUE),
  list(id=21, cat="Espresso Hot",         name="Caramel Latte",          price=130, avail=TRUE),
  list(id=22, cat="Ice Blended Espresso", name="Coffee Frappe",          price=120, avail=TRUE),
  list(id=23, cat="Ice Blended Espresso", name="Mocha Frappe",           price=140, avail=TRUE),
  list(id=24, cat="Starred Drinks",       name="Caramel Frappe",         price=150, avail=TRUE),
  list(id=25, cat="Starred Drinks",       name="Choco Chip Frappe",      price=150, avail=TRUE),
  list(id=26, cat="Ice Blended Espresso", name="Biscoffee Frappe",       price=180, avail=TRUE),
  list(id=27, cat="Ice Blended Cream",    name="Strawberry Frappe",      price=150, avail=TRUE),
  list(id=28, cat="Ice Blended Cream",    name="Matcha Frappe",          price=160, avail=TRUE),
  list(id=29, cat="Starred Drinks",       name="Milo Dino Seasalt",      price=100, avail=TRUE),
  list(id=30, cat="Non-Coffee",           name="Dark Choco",             price=120, avail=TRUE),
  list(id=31, cat="Starred Drinks",       name="Matcha Latte",           price=120, avail=TRUE),
  list(id=32, cat="Non-Coffee",           name="Strawberry Soda",        price=120, avail=TRUE),
  list(id=33, cat="Non-Coffee",           name="Blue Lemon Soda",        price=120, avail=TRUE),
  list(id=34, cat="Non-Coffee",           name="Green Apple Soda",       price=120, avail=TRUE),
  list(id=35, cat="Non-Coffee",           name="Milky Strawberry",       price=140, avail=TRUE),
  list(id=36, cat="Non-Coffee",           name="Strawberry Matcha",      price=160, avail=TRUE),
  list(id=37, cat="Snacks",               name="Plain Nutella Croffle",  price=110, avail=TRUE),
  list(id=38, cat="Starred Drinks",       name="Almond Nutella Croffle", price=120, avail=TRUE),
  list(id=39, cat="Snacks",               name="Plain Biscoff Croffle",  price=130, avail=TRUE),
  list(id=40, cat="Starred Drinks",       name="Smores Croffle",         price=130, avail=TRUE),
  list(id=41, cat="Snacks",               name="Almond Biscoff Croffle", price=140, avail=TRUE),
  list(id=42, cat="Snacks",               name="Crunchy Biscoff",        price=140, avail=TRUE),
  list(id=43, cat="Snacks",               name="Lotus Caramel Croffle",  price=140, avail=TRUE),
  list(id=44, cat="Snacks",               name="Ham Sandwich",           price=150, avail=TRUE),
  list(id=45, cat="Snacks",               name="Tuna Sandwich",          price=150, avail=TRUE),
  list(id=46, cat="Starred Drinks",       name="Spam Sandwich",          price=180, avail=TRUE),
  list(id=47, cat="Add Ons",              name="Sub Oat",                price=20,  avail=TRUE),
  list(id=48, cat="Add Ons",              name="Seasalt",                price=30,  avail=TRUE),
  list(id=49, cat="Add Ons",              name="Whipped Cream",          price=30,  avail=TRUE),
  list(id=50, cat="Add Ons",             name="Single Shot",             price=40,  avail=TRUE),
  list(id=51, cat="Add Ons",              name="Double Shot",            price=50,  avail=TRUE)
)

rewards_list = list(
  list(id=1, pts=10, label="Free add-on (Whipped Cream / Sea Salt)"),
  list(id=2, pts=15, label="Free size upgrade OR P10 off"),
  list(id=3, pts=25, label="Free Americano / Cafe Latte"),
  list(id=4, pts=30, label="Free drink (any up to P130)"),
  list(id=5, pts=40, label="Free drink (any item on menu)"),
  list(id=6, pts=55, label="Free drink + croffle combo")
)

promo_types = c(
  "Combo Deal"         = "combo",
  "Buy 1 Get 1"        = "bogo",
  "% Discount"         = "percent",
  "Fixed Discount"     = "fixed",
  "Limited Time Offer" = "lto"
)

promo_type_labels = c(combo="Combo Deal", bogo="Buy 1 Get 1", percent="% Discount",
                      fixed="Fixed Discount", lto="Limited Time Offer")

recurring_opts = c(
  "None (one-time)"="none", "Every day"="daily", "Every Monday"="mon",
  "Every Tuesday"="tue", "Every Wednesday"="wed", "Every Thursday"="thu",
  "Every Friday"="fri", "Every Saturday"="sat", "Every Sunday"="sun",
  "Weekdays only"="weekdays", "Weekends only"="weekends",
  "Every 2 weeks"="biweekly", "Every month"="monthly"
)

recurring_labels = c(
  none="One-time only", daily="Every day", mon="Every Monday", tue="Every Tuesday",
  wed="Every Wednesday", thu="Every Thursday", fri="Every Friday", sat="Every Saturday",
  sun="Every Sunday", weekdays="Weekdays only (Mon-Fri)", weekends="Weekends only (Sat-Sun)",
  biweekly="Every 2 weeks", monthly="Every month"
)

valid_points = function(user) {
  if (is.null(user) || is.null(user$points_log)) return(0)
  df = user$points_log
  if (length(df) == 0) return(0)
  if (!is.data.frame(df)) df = as.data.frame(do.call(rbind, lapply(df, as.data.frame)), stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(0)
  df$expires = as.POSIXct(df$expires)
  sum(df$pts[df$expires > Sys.time()], na.rm = TRUE)
}

valid_log = function(user) {
  empty = data.frame(pts=numeric(), earned=character(), expires=as.POSIXct(character()), stringsAsFactors=FALSE)
  if (is.null(user) || is.null(user$points_log)) return(empty)
  df = user$points_log
  if (length(df) == 0) return(empty)
  if (!is.data.frame(df)) {
    tryCatch({
      df = as.data.frame(do.call(rbind, lapply(df, function(x)
        data.frame(pts=x$pts, earned=x$earned, expires=x$expires, stringsAsFactors=FALSE)
      )), stringsAsFactors=FALSE)
    }, error=function(e) return(empty))
  }
  if (nrow(df) == 0) return(df)
  df$expires = as.POSIXct(df$expires)
  df = df[df$expires > Sys.time(), , drop=FALSE]
  df[order(df$expires), ]
}

normalize_points_log = function(points_log) {
  empty = data.frame(pts=numeric(), earned=character(), expires=character(), stringsAsFactors=FALSE)
  if (is.null(points_log) || length(points_log) == 0) return(empty)
  if (is.data.frame(points_log)) {
    df = points_log
  } else {
    df = tryCatch(as.data.frame(do.call(rbind, lapply(points_log, as.data.frame)), stringsAsFactors=FALSE),
                  error=function(e) empty)
  }
  if (nrow(df) == 0) return(empty)
  if (is.null(df$pts)) df$pts = 0
  if (is.null(df$earned)) df$earned = ""
  if (is.null(df$expires)) df$expires = as.character(Sys.time())
  df$pts = as.numeric(df$pts)
  df$expires = as.character(df$expires)
  df
}

subtract_active_points = function(user, pts_to_remove) {
  if (is.null(user) || is.null(pts_to_remove) || is.na(pts_to_remove) || pts_to_remove <= 0) return(user)
  df = normalize_points_log(user$points_log)
  if (nrow(df) == 0) { user$points_log = df; return(user) }
  expires = as.POSIXct(df$expires)
  remaining = pts_to_remove
  for (j in rev(seq_len(nrow(df)))) {
    if (remaining <= 0) break
    if (is.na(expires[j]) || expires[j] <= Sys.time() || is.na(df$pts[j]) || df$pts[j] <= 0) next
    take = min(df$pts[j], remaining)
    df$pts[j] = df$pts[j] - take
    remaining = remaining - take
  }
  user$points_log = df[df$pts > 0, , drop=FALSE]
  user
}

recurring_day_match = function(promo) {
  r = promo$recurring
  if (is.null(r) || r == "none") return(TRUE)
  wd = tolower(format(Sys.time(), tz="Asia/Manila", "%a"))
  switch(r,
         daily    = TRUE,
         mon      = wd=="mon", tue=wd=="tue", wed=wd=="wed",
         thu      = wd=="thu", fri=wd=="fri", sat=wd=="sat", sun=wd=="sun",
         weekdays = wd %in% c("mon","tue","wed","thu","fri"),
         weekends = wd %in% c("sat","sun"),
         biweekly = {
           if (!is.null(promo$start_date)) {
             days = as.numeric(difftime(Sys.time(), as.POSIXct(promo$start_date), units="days"))
             floor(days/7) %% 2 == 0
           } else TRUE
         },
         monthly = {
           if (!is.null(promo$start_date)) {
             sd = as.POSIXct(promo$start_date)
             as.integer(format(Sys.time(), "%d")) == as.integer(format(sd, "%d"))
           } else TRUE
         },
         TRUE
  )
}

is_promo_active = function(promo) {
  now = Sys.time()
  if (!isTRUE(promo$visible)) return(FALSE)
  start_ok = is.null(promo$start_date) || as.POSIXct(promo$start_date) <= now
  end_ok   = is.null(promo$end_date)   || as.POSIXct(promo$end_date)   >= now
  start_ok && end_ok && recurring_day_match(promo)
}

next_card_number = function(user_list) sprintf("%06d", length(user_list) + 1)

is_store_open = function(open_hour, close_hour) {
  now_ph       = as.POSIXct(format(Sys.time(), tz="Asia/Manila"), tz="Asia/Manila")
  current_hour = as.integer(format(now_ph, "%H"))
  current_min  = as.integer(format(now_ph, "%M"))
  current_frac = current_hour + current_min / 60
  if (close_hour > open_hour) {
    current_frac >= open_hour && current_frac < close_hour
  } else {
    if (close_hour == 0) close_hour = 24
    current_frac >= open_hour || current_frac < (close_hour - 24)
  }
}

format_hour = function(h) {
  if (h == 0 || h == 24) return("12:00 AM")
  if (h == 12) return("12:00 PM")
  if (h < 12)  return(paste0(h, ":00 AM"))
  return(paste0(h - 12, ":00 PM"))
}

promo_detail_lines = function(p) {
  lines = character(0)
  item_label = function(x, fallback) {
    if (!is.null(x) && length(x) > 0 && nchar(trimws(paste(x, collapse=", "))) > 0) paste(x, collapse=", ") else fallback
  }
  
  if (p$type == "combo") {
    d = if (!is.null(p$combo_items) && length(p$combo_items) >= 2)
      paste(p$combo_items[1], "+", p$combo_items[2])
    else "Drink + Snack combo"
    if (!is.null(p$disc_price) && !is.na(p$disc_price) && p$disc_price > 0)
      lines = c(lines, paste0("Deal: ", d, " for only P", p$disc_price))
    else
      lines = c(lines, paste0("Combo: ", d))
    
  } else if (p$type == "bogo") {
    item = if (!is.null(p$bogo_item) && nchar(p$bogo_item) > 0) p$bogo_item else "selected item"
    lines = c(lines, paste0("Buy 1 Get 1 FREE on: ", item))
    
  } else if (p$type == "percent") {
    pct_val = if (!is.null(p$pct) && !is.na(p$pct)) paste0(p$pct, "%") else ""
    applies = if (!is.null(p$pct_applies) && nchar(trimws(p$pct_applies)) > 0) p$pct_applies else "select items"
    lines = c(lines, paste0(pct_val, " OFF on: ", applies))
    
  } else if (p$type == "fixed") {
    disc_val = if (!is.null(p$fixed_disc) && !is.na(p$fixed_disc)) paste0("P", p$fixed_disc, " off") else ""
    fixed_items = item_label(p$fixed_items %||% p$fixed_item, "selected items")
    if (nchar(disc_val) > 0) lines = c(lines, paste0("Discount: ", disc_val, " on: ", fixed_items))
    if (!is.null(p$fixed_min) && !is.na(p$fixed_min) && p$fixed_min > 0)
      lines = c(lines, paste0("Min. spend: P", p$fixed_min))
    
  } else if (p$type == "lto") {
    item = if (!is.null(p$bogo_item) && nchar(p$bogo_item) > 0) p$bogo_item else "featured item"
    if (!is.null(p$disc_price) && !is.na(p$disc_price) && p$disc_price > 0)
      lines = c(lines, paste0("LTO Price: ", item, " for P", p$disc_price))
    else
      lines = c(lines, paste0("Limited Offer: ", item))
  }
  
  has_start = !is.null(p$start_date) && nchar(p$start_date) > 0
  has_end   = !is.null(p$end_date)   && nchar(p$end_date)   > 0
  if (has_start && has_end) {
    s = format(as.POSIXct(p$start_date), "%b %d, %Y")
    e = format(as.POSIXct(p$end_date),   "%b %d, %Y")
    lines = c(lines, paste0("Valid: ", s, " \u2013 ", e))
  } else if (has_end) {
    e = format(as.POSIXct(p$end_date), "%b %d, %Y")
    lines = c(lines, paste0("Until: ", e))
  }

  rec = p$recurring %||% "none"
  if (rec != "none") {
    rl = recurring_labels[rec]
    if (!is.na(rl)) lines = c(lines, paste0("Schedule: ", rl))
  }
  
  lines
}


app_css = "
@import url('https://fonts.googleapis.com/css2?family=Cinzel:wght@400;600;700&family=Playfair+Display:ital,wght@0,400;0,600;1,400&family=DM+Sans:wght@300;400;500&display=swap');
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0;}
:root{
  --brown-deep:#2C1A0E;--brown-dark:#4A2C17;--brown-mid:#7B4A2D;
  --brown-warm:#A0622A;--amber:#C8861D;--amber-light:#E8A83C;
  --gold:#F2C063;--cream:#FAF3E8;--gray-dark:#28272A;--gray-mid:#3D3A36;
  --gray-soft:#5C5852;--gray-muted:#9C9890;--gray-light:#E8E4DC;
  --white:#FFFDF8;--cream-warm:#FFF6EA;--panel-warm:rgba(42,25,13,0.62);
  --line-gold:rgba(242,192,99,0.24);--shadow-warm:rgba(28,14,6,0.46);
}
body{font-family:'DM Sans',sans-serif;background:radial-gradient(circle at 15% 8%,rgba(100,52,18,0.22),transparent 30%),linear-gradient(135deg,#1d1b18,#29211b 48%,#1b1a18);min-height:100vh;color:var(--white);}
.navbar,.navbar-default{display:none!important;}
.container-fluid{padding:0!important;}
.hex-bg{position:fixed;inset:0;z-index:0;overflow:hidden;pointer-events:none;}
.hex-bg svg{width:100%;height:100%;}
.brand-bar{position:relative;z-index:10;background:linear-gradient(90deg,rgba(44,26,14,0.98),rgba(31,18,9,0.94));border-bottom:1.5px solid var(--amber);padding:0 2.1rem;min-height:58px;display:flex;align-items:center;gap:16px;box-shadow:0 10px 30px rgba(0,0,0,0.28);}
.brand-logo{font-family:'Cinzel',serif;font-size:23px;font-weight:700;color:var(--gold);text-transform:uppercase;letter-spacing:0.02em;}
.brand-tagline{font-size:13px;color:rgba(255,253,248,0.82);border-left:1px solid rgba(242,192,99,0.45);padding-left:16px;}
.brand-note{margin-left:auto;display:flex;align-items:center;gap:10px;color:rgba(255,253,248,0.82);font-size:12.5px;line-height:1.35;}
.brand-note-badge{width:34px;height:38px;display:flex;align-items:center;justify-content:center;color:var(--gold);filter:drop-shadow(0 3px 10px rgba(200,134,29,0.22));}
.brand-note-badge svg{width:34px;height:38px;display:block;}
.auth-page{position:relative;z-index:5;min-height:calc(100vh - 58px);display:grid;grid-template-columns:minmax(0,1fr) 420px;}
.left-panel{display:flex;flex-direction:column;align-items:center;justify-content:center;padding:3.2rem 2rem;}
.left-title{font-family:'Playfair Display',serif;font-size:40px;font-weight:600;color:var(--white);text-align:center;line-height:1.18;margin-bottom:0.8rem;text-shadow:0 2px 18px rgba(0,0,0,0.5);}
.left-title span{color:var(--gold);font-style:italic;}
.left-sub{font-size:15px;color:rgba(255,253,248,0.72);margin-bottom:2.2rem;text-align:center;}
.left-divider{display:none;}
.feature-row{display:grid;grid-template-columns:repeat(4,minmax(118px,1fr));gap:0;width:min(760px,100%);margin-top:1.2rem;}
.perk{position:relative;display:flex;flex-direction:column;align-items:center;text-align:center;gap:8px;color:rgba(255,253,248,0.76);font-size:13px;padding:0 18px;}
.perk:not(:last-child){border-right:1px solid rgba(200,134,29,0.32);}
.perk-dot{display:none;}
.perk-hex{width:72px;height:72px;display:flex;align-items:center;justify-content:center;margin-bottom:6px;color:var(--gold);background:rgba(18,13,9,0.34);clip-path:polygon(25% 6%,75% 6%,100% 50%,75% 94%,25% 94%,0 50%);position:relative;}
.perk-hex::before{content:'';position:absolute;inset:1px;background:linear-gradient(135deg,var(--gold),var(--amber));clip-path:inherit;z-index:0;}
.perk-hex::after{content:'';position:absolute;inset:2px;background:rgba(18,13,9,0.78);clip-path:inherit;z-index:1;}
.perk-icon{position:relative;z-index:2;display:inline-flex;align-items:center;justify-content:center;width:34px;height:34px;font-size:27px;line-height:1;color:var(--gold);}
.perk-icon svg{width:34px;height:34px;stroke:currentColor;fill:none;stroke-width:1.8;stroke-linecap:round;stroke-linejoin:round;display:block;}
.perk-label{color:var(--white);font-size:15px;font-weight:700;}
.perk-desc{color:rgba(255,253,248,0.72);font-size:12.5px;}
.right-panel{display:flex;align-items:center;justify-content:center;padding:2.5rem 1.7rem;background:linear-gradient(180deg,rgba(18,15,12,0.34),rgba(10,8,6,0.58));backdrop-filter:blur(2px);border-left:1px solid rgba(200,134,29,0.13);}
.auth-card{background:linear-gradient(145deg,#fffaf3,#f8ecdc);border:1px solid rgba(242,192,99,0.24);border-radius:24px;padding:2.35rem 2.15rem;width:100%;max-width:382px;box-shadow:0 26px 70px rgba(23,12,5,0.54),0 0 0 1px rgba(255,255,255,0.5) inset;}
.card-badge{display:inline-flex;align-items:center;gap:7px;background:linear-gradient(135deg,#FFE9BA,#F7D490);box-shadow:0 8px 18px rgba(160,98,42,0.15);border:1px solid rgba(200,134,29,0.18);border-radius:30px;padding:6px 15px;font-size:12px;color:#87521f;font-weight:600;margin-bottom:1.25rem;}
.auth-tabs{display:flex;border-bottom:1.5px solid var(--gray-light);margin-bottom:2rem;}
.auth-tab-btn{flex:1;background:none;border:none;padding:0.6rem 0;font-family:'DM Sans',sans-serif;font-size:14px;font-weight:500;color:var(--gray-muted);cursor:pointer;position:relative;transition:color 0.2s;}
.auth-tab-btn::after{content:'';position:absolute;bottom:-1.5px;left:0;right:0;height:2px;background:var(--brown-warm);transform:scaleX(0);transition:transform 0.2s;}
.auth-tab-btn.active{color:var(--brown-dark);}
.auth-tab-btn.active::after{transform:scaleX(1);}
.form-panel{display:none;}
.form-panel.active{display:block;}
.form-title{font-family:'Playfair Display',serif;font-size:22px;color:var(--brown-deep);margin-bottom:0.3rem;}
.form-sub{font-size:12px;color:var(--gray-muted);font-style:italic;margin-bottom:1.5rem;}
.kof-label{display:block;font-size:11.5px;font-weight:500;color:var(--gray-soft);letter-spacing:0.08em;text-transform:uppercase;margin-bottom:0.4rem;margin-top:1rem;}
.kof-input{width:100%;height:46px;border:1.5px solid rgba(74,44,23,0.18);border-radius:10px;padding:0 14px;font-family:'DM Sans',sans-serif;font-size:15px;color:var(--brown-deep);background:#fbf1e6;outline:none;transition:border-color 0.2s,box-shadow 0.2s,background 0.2s;}
.kof-input:focus{border-color:var(--amber);background:var(--white);box-shadow:0 0 0 3px rgba(200,134,29,0.12);}
.terms-row{display:flex;align-items:flex-start;gap:10px;margin:1.2rem 0;}
.terms-row input[type=checkbox]{width:16px;height:16px;margin-top:3px;accent-color:var(--brown-warm);}
.terms-row label{font-size:12.5px;color:var(--gray-soft);line-height:1.5;cursor:pointer;}
.terms-row a{color:var(--brown-warm);text-decoration:none;font-weight:500;}
.btn-primary{width:100%;height:46px;background:linear-gradient(135deg,#241104 0%,#6f3d0f 54%,#a76013 100%);border:none;border-radius:10px;color:var(--white);font-family:'DM Sans',sans-serif;font-size:15px;font-weight:600;cursor:pointer;margin-top:1rem;margin-bottom:0.75rem;transition:transform 0.18s,filter 0.18s,box-shadow 0.18s;box-shadow:0 10px 22px rgba(98,52,14,0.22);}
.btn-primary:hover{filter:brightness(1.08);transform:translateY(-1px);}
.btn-secondary{width:100%;height:40px;background:none;border:1.5px solid var(--gray-light);border-radius:10px;color:var(--gray-soft);font-family:'DM Sans',sans-serif;font-size:13.5px;cursor:pointer;transition:border-color 0.2s,color 0.2s;}
.btn-secondary:hover{border-color:var(--brown-warm);color:var(--brown-warm);}
.or-divider{display:flex;align-items:center;gap:10px;margin:0.75rem 0;font-size:11px;color:var(--gray-muted);}
.or-divider::before,.or-divider::after{content:'';flex:1;height:1px;background:var(--gray-light);}
.kof-icon{display:inline-flex;align-items:center;justify-content:center;color:var(--gold);line-height:1;}
.kof-icon svg{width:1em;height:1em;stroke:currentColor;fill:none;stroke-width:1.8;stroke-linecap:round;stroke-linejoin:round;display:block;}
.inner-page{position:relative;z-index:5;min-height:calc(100vh - 58px);padding:2rem 2.5rem;background:linear-gradient(180deg,rgba(23,17,12,0.12),rgba(23,17,12,0.32));}
.dash-header{display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:2rem;flex-wrap:wrap;gap:1rem;}
.dash-greet{font-family:'Playfair Display',serif;font-size:30px;color:var(--white);}
.dash-greet span{color:var(--gold);font-style:italic;}
.dash-sub{font-size:13px;color:var(--gray-muted);margin-top:4px;font-style:italic;}
.dash-logout{background:rgba(255,246,234,0.06);border:1px solid var(--line-gold);color:rgba(255,253,248,0.72);border-radius:10px;padding:8px 18px;font-size:13px;cursor:pointer;font-family:'DM Sans',sans-serif;transition:all 0.2s;}
.dash-logout:hover{border-color:var(--amber);color:var(--amber);}
.points-banner{background:linear-gradient(135deg,rgba(74,44,23,0.95),rgba(123,74,45,0.84));border:1px solid rgba(242,192,99,0.28);border-radius:16px;padding:1.75rem 2rem;display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:1rem;margin-bottom:2rem;box-shadow:0 16px 42px rgba(16,9,4,0.28);}
.points-label{font-size:11px;color:rgba(250,240,220,0.6);letter-spacing:0.1em;text-transform:uppercase;margin-bottom:4px;}
.points-value{font-family:'Playfair Display',serif;font-size:40px;color:var(--gold);font-weight:600;line-height:1;}
.points-hint{font-size:12px;color:rgba(250,240,220,0.5);margin-top:4px;font-style:italic;}
.stamp-row{display:flex;gap:8px;flex-wrap:wrap;}
.stamp{width:34px;height:34px;border-radius:50%;border:2px solid rgba(200,134,29,0.4);display:flex;align-items:center;justify-content:center;font-size:15px;}
.stamp.filled{background:var(--amber);border-color:var(--amber);}
.hours-banner{display:flex;align-items:center;gap:14px;background:var(--panel-warm);border:1px solid var(--line-gold);border-radius:14px;padding:13px 18px;margin-bottom:1.5rem;flex-wrap:wrap;box-shadow:0 12px 34px rgba(16,9,4,0.2);}
.hours-status-open{display:inline-flex;align-items:center;gap:6px;background:rgba(111,207,151,0.15);border:1px solid rgba(111,207,151,0.4);border-radius:20px;padding:4px 12px;font-size:13px;color:#6fcf97;font-weight:600;}
.hours-status-closed{display:inline-flex;align-items:center;gap:6px;background:rgba(235,87,87,0.12);border:1px solid rgba(235,87,87,0.35);border-radius:20px;padding:4px 12px;font-size:13px;color:#eb5757;font-weight:600;}
.hours-status-forced-closed{display:inline-flex;align-items:center;gap:6px;background:rgba(235,87,87,0.2);border:1px solid rgba(235,87,87,0.6);border-radius:20px;padding:4px 12px;font-size:13px;color:#ff7070;font-weight:700;}
.hours-dot{width:7px;height:7px;border-radius:50%;display:inline-block;}
.hours-dot-open{background:#6fcf97;}
.hours-dot-closed{background:#eb5757;}
.hours-text{color:var(--gray-muted);font-size:13px;}
.hours-time{color:var(--gold);font-weight:500;}
.forced-closed-banner{background:rgba(235,87,87,0.12);border:1px solid rgba(235,87,87,0.4);border-radius:10px;padding:10px 16px;margin-bottom:1rem;color:#ff7070;font-size:13px;font-weight:600;}
.card-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:16px;margin-bottom:2rem;}
.dash-card{background:var(--panel-warm);border:1px solid var(--line-gold);border-radius:14px;padding:1.5rem;cursor:pointer;transition:background 0.2s,border-color 0.2s,transform 0.15s,box-shadow 0.2s;box-shadow:0 12px 32px rgba(16,9,4,0.18);}
.dash-card:hover{background:rgba(74,44,23,0.68);border-color:rgba(242,192,99,0.46);transform:translateY(-2px);box-shadow:0 18px 42px rgba(16,9,4,0.28);}
.dash-card-icon{font-size:28px;margin-bottom:0.75rem;}
.dash-card-label{font-family:'Playfair Display',serif;font-size:16px;color:var(--white);font-weight:600;margin-bottom:0.25rem;}
.dash-card-hint{font-size:12.5px;color:var(--gray-muted);font-style:italic;}
.page-card{background:var(--panel-warm);border:1px solid var(--line-gold);border-radius:14px;padding:1.5rem 2rem;margin-bottom:1.5rem;box-shadow:0 14px 36px rgba(16,9,4,0.2);}
.page-title{font-family:'Playfair Display',serif;font-size:26px;color:var(--white);margin-bottom:1rem;}
.page-section-title{font-family:'Playfair Display',serif;font-size:18px;color:var(--gold);margin-bottom:0.75rem;}
.back-btn{background:rgba(200,134,29,0.14);border:1px solid rgba(242,192,99,0.32);color:var(--gold);border-radius:10px;padding:8px 16px;font-size:14px;cursor:pointer;font-family:'DM Sans',sans-serif;margin-bottom:1.5rem;transition:all 0.2s;}
.back-btn:hover{background:rgba(200,134,29,0.25);border-color:rgba(242,192,99,0.52);}
.menu-layout{display:grid;grid-template-columns:200px 1fr;gap:1.5rem;}
.cat-btn{display:block;width:100%;margin-bottom:6px;padding:10px 14px;text-align:left;background:rgba(255,246,234,0.045);border:1px solid rgba(242,192,99,0.18);border-radius:9px;color:rgba(255,253,248,0.72);font-family:'DM Sans',sans-serif;font-size:13.5px;cursor:pointer;transition:all 0.2s;}
.cat-btn:hover,.cat-btn.active{background:rgba(200,134,29,0.15);border-color:rgba(200,134,29,0.4);color:var(--gold);}
.menu-item-row{display:grid;grid-template-columns:1fr 110px 120px;align-items:center;gap:8px;background:rgba(42,25,13,0.54);border:1px solid rgba(242,192,99,0.16);border-radius:11px;padding:11px 14px;margin-bottom:8px;box-shadow:0 8px 22px rgba(16,9,4,0.14);}
.menu-item-row.starred-row,.admin-food-row.starred-row,.page-card.starred-row{background:linear-gradient(135deg,rgba(200,134,29,0.22),rgba(74,44,23,0.66));border-color:rgba(242,192,99,0.48);box-shadow:0 10px 26px rgba(200,134,29,0.12);}
.menu-item-name{color:var(--white);font-size:14.5px;font-weight:500;}
.menu-item-price{color:var(--gold);font-size:14px;margin-top:2px;}
.menu-item-status-col{display:flex;align-items:center;justify-content:center;}
.menu-item-btn-col{display:flex;align-items:center;justify-content:flex-end;}
.menu-item-status-ok{color:#6fcf97;font-size:12px;}
.menu-item-status-no{color:#eb5757;font-size:12px;}
.view-btn{padding:6px 14px;background:linear-gradient(135deg,var(--brown-dark),var(--brown-warm));border:1px solid rgba(242,192,99,0.18);border-radius:8px;color:var(--gold);font-size:12.5px;cursor:pointer;font-family:'DM Sans',sans-serif;transition:filter 0.2s,transform 0.15s;}
.view-btn:hover{filter:brightness(1.12);transform:translateY(-1px);}
.admin-grid{display:grid;grid-template-columns:1fr 1fr;gap:16px;margin-bottom:2rem;}
.admin-btn{height:80px;background:var(--panel-warm);border:1px solid var(--line-gold);border-radius:14px;color:var(--white);font-family:'Playfair Display',serif;font-size:16px;cursor:pointer;transition:all 0.2s;box-shadow:0 12px 32px rgba(16,9,4,0.18);}
.admin-btn:hover{background:rgba(200,134,29,0.12);border-color:rgba(200,134,29,0.4);}
.admin-food-row{display:grid;grid-template-columns:1fr 130px 160px;align-items:center;gap:8px;background:rgba(42,25,13,0.54);border:1px solid rgba(242,192,99,0.16);border-radius:11px;padding:11px 14px;margin-bottom:8px;box-shadow:0 8px 22px rgba(16,9,4,0.14);}
.admin-food-status-col{display:flex;align-items:center;justify-content:center;}
.admin-food-btn-col{display:flex;align-items:center;justify-content:flex-end;}
.txn-card{background:rgba(42,25,13,0.54);border:1px solid rgba(242,192,99,0.16);border-radius:11px;padding:14px 16px;margin-bottom:10px;box-shadow:0 8px 22px rgba(16,9,4,0.14);}
.txn-card.undone{opacity:0.74;border-color:rgba(156,152,144,0.22);}
.txn-undone-badge{display:inline-block;margin-top:6px;padding:3px 9px;border-radius:20px;background:rgba(156,152,144,0.14);border:1px solid rgba(156,152,144,0.28);color:var(--gray-muted);font-size:11px;font-weight:700;}
.txn-undo-btn{padding:6px 12px;border-radius:8px;border:1px solid rgba(242,192,99,0.3);background:rgba(200,134,29,0.12);color:var(--gold);font-size:12px;cursor:pointer;font-family:'DM Sans',sans-serif;transition:all 0.2s;}
.txn-undo-btn:hover{background:rgba(200,134,29,0.24);}
.txn-date{font-size:11px;color:var(--gray-muted);}
.txn-amount{color:var(--white);font-size:14px;margin:3px 0;}
.txn-pts-plus{color:#6fcf97;font-size:13px;margin:2px 0;}
.txn-pts-minus{color:#eb5757;font-size:13px;margin:2px 0;}
.txn-stamp{color:var(--amber);font-size:13px;margin:2px 0;}
.cust-card{background:rgba(42,25,13,0.56);border:1px solid rgba(242,192,99,0.18);border-radius:12px;padding:16px 18px;margin-bottom:12px;box-shadow:0 10px 26px rgba(16,9,4,0.16);}
.cust-name{color:var(--white);font-size:15px;font-weight:600;}
.cust-meta{color:var(--gray-muted);font-size:12.5px;margin:2px 0;}
.cust-pts{color:var(--gold);font-size:14px;font-weight:600;}
.cust-action-btn{padding:7px 12px;border-radius:8px;border:1px solid rgba(242,192,99,0.3);background:rgba(200,134,29,0.12);color:var(--gold);font-size:12.5px;cursor:pointer;font-family:'DM Sans',sans-serif;margin-bottom:5px;width:100%;transition:all 0.2s;}
.cust-action-btn:hover{background:rgba(200,134,29,0.24);border-color:rgba(242,192,99,0.5);}
.cust-delete-btn{border-color:rgba(235,87,87,0.42)!important;background:rgba(235,87,87,0.1)!important;color:#ff8585!important;}
.cust-delete-btn:hover{background:rgba(235,87,87,0.2)!important;}
.expand-panel{background:rgba(14,9,5,0.34);border-radius:11px;padding:14px;margin-top:12px;border:1px solid rgba(242,192,99,0.16);}
.kof-num-input{width:100%;height:40px;border:1.5px solid rgba(200,134,29,0.25);border-radius:8px;padding:0 12px;background:rgba(255,255,255,0.06);color:var(--white);font-family:'DM Sans',sans-serif;font-size:14px;outline:none;margin-bottom:8px;}
.kof-num-input:focus{border-color:var(--amber);}
.kof-check-row{display:flex;align-items:center;gap:8px;color:var(--gray-muted);font-size:13px;margin-bottom:6px;}
.kof-check-row input[type=checkbox]{accent-color:var(--amber);width:16px;height:16px;}
.preview-box{background:rgba(200,134,29,0.08);border:1px solid rgba(200,134,29,0.2);border-radius:8px;padding:10px 14px;margin-bottom:10px;}
.preview-box p{color:var(--gray-muted);font-size:13px;margin:2px 0;}
.preview-box .preview-total{color:var(--gold);font-weight:600;font-size:14px;}
.confirm-btn{width:100%;height:42px;background:linear-gradient(135deg,#27ae60,#2ecc71);border:none;border-radius:8px;color:white;font-family:'DM Sans',sans-serif;font-size:14px;cursor:pointer;}
.promo-card{background:rgba(200,134,29,0.1);border:1px solid rgba(242,192,99,0.28);border-radius:12px;padding:16px 18px;margin-bottom:12px;box-shadow:0 10px 28px rgba(16,9,4,0.14);}
.promo-title-text{color:var(--white);font-size:15px;font-weight:600;margin-bottom:8px;}
.promo-detail-text{color:var(--gray-muted);font-size:13px;margin:2px 0;}
.promo-detail-highlight{color:var(--amber-light);font-size:13.5px;font-weight:500;margin:4px 0;}
.promo-divider{border:none;border-top:1px solid rgba(200,134,29,0.18);margin:10px 0;}
.promo-terms-text{color:var(--gray-muted);font-size:12px;font-style:italic;margin-top:6px;}
.promo-badge{display:inline-block;background:rgba(200,134,29,0.2);border-radius:20px;padding:2px 10px;font-size:11px;color:var(--gold);margin-bottom:8px;}
.admin-promo-card{background:rgba(42,25,13,0.56);border:1px solid rgba(242,192,99,0.18);border-radius:12px;padding:14px 16px;margin-bottom:12px;box-shadow:0 10px 26px rgba(16,9,4,0.16);}
.status-active{color:#6fcf97;font-size:11px;font-weight:700;}
.status-sched{color:var(--amber-light);font-size:11px;font-weight:700;}
.status-inactive{color:var(--gray-muted);font-size:11px;font-weight:700;}
.promo-action-btn{width:100%;padding:7px;border-radius:7px;border:1px solid rgba(200,134,29,0.2);background:rgba(255,255,255,0.04);color:var(--gray-muted);font-size:12px;cursor:pointer;font-family:'DM Sans',sans-serif;margin-bottom:4px;transition:all 0.2s;}
.promo-action-btn:hover{background:rgba(200,134,29,0.12);color:var(--gold);}
.promo-del-btn{color:#eb5757!important;}
.edit-danger-btn{padding:8px 14px;border-radius:8px;border:1px solid rgba(235,87,87,0.42);background:rgba(235,87,87,0.1);color:#ff8585;font-size:13px;cursor:pointer;font-family:'DM Sans',sans-serif;}
.edit-danger-btn:hover{background:rgba(235,87,87,0.2);}
.pager-row{display:flex;align-items:center;justify-content:center;gap:8px;flex-wrap:wrap;margin:1.25rem 0 0;}
.pager-btn{padding:7px 13px;border-radius:8px;border:1px solid rgba(242,192,99,0.3);background:rgba(200,134,29,0.1);color:var(--gold);font-size:12.5px;cursor:pointer;font-family:'DM Sans',sans-serif;transition:all 0.2s;}
.pager-btn:hover{background:rgba(200,134,29,0.22);border-color:rgba(242,192,99,0.5);}
.pager-btn.active{background:linear-gradient(135deg,var(--brown-dark),var(--brown-warm));border-color:rgba(242,192,99,0.45);color:var(--white);}
.pager-note{width:100%;text-align:center;color:var(--gray-muted);font-size:12px;margin-top:3px;}
.pts-balance-card{background:linear-gradient(135deg,rgba(74,44,23,0.95),rgba(123,74,45,0.84));border:1px solid rgba(242,192,99,0.28);border-radius:16px;padding:1.5rem 2rem;margin-bottom:1.5rem;box-shadow:0 16px 42px rgba(16,9,4,0.28);}
.pts-name{font-family:'Playfair Display',serif;font-size:22px;color:var(--white);}
.pts-num{font-family:'Playfair Display',serif;font-size:42px;color:var(--gold);font-weight:600;line-height:1;}
.pts-card-num{font-size:12px;color:rgba(250,240,220,0.5);margin-top:4px;}
.expiry-table{width:100%;border-collapse:collapse;font-size:13px;}
.expiry-table th{border-bottom:1px solid rgba(200,134,29,0.2);padding:6px;text-align:left;color:var(--gray-muted);font-size:12px;}
.expiry-table td{padding:6px;border-bottom:1px solid rgba(255,255,255,0.05);color:var(--white);font-size:13px;}
.modal-dialog{margin:6vh auto!important;}
.modal-header,.modal-footer{background:var(--brown-deep)!important;color:var(--gold)!important;border-color:rgba(242,192,99,0.18)!important;padding:14px 18px!important;}
.modal-title{color:var(--gold)!important;font-family:'Playfair Display',serif!important;}
.modal-content{background:linear-gradient(145deg,rgba(55,34,20,0.98),rgba(38,30,26,0.98))!important;color:var(--white)!important;border:1px solid rgba(200,134,29,0.35)!important;border-radius:14px!important;box-shadow:0 24px 70px rgba(0,0,0,0.45)!important;overflow:hidden;}
.modal-body{color:var(--white)!important;padding:18px!important;}
.modal-body label,.modal-body p,.modal-body h4,.modal-body h5{color:var(--white)!important;}
.modal-body input,.modal-body select,.modal-body .form-control{background:rgba(255,255,255,0.08)!important;color:var(--white)!important;border:1px solid rgba(200,134,29,0.3)!important;border-radius:8px!important;min-height:38px!important;}
.modal-body input[type=checkbox]{width:16px;height:16px;accent-color:var(--amber);}
.modal-footer{display:flex!important;align-items:center!important;justify-content:flex-end!important;gap:8px!important;flex-wrap:wrap!important;}
.modal-footer .btn,.modal-footer button{margin:0!important;border-radius:8px!important;padding:8px 14px!important;}
.btn-default{background:rgba(255,255,255,0.08)!important;color:var(--white)!important;border:1px solid rgba(200,134,29,0.2)!important;}
.btn-default:hover{background:rgba(200,134,29,0.15)!important;}
.shiny-notification{background:var(--brown-deep)!important;color:var(--gold)!important;border:1px solid var(--amber)!important;border-radius:10px!important;}
.shiny-notification-message{color:var(--gold)!important;}
.shiny-notification-error{color:#ff6b6b!important;}
.info-block{background:rgba(255,255,255,0.04);border:1px solid rgba(200,134,29,0.15);border-radius:12px;padding:1.25rem 1.5rem;margin-bottom:1rem;}
.info-label{font-size:12px;color:var(--amber);text-transform:uppercase;letter-spacing:0.1em;margin-bottom:6px;}
.info-value{color:var(--white);font-size:15px;}
.info-link{color:var(--gold)!important;font-weight:600;}
.social-links-row{display:flex;gap:10px;flex-wrap:wrap;margin-top:8px;}
.social-link-btn{display:inline-flex;align-items:center;gap:7px;padding:8px 14px;border-radius:8px;font-size:13.5px;font-weight:600;text-decoration:none;transition:opacity 0.2s;}
.social-link-fb{background:rgba(24,119,242,0.18);border:1px solid rgba(24,119,242,0.4);color:#5b9cf6!important;}
.social-link-ig{background:rgba(225,48,108,0.15);border:1px solid rgba(225,48,108,0.4);color:#f06292!important;}
.social-link-tt{background:rgba(0,0,0,0.35);border:1px solid rgba(255,255,255,0.25);color:#ffffff!important;}
.social-link-btn:hover{opacity:0.8;text-decoration:none;}
.promo-stats{display:flex;gap:12px;margin-bottom:1.5rem;flex-wrap:wrap;}
.promo-stat{background:rgba(255,255,255,0.04);border:1px solid rgba(200,134,29,0.15);border-radius:10px;padding:14px 20px;flex:1;min-width:80px;text-align:center;}
.promo-stat-num{font-family:'Playfair Display',serif;font-size:28px;color:var(--gold);font-weight:600;}
.promo-stat-label{font-size:12px;color:var(--gray-muted);}
.create-promo-btn{padding:12px 20px;background:linear-gradient(135deg,var(--brown-dark),var(--brown-warm));border:none;border-radius:10px;color:var(--gold);font-family:'DM Sans',sans-serif;font-size:14px;cursor:pointer;height:100%;width:100%;}
.k-emblem-wrap{width:170px;height:170px;position:relative;margin-bottom:2rem;filter:drop-shadow(0 8px 32px rgba(200,134,29,0.45));display:flex;align-items:center;justify-content:center;}
.force-close-btn{padding:9px 16px;border-radius:8px;font-size:13px;font-weight:600;cursor:pointer;font-family:'DM Sans',sans-serif;border:none;transition:all 0.2s;}
.force-close-btn-red{background:rgba(235,87,87,0.18);border:1px solid rgba(235,87,87,0.5)!important;color:#eb5757;}
.force-close-btn-red:hover{background:rgba(235,87,87,0.3);}
.force-close-btn-green{background:rgba(111,207,151,0.15);border:1px solid rgba(111,207,151,0.45)!important;color:#6fcf97;}
.force-close-btn-green:hover{background:rgba(111,207,151,0.28);}
.feedback-card{background:rgba(255,255,255,0.04);border:1px solid rgba(200,134,29,0.18);border-radius:12px;padding:14px 18px;margin-bottom:10px;}
.feedback-card-time{font-size:11px;color:var(--gray-muted);margin-bottom:6px;}
.feedback-card-text{color:var(--white);font-size:14px;line-height:1.6;}
.feedback-loading{color:var(--gray-muted);font-size:13px;font-style:italic;padding:1rem 0;}
.feedback-refresh-btn{padding:8px 16px;border-radius:8px;background:rgba(200,134,29,0.12);border:1px solid rgba(200,134,29,0.3);color:var(--gold);font-size:13px;cursor:pointer;font-family:'DM Sans',sans-serif;margin-bottom:1rem;transition:background 0.2s;}
.feedback-refresh-btn:hover{background:rgba(200,134,29,0.22);}
.feedback-open-btn{display:inline-flex;align-items:center;gap:7px;padding:9px 18px;border-radius:8px;font-size:13.5px;font-weight:600;text-decoration:none;transition:opacity 0.2s;background:rgba(200,134,29,0.15);border:1px solid rgba(200,134,29,0.4);color:var(--gold)!important;margin-bottom:1rem;}
.feedback-open-btn:hover{opacity:0.8;text-decoration:none;}
.feedback-sheet-btn{display:inline-flex;align-items:center;gap:7px;padding:9px 18px;border-radius:8px;font-size:13.5px;font-weight:600;text-decoration:none;transition:opacity 0.2s;background:rgba(66,133,244,0.15);border:1px solid rgba(66,133,244,0.4);color:#7baaf7!important;margin-bottom:1rem;margin-left:8px;}
.feedback-sheet-btn:hover{opacity:0.8;text-decoration:none;}

.vcard-wrap{perspective:1200px;margin:0 auto 2rem;max-width:420px;}
.vcard{
  width:100%;aspect-ratio:1.586;border-radius:20px;position:relative;overflow:hidden;
  background:linear-gradient(135deg,#1a1a1c 0%,#2e2e32 18%,#4a4a50 34%,#6a6a72 50%,#4e4e56 66%,#323238 82%,#1c1c20 100%);
  box-shadow:0 24px 64px rgba(0,0,0,0.75),0 0 0 1px rgba(255,255,255,0.12),inset 0 1px 0 rgba(255,255,255,0.15);
  padding:26px 28px;display:flex;flex-direction:column;justify-content:space-between;font-family:'DM Sans',sans-serif;
}
.vcard-facets{position:absolute;inset:0;pointer-events:none;background:linear-gradient(115deg,rgba(255,255,255,0.07) 0%,transparent 40%),linear-gradient(245deg,rgba(255,255,255,0.05) 0%,transparent 45%),linear-gradient(200deg,rgba(0,0,0,0.35) 30%,transparent 70%),linear-gradient(20deg,rgba(0,0,0,0.25) 0%,transparent 55%);}
.vcard-shine{position:absolute;inset:0;pointer-events:none;background:linear-gradient(130deg,rgba(255,255,255,0.13) 0%,rgba(255,255,255,0.04) 35%,transparent 55%,rgba(255,255,255,0.03) 85%,rgba(200,200,210,0.06) 100%);}
.vcard-top{display:flex;justify-content:space-between;align-items:flex-start;position:relative;z-index:2;}
.vcard-brand{font-family:'Cinzel',serif;font-size:19px;font-weight:700;color:rgba(255,255,255,0.92);letter-spacing:0.08em;text-shadow:0 1px 6px rgba(0,0,0,0.6);}
.vcard-chip{width:42px;height:32px;border-radius:5px;background:linear-gradient(135deg,#d4c06a,#b09840,#e8d888,#b09840);display:flex;align-items:center;justify-content:center;box-shadow:0 2px 8px rgba(0,0,0,0.5);}
.vcard-mid{position:relative;z-index:2;text-align:center;}
.vcard-k{font-family:'Cinzel',serif;font-size:60px;font-weight:700;color:rgba(255,255,255,0.08);line-height:1;user-select:none;text-shadow:0 2px 12px rgba(0,0,0,0.4);}
.vcard-bottom{position:relative;z-index:2;}
.vcard-num{font-family:'Courier New',monospace;font-size:14px;letter-spacing:0.22em;color:rgba(255,255,255,0.82);margin-bottom:12px;word-spacing:0.4em;text-shadow:0 1px 4px rgba(0,0,0,0.5);}
.vcard-row{display:flex;justify-content:space-between;align-items:flex-end;}
.vcard-holder-label{font-size:9px;color:rgba(255,255,255,0.45);letter-spacing:0.12em;text-transform:uppercase;margin-bottom:2px;}
.vcard-holder-name{font-size:13px;font-weight:600;color:rgba(255,255,255,0.90);letter-spacing:0.06em;text-transform:uppercase;text-shadow:0 1px 4px rgba(0,0,0,0.4);}
.vcard-pts-label{font-size:9px;color:rgba(255,255,255,0.45);letter-spacing:0.12em;text-transform:uppercase;margin-bottom:2px;text-align:right;}
.vcard-pts-val{font-family:'Playfair Display',serif;font-size:20px;color:rgba(230,210,130,0.95);font-weight:600;text-align:right;text-shadow:0 1px 6px rgba(0,0,0,0.5);}
.vcard-tagline{font-size:9px;color:rgba(255,255,255,0.30);letter-spacing:0.18em;text-transform:uppercase;text-align:center;margin-top:8px;}
.vcard-contactless{font-size:18px;color:rgba(255,255,255,0.55);line-height:1;}

.modal-overlay{display:none;position:fixed;inset:0;z-index:9999;background:rgba(10,8,6,0.8);align-items:center;justify-content:center;}
.modal-overlay.open{display:flex;}
.modal-box{background:var(--gray-mid);border:1px solid rgba(200,134,29,0.3);border-radius:18px;padding:2rem 2.25rem;width:90%;max-width:480px;}
.modal-title-kof{font-family:'Cinzel',serif;font-size:20px;color:var(--gold);margin-bottom:1rem;padding-bottom:0.75rem;border-bottom:1px solid rgba(200,134,29,0.2);}
.modal-body-kof{font-size:13.5px;color:var(--gray-muted);line-height:1.75;margin-bottom:0.5rem;}
.modal-body-kof p{margin-bottom:4px;}
@media (max-width:960px){
  .brand-bar{padding:0.55rem 0.85rem;min-height:auto;flex-wrap:nowrap;gap:10px;}
  .brand-logo{flex:0 0 auto;white-space:nowrap;}
  .brand-tagline{flex:1 1 auto;min-width:0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;padding-left:10px;}
  .brand-note{width:auto;margin-left:auto;justify-content:flex-end;flex:0 1 auto;white-space:nowrap;}
  .auth-page{grid-template-columns:1fr;}
  .left-panel{padding:2.4rem 1.1rem 1.3rem;}
  .right-panel{border-left:none;border-top:1px solid rgba(200,134,29,0.12);padding:1.3rem 1rem 2.4rem;}
  .feature-row{grid-template-columns:repeat(2,minmax(130px,1fr));gap:20px 0;}
  .perk:nth-child(2){border-right:none;}
  .menu-layout{grid-template-columns:1fr;}
  .cust-card .col-sm-5{align-items:stretch;margin-top:12px;}
  .cust-action-btn{width:100%;}
}
@media (max-width:640px){
  .brand-bar{padding:0.5rem 0.65rem;gap:7px;}
  .brand-logo{font-size:16px;}
  .brand-tagline{font-size:10.5px;padding-left:8px;}
  .brand-note{font-size:9.5px;line-height:1.2;gap:5px;}
  .brand-note-badge{width:24px;height:27px;flex:0 0 24px;}
  .brand-note-badge svg{width:24px;height:27px;}
  .left-title{font-size:32px;}
  .left-sub{font-size:13px;}
  .k-emblem-wrap{width:132px;height:132px;margin-bottom:1.4rem;}
  .k-emblem-wrap svg{width:132px!important;height:132px!important;}
  .feature-row{grid-template-columns:repeat(4,minmax(0,1fr));gap:0;width:100%;margin-top:1rem;}
  .perk{gap:5px;padding:0 4px;min-width:0;}
  .perk-hex{width:48px;height:48px;margin-bottom:4px;}
  .perk-icon{width:24px;height:24px;font-size:20px;}
  .perk-icon svg{width:24px;height:24px;stroke-width:1.7;}
  .perk-label{font-size:11px;line-height:1.15;}
  .perk-desc{font-size:9.5px;line-height:1.2;}
  .perk{border-right:none!important;}
  .auth-card{padding:1.8rem 1.45rem;border-radius:20px;}
  .inner-page{padding:1.35rem 1rem;}
  .admin-grid{grid-template-columns:1fr;}
  .menu-item-row,.admin-food-row{grid-template-columns:1fr;align-items:flex-start;}
  .menu-item-status-col,.menu-item-btn-col,.admin-food-status-col,.admin-food-btn-col{justify-content:flex-start;}
  .confirm-btn{width:100%;}
  .page-card{padding:1.2rem;}
}
"


hex_bg = tags$div(class="hex-bg",
                  tags$svg(viewBox="0 0 1200 800", preserveAspectRatio="xMidYMid slice", xmlns="http://www.w3.org/2000/svg",
                           tags$defs(tags$pattern(id="hexPat", x="0", y="0", width="90", height="78", patternUnits="userSpaceOnUse",
                                                  tags$polygon(points="45,2 88,24 88,68 45,90 2,68 2,24", fill="none", stroke="rgba(160,98,42,0.13)", `stroke-width`="1")
                           )),
                           tags$rect(width="1200", height="800", fill="url(#hexPat)"),
                           tags$ellipse(cx="200", cy="200", rx="260", ry="200", fill="rgba(120,60,20,0.22)"),
                           tags$ellipse(cx="900", cy="550", rx="300", ry="220", fill="rgba(90,45,15,0.18)")
                  )
)

brand_bar = tags$div(class="brand-bar",
                     tags$span(class="brand-logo", "Kofidence"),
                     tags$span(class="brand-tagline", "coffee + confidence"),
                     tags$div(class="brand-note",
                              tags$span(class="brand-note-badge",
                                        tags$svg(viewBox="0 0 40 44", xmlns="http://www.w3.org/2000/svg",
                                                 tags$polygon(points="20,2 36,11 36,33 20,42 4,33 4,11", fill="rgba(34,20,9,0.72)", stroke="#C8861D", `stroke-width`="1.4"),
                                                 tags$path(d="M20 29 C20 22 14 19 10 18 C11 24 14 28 20 29 Z", fill="none", stroke="#F2C063", `stroke-width`="1.6", `stroke-linecap`="round", `stroke-linejoin`="round"),
                                                 tags$path(d="M20 29 C20 22 26 19 30 18 C29 24 26 28 20 29 Z", fill="none", stroke="#F2C063", `stroke-width`="1.6", `stroke-linecap`="round", `stroke-linejoin`="round"),
                                                 tags$path(d="M20 30 L20 16", fill="none", stroke="#F2C063", `stroke-width`="1.5", `stroke-linecap`="round")
                                        )
                              ),
                              tags$span("Brewed for you.", tags$br(), "Rewards for you.")
                     )
)

grand_k_emblem = tags$svg(
  viewBox="0 0 170 170", xmlns="http://www.w3.org/2000/svg",
  style="width:170px;height:170px;display:block;",
  tags$defs(tags$style(".emblem-k{font-family:'Cinzel',serif;font-weight:700;font-size:110px;fill:#F2C063;dominant-baseline:central;text-anchor:middle;}")),
  tags$polygon(points="85,5 154.64,45 154.64,125 85,165 15.36,125 15.36,45", fill="rgba(20,12,6,0.88)", stroke="#C8861D", `stroke-width`="2"),
  tags$polygon(points="85,30 132.63,57.5 132.63,112.5 85,140 37.37,112.5 37.37,57.5", fill="rgba(42,24,10,0.65)", stroke="rgba(242,192,99,0.3)", `stroke-width`="1"),
  tags$text(x="85", y="88", class="emblem-k", "K")
)

kof_icon = function(name, class="kof-icon") {
  paths = switch(name,
    cup = tagList(
      tags$path(d="M8 10h13v8a6 6 0 0 1-6 6h-1a6 6 0 0 1-6-6z"),
      tags$path(d="M21 12h3a3 3 0 0 1 0 6h-3"),
      tags$path(d="M7 27h16")
    ),
    user = tagList(
      tags$circle(cx="16", cy="10", r="5"),
      tags$path(d="M7 27c1.2-6 16.8-6 18 0"),
      tags$path(d="M9 27h14")
    ),
    gift = tagList(
      tags$rect(x="7", y="12", width="18", height="14", rx="2"),
      tags$path(d="M16 12v14M7 17h18"),
      tags$path(d="M16 12c-4-1-6-5-3-6 2-1 3 2 3 6zM16 12c4-1 6-5 3-6-2-1-3 2-3 6z")
    ),
    star = tagList(tags$path(d="M16 5l3.2 6.5 7.1 1-5.1 5 1.2 7-6.4-3.4-6.4 3.4 1.2-7-5.1-5 7.1-1z")),
    card = tagList(tags$rect(x="5", y="8", width="22", height="16", rx="3"), tags$path(d="M5 13h22M9 19h5")),
    menu = tagList(tags$path(d="M8 7h16M8 14h16M8 21h16")),
    cart = tagList(tags$path(d="M6 7h3l2 12h11l3-8H11"), tags$circle(cx="13", cy="24", r="1.7"), tags$circle(cx="22", cy="24", r="1.7")),
    medal = tagList(tags$circle(cx="16", cy="17", r="6"), tags$path(d="M12 4l4 7 4-7M13 17l2 2 4-4")),
    dots = tagList(tags$circle(cx="9", cy="16", r="1.8"), tags$circle(cx="16", cy="16", r="1.8"), tags$circle(cx="23", cy="16", r="1.8")),
    link = tagList(tags$path(d="M12 17l8-8a4 4 0 0 1 6 6l-3 3"), tags$path(d="M20 15l-8 8a4 4 0 0 1-6-6l3-3")),
    feedback = tagList(tags$path(d="M7 7h18v13H12l-5 5z"), tags$path(d="M11 12h10M11 16h7")),
    tagList(tags$circle(cx="16", cy="16", r="10"))
  )
  tags$span(class=class, tags$svg(viewBox="0 0 32 32", xmlns="http://www.w3.org/2000/svg", paths))
}

social_links = function() {
  tags$div(class="social-links-row",
           tags$a(href="https://www.facebook.com/share/1au4kqcApA/", target="_blank", class="social-link-btn social-link-fb", kof_icon("link"), "Facebook"),
           tags$a(href="https://www.instagram.com/kofi.dence", target="_blank", class="social-link-btn social-link-ig", kof_icon("link"), "Instagram"),
           tags$a(href="https://www.tiktok.com/@kofi.dence?is_from_webapp=1&sender_device=pc", target="_blank", class="social-link-btn social-link-tt", kof_icon("link"), "TikTok")
  )
}

social_links_order = function() {
  tags$div(class="social-links-row",
           tags$a(href="https://www.facebook.com/share/1au4kqcApA/", target="_blank", class="social-link-btn social-link-fb", kof_icon("link"), "Order via Facebook"),
           tags$a(href="https://www.instagram.com/kofi.dence", target="_blank", class="social-link-btn social-link-ig", kof_icon("link"), "Visit on Instagram"),
           tags$a(href="https://www.tiktok.com/@kofi.dence?is_from_webapp=1&sender_device=pc", target="_blank", class="social-link-btn social-link-tt", kof_icon("link"), "TikTok")
  )
}

auth_ui = tags$div(id="authPage", class="auth-page",
                   tags$div(class="left-panel",
                            tags$div(class="k-emblem-wrap", grand_k_emblem),
                            tags$div(class="left-title", "Where every sip", tags$br(), "builds your ", tags$span("confidence")),
                            tags$div(class="left-sub", "quality crafted - daily enjoyed"),
                            tags$div(class="left-divider"),
                            tags$div(class="feature-row",
                                     tags$div(class="perk",
                                              tags$div(class="perk-hex", kof_icon("cup", "perk-icon")),
                                              tags$div(class="perk-label", "Earn points"),
                                              tags$div(class="perk-desc", "on every purchase")
                                     ),
                                     tags$div(class="perk",
                                              tags$div(class="perk-hex", kof_icon("user", "perk-icon")),
                                              tags$div(class="perk-label", "Stamp card"),
                                              tags$div(class="perk-desc", "9 stamps = free drink")
                                     ),
                                     tags$div(class="perk",
                                              tags$div(class="perk-hex", kof_icon("gift", "perk-icon")),
                                              tags$div(class="perk-label", "Exclusive promos"),
                                              tags$div(class="perk-desc", "for members")
                                     ),
                                     tags$div(class="perk",
                                              tags$div(class="perk-hex", kof_icon("star", "perk-icon")),
                                              tags$div(class="perk-label", "First visit bonus"),
                                              tags$div(class="perk-desc", "+2 points")
                                     )
                            )
                   ),
                   tags$div(class="right-panel",
                            tags$div(class="auth-card",
                                     tags$div(class="card-badge", "+ Advantage Card - Member Area"),
                                     tags$div(class="auth-tabs",
                                              tags$button(class="auth-tab-btn active", id="tabLogin",  onclick="switchTab('login')",  "Log In"),
                                              tags$button(class="auth-tab-btn",        id="tabSignup", onclick="switchTab('signup')", "Create Account")
                                     ),
                                     tags$div(class="form-panel active", id="panelLogin",
                                              tags$div(class="form-title", "Welcome back"),
                                              tags$div(class="form-sub",   "Sign in to your Kofidence card"),
                                              tags$label(class="kof-label", "Contact Number"),
                                              tags$input(type="tel",      id="loginContact", class="kof-input", placeholder="09XXXXXXXXX",    maxlength="11"),
                                              tags$label(class="kof-label", "PIN"),
                                              tags$input(type="password", id="loginPin",     class="kof-input", placeholder="Enter your PIN", maxlength="6"),
                                              tags$button(class="btn-primary", onclick="doLogin()", "Log In"),
                                              tags$div(class="or-divider", tags$span("or")),
                                              tags$button(class="btn-secondary", onclick="switchTab('signup')", "Create new account")
                                     ),
                                     tags$div(class="form-panel", id="panelSignup",
                                              tags$div(class="form-title", "Create Account"),
                                              tags$div(class="form-sub",   "Join the Kofidence loyalty program"),
                                              tags$label(class="kof-label", "Full Name"),
                                              tags$input(type="text",     id="signupName",    class="kof-input", placeholder="Your name",    maxlength="30"),
                                              tags$label(class="kof-label", "Contact Number"),
                                              tags$input(type="tel",      id="signupContact", class="kof-input", placeholder="09XXXXXXXXX", maxlength="11"),
                                              tags$label(class="kof-label", "PIN (up to 6 digits)"),
                                              tags$input(type="password", id="signupPin",     class="kof-input", placeholder="Choose a PIN", maxlength="6"),
                                              tags$div(class="terms-row",
                                                       tags$input(type="checkbox", id="agreeTerms"),
                                                       tags$label(`for`="agreeTerms",
                                                                  "I agree to the ",
                                                                  tags$a(href="javascript:void(0)", onclick="openModal('terms')",   "Terms and Condition"),
                                                                  " and ",
                                                                  tags$a(href="javascript:void(0)", onclick="openModal('privacy')", "Privacy Policy")
                                                       )
                                              ),
                                              tags$button(class="btn-primary",   onclick="doSignupJS()",       "Create Account"),
                                              tags$div(class="or-divider", tags$span("or")),
                                              tags$button(class="btn-secondary", onclick="switchTab('login')", "Already have an account")
                                     )
                            )
                   )
)

modals_ui = tagList(
  tags$div(class="modal-overlay", id="modalTerms",
           tags$div(class="modal-box", style="max-height:80vh;overflow-y:auto;",
                    tags$div(class="modal-title-kof", "Terms and Condition"),
                    tags$div(class="modal-body-kof",
                             tags$p(style="font-weight:600;margin-bottom:8px;", "Kofidence Cafe Advantage Card â€” Terms and Conditions"),
                             tags$p("By registering for and using the Kofidence Advantage Card, you agree to the following terms:"),
                             tags$br(),
                             tags$p(style="font-weight:600;", "1. Membership"),
                             tags$p("Membership is open to individuals 18 years of age or older. Each person may hold only one active Advantage Card. The card is non-transferable and is for personal use only."),
                             tags$br(),
                             tags$p(style="font-weight:600;", "2. Points and Stamps"),
                             tags$p("Points are earned at a rate of 1 point for every P30 spent. Bonus points may be awarded for starred items, combo purchases, or special promotions. Points expire 90 days from the date they are earned. Stamps are collected separately; 9 stamps entitle the holder to one free drink. Stamps and points have no cash value and cannot be transferred or converted."),
                             tags$br(),
                             tags$p(style="font-weight:600;", "3. Rewards and Redemption"),
                             tags$p("Rewards may only be redeemed at participating Kofidence Cafe locations. Only one reward may be redeemed per transaction. Kofidence Cafe reserves the right to modify, suspend, or terminate any reward at any time without prior notice."),
                             tags$br(),
                             tags$p(style="font-weight:600;", "4. Account Responsibility"),
                             tags$p("You are responsible for keeping your PIN confidential. Kofidence Cafe is not liable for unauthorized use of your account. If you suspect unauthorized activity, contact us immediately."),
                             tags$br(),
                             tags$p(style="font-weight:600;", "5. Program Changes"),
                             tags$p("Kofidence Cafe reserves the right to change, suspend, or discontinue the Advantage Card program at any time."),
                             tags$br(),
                             tags$p(style="font-weight:600;", "6. Termination"),
                             tags$p("Kofidence Cafe may suspend or terminate your membership if these terms are violated. Upon termination, all accumulated points and stamps are forfeited.")
                    ),
                    tags$button(class="btn-primary", style="margin-top:1rem;", onclick="closeModal('terms')", "Close")
           )
  ),
  tags$div(class="modal-overlay", id="modalPrivacy",
           tags$div(class="modal-box", style="max-height:80vh;overflow-y:auto;",
                    tags$div(class="modal-title-kof", "Privacy Policy"),
                    tags$div(class="modal-body-kof",
                             tags$p(style="font-weight:600;margin-bottom:8px;", "Kofidence Cafe Advantage Card â€” Privacy Policy"),
                             tags$p("Kofidence Cafe respects your privacy and is committed to protecting your personal information."),
                             tags$br(),
                             tags$p(style="font-weight:600;", "1. Information We Collect"),
                             tags$p("When you register, we collect your full name, contact number, and a PIN you create. We also record your transaction history, points earned or redeemed, and stamp activity."),
                             tags$br(),
                             tags$p(style="font-weight:600;", "2. How We Use Your Information"),
                             tags$p("Your data is used to create and maintain your account, track your points and stamps, process redemptions, send updates, and improve our services."),
                             tags$br(),
                             tags$p(style="font-weight:600;", "3. Data Sharing"),
                             tags$p("We do not sell or share your personal information with third parties for commercial purposes. Access is limited to authorized Kofidence Cafe staff."),
                             tags$br(),
                             tags$p(style="font-weight:600;", "4. Data Storage"),
                             tags$p("Your data is stored securely. We retain your data only as long as your account is active or as required by law."),
                             tags$br(),
                             tags$p(style="font-weight:600;", "5. Your Rights"),
                             tags$p("You may request access to, correction of, or deletion of your personal data at any time by contacting us."),
                             tags$br(),
                             tags$p(style="color:#9C9890;font-style:italic;", "Contact: 09389792980 | Ground Floor, Ampil Building, A Bonifacio Avenue, Cainta.")
                    ),
                    tags$button(class="btn-primary", style="margin-top:1rem;", onclick="closeModal('privacy')", "Close")
           )
  )
)

app_js = "
function switchTab(tab){
  document.getElementById('panelLogin').classList.toggle('active',tab==='login');
  document.getElementById('panelSignup').classList.toggle('active',tab==='signup');
  document.getElementById('tabLogin').classList.toggle('active',tab==='login');
  document.getElementById('tabSignup').classList.toggle('active',tab==='signup');
}
function openModal(which){
  var id='modal'+which.charAt(0).toUpperCase()+which.slice(1);
  document.getElementById(id).classList.add('open');
}
function closeModal(which){
  var id='modal'+which.charAt(0).toUpperCase()+which.slice(1);
  document.getElementById(id).classList.remove('open');
}
function doLogin(){
  var c=document.getElementById('loginContact').value.trim();
  var p=document.getElementById('loginPin').value.trim();
  Shiny.setInputValue('js_login',{contact:c,pin:p,ts:Math.random()});
}
function doSignupJS(){
  var n=document.getElementById('signupName').value.trim();
  var c=document.getElementById('signupContact').value.trim();
  var p=document.getElementById('signupPin').value.trim();
  var a=document.getElementById('agreeTerms').checked;
  Shiny.setInputValue('js_signup',{name:n,contact:c,pin:p,agreed:a,ts:Math.random()});
}
Shiny.addCustomMessageHandler('switchToLogin',function(msg){
  document.getElementById('authPage').style.display='';
  switchTab('login');
});
Shiny.addCustomMessageHandler('clearSignup',function(msg){
  document.getElementById('signupName').value='';
  document.getElementById('signupContact').value='';
  document.getElementById('signupPin').value='';
  document.getElementById('agreeTerms').checked=false;
  switchTab('login');
});
Shiny.addCustomMessageHandler('hideAuth',function(msg){
  document.getElementById('authPage').style.display='none';
});
"


ui = fluidPage(
  tags$head(
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
    tags$link(rel="icon", type="image/svg+xml", href="favicon.svg?v=2"),
    tags$style(HTML(app_css)),
    tags$style(HTML("
      .shiny-input-container{margin-bottom:0!important;}
      #menu_search_box input,#admin_food_search_box input,#admin_search input,#admin_hist_search input{
        width:100%!important;height:42px!important;
        border:1.5px solid rgba(242,192,99,0.32)!important;border-radius:11px!important;
        padding:0 14px!important;background:rgba(42,25,13,0.54)!important;
        color:var(--white,#FFFDF8)!important;font-family:'DM Sans',sans-serif!important;
        font-size:14px!important;outline:none!important;margin-bottom:1rem!important;display:block!important;
        box-shadow:0 8px 22px rgba(16,9,4,0.14)!important;
      }
      #menu_search_box input::placeholder,#admin_food_search_box input::placeholder,
      #admin_search input::placeholder,#admin_hist_search input::placeholder{color:var(--gray-muted,#9C9890)!important;}
      #menu_search_box input:focus,#admin_food_search_box input:focus,
      #admin_search input:focus,#admin_hist_search input:focus{border-color:var(--gold,#F2C063)!important;box-shadow:0 0 0 3px rgba(200,134,29,0.14)!important;}
      #menu_search_box label,#admin_food_search_box label,
      #admin_search label,#admin_hist_search label{display:none!important;}
    ")),
    tags$script(HTML(app_js))
  ),
  hex_bg,
  brand_bar,
  auth_ui,
  modals_ui,
  uiOutput("main_page")
)


server = function(input, output, session) {
  
  saved = load_data()
  
  users            = reactiveVal(if (!is.null(saved$users))            saved$users            else list())
  all_transactions = reactiveVal(if (!is.null(saved$all_transactions)) saved$all_transactions else list())
  promos           = reactiveVal(if (!is.null(saved$promos))           saved$promos           else list())
  promo_id_ctr     = reactiveVal(if (!is.null(saved$promo_id_ctr))     saved$promo_id_ctr     else 1)
  store_open_hour  = reactiveVal(if (!is.null(saved$store_open_hour))  saved$store_open_hour  else 14)
  store_close_hour = reactiveVal(if (!is.null(saved$store_close_hour)) saved$store_close_hour else 0)
  store_force_close= reactiveVal(if (!is.null(saved$store_force_close))saved$store_force_close else FALSE)
  force_close_note = reactiveVal(if (!is.null(saved$force_close_note)) saved$force_close_note else "")
  saved_menu = if (!is.null(saved$menu_items) && length(saved$menu_items) > 0) saved$menu_items else default_menu_items
  menu_items       = reactiveVal(saved_menu)
  
  current_user  = reactiveVal(NULL)
  current_page  = reactiveVal("none")
  selected_food = reactiveVal(NULL)
  selected_cat  = reactiveVal("Starred Drinks")
  expanded_cust = reactiveVal(NULL)
  admin_food_page = reactiveVal(1)
  admin_cust_page = reactiveVal(1)
  admin_hist_page = reactiveVal(1)
  admin_promo_page = reactiveVal(1)
  
  observe({
    save_data(
      users(), all_transactions(), promos(), promo_id_ctr(),
      store_open_hour(), store_close_hour(), store_force_close(),
      force_close_note(), menu_items()
    )
  })
  
  go_to = function(pg) current_page(pg)
  
  effective_open = function() {
    if (isTRUE(store_force_close())) return(FALSE)
    is_store_open(store_open_hour(), store_close_hour())
  }
  
  get_menu_search = function() tolower(trimws(if (!is.null(input$menu_search_box))       input$menu_search_box       else ""))
  get_food_search = function() tolower(trimws(if (!is.null(input$admin_food_search_box)) input$admin_food_search_box else ""))
  get_cust_search = function() tolower(trimws(if (!is.null(input$admin_search))          input$admin_search          else ""))
  get_hist_search = function() tolower(trimws(if (!is.null(input$admin_hist_search))     input$admin_hist_search     else ""))
  page_slice = function(n, page, per_page=10) {
    if (n <= 0) return(integer(0))
    max_page = max(1, ceiling(n / per_page))
    page = max(1, min(as.integer(page %||% 1), max_page))
    seq.int((page - 1) * per_page + 1, min(n, page * per_page))
  }
  pager_ui = function(total, page, input_name, per_page=10) {
    max_page = max(1, ceiling(total / per_page))
    page = max(1, min(as.integer(page %||% 1), max_page))
    if (max_page <= 1) return(NULL)
    tags$div(class="pager-row",
             tags$button(class="pager-btn", onclick=paste0("Shiny.setInputValue('",input_name,"',",max(1,page-1),",{priority:'event'})"), "Prev"),
             tagList(lapply(seq_len(max_page), function(pg)
               tags$button(class=paste("pager-btn", if (pg==page) "active" else ""),
                           onclick=paste0("Shiny.setInputValue('",input_name,"',",pg,",{priority:'event'})"), pg)
             )),
             tags$button(class="pager-btn", onclick=paste0("Shiny.setInputValue('",input_name,"',",min(max_page,page+1),",{priority:'event'})"), "Next"),
             tags$div(class="pager-note", paste0("Showing ", ((page-1)*per_page)+1, "-", min(total, page*per_page), " of ", total))
    )
  }
  
  hours_status_widget = function() {
    oh = store_open_hour(); ch = store_close_hour()
    forced = isTRUE(store_force_close()); open_now = effective_open()
    open_label = format_hour(oh); close_label = format_hour(ch); fnote = force_close_note()
    tags$div(class="hours-banner",
             if (forced)        tags$div(class="hours-status-forced-closed", tags$span(class="hours-dot hours-dot-closed"), "Temporarily Closed")
             else if (open_now) tags$div(class="hours-status-open",          tags$span(class="hours-dot hours-dot-open"),   "Open Now")
             else               tags$div(class="hours-status-closed",        tags$span(class="hours-dot hours-dot-closed"), "Closed"),
             tags$div(class="hours-text",
                      if (forced && nchar(fnote) > 0)
                        paste0("Notice: ", fnote, " \u2022 Hours: ", open_label, " \u2013 ", close_label)
                      else
                        paste0("Today's hours: ", open_label, " \u2013 ", close_label, " \u2022 Open daily")
             )
    )
  }
  
  make_virtual_card = function(u) {
    pts = valid_points(u)
    card_num_padded = sprintf("%04d", as.integer(u$card))
    formatted_num = paste("2023", "0925", card_num_padded)
    tags$div(class="vcard-wrap",
             tags$div(class="vcard",
                      tags$div(class="vcard-facets"),
                      tags$div(class="vcard-shine"),
                      tags$div(class="vcard-top",
                               tags$div(class="vcard-brand", "KOFIDENCE"),
                               tags$div(style="display:flex;align-items:center;gap:8px;",
                                        tags$div(class="vcard-contactless", kof_icon("link")),
                                        tags$div(class="vcard-chip",
                                                 tags$svg(viewBox="0 0 36 26", xmlns="http://www.w3.org/2000/svg",
                                                          tags$rect(x="0",  y="0",  width="36", height="26", rx="3", fill="#b8922a"),
                                                          tags$rect(x="12", y="0",  width="12", height="26", fill="rgba(255,220,100,0.3)"),
                                                          tags$rect(x="0",  y="8",  width="36", height="10", fill="rgba(255,220,100,0.25)"),
                                                          tags$line(x1="12",y1="0", x2="12",y2="26", stroke="rgba(200,160,50,0.5)", `stroke-width`="0.5"),
                                                          tags$line(x1="24",y1="0", x2="24",y2="26", stroke="rgba(200,160,50,0.5)", `stroke-width`="0.5"),
                                                          tags$line(x1="0", y1="8", x2="36",y2="8",  stroke="rgba(200,160,50,0.5)", `stroke-width`="0.5"),
                                                          tags$line(x1="0", y1="18",x2="36",y2="18", stroke="rgba(200,160,50,0.5)", `stroke-width`="0.5")
                                                 )
                                        )
                               )
                      ),
                      tags$div(class="vcard-mid", tags$div(class="vcard-k", "K")),
                      tags$div(class="vcard-bottom",
                               tags$div(class="vcard-num", formatted_num),
                               tags$div(class="vcard-row",
                                        tags$div(
                                          tags$div(class="vcard-holder-label", "Card Holder"),
                                          tags$div(class="vcard-holder-name",  toupper(u$name))
                                        ),
                                        tags$div(
                                          tags$div(class="vcard-pts-label", "Points"),
                                          tags$div(class="vcard-pts-val",   pts)
                                        )
                               ),
                               tags$div(class="vcard-tagline", "Advantage Card \u2022 Est. 2023")
                      )
             )
    )
  }
  
  observeEvent(input$js_login, {
    d = input$js_login; c = trimws(d$contact); p = trimws(d$pin)
    if (nchar(c) == 0 || nchar(p) == 0) { showNotification("Please fill in all fields.", type="error"); return() }
    if (c == "1" && p == "1") {
      showNotification("Welcome, Admin!", type="message")
      session$sendCustomMessage("hideAuth", ""); go_to("admin"); return()
    }
    if (nchar(c) != 11) { showNotification("Contact must be 11 digits.", type="error"); return() }
    found = FALSE
    for (u in users()) {
      if (u$contact == c && u$pin == p) { found = TRUE; current_user(u); break }
    }
    if (!found) { showNotification("Wrong contact or PIN.", type="error"); return() }
    showNotification(paste("Welcome,", current_user()$name), type="message")
    session$sendCustomMessage("hideAuth", ""); go_to("dashboard")
  })
  
  observeEvent(input$js_signup, {
    d = input$js_signup
    name    = trimws(d$name); contact = trimws(d$contact)
    pin     = trimws(d$pin);  agreed  = isTRUE(d$agreed)
    if (nchar(name)==0||nchar(contact)==0||nchar(pin)==0) { showNotification("Please fill in all fields.", type="error"); return() }
    if (!agreed)                               { showNotification("Please agree to Terms and Privacy Policy.", type="error"); return() }
    if (nchar(name) > 30)                      { showNotification("Name must be 30 chars or less.", type="error"); return() }
    if (nchar(contact)!=11||grepl("[^0-9]",contact)) { showNotification("Contact must be exactly 11 digits.", type="error"); return() }
    if (nchar(pin)<1||nchar(pin)>6||grepl("[^0-9]",pin)) { showNotification("PIN must be 1-6 digits only.", type="error"); return() }
    if (any(sapply(users(), function(u) u$contact == contact))) { showNotification("Contact already registered.", type="error"); return() }
    card  = next_card_number(users())
    boost = data.frame(pts=2, earned="First Visit Boost", expires=as.character(Sys.time()+90*86400), stringsAsFactors=FALSE)
    new_user = list(name=name, contact=contact, pin=pin, card=card, stamps=0, points_log=boost)
    users(append(users(), list(new_user)))
    showNotification(paste0("Welcome, ", name, "! Card #", card, " | +2 First Visit Points!"), type="message")
    session$sendCustomMessage("clearSignup", "")
  })
  
  output$main_page = renderUI({
    pg = current_page()
    if (pg == "none") return(NULL)
    
    wrap = function(...) tags$div(class="inner-page", tags$style(HTML("body{overflow-x:hidden;}")), ...)
    
    if (pg == "dashboard") {
      u = current_user(); if (is.null(u)) return(NULL)
      pts = valid_points(u)
      stamp_html = lapply(1:9, function(i)
        tags$div(class=if(i<=u$stamps)"stamp filled" else "stamp", if(i<=u$stamps)"+" else "o")
      )
      active_promos = Filter(is_promo_active, promos())

      promo_section = if (length(active_promos) > 0)
        tags$div(style="margin-top:1.5rem;",
                 tags$div(class="page-section-title", paste0("Current Promos (", length(active_promos), ")")),
                 tagList(lapply(active_promos, function(p) {
                   detail_lines = promo_detail_lines(p)
                   has_terms    = !is.null(p$terms) && nchar(trimws(p$terms)) > 0
                   
                   tags$div(class="promo-card",
                            # Badge row: type pill
                            tags$div(class="promo-badge", promo_type_labels[p$type]),
                            # Title
                            tags$div(class="promo-title-text", p$title),
                            # Detail lines (highlighted)
                            tagList(lapply(detail_lines, function(ln)
                              tags$div(class="promo-detail-highlight", ln)
                            )),
                            # Terms (if any)
                            if (has_terms) tagList(
                              tags$hr(class="promo-divider"),
                              tags$div(class="promo-terms-text",
                                       tags$span(style="color:var(--amber);font-weight:600;", "Terms: "),
                                       p$terms
                              )
                            )
                   )
                 }))
        ) else NULL
      
      wrap(
        tags$div(class="dash-header",
                 tags$div(
                   tags$div(class="dash-greet", "Good day, ", tags$span(u$name)),
                   tags$div(class="dash-sub", paste0("Card #", u$card, " - Your loyalty dashboard"))
                 ),
                 tags$button(class="dash-logout", onclick="Shiny.setInputValue('do_logout',Math.random())", "Log Out")
        ),
        hours_status_widget(),
        tags$div(class="points-banner",
                 tags$div(
                   tags$div(class="points-label", "VALID POINTS"),
                   tags$div(class="points-value", pts),
                   tags$div(class="points-hint",  "P30 spent = 1 point - expires in 90 days")
                 ),
                 tags$div(
                   tags$div(class="points-label", style="margin-bottom:10px;", "STAMP CARD"),
                   tags$div(class="stamp-row", tagList(stamp_html)),
                   tags$div(class="points-hint", style="margin-top:8px;", "9 stamps = 1 free drink")
                 )
        ),
        tags$div(class="card-grid",
                 tags$div(class="dash-card", onclick="Shiny.setInputValue('go_vcard',Math.random())",
                          tags$div(class="dash-card-icon", kof_icon("card")),
                          tags$div(class="dash-card-label", "My Card"),
                          tags$div(class="dash-card-hint",  "View your virtual advantage card")
                 ),
                 tags$div(class="dash-card", onclick="Shiny.setInputValue('go_menu',Math.random())",
                          tags$div(class="dash-card-icon", kof_icon("menu")),
                          tags$div(class="dash-card-label", "Menu"),
                          tags$div(class="dash-card-hint",  "Browse all drinks and snacks")
                 ),
                 tags$div(class="dash-card", onclick="Shiny.setInputValue('go_delivery',Math.random())",
                          tags$div(class="dash-card-icon", kof_icon("cart")),
                          tags$div(class="dash-card-label", "Delivery"),
                          tags$div(class="dash-card-hint",  "Order via Facebook or call")
                 ),
                 tags$div(class="dash-card", onclick="Shiny.setInputValue('go_points',Math.random())",
                          tags$div(class="dash-card-icon", kof_icon("medal")),
                          tags$div(class="dash-card-label", "Points"),
                          tags$div(class="dash-card-hint",  "Rewards and transaction history")
                 ),
                 tags$div(class="dash-card", onclick="Shiny.setInputValue('go_more',Math.random())",
                          tags$div(class="dash-card-icon", kof_icon("dots")),
                          tags$div(class="dash-card-label", "More"),
                          tags$div(class="dash-card-hint",  "Account, about us and feedback")
                 )
        ),
        promo_section
      )
      
    } else if (pg == "vcard") {
      u = current_user(); if (is.null(u)) return(NULL)
      pts = valid_points(u)
      wrap(
        actionButton("back_vcard", "Back", class="back-btn"),
        tags$div(class="page-title", "My Advantage Card"),
        make_virtual_card(u),
        tags$div(class="page-card",
                 tags$div(class="page-section-title", "Card Details"),
                 tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:14px;",
                          tags$div(tags$div(class="info-label","Card Holder"),  tags$div(class="info-value", u$name)),
                          tags$div(tags$div(class="info-label","Card Number"),  tags$div(class="info-value", paste0("#", u$card))),
                          tags$div(tags$div(class="info-label","Valid Points"), tags$div(class="info-value", style="color:var(--gold);font-size:20px;font-weight:700;", pts)),
                          tags$div(tags$div(class="info-label","Stamps"),       tags$div(class="info-value", paste0(u$stamps, " / 9")))
                 )
        ),
        tags$div(class="page-card",
                 tags$div(class="page-section-title", "How to use your card"),
                 tags$p(style="color:var(--gray-muted);font-size:13.5px;line-height:1.8;",
                        "Show this card to our staff at Kofidence Cafe to:", tags$br(),
                        "- Earn points on every purchase (P30 = 1 point)", tags$br(),
                        "- Collect stamps (9 stamps = 1 FREE drink)", tags$br(),
                        "- Redeem points for rewards", tags$br(),
                        "- Take advantage of special promos"
                 )
        )
      )
      
    } else if (pg == "menu") {
      srch = get_menu_search()
      cat  = selected_cat()
      items = menu_items()
      show_it = if (cat != "All") Filter(function(x) x$cat == cat, items) else items
      if (nchar(srch) > 0)
        show_it = Filter(function(x) grepl(srch, tolower(x$name), fixed=TRUE), show_it)
      
      item_rows = if (length(show_it) == 0)
        tags$p(style="color:var(--gray-muted);padding:1rem 0;", "No items found.")
      else tagList(lapply(show_it, function(it) {
        st   = if (it$avail) "Available" else "Not Available"
        sc   = if (it$avail) "menu-item-status-ok" else "menu-item-status-no"
        row_cls = paste("menu-item-row", if (it$cat == "Starred Drinks") "starred-row" else "")
        tags$div(class=row_cls,
                 tags$div(
                   tags$div(class="menu-item-name",  it$name),
                   tags$div(class="menu-item-price", paste0("P", it$price))
                 ),
                 tags$div(class="menu-item-status-col", tags$span(class=sc, st)),
                 tags$div(class="menu-item-btn-col",    actionButton(paste0("view_food_", it$id), "View", class="view-btn"))
        )
      }))
      
      cats = c("Starred Drinks","All","Espresso Iced","Espresso Hot",
               "Ice Blended Espresso","Ice Blended Cream","Non-Coffee","Snacks","Add Ons")
      cat_btns = tagList(lapply(cats, function(cn) {
        cls = paste0("cat-btn", if (selected_cat() == cn) " active" else "")
        actionButton(paste0("cat_", gsub("[^A-Za-z0-9]", "_", cn)), cn, class=cls)
      }))
      
      wrap(
        actionButton("back_menu", "Back", class="back-btn"),
        tags$div(class="page-title", "Menu"),
        div(id="menu_search_box",
            textInput("menu_search_box", NULL, value=isolate(input$menu_search_box) %||% "",
                      placeholder="Search drinks or snacks...", width="100%")
        ),
        tags$div(class="menu-layout",
                 tags$div(tags$div(class="page-section-title","Categories"), cat_btns),
                 tags$div(tags$div(class="page-section-title","Items"),      item_rows)
        )
      )
      
    } else if (pg == "food_view") {
      food = selected_food()
      if (is.null(food)) return(wrap(actionButton("back_foodview","Back",class="back-btn"), tags$p("No food selected.")))
      sc   = if (food$avail) "color:#6fcf97;" else "color:#eb5757;"
      st   = if (food$avail) "Available" else "Not Available"
      wrap(
        actionButton("back_foodview", "Back", class="back-btn"),
        tags$div(class=paste("page-card", if (food$cat == "Starred Drinks") "starred-row" else ""),
                 tags$div(class="page-title", food$name),
                 tags$div(style="color:var(--gray-muted);font-size:14px;margin-bottom:8px;",  paste("Category:", food$cat)),
                 tags$div(style="font-family:'Playfair Display',serif;font-size:32px;color:var(--gold);margin-bottom:8px;", paste0("P", food$price)),
                 tags$div(style=paste0(sc,"font-size:14px;margin-bottom:1.5rem;"), st),
                 tags$hr(style="border-color:rgba(200,134,29,0.2);"),
                 tags$div(style="color:var(--gray-muted);font-size:14px;margin-top:1rem;", "To order, visit us on Facebook, Instagram, or TikTok!"),
                 social_links_order()
        )
      )
      
    } else if (pg == "delivery") {
      wrap(
        actionButton("back_delivery", "Back", class="back-btn"),
        tags$div(class="page-title", "How to Order"),
        tags$div(class="info-block", tags$div(class="info-label","Contact Us"),   tags$div(class="info-value","09389792980")),
        tags$div(class="info-block", tags$div(class="info-label","Our Location"), tags$div(class="info-value","Ground Floor, Ampil Building, A Bonifacio Avenue, Cainta")),
        tags$div(class="info-block",
                 tags$div(class="info-label", "Order Online"),
                 tags$div(class="info-value", "Visit us on Facebook, Instagram, or TikTok:"),
                 social_links_order()
        )
      )
      
    } else if (pg == "points") {
      u    = current_user(); pts  = valid_points(u); vlog = valid_log(u)
      txns = Filter(function(t) t$contact == u$contact, all_transactions())
      stamp_html2 = lapply(1:9, function(i)
        tags$div(class=if(i<=u$stamps)"stamp filled" else "stamp", if(i<=u$stamps)"+" else "o")
      )
      expiry_table = if (nrow(vlog) == 0)
        tags$p(style="color:var(--gray-muted);", "No active points.")
      else {
        show_df = head(vlog, 3)
        show_df$expires = format(as.POSIXct(show_df$expires), "%b %d, %Y")
        tags$table(class="expiry-table",
                   tags$thead(tags$tr(tags$th("Points"), tags$th("Earned For"), tags$th("Expires"))),
                   tags$tbody(lapply(seq_len(nrow(show_df)), function(i)
                     tags$tr(tags$td(show_df$pts[i]), tags$td(show_df$earned[i]), tags$td(show_df$expires[i]))
                   ))
        )
      }
      txn_html = if (length(txns) == 0)
        tags$p(style="color:var(--gray-muted);", "No transactions yet.")
      else tagList(lapply(rev(txns), function(t)
        tags$div(class=paste("txn-card", if (isTRUE(t$undone)) "undone" else ""),
                 tags$div(class="txn-date",   t$datetime),
                 if (!is.null(t$amount)       && t$amount > 0)       tags$div(class="txn-amount",    paste0("Spent: P", t$amount)),
                 if (!is.null(t$pts_earned)   && t$pts_earned > 0)   tags$div(class="txn-pts-plus",  paste0("Points earned: +", t$pts_earned)),
                 if (!is.null(t$pts_deducted) && t$pts_deducted > 0) tags$div(class="txn-pts-minus", paste0("Points used: -", t$pts_deducted, " (", t$reward_redeemed, ")")),
                 if (isTRUE(t$stamp_added))                          tags$div(class="txn-stamp",     "Stamp added!"),
                 if (isTRUE(t$undone))                               tags$div(class="txn-undone-badge", paste("Undone", t$undone_at %||% ""))
        )
      ))
      wrap(
        fluidRow(
          column(9, actionButton("back_points", "Back", class="back-btn")),
          column(3, tags$div(style="text-align:right;", actionButton("btn_help", "? Help", class="cust-action-btn", style="width:auto;")))
        ),
        tags$div(class="pts-balance-card",
                 tags$div(class="pts-name",     paste("Card:", u$name)),
                 tags$div(class="pts-num",      pts),
                 tags$div(class="pts-card-num", paste0("Card #", u$card, " - Valid Points"))
        ),
        tags$div(class="page-card", tags$div(class="page-section-title","Points Expiry"),      expiry_table),
        tags$div(class="page-card", tags$div(class="page-section-title","Loyalty Stamp Card"), tags$div(class="stamp-row", tagList(stamp_html2)), tags$p(style="color:var(--gray-muted);font-size:12px;margin-top:8px;","9 stamps = 1 FREE drink!")),
        tags$div(class="page-card", tags$div(class="page-section-title","Transaction History"), txn_html)
      )
      
    } else if (pg == "about") {
      wrap(
        actionButton("back_about", "Back", class="back-btn"),
        tags$div(class="page-title", "About Us"),
        tags$div(class="info-block",
                 tags$p(style="color:var(--white);font-size:14.5px;line-height:1.7;",
                        "Kofidence is a cozy coffee kiosk serving quality-crafted drinks at prices that stay friendly, because great coffee should be enjoyed every day."),
                 tags$p(style="color:var(--gray-muted);font-size:13.5px;margin-top:10px;",
                        "Started as a home-based venture in 2023. By September 2025, we opened our own kiosk.")
        ),
        tags$div(class="info-block", tags$div(class="info-label","Location"), tags$div(class="info-value","Ground Floor, Ampil Building, A Bonifacio Avenue, Cainta")),
        tags$div(class="info-block", tags$div(class="info-label","Contact"),  tags$div(class="info-value","09389792980")),
        tags$div(class="info-block", tags$div(class="info-label","Follow Us"), social_links())
      )
      
    } else if (pg == "feedback") {
      wrap(
        actionButton("back_feedback", "Back", class="back-btn"),
        tags$div(class="page-card",
                 tags$div(class="page-title", "Feedback"),
                 tags$p(style="color:var(--white);font-size:15px;margin-bottom:1.25rem;",
                        "We value your input! Share your thoughts with us:"),
                 tags$a(
                   href   = CUSTOMER_FEEDBACK_URL,
                   target = "_blank",
                   rel    = "noopener noreferrer",
                   class  = "feedback-open-btn",
                   kof_icon("feedback"), "Open Feedback Form"
                 )
        )
      )
      
    } else if (pg == "admin") {
      oh = store_open_hour(); ch = store_close_hour()
      forced = isTRUE(store_force_close()); open_now = effective_open()
      open_label = format_hour(oh); close_label = format_hour(ch); fnote = force_close_note()
      force_close_section = tags$div(style="margin-bottom:1.5rem;",
                                     tags$div(class="hours-banner",
                                              if (forced)        tags$div(class="hours-status-forced-closed", tags$span(class="hours-dot hours-dot-closed"), "Temporarily Closed")
                                              else if (open_now) tags$div(class="hours-status-open",          tags$span(class="hours-dot hours-dot-open"),   "Open Now")
                                              else               tags$div(class="hours-status-closed",        tags$span(class="hours-dot hours-dot-closed"), "Closed"),
                                              tags$div(class="hours-text", "Hours: ", tags$span(class="hours-time", paste0(open_label, " \u2013 ", close_label)),
                                                       if (forced && nchar(fnote) > 0) paste0(" \u2022 Reason: ", fnote)
                                              ),
                                              tags$div(style="margin-left:auto;display:flex;gap:8px;flex-wrap:wrap;",
                                                       actionButton("edit_hours_btn", "Edit Hours", class="cust-action-btn", style="width:auto;margin-bottom:0;"),
                                                       if (forced) actionButton("reopen_store_btn", "Reopen Store", class="force-close-btn force-close-btn-green")
                                                       else        actionButton("force_close_btn",  "Force Close",  class="force-close-btn force-close-btn-red")
                                              )
                                     ),
                                     if (forced) tags$div(class="forced-closed-banner",
                                                          paste0("Store is temporarily closed.", if (nchar(fnote) > 0) paste0(" Reason: ", fnote) else "")
                                     )
      )
      wrap(
        tags$div(class="page-title", "Admin Dashboard"),
        force_close_section,
        tags$div(class="admin-grid",
                 actionButton("admin_foodlist",  "Food List",            class="admin-btn"),
                 actionButton("admin_customers", "Customer List",        class="admin-btn"),
                 actionButton("admin_history",   "Transaction History",  class="admin-btn"),
                 actionButton("admin_promos",    "Promo Management",     class="admin-btn"),
                 actionButton("admin_feedback",  "View Feedback", class="admin-btn")
        ),
        actionButton("admin_logout", "Log Out", class="back-btn")
      )
      
    } else if (pg == "admin_feedback") {
      wrap(
        actionButton("back_admin_feedback", "Back to Admin", class="back-btn"),
        tags$div(class="page-title", "Customer Feedback"),
        tags$div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:1.5rem;align-items:center;",
                 tags$a(href=FEEDBACK_SHEET_URL, target="_blank", class="feedback-open-btn",  kof_icon("link"), "Open Google Sheets Responses"),
                 tags$a(href=FEEDBACK_CSV_URL,   target="_blank", class="feedback-sheet-btn", kof_icon("link"), "Download as CSV")
        ),
        tags$div(class="page-card",
                 tags$div(class="page-section-title", "Latest Responses (inline preview)"),
                 tags$button(class="feedback-refresh-btn", id="fb_refresh_btn", "Refresh Responses"),
                 tags$div(id="fb_response_area",
                          tags$p(class="feedback-loading", "Click Refresh to load the latest responses.")
                 )
        ),
        tags$script(HTML(paste0("
(function(){
  var CSV_URL='", FEEDBACK_CSV_URL, "';
  function parseCSV(text){
    var lines=text.trim().split('\\n');
    if(lines.length<2)return[];
    var headers=lines[0].split(',').map(function(h){return h.replace(/^\"|\"$/g,'').trim();});
    var rows=[];
    for(var i=1;i<lines.length;i++){
      var cols=lines[i].split(',').map(function(c){return c.replace(/^\"|\"$/g,'').trim();});
      var obj={};
      headers.forEach(function(h,j){obj[h]=cols[j]||'';});
      rows.push(obj);
    }
    return rows;
  }
  function renderResponses(rows){
    var area=document.getElementById('fb_response_area');
    if(!area)return;
    if(!rows||rows.length===0){area.innerHTML='<p class=\"feedback-loading\">No responses found yet.</p>';return;}
    var html='';
    rows.slice().reverse().slice(0,20).forEach(function(row){
      var keys=Object.keys(row);
      html+='<div class=\"feedback-card\">';
      if(row['Timestamp'])html+='<div class=\"feedback-card-time\">'+row['Timestamp']+'</div>';
      keys.forEach(function(k){
        if(k==='Timestamp')return;
        if(row[k]&&row[k].length>0)html+='<div style=\"margin-bottom:6px;\"><span style=\"color:var(--amber);font-size:11px;text-transform:uppercase;letter-spacing:0.06em;\">'+k+'</span><div class=\"feedback-card-text\">'+row[k]+'</div></div>';
      });
      html+='</div>';
    });
    area.innerHTML=html;
  }
  function fetchResponses(){
    var area=document.getElementById('fb_response_area');
    if(area)area.innerHTML='<p class=\"feedback-loading\">Loading\u2026</p>';
    fetch(CSV_URL+'&cachebust='+Date.now(),{cache:'no-store'})
      .then(function(r){return r.text();})
      .then(function(text){renderResponses(parseCSV(text));})
      .catch(function(err){if(area)area.innerHTML='<p class=\"feedback-loading\" style=\"color:#eb5757;\">Could not load responses. Error: '+err.message+'</p>';});
  }
  document.addEventListener('click',function(e){if(e.target&&e.target.id==='fb_refresh_btn')fetchResponses();});
})();
        ")))
      )
      
    } else if (pg == "admin_food") {
      srch  = get_food_search()
      if (length(menu_items()) == 0) menu_items(default_menu_items)
      items = menu_items()
      if (nchar(srch) > 0)
        items = Filter(function(x) grepl(srch, tolower(x$name), fixed=TRUE), items)
      total_items = length(items)
      fp = min(admin_food_page(), max(1, ceiling(max(total_items,1)/10)))
      admin_food_page(fp)
      items_page = items[page_slice(total_items, fp)]
      
      rows = if (length(items_page) == 0) {
        tags$p(style="color:var(--gray-muted);padding:1rem 0;", "No menu items found.")
      } else tagList(lapply(items_page, function(it) {
        st   = if (it$avail) "Available" else "Not Available"
        sc   = if (it$avail) "color:#6fcf97;" else "color:#eb5757;"
        row_cls = paste("admin-food-row", if (it$cat == "Starred Drinks") "starred-row" else "")
        tags$div(class=row_cls,
                 tags$div(
                   tags$div(class="menu-item-name", it$name),
                   tags$div(style="color:var(--gray-muted);font-size:12px;", paste0(it$cat, " - P", it$price))
                 ),
                 tags$div(class="admin-food-status-col",
                          tags$span(style=paste0(sc,"font-size:12px;font-weight:600;"), st)
                 ),
                 tags$div(class="admin-food-btn-col",
                          tags$button(class="view-btn", onclick=paste0("Shiny.setInputValue('edit_food',{id:",it$id,",ts:Math.random()})"), "Edit")
                 )
        )
      }))
      
      wrap(
        actionButton("back_admin_food", "Back to Admin", class="back-btn"),
        tags$div(class="page-title", "Food List"),
        div(id="admin_food_search_box",
            textInput("admin_food_search_box", NULL, value=isolate(input$admin_food_search_box) %||% "",
                      placeholder="Search menu items...", width="100%")
        ),
        rows,
        pager_ui(total_items, fp, "admin_food_page_go")
      )
      
    } else if (pg == "admin_customers") {
      srch  = get_cust_search()
      all_u = users()
      if (nchar(srch) > 0)
        all_u = Filter(function(u) grepl(srch,tolower(u$name),fixed=TRUE)||grepl(srch,tolower(u$contact),fixed=TRUE), all_u)
      total_customers = length(all_u)
      cp = min(admin_cust_page(), max(1, ceiling(max(total_customers,1)/10)))
      admin_cust_page(cp)
      page_idx = page_slice(total_customers, cp)
      
      exp_c = expanded_cust()
      rows  = tagList(lapply(page_idx, function(i) {
        u      = all_u[[i]]; pts = valid_points(u); is_exp = !is.null(exp_c) && exp_c == u$contact
        stamp_disp = paste0(paste(rep("+", u$stamps), collapse=""), paste(rep("o", 9-u$stamps), collapse=""), " (", u$stamps, "/9)")
        expand_panel = if (is_exp) tags$div(class="expand-panel",
                                            tags$div(class="page-section-title", paste("Add Transaction -", u$name)),
                                            tags$div(class="cust-pts", paste("Valid Points:", pts)),
                                            tags$hr(style="border-color:rgba(200,134,29,0.2);margin:10px 0;"),
                                            numericInput(paste0("txn_amt_",i), "Amount Spent (P):", value=0, min=0, step=10),
                                            tags$div(class="kof-check-row", tags$input(type="checkbox",id=paste0("txn_star_",i)),  tags$label(`for`=paste0("txn_star_",i),  "Starred drink? (+1 pt)")),
                                            tags$div(class="kof-check-row", tags$input(type="checkbox",id=paste0("txn_combo_",i)), tags$label(`for`=paste0("txn_combo_",i), "Drink + croffle combo? (+2 pts)")),
                                            tags$div(class="kof-check-row", tags$input(type="checkbox",id=paste0("txn_dbl_",i)),   tags$label(`for`=paste0("txn_dbl_",i),   "Double Points Hour? (2x)")),
                                            uiOutput(paste0("txn_prev_",i)),
                                            tags$button(class="confirm-btn", onclick=paste0("Shiny.setInputValue('confirm_txn',{idx:",i,",ts:Math.random()})"), "Confirm Transaction")
        ) else NULL
        
        tags$div(class="cust-card",
                 fluidRow(
                   column(7,
                          tags$div(class="cust-name", paste0("#",i," ",u$name)),
                          tags$div(class="cust-meta", paste("Contact:", u$contact)),
                          tags$div(class="cust-meta", paste("Card #:", u$card)),
                          tags$div(class="cust-pts",  paste("Points:", pts)),
                          tags$div(class="cust-meta", stamp_disp)
                   ),
                   column(5,
                          actionButton(paste0("tog_exp_",i),  if (is_exp) "Close" else "Transaction", class="cust-action-btn"),
                          actionButton(paste0("add_stamp_",i), "Add Stamp",     class="cust-action-btn"),
                          actionButton(paste0("redeem_",i),    "Redeem Points", class="cust-action-btn"),
                          tags$button(class="cust-action-btn",
                                      onclick=paste0("Shiny.setInputValue('edit_customer',{contact:'", u$contact, "',ts:Math.random()})"),
                                      "Edit Account")
                   )
                 ),
                 expand_panel
        )
      }))
      
      wrap(
        fluidRow(
          column(8, actionButton("back_admin_cust","Back to Admin",class="back-btn")),
          column(4, tags$div(style="text-align:right;padding-top:5px;", actionButton("baf_btn","Bring a Friend",class="cust-action-btn",style="width:auto;")))
        ),
        tags$div(class="page-title", paste("Customer List (", length(users()), ")")),
        div(id="admin_search",
            textInput("admin_search", NULL, value=isolate(input$admin_search) %||% "",
                      placeholder="Search by name or contact...", width="100%")
        ),
        rows,
        pager_ui(total_customers, cp, "admin_cust_page_go")
      )
      
    } else if (pg == "admin_history") {
      srch = get_hist_search()
      txns = all_transactions()
      txn_idx = seq_along(txns)
      if (nchar(srch) > 0 && length(txns) > 0) {
        keep = sapply(txns, function(t) grepl(srch,tolower(t$name),fixed=TRUE)||grepl(srch,tolower(t$contact),fixed=TRUE))
        txns = txns[keep]
        txn_idx = txn_idx[keep]
      }
      total_txns = length(txns)
      hp = min(admin_hist_page(), max(1, ceiling(max(total_txns,1)/10)))
      admin_hist_page(hp)
      hist_page_idx = page_slice(total_txns, hp)
      
      rows = if (length(txns) == 0)
        tags$p(style="color:var(--gray-muted);", "No transactions yet.")
      else tagList(lapply(rev(hist_page_idx), function(k) {
        t = txns[[k]]
        orig_idx = txn_idx[[k]]
        undone = isTRUE(t$undone)
        tags$div(class=paste("txn-card", if (undone) "undone" else ""),
                 tags$div(style="display:flex;justify-content:space-between;gap:10px;align-items:flex-start;",
                          tags$div(
                 tags$div(class="cust-name",  t$name),
                 tags$div(class="txn-date",   paste(t$contact, "-", t$datetime)),
                 if (!is.null(t$amount)       && t$amount > 0)       tags$div(class="txn-amount",    paste0("Spent: P", t$amount)),
                 if (!is.null(t$pts_earned)   && t$pts_earned > 0)   tags$div(class="txn-pts-plus",  paste0("+", t$pts_earned, " pts earned")),
                 if (!is.null(t$pts_deducted) && t$pts_deducted > 0) tags$div(class="txn-pts-minus", paste0("-", t$pts_deducted, " pts (", t$reward_redeemed, ")")),
                 if (isTRUE(t$stamp_added))                          tags$div(class="txn-stamp",     "Stamp added!"),
                 if (undone) tags$div(class="txn-undone-badge", paste("Undone", t$undone_at %||% ""))
                          ),
                          tags$button(class="txn-undo-btn",
                                      onclick=paste0("Shiny.setInputValue('view_txn',{idx:", orig_idx, ",ts:Math.random()})"),
                                      "View")
                 )
        )
      }))
      wrap(
        actionButton("back_admin_hist","Back to Admin",class="back-btn"),
        tags$div(class="page-title","Transaction History"),
        div(id="admin_hist_search",
            textInput("admin_hist_search", NULL, value=isolate(input$admin_hist_search) %||% "",
                      placeholder="Search by name or contact...", width="100%")
        ),
        rows,
        pager_ui(total_txns, hp, "admin_hist_page_go")
      )
      
    } else if (pg == "admin_promos") {
      all_promos = promos()
      n_active   = length(Filter(is_promo_active, all_promos))
      n_sched    = length(Filter(function(p) isTRUE(p$visible) && !is.null(p$start_date) && as.POSIXct(p$start_date) > Sys.time(), all_promos))
      n_inactive = length(Filter(function(p) !is_promo_active(p), all_promos))
      promo_display = rev(all_promos)
      total_promos = length(promo_display)
      pp = min(admin_promo_page(), max(1, ceiling(max(total_promos,1)/10)))
      admin_promo_page(pp)
      promo_page = promo_display[page_slice(total_promos, pp)]
      
      promo_rows = if (length(all_promos) == 0)
        tags$p(style="color:var(--gray-muted);text-align:center;padding:2rem;", "No promos yet. Create one!")
      else tagList(lapply(promo_page, function(p) {
        active = is_promo_active(p)
        sched  = isTRUE(p$visible) && !is.null(p$start_date) && as.POSIXct(p$start_date) > Sys.time()
        st_cls = if (active) "status-active" else if (sched) "status-sched" else "status-inactive"
        st_txt = if (active) "ACTIVE"         else if (sched) "SCHEDULED"    else "INACTIVE"
        tags$div(class="admin-promo-card",
                 fluidRow(
                   column(8,
                          tags$div(class=st_cls, st_txt),
                          tags$div(class="promo-badge", promo_type_labels[p$type]),
                          tags$div(class="promo-title-text", paste0("[#",p$id,"] ",p$title)),
                          tagList(lapply(promo_detail_lines(p), function(ln) tags$div(class="promo-detail-text", ln))),
                          if (!is.null(p$terms) && nchar(p$terms) > 0) tags$div(class="promo-detail-text", paste("*", p$terms)),
                          tags$div(class="txn-date", paste("Created:", p$created_at))
                   ),
                   column(4,
                          actionButton(paste0("ptv_",p$id),  if (isTRUE(p$visible)) "Hide" else "Show", class="promo-action-btn"),
                          actionButton(paste0("pedit_",p$id), "Edit", class="promo-action-btn")
                   )
                 )
        )
      }))
      
      wrap(
        actionButton("back_admin_promos","Back to Admin",class="back-btn"),
        tags$div(class="page-title","Promo Management"),
        tags$div(class="promo-stats",
                 tags$div(class="promo-stat", tags$div(class="promo-stat-num",n_active),   tags$div(class="promo-stat-label","Active")),
                 tags$div(class="promo-stat", tags$div(class="promo-stat-num",n_sched),    tags$div(class="promo-stat-label","Scheduled")),
                 tags$div(class="promo-stat", tags$div(class="promo-stat-num",n_inactive), tags$div(class="promo-stat-label","Inactive")),
                 tags$div(style="flex:1;", actionButton("create_promo","+ Create Promo",class="create-promo-btn"))
        ),
        promo_rows,
        pager_ui(total_promos, pp, "admin_promo_page_go")
      )
    } else NULL
  })
  
  observeEvent(input$force_close_btn, {
    showModal(modalDialog(title="Force Close Store",
                          tags$p(style="color:var(--gray-muted);font-size:13px;margin-bottom:1rem;","This will immediately mark the store as closed."),
                          textInput("force_close_reason","Reason (optional):",placeholder="e.g. Staff emergency"),
                          footer=tagList(modalButton("Cancel"),actionButton("confirm_force_close","Close Store Now",
                                                                            style="background:rgba(235,87,87,0.25);color:#eb5757;border:1px solid rgba(235,87,87,0.5);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
  })
  observeEvent(input$confirm_force_close, {
    reason = trimws(if (!is.null(input$force_close_reason)) input$force_close_reason else "")
    store_force_close(TRUE); force_close_note(reason); removeModal()
    showNotification(if (nchar(reason)>0) paste0("Store force closed. Reason: ",reason) else "Store force closed.", type="warning", duration=5)
  })
  observeEvent(input$reopen_store_btn, {
    showModal(modalDialog(title="Reopen Store",
                          tags$p(style="color:var(--gray-muted);","Remove the temporary closure and restore normal hours?"),
                          footer=tagList(modalButton("Cancel"),actionButton("confirm_reopen","Yes, Reopen",
                                                                            style="background:rgba(111,207,151,0.2);color:#6fcf97;border:1px solid rgba(111,207,151,0.5);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
  })
  observeEvent(input$confirm_reopen, {
    store_force_close(FALSE); force_close_note(""); removeModal()
    showNotification("Store reopened!", type="message", duration=4)
  })
  
  observeEvent(input$edit_hours_btn, {
    oh = store_open_hour(); ch = store_close_hour()
    showModal(modalDialog(title="Edit Store Hours",
                          tags$p(style="color:var(--gray-muted);font-size:13px;margin-bottom:1rem;","Set opening and closing times. Use 24-hour format (0=midnight)."),
                          fluidRow(
                            column(6, numericInput("new_open_hour",  "Opening Hour (0-23):", value=oh, min=0, max=23, step=1)),
                            column(6, numericInput("new_close_hour", "Closing Hour (0-23):", value=ch, min=0, max=23, step=1))
                          ),
                          uiOutput("hours_preview_ui"),
                          tags$p(style="color:var(--gray-muted);font-size:12px;margin-top:8px;","Tip: 2 PM = 14, Midnight = 0."),
                          footer=tagList(modalButton("Cancel"),actionButton("save_hours_btn","Save Hours",
                                                                            style="background:var(--brown-warm);color:var(--gold);border:none;padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
  })
  output$hours_preview_ui = renderUI({
    oh = input$new_open_hour; ch = input$new_close_hour; if (is.null(oh)||is.null(ch)) return(NULL)
    open_now = is_store_open(oh, ch)
    tags$div(style="background:rgba(200,134,29,0.08);border:1px solid rgba(200,134,29,0.2);border-radius:8px;padding:10px 14px;margin-top:8px;",
             tags$p(style="color:var(--gold);font-size:14px;font-weight:600;margin:0;",
                    paste0("Preview: ",format_hour(oh)," \u2013 ",format_hour(ch)," \u2022 ",if(open_now)"Currently OPEN" else "Currently CLOSED")
             )
    )
  })
  observeEvent(input$save_hours_btn, {
    oh = input$new_open_hour; ch = input$new_close_hour
    if (is.null(oh)||is.null(ch)) { showNotification("Invalid hours!",type="error"); return() }
    store_open_hour(as.integer(oh)); store_close_hour(as.integer(ch)); removeModal()
    showNotification(paste0("Hours updated: ",format_hour(oh)," \u2013 ",format_hour(ch)),type="message",duration=4)
  })
  
  observeEvent(input$go_vcard,    go_to("vcard"))
  observeEvent(input$go_menu,     go_to("menu"))
  observeEvent(input$go_delivery, go_to("delivery"))
  observeEvent(input$go_points,   go_to("points"))
  observeEvent(input$go_more, {
    showModal(modalDialog(title="More",
                          actionButton("more_myaccount","My Account",  style="display:block;width:100%;margin-bottom:5px;"),
                          actionButton("more_about",    "About Us",    style="display:block;width:100%;margin-bottom:5px;"),
                          actionButton("more_feedback", "Feedback",    style="display:block;width:100%;margin-bottom:5px;"),
                          footer=tagList(modalButton("Close"),actionButton("more_logout","Log Out")),easyClose=TRUE))
  })
  observeEvent(input$more_about,     { removeModal(); go_to("about") })
  observeEvent(input$more_feedback,  { removeModal(); go_to("feedback") })
  observeEvent(input$more_myaccount, {
    removeModal(); u = current_user(); if (is.null(u)) return()
    pts = valid_points(u)
    showModal(modalDialog(title="My Account",
                          tags$p(paste("Name:", u$name)), tags$p(paste("Contact:", u$contact)),
                          tags$p(paste("Card #:", u$card)), tags$hr(),
                          tags$p(paste("Valid Points:", pts), style="font-weight:bold;color:var(--gold,#F2C063);"),
                          footer=modalButton("Close"),easyClose=TRUE))
  })
  observeEvent(input$do_logout, {
    showModal(modalDialog(title="Log Out","Are you sure you want to log out?",
                          footer=tagList(modalButton("No, Stay"),actionButton("confirm_logout","Yes, Log Out")),easyClose=TRUE))
  })
  observeEvent(input$more_logout, {
    removeModal()
    showModal(modalDialog(title="Log Out","Are you sure?",
                          footer=tagList(modalButton("Cancel"),actionButton("confirm_logout","Yes, Log Out")),easyClose=TRUE))
  })
  observeEvent(input$confirm_logout, {
    current_user(NULL); removeModal(); current_page("none")
    session$sendCustomMessage("switchToLogin","")
    showNotification("Logged out successfully!",type="message")
  })
  
  observeEvent(input$back_vcard,          go_to("dashboard"))
  observeEvent(input$back_menu,           go_to("dashboard"))
  observeEvent(input$back_delivery,       go_to("dashboard"))
  observeEvent(input$back_points,         go_to("dashboard"))
  observeEvent(input$back_about,          go_to("dashboard"))
  observeEvent(input$back_feedback,       go_to("dashboard"))
  observeEvent(input$back_foodview,       go_to("menu"))
  observeEvent(input$admin_foodlist,      go_to("admin_food"))
  observeEvent(input$admin_customers,     go_to("admin_customers"))
  observeEvent(input$admin_history,       go_to("admin_history"))
  observeEvent(input$admin_promos,        go_to("admin_promos"))
  observeEvent(input$admin_feedback,      go_to("admin_feedback"))
  observeEvent(input$back_admin_food,     go_to("admin"))
  observeEvent(input$back_admin_cust,     go_to("admin"))
  observeEvent(input$back_admin_hist,     go_to("admin"))
  observeEvent(input$back_admin_promos,   go_to("admin"))
  observeEvent(input$back_admin_feedback, go_to("admin"))
  observeEvent(input$admin_logout, {
    showModal(modalDialog(title="Admin Log Out","Are you sure?",
                          footer=tagList(modalButton("No"),actionButton("admin_confirm_logout","Yes, Log Out")),easyClose=TRUE))
  })
  observeEvent(input$admin_confirm_logout, {
    removeModal(); current_page("none")
    session$sendCustomMessage("switchToLogin","")
    showNotification("Admin logged out.",type="message")
  })

  observeEvent(input$admin_food_page_go,   admin_food_page(as.integer(input$admin_food_page_go)))
  observeEvent(input$admin_cust_page_go,   admin_cust_page(as.integer(input$admin_cust_page_go)))
  observeEvent(input$admin_hist_page_go,   admin_hist_page(as.integer(input$admin_hist_page_go)))
  observeEvent(input$admin_promo_page_go,  admin_promo_page(as.integer(input$admin_promo_page_go)))

  observeEvent(input$edit_customer, {
    contact = trimws(input$edit_customer$contact %||% "")
    match = Filter(function(u) u$contact == contact, users())
    if (length(match) == 0) { showNotification("Customer not found.", type="error"); return() }
    u = match[[1]]
    session$userData$edit_customer_contact = contact
    showModal(modalDialog(title=paste("Edit Account -", u$name), size="m", easyClose=TRUE,
                          textInput("ec_name","Full Name",value=u$name),
                          textInput("ec_contact","Contact Number",value=u$contact),
                          textInput("ec_pin","PIN",value=u$pin),
                          numericInput("ec_stamps","Stamps",value=as.numeric(u$stamps %||% 0),min=0,max=9,step=1),
                          numericInput("ec_points_adjust","Points Adjustment (+/-)",value=0,step=1),
                          tags$p(style="color:var(--gray-muted);font-size:12px;",
                                 "Use a positive number to add points or a negative number to deduct points."),
                          tags$div(style="display:flex;gap:8px;flex-wrap:wrap;margin:12px 0;",
                                   tags$button(type="button", class="pager-btn", onclick="Shiny.setInputValue('reset_customer_points',Math.random())", "Reset Points"),
                                   tags$button(type="button", class="pager-btn", onclick="Shiny.setInputValue('reset_customer_stamps',Math.random())", "Reset Stamps"),
                                   tags$button(type="button", class="pager-btn", onclick="Shiny.setInputValue('clear_customer_txns',Math.random())", "Clear Transactions")
                          ),
                          footer=tagList(
                            tags$button(type="button", class="edit-danger-btn", onclick="Shiny.setInputValue('edit_customer_delete',Math.random())", "Delete Account"),
                            modalButton("Cancel"),
                            actionButton("save_edit_customer","Save Changes")
                          )))
  })
  
  observeEvent(input$save_edit_customer, {
    old_contact = session$userData$edit_customer_contact %||% ""
    name = trimws(input$ec_name %||% "")
    contact = trimws(input$ec_contact %||% "")
    pin = trimws(input$ec_pin %||% "")
    stamps = as.integer(input$ec_stamps %||% 0)
    adjust = as.numeric(input$ec_points_adjust %||% 0)
    if (nchar(name)==0 || nchar(contact)==0 || nchar(pin)==0) { showNotification("Please fill in name, contact, and PIN.", type="error"); return() }
    if (nchar(contact)!=11 || grepl("[^0-9]",contact)) { showNotification("Contact must be exactly 11 digits.", type="error"); return() }
    if (nchar(pin)<1 || nchar(pin)>6 || grepl("[^0-9]",pin)) { showNotification("PIN must be 1-6 digits.", type="error"); return() }
    if (any(sapply(users(), function(u) u$contact == contact && u$contact != old_contact))) { showNotification("Contact already registered.", type="error"); return() }
    updated = lapply(users(), function(usr) {
      if (usr$contact == old_contact) {
        usr$name = name; usr$contact = contact; usr$pin = pin; usr$stamps = max(0, min(9, stamps))
        if (!is.na(adjust) && adjust > 0) {
          row = data.frame(pts=adjust, earned="Admin Adjustment", expires=as.character(Sys.time()+90*86400), stringsAsFactors=FALSE)
          usr$points_log = rbind(normalize_points_log(usr$points_log), row)
        } else if (!is.na(adjust) && adjust < 0) {
          usr = subtract_active_points(usr, abs(adjust))
        }
        if (!is.null(current_user()) && current_user()$contact == old_contact) current_user(usr)
      }
      usr
    })
    users(updated)
    all_transactions(lapply(all_transactions(), function(t) {
      if (t$contact == old_contact) { t$contact = contact; t$name = name }
      t
    }))
    if (!is.null(expanded_cust()) && expanded_cust() == old_contact) expanded_cust(contact)
    session$userData$edit_customer_contact = NULL
    removeModal()
    showNotification("Customer account updated.", type="message", duration=4)
  })
  
  observeEvent(input$edit_customer_delete, {
    contact = session$userData$edit_customer_contact %||% ""
    if (nchar(contact) == 0) return()
    match = Filter(function(u) u$contact == contact, users())
    if (length(match) == 0) { showNotification("Customer not found.", type="error"); return() }
    session$userData$delete_customer_contact = contact
    showModal(modalDialog(title="Delete Customer Account",
                          tags$p(paste0("Delete ", match[[1]]$name, " (", contact, ")?")),
                          tags$p(style="color:var(--gray-muted);font-size:13px;",
                                 "This removes the customer account and that customer's transaction history."),
                          footer=tagList(modalButton("Cancel"), actionButton("confirm_delete_customer","Yes, Delete Account",
                                                                              style="background:rgba(235,87,87,0.25);color:#ff8585;border:1px solid rgba(235,87,87,0.5);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
  })

  observeEvent(input$reset_customer_points, {
    contact = session$userData$edit_customer_contact %||% ""
    if (nchar(contact) == 0) return()
    showModal(modalDialog(title="Reset Customer Points",
                          tags$p("Reset this customer's active points to 0?"),
                          footer=tagList(modalButton("Cancel"), actionButton("confirm_reset_customer_points","Yes, Reset Points",
                                                                              style="background:rgba(200,134,29,0.2);color:var(--gold);border:1px solid rgba(242,192,99,0.45);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
  })
  observeEvent(input$confirm_reset_customer_points, {
    contact = session$userData$edit_customer_contact %||% ""
    users(lapply(users(), function(usr) {
      if (usr$contact == contact) {
        usr$points_log = data.frame(pts=numeric(), earned=character(), expires=character(), stringsAsFactors=FALSE)
        if (!is.null(current_user()) && current_user()$contact == contact) current_user(usr)
      }
      usr
    }))
    removeModal()
    showNotification("Customer points reset.", type="message", duration=3)
  })
  observeEvent(input$reset_customer_stamps, {
    contact = session$userData$edit_customer_contact %||% ""
    if (nchar(contact) == 0) return()
    showModal(modalDialog(title="Reset Customer Stamps",
                          tags$p("Reset this customer's stamps to 0?"),
                          footer=tagList(modalButton("Cancel"), actionButton("confirm_reset_customer_stamps","Yes, Reset Stamps",
                                                                              style="background:rgba(200,134,29,0.2);color:var(--gold);border:1px solid rgba(242,192,99,0.45);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
  })
  observeEvent(input$confirm_reset_customer_stamps, {
    contact = session$userData$edit_customer_contact %||% ""
    users(lapply(users(), function(usr) {
      if (usr$contact == contact) {
        usr$stamps = 0
        if (!is.null(current_user()) && current_user()$contact == contact) current_user(usr)
      }
      usr
    }))
    removeModal()
    showNotification("Customer stamps reset.", type="message", duration=3)
  })
  observeEvent(input$clear_customer_txns, {
    contact = session$userData$edit_customer_contact %||% ""
    if (nchar(contact) == 0) return()
    showModal(modalDialog(title="Clear Customer Transactions",
                          tags$p("Remove this customer's transaction history? Points and stamps are not changed."),
                          footer=tagList(modalButton("Cancel"), actionButton("confirm_clear_customer_txns","Yes, Clear History",
                                                                              style="background:rgba(235,87,87,0.18);color:#ff8585;border:1px solid rgba(235,87,87,0.45);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
  })
  observeEvent(input$confirm_clear_customer_txns, {
    contact = session$userData$edit_customer_contact %||% ""
    all_transactions(Filter(function(t) t$contact != contact, all_transactions()))
    removeModal()
    showNotification("Customer transaction history cleared.", type="message", duration=3)
  })

  observeEvent(input$edit_food, {
    item_id = as.integer(input$edit_food$id %||% NA)
    found = Filter(function(x) x$id == item_id, menu_items())
    if (length(found) == 0) { showNotification("Menu item not found.", type="error"); return() }
    it = found[[1]]
    session$userData$edit_food_id = item_id
    cats = c("Starred Drinks","Espresso Iced","Espresso Hot","Ice Blended Espresso","Ice Blended Cream","Non-Coffee","Snacks","Add Ons")
    showModal(modalDialog(title=paste("Edit Item -", it$name), size="m", easyClose=TRUE,
                          textInput("ef_name","Item Name",value=it$name),
                          selectInput("ef_cat","Category",choices=cats,selected=it$cat),
                          numericInput("ef_price","Price (P)",value=it$price,min=0,step=5),
                          checkboxInput("ef_avail","Available",value=isTRUE(it$avail)),
                          footer=tagList(
                            tags$button(type="button", class="edit-danger-btn", onclick="Shiny.setInputValue('edit_food_delete',Math.random())", "Delete Item"),
                            modalButton("Cancel"),
                            actionButton("save_edit_food","Save Changes")
                          )))
  })
  
  observeEvent(input$save_edit_food, {
    item_id = as.integer(session$userData$edit_food_id %||% NA)
    name = trimws(input$ef_name %||% "")
    if (is.na(item_id) || nchar(name)==0) { showNotification("Item name is required.", type="error"); return() }
    updated = lapply(menu_items(), function(x) {
      if (x$id == item_id) {
        x$name = name
        x$cat = input$ef_cat
        x$price = as.numeric(input$ef_price %||% x$price)
        x$avail = isTRUE(input$ef_avail)
      }
      x
    })
    menu_items(updated)
    session$userData$edit_food_id = NULL
    removeModal()
    showNotification("Menu item updated.", type="message", duration=3)
  })
  
  observeEvent(input$edit_food_delete, {
    item_id = as.integer(session$userData$edit_food_id %||% NA)
    found = Filter(function(x) x$id == item_id, menu_items())
    if (length(found) == 0) { showNotification("Menu item not found.", type="error"); return() }
    showModal(modalDialog(title="Delete Menu Item",
                          tags$p(paste0("Delete ", found[[1]]$name, "?")),
                          tags$p(style="color:var(--gray-muted);font-size:13px;", "This removes the item from Food List and the customer Menu."),
                          footer=tagList(modalButton("Cancel"), actionButton("confirm_delete_food","Yes, Delete Item",
                                                                              style="background:rgba(235,87,87,0.25);color:#ff8585;border:1px solid rgba(235,87,87,0.5);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
  })
  
  observeEvent(input$confirm_delete_food, {
    item_id = as.integer(session$userData$edit_food_id %||% NA)
    if (is.na(item_id)) { removeModal(); return() }
    menu_items(Filter(function(x) x$id != item_id, menu_items()))
    session$userData$edit_food_id = NULL
    removeModal()
    showNotification("Menu item deleted.", type="message", duration=3)
  })

  observeEvent(input$delete_customer, {
    d = input$delete_customer
    contact = trimws(d$contact %||% "")
    match = Filter(function(u) u$contact == contact, users())
    if (length(match) == 0) { showNotification("Customer not found.", type="error"); return() }
    u = match[[1]]
    showModal(modalDialog(title="Delete Customer Account",
                          tags$p(paste0("Delete ", u$name, " (", u$contact, ")?")),
                          tags$p(style="color:var(--gray-muted);font-size:13px;",
                                 "This removes the customer account and that customer's transaction history."),
                          footer=tagList(modalButton("Cancel"),
                                         actionButton("confirm_delete_customer","Yes, Delete Account",
                                                      style="background:rgba(235,87,87,0.25);color:#ff8585;border:1px solid rgba(235,87,87,0.5);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
    session$userData$delete_customer_contact = contact
  })
  
  observeEvent(input$confirm_delete_customer, {
    contact = session$userData$delete_customer_contact %||% ""
    if (nchar(contact) == 0) { removeModal(); return() }
    target = Filter(function(u) u$contact == contact, users())
    users(Filter(function(u) u$contact != contact, users()))
    all_transactions(Filter(function(t) t$contact != contact, all_transactions()))
    if (!is.null(current_user()) && current_user()$contact == contact) current_user(NULL)
    if (!is.null(expanded_cust()) && expanded_cust() == contact) expanded_cust(NULL)
    session$userData$delete_customer_contact = NULL
    removeModal()
    showNotification(paste("Deleted account:", if (length(target)>0) target[[1]]$name else contact), type="message", duration=4)
  })

  observeEvent(input$view_txn, {
    idx = as.integer(input$view_txn$idx %||% NA)
    txns = all_transactions()
    if (is.na(idx) || idx < 1 || idx > length(txns)) { showNotification("Transaction not found.", type="error"); return() }
    t = txns[[idx]]
    session$userData$view_txn_idx = idx
    detail = tagList(
      tags$p(paste("Customer:", t$name)),
      tags$p(paste("Contact:", t$contact)),
      tags$p(paste("Date:", t$datetime)),
      if (!is.null(t$amount) && t$amount > 0) tags$p(paste0("Amount: P", t$amount)),
      if (!is.null(t$pts_earned) && t$pts_earned > 0) tags$p(paste0("Points earned: +", t$pts_earned)),
      if (!is.null(t$pts_deducted) && t$pts_deducted > 0) tags$p(paste0("Points used: -", t$pts_deducted, " (", t$reward_redeemed, ")")),
      if (isTRUE(t$stamp_added)) tags$p("Stamp added"),
      if (isTRUE(t$undone)) tags$p(style="color:var(--gray-muted);", paste("Status: Undone", t$undone_at %||% ""))
    )
    showModal(modalDialog(title="Transaction Details", detail,
                          footer=tagList(
                            if (!isTRUE(t$undone)) actionButton("modal_undo_txn","Undo Transaction",
                                                                 style="background:rgba(200,134,29,0.2);color:var(--gold);border:1px solid rgba(242,192,99,0.45);padding:8px 18px;border-radius:8px;cursor:pointer;"),
                            if (isTRUE(t$undone)) actionButton("modal_delete_txn","Delete Record",
                                                               style="background:rgba(235,87,87,0.18);color:#ff8585;border:1px solid rgba(235,87,87,0.45);padding:8px 18px;border-radius:8px;cursor:pointer;"),
                            modalButton("Close")
                          ),
                          easyClose=TRUE))
  })
  observeEvent(input$modal_undo_txn, {
    idx = session$userData$view_txn_idx
    removeModal()
    txns = all_transactions()
    if (is.null(idx) || idx < 1 || idx > length(txns)) { showNotification("Transaction not found.", type="error"); return() }
    t = txns[[idx]]
    showModal(modalDialog(title="Undo Transaction",
                          tags$p(paste0("Undo transaction for ", t$name, " (", t$contact, ")?")),
                          tags$p(style="color:var(--gray-muted);font-size:13px;",
                                 "This reverses the points or stamp change and keeps the transaction as an audit record."),
                          footer=tagList(modalButton("Cancel"),
                                         actionButton("confirm_undo_txn","Yes, Undo",
                                                      style="background:rgba(200,134,29,0.2);color:var(--gold);border:1px solid rgba(242,192,99,0.45);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
    session$userData$undo_txn_idx = idx
  })
  observeEvent(input$modal_delete_txn, {
    idx = session$userData$view_txn_idx
    showModal(modalDialog(title="Delete Transaction Record",
                          tags$p("Delete this already-undone transaction record?"),
                          footer=tagList(modalButton("Cancel"), actionButton("confirm_delete_txn_record","Yes, Delete Record",
                                                                              style="background:rgba(235,87,87,0.18);color:#ff8585;border:1px solid rgba(235,87,87,0.45);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
  })
  observeEvent(input$confirm_delete_txn_record, {
    idx = as.integer(session$userData$view_txn_idx %||% NA)
    txns = all_transactions()
    if (!is.na(idx) && idx >= 1 && idx <= length(txns) && isTRUE(txns[[idx]]$undone)) {
      all_transactions(txns[-idx])
      showNotification("Transaction record deleted.", type="message", duration=3)
    }
    session$userData$view_txn_idx = NULL
    removeModal()
  })

  observeEvent(input$undo_txn, {
    d = input$undo_txn
    idx = as.integer(d$idx %||% NA)
    txns = all_transactions()
    if (is.na(idx) || idx < 1 || idx > length(txns)) { showNotification("Transaction not found.", type="error"); return() }
    t = txns[[idx]]
    if (isTRUE(t$undone)) { showNotification("Transaction already undone.", type="warning"); return() }
    showModal(modalDialog(title="Undo Transaction",
                          tags$p(paste0("Undo transaction for ", t$name, " (", t$contact, ")?")),
                          tags$p(style="color:var(--gray-muted);font-size:13px;",
                                 "This reverses the points or stamp change and keeps the transaction as an audit record."),
                          footer=tagList(modalButton("Cancel"),
                                         actionButton("confirm_undo_txn","Yes, Undo",
                                                      style="background:rgba(200,134,29,0.2);color:var(--gold);border:1px solid rgba(242,192,99,0.45);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
    session$userData$undo_txn_idx = idx
  })
  
  observeEvent(input$confirm_undo_txn, {
    idx = as.integer(session$userData$undo_txn_idx %||% NA)
    txns = all_transactions()
    if (is.na(idx) || idx < 1 || idx > length(txns)) { removeModal(); showNotification("Transaction not found.", type="error"); return() }
    t = txns[[idx]]
    if (isTRUE(t$undone)) { removeModal(); showNotification("Transaction already undone.", type="warning"); return() }
    updated_users = lapply(users(), function(usr) {
      if (usr$contact == t$contact) {
        if (!is.null(t$pts_earned) && t$pts_earned > 0) {
          usr = subtract_active_points(usr, as.numeric(t$pts_earned))
        }
        if (!is.null(t$pts_deducted) && t$pts_deducted > 0) {
          restore_row = data.frame(pts=as.numeric(t$pts_deducted),
                                   earned=paste("Undo:", t$reward_redeemed %||% "Redemption"),
                                   expires=as.character(Sys.time()+90*86400),
                                   stringsAsFactors=FALSE)
          usr$points_log = rbind(normalize_points_log(usr$points_log), restore_row)
        }
        if (isTRUE(t$stamp_added)) usr$stamps = max(0, as.numeric(usr$stamps %||% 0) - 1)
        if (!is.null(current_user()) && current_user()$contact == usr$contact) current_user(usr)
      }
      usr
    })
    users(updated_users)
    txns[[idx]]$undone = TRUE
    txns[[idx]]$undone_at = ph_time()
    all_transactions(txns)
    session$userData$undo_txn_idx = NULL
    removeModal()
    showNotification("Transaction undone.", type="message", duration=4)
  })
  
  cats_map = list(
    "cat_Starred_Drinks"="Starred Drinks","cat_All"="All","cat_Espresso_Iced"="Espresso Iced",
    "cat_Espresso_Hot"="Espresso Hot","cat_Ice_Blended_Espresso"="Ice Blended Espresso",
    "cat_Ice_Blended_Cream"="Ice Blended Cream","cat_Non_Coffee"="Non-Coffee",
    "cat_Snacks"="Snacks","cat_Add_Ons"="Add Ons"
  )
  lapply(names(cats_map), function(btn_id) {
    local({ b = btn_id; v = cats_map[[b]]
    observeEvent(input[[b]], { selected_cat(v) }, ignoreInit=TRUE)
    })
  })
  
  observe({
    lapply(menu_items(), function(it) {
      local({ item = it
      observeEvent(input[[paste0("view_food_", item$id)]], {
        latest = Filter(function(x) x$id == item$id, menu_items())
        if (length(latest) > 0) selected_food(latest[[1]])
        go_to("food_view")
      }, ignoreInit=TRUE)
      })
    })
  })
  
  lapply(1:51, function(id) {
    local({ item_id = id
    observeEvent(input[[paste0("tog_", item_id)]], {
      updated = lapply(menu_items(), function(x) { if (x$id == item_id) x$avail = !x$avail; x })
      menu_items(updated)
      ni = Filter(function(x) x$id == item_id, updated)[[1]]
      showNotification(paste(ni$name, "->", if (ni$avail) "Available" else "Not Available"), type="message")
    }, ignoreInit=TRUE)
    })
  })
  
  lapply(1:200, function(i) {
    local({ idx = i
    
    observeEvent(input[[paste0("tog_exp_", idx)]], {
      all_u = users()
      srch  = tolower(isolate(input$admin_search) %||% "")
      if (nchar(srch) > 0)
        all_u = Filter(function(u) grepl(srch,tolower(u$name),fixed=TRUE)||grepl(srch,tolower(u$contact),fixed=TRUE), all_u)
      if (idx <= length(all_u)) {
        u = all_u[[idx]]
        if (!is.null(expanded_cust()) && expanded_cust() == u$contact) expanded_cust(NULL)
        else expanded_cust(u$contact)
      }
    }, ignoreInit=TRUE)
    
    output[[paste0("txn_prev_", idx)]] = renderUI({
      amt   = input[[paste0("txn_amt_",  idx)]]
      star  = isTRUE(input[[paste0("txn_star_",  idx)]])
      combo = isTRUE(input[[paste0("txn_combo_", idx)]])
      dbl   = isTRUE(input[[paste0("txn_dbl_",   idx)]])
      if (is.null(amt) || amt <= 0) return(tags$p(style="color:var(--gray-muted,#9C9890);","Enter amount to preview."))
      base  = floor(amt/30)
      total = (base + if(star)1 else 0 + if(combo)2 else 0) * (if(dbl)2 else 1)
      tags$div(class="preview-box",
               tags$p(paste("Base points: +", base)),
               if (star)  tags$p("Starred bonus: +1"),
               if (combo) tags$p("Combo bonus: +2"),
               if (dbl)   tags$p("Double Points! x2"),
               tags$p(class="preview-total", paste("Total to Add: +", total))
      )
    })
    
    observeEvent(input[[paste0("add_stamp_", idx)]], {
      all_u = users()
      srch  = tolower(isolate(input$admin_search) %||% "")
      if (nchar(srch) > 0)
        all_u = Filter(function(u) grepl(srch,tolower(u$name),fixed=TRUE)||grepl(srch,tolower(u$contact),fixed=TRUE), all_u)
      if (idx > length(all_u)) return()
      u = all_u[[idx]]
      updated = lapply(users(), function(usr) {
        if (usr$contact == u$contact) {
          usr$stamps = usr$stamps + 1
          if (usr$stamps >= 9) { usr$stamps = 0; showNotification(paste(usr$name,"earned a FREE drink! Stamps reset!"),type="message",duration=5) }
          else showNotification(paste("Stamp added:",usr$stamps,"/9"),type="message",duration=3)
          if (!is.null(current_user()) && current_user()$contact == usr$contact) current_user(usr)
        }; usr
      })
      users(updated)
      txn = list(contact=u$contact,name=u$name,datetime=ph_time(),amount=0,pts_earned=0,pts_deducted=0,reward_redeemed="",stamp_added=TRUE)
      all_transactions(append(all_transactions(), list(txn)))
    }, ignoreInit=TRUE)
    
    observeEvent(input[[paste0("redeem_", idx)]], {
      all_u = users()
      srch  = tolower(isolate(input$admin_search) %||% "")
      if (nchar(srch) > 0)
        all_u = Filter(function(u) grepl(srch,tolower(u$name),fixed=TRUE)||grepl(srch,tolower(u$contact),fixed=TRUE), all_u)
      if (idx > length(all_u)) return()
      u = all_u[[idx]]; cur_pts = valid_points(u)
      showModal(modalDialog(title=paste("Redeem Points -",u$name),
                            tags$p(paste("Valid Points:",cur_pts),style="font-weight:bold;"),
                            tagList(lapply(rewards_list, function(r) {
                              can = cur_pts >= r$pts
                              actionButton(paste0("do_redeem_",idx,"_",r$id), paste0(r$label," - ",r$pts," pts"),
                                           style=paste0("display:block;width:100%;margin-bottom:5px;",if(!can)"opacity:0.4;pointer-events:none;" else ""))
                            })),
                            footer=modalButton("Cancel"),easyClose=TRUE))
    }, ignoreInit=TRUE)
    
    lapply(rewards_list, function(r) {
      local({ rw = r
      observeEvent(input[[paste0("do_redeem_",idx,"_",rw$id)]], {
        all_u = users()
        srch  = tolower(isolate(input$admin_search) %||% "")
        if (nchar(srch) > 0)
          all_u = Filter(function(u) grepl(srch,tolower(u$name),fixed=TRUE)||grepl(srch,tolower(u$contact),fixed=TRUE), all_u)
        if (idx > length(all_u)) return()
        u = all_u[[idx]]
        if (valid_points(u) < rw$pts) { showNotification("Not enough points!",type="error"); return() }
        updated = lapply(users(), function(usr) {
          if (usr$contact == u$contact) {
            df = usr$points_log
            if (!is.data.frame(df)) df = as.data.frame(do.call(rbind,lapply(df,as.data.frame)),stringsAsFactors=FALSE)
            df$expires = as.POSIXct(df$expires); df = df[order(df$expires),]; to_ded = rw$pts
            for (j in seq_len(nrow(df))) {
              if (to_ded <= 0) break
              if (df$expires[j] <= Sys.time()) next
              take = min(df$pts[j],to_ded); df$pts[j] = df$pts[j]-take; to_ded = to_ded-take
            }
            usr$points_log = df[df$pts>0,,drop=FALSE]
            if (!is.null(current_user()) && current_user()$contact==usr$contact) current_user(usr)
          }; usr
        })
        users(updated)
        txn = list(contact=u$contact,name=u$name,datetime=ph_time(),amount=0,pts_earned=0,pts_deducted=rw$pts,reward_redeemed=rw$label,stamp_added=FALSE)
        all_transactions(append(all_transactions(),list(txn)))
        removeModal()
        showNotification(paste("Redeemed:",rw$label,"for",u$name),type="message",duration=3)
      }, ignoreInit=TRUE)
      })
    })
    })
  })
  
  observeEvent(input$confirm_txn, {
    d   = input$confirm_txn; idx = d$idx
    all_u = users()
    srch  = tolower(isolate(input$admin_search) %||% "")
    if (nchar(srch) > 0)
      all_u = Filter(function(u) grepl(srch,tolower(u$name),fixed=TRUE)||grepl(srch,tolower(u$contact),fixed=TRUE), all_u)
    if (idx > length(all_u)) return()
    u     = all_u[[idx]]
    amt   = input[[paste0("txn_amt_",  idx)]]
    star  = isTRUE(input[[paste0("txn_star_",  idx)]])
    combo = isTRUE(input[[paste0("txn_combo_", idx)]])
    dbl   = isTRUE(input[[paste0("txn_dbl_",   idx)]])
    if (is.null(amt) || amt <= 0) { showNotification("Enter a valid amount!",type="error"); return() }
    base  = floor(amt/30)
    total = (base + if(star)1 else 0 + if(combo)2 else 0) * (if(dbl)2 else 1)
    new_row = data.frame(pts=total, earned="Purchase", expires=as.character(Sys.time()+90*86400), stringsAsFactors=FALSE)
    updated = lapply(users(), function(usr) {
      if (usr$contact == u$contact) {
        if (!is.data.frame(usr$points_log)) usr$points_log = as.data.frame(do.call(rbind,lapply(usr$points_log,as.data.frame)),stringsAsFactors=FALSE)
        usr$points_log = rbind(usr$points_log, new_row)
        if (!is.null(current_user()) && current_user()$contact==usr$contact) current_user(usr)
      }; usr
    })
    users(updated)
    txn = list(contact=u$contact,name=u$name,datetime=ph_time(),amount=amt,pts_earned=total,pts_deducted=0,reward_redeemed="",stamp_added=FALSE)
    all_transactions(append(all_transactions(),list(txn)))
    expanded_cust(NULL)
    showNotification(paste("+",total,"pts for",u$name),type="message",duration=3)
  })
  
  observeEvent(input$baf_btn, {
    if (length(users()) < 2) { showNotification("Need at least 2 customers!",type="error"); return() }
    showModal(modalDialog(title="Bring a Friend - +2 Points Each",
                          textInput("baf_c1","Customer 1 - Contact:",placeholder="09XXXXXXXXX"), uiOutput("baf_p1"),
                          textInput("baf_c2","Customer 2 - Contact:",placeholder="09XXXXXXXXX"), uiOutput("baf_p2"),
                          footer=tagList(modalButton("Cancel"),actionButton("baf_confirm","Give +2 Points to Both",style="background:green;color:white;")),
                          easyClose=TRUE))
  })
  output$baf_p1 = renderUI({
    c = input$baf_c1; if (is.null(c)||nchar(c)==0) return(NULL)
    m = Filter(function(u) u$contact==c, users())
    if (length(m)==0) tags$p("Not found.",style="color:#eb5757;font-size:12px;")
    else              tags$p(paste("Found:",m[[1]]$name),style="color:#6fcf97;font-size:12px;")
  })
  output$baf_p2 = renderUI({
    c = input$baf_c2; if (is.null(c)||nchar(c)==0) return(NULL)
    m = Filter(function(u) u$contact==c, users())
    if (length(m)==0) tags$p("Not found.",style="color:#eb5757;font-size:12px;")
    else              tags$p(paste("Found:",m[[1]]$name),style="color:#6fcf97;font-size:12px;")
  })
  observeEvent(input$baf_confirm, {
    c1 = trimws(input$baf_c1); c2 = trimws(input$baf_c2)
    m1 = Filter(function(u) u$contact==c1, users())
    m2 = Filter(function(u) u$contact==c2, users())
    if (nchar(c1)==0||nchar(c2)==0) { showNotification("Enter both contacts!",type="error");return() }
    if (length(m1)==0)               { showNotification("Customer 1 not found!",type="error");return() }
    if (length(m2)==0)               { showNotification("Customer 2 not found!",type="error");return() }
    if (c1==c2)                      { showNotification("Enter two different customers!",type="error");return() }
    brow = function() data.frame(pts=2,earned="Bring a Friend",expires=as.character(Sys.time()+90*86400),stringsAsFactors=FALSE)
    updated = lapply(users(), function(usr) {
      if (usr$contact==c1||usr$contact==c2) {
        if (!is.data.frame(usr$points_log)) usr$points_log = as.data.frame(do.call(rbind,lapply(usr$points_log,as.data.frame)),stringsAsFactors=FALSE)
        usr$points_log = rbind(usr$points_log, brow())
        if (!is.null(current_user()) && current_user()$contact==usr$contact) current_user(usr)
      }; usr
    })
    users(updated)
    now = ph_time()
    for (ct in c(c1,c2)) {
      u   = Filter(function(x) x$contact==ct, users())[[1]]
      txn = list(contact=u$contact,name=u$name,datetime=now,amount=0,pts_earned=2,pts_deducted=0,reward_redeemed="",stamp_added=FALSE)
      all_transactions(append(all_transactions(),list(txn)))
    }
    removeModal()
    showNotification(paste("+2 pts each for",m1[[1]]$name,"and",m2[[1]]$name,"!"),type="message",duration=4)
  })
  
  observeEvent(input$btn_help, {
    showModal(modalDialog(title="Points Guide",
                          tags$h5("How to Earn"),
                          tags$p("Every P30 spent = 1 point"),tags$p("Starred drinks: +1 bonus point"),
                          tags$p("Drink + croffle/sandwich: +2 bonus points"),tags$p("Double Points Hour: 2x total"),
                          tags$hr(),
                          tags$h5("Rewards"),
                          tags$p("10 pts - Free add-on"),tags$p("15 pts - Free size upgrade or P10 off"),
                          tags$p("25 pts - Free Americano / Cafe Latte"),tags$p("30 pts - Free drink (up to P130)"),
                          tags$p("40 pts - Free drink (any menu item)"),tags$p("55 pts - Free drink + croffle"),
                          tags$hr(),
                          tags$h5("Rules"),
                          tags$p("Points valid 90 days from purchase"),tags$p("1 reward per transaction"),
                          tags$p("9 stamps = 1 FREE drink (then resets)"),
                          footer=modalButton("Close"),easyClose=TRUE))
  })
  
  observeEvent(input$create_promo, {
    showModal(modalDialog(title="Create New Promo", size="l", easyClose=FALSE,
                          selectInput("np_type","Promo Type",choices=promo_types),
                          textInput("np_title","Promo Title *",placeholder="e.g. Rainy Day Combo"),
                          tags$hr(), uiOutput("np_fields"), tags$hr(),
                          textInput("np_terms","Terms (optional)",placeholder="e.g. One per customer."),
                          tags$hr(),
                          fluidRow(
                            column(6, dateInput("np_start","Start Date",value=Sys.Date())),
                            column(6, dateInput("np_end",  "End Date",  value=Sys.Date()+30))
                          ),
                          selectInput("np_recurring","Recurring",choices=recurring_opts,selected="none"),
                          checkboxInput("np_visible","Visible to customers immediately",value=TRUE),
                          footer=tagList(modalButton("Cancel"),actionButton("save_promo","Create Promo"))))
  })
  
  output$np_fields = renderUI({
    type  = input$np_type; if (is.null(type)) return(NULL)
    if (length(menu_items()) == 0) menu_items(default_menu_items)
    items = menu_items()
    choice_labels = function(xs) unlist(lapply(xs, function(x) {
      label = paste0(x$name, " (", x$cat, ")")
      setNames(label, label)
    }))
    drink_items = Filter(function(x) x$cat %in% c("Espresso Iced","Espresso Hot","Ice Blended Espresso","Ice Blended Cream","Non-Coffee","Starred Drinks"), items)
    snack_items = Filter(function(x) x$cat=="Snacks", items)
    drinks = c("(Any drink)"="", choice_labels(drink_items))
    snacks = c("(Any snack)"="", choice_labels(snack_items))
    all_item_choices = choice_labels(items)
    switch(type,
           combo   = tagList(selectInput("np_combo_drink","Drink",choices=drinks), selectInput("np_combo_snack","Snack",choices=snacks), numericInput("np_disc_price","Special Price (P)",value=NA,min=0)),
           bogo    = tagList(selectInput("np_bogo_item","BOGO Item",choices=c("(Any)"="",all_item_choices))),
           percent = tagList(selectInput("np_pct","Discount %",choices=c("5%"="5","10%"="10","15%"="15","20%"="20","25%"="25","30%"="30","50%"="50")), textInput("np_pct_applies","Applies to",placeholder="e.g. All iced drinks")),
           fixed   = tagList(selectInput("np_fixed","Fixed Discount",choices=c("P10 off"="10","P15 off"="15","P20 off"="20","P25 off"="25","P30 off"="30","P50 off"="50")), selectInput("np_fixed_items","Applies to Item(s)",choices=all_item_choices,multiple=TRUE), numericInput("np_fixed_min","Min Spend (P, optional)",value=NA,min=0)),
           lto     = tagList(selectInput("np_lto_item","Featured Item",choices=c("(Custom)"="",all_item_choices)), numericInput("np_lto_price","Special Price (P)",value=NA,min=0)),
           NULL
    )
  })

  output$ep_fields = renderUI({
    pid = session$userData$edit_promo_id
    p = Filter(function(x) x$id == pid, promos())
    if (length(p) == 0) return(NULL)
    p = p[[1]]
    type = input$ep_type %||% p$type
    if (length(menu_items()) == 0) menu_items(default_menu_items)
    items = menu_items()
    choice_labels = function(xs) unlist(lapply(xs, function(x) {
      label = paste0(x$name, " (", x$cat, ")")
      setNames(label, label)
    }))
    drink_items = Filter(function(x) x$cat %in% c("Espresso Iced","Espresso Hot","Ice Blended Espresso","Ice Blended Cream","Non-Coffee","Starred Drinks"), items)
    snack_items = Filter(function(x) x$cat=="Snacks", items)
    drinks = c("(Any drink)"="", choice_labels(drink_items))
    snacks = c("(Any snack)"="", choice_labels(snack_items))
    all_item_choices = choice_labels(items)
    switch(type,
           combo   = tagList(selectInput("ep_combo_drink","Drink",choices=drinks,selected=(p$combo_items %||% c(""))[1]),
                             selectInput("ep_combo_snack","Snack",choices=snacks,selected=(p$combo_items %||% c("",""))[2]),
                             numericInput("ep_disc_price","Special Price (P)",value=p$disc_price %||% NA,min=0)),
           bogo    = tagList(selectInput("ep_bogo_item","BOGO Item",choices=c("(Any)"="",all_item_choices),selected=p$bogo_item %||% "")),
           percent = tagList(selectInput("ep_pct","Discount %",choices=c("5%"="5","10%"="10","15%"="15","20%"="20","25%"="25","30%"="30","50%"="50"),selected=as.character(p$pct %||% "10")),
                             textInput("ep_pct_applies","Applies to",value=p$pct_applies %||% "",placeholder="e.g. All iced drinks")),
           fixed   = tagList(selectInput("ep_fixed","Fixed Discount",choices=c("P10 off"="10","P15 off"="15","P20 off"="20","P25 off"="25","P30 off"="30","P50 off"="50"),selected=as.character(p$fixed_disc %||% "10")),
                             selectInput("ep_fixed_items","Applies to Item(s)",choices=all_item_choices,selected=p$fixed_items %||% character(0),multiple=TRUE),
                             numericInput("ep_fixed_min","Min Spend (P, optional)",value=p$fixed_min %||% NA,min=0)),
           lto     = tagList(selectInput("ep_lto_item","Featured Item",choices=c("(Custom)"="",all_item_choices),selected=p$bogo_item %||% ""),
                             numericInput("ep_lto_price","Special Price (P)",value=p$disc_price %||% NA,min=0)),
           NULL
    )
  })
  
  observeEvent(input$save_promo, {
    title = trimws(input$np_title); type = input$np_type
    if (nchar(title) == 0) { showNotification("Title required!",type="error"); return() }
    pid = promo_id_ctr(); promo_id_ctr(pid+1)
    combo_items = NULL; bogo_item = NULL; pct = NULL; fixed_disc = NULL; fixed_items = NULL
    disc_price  = NULL; pct_applies = NULL; fixed_min = NULL
    if      (type=="combo")   { d = input$np_combo_drink; s = input$np_combo_snack; combo_items = c(if(!is.null(d)&&nchar(d)>0)d else "Any drink", if(!is.null(s)&&nchar(s)>0)s else "Any snack"); disc_price = input$np_disc_price }
    else if (type=="bogo")    { bogo_item = input$np_bogo_item }
    else if (type=="percent") { pct = as.numeric(input$np_pct); pct_applies = trimws(input$np_pct_applies) }
    else if (type=="fixed")   { fixed_disc = as.numeric(input$np_fixed); fixed_items = input$np_fixed_items; fixed_min = input$np_fixed_min }
    else if (type=="lto")     { bogo_item = input$np_lto_item; disc_price = input$np_lto_price }
    new_p = list(
      id=pid, type=type, title=title,
      terms=trimws(if(!is.null(input$np_terms))input$np_terms else ""),
      combo_items=combo_items, bogo_item=bogo_item, pct=pct, pct_applies=pct_applies,
      fixed_disc=fixed_disc, fixed_items=fixed_items, fixed_min=fixed_min, disc_price=disc_price,
      start_date=as.character(as.POSIXct(paste(as.character(input$np_start),"00:00:00"))),
      end_date  =as.character(as.POSIXct(paste(as.character(input$np_end),  "23:59:59"))),
      recurring=input$np_recurring, visible=isTRUE(input$np_visible), created_at=ph_time()
    )
    promos(append(promos(), list(new_p))); removeModal()
    showNotification(paste("Promo created:", title),type="message",duration=4)
  })

  observeEvent(input$save_edit_promo, {
    pid = session$userData$edit_promo_id
    title = trimws(input$ep_title %||% "")
    type = input$ep_type
    if (is.null(pid) || nchar(title) == 0) { showNotification("Promo title required.", type="error"); return() }
    combo_items = NULL; bogo_item = NULL; pct = NULL; fixed_disc = NULL; fixed_items = NULL
    disc_price  = NULL; pct_applies = NULL; fixed_min = NULL
    if      (type=="combo")   { d = input$ep_combo_drink; s = input$ep_combo_snack; combo_items = c(if(!is.null(d)&&nchar(d)>0)d else "Any drink", if(!is.null(s)&&nchar(s)>0)s else "Any snack"); disc_price = input$ep_disc_price }
    else if (type=="bogo")    { bogo_item = input$ep_bogo_item }
    else if (type=="percent") { pct = as.numeric(input$ep_pct); pct_applies = trimws(input$ep_pct_applies %||% "") }
    else if (type=="fixed")   { fixed_disc = as.numeric(input$ep_fixed); fixed_items = input$ep_fixed_items; fixed_min = input$ep_fixed_min }
    else if (type=="lto")     { bogo_item = input$ep_lto_item; disc_price = input$ep_lto_price }
    updated = lapply(promos(), function(p) {
      if (p$id == pid) {
        p$type = type
        p$title = title
        p$terms = trimws(input$ep_terms %||% "")
        p$combo_items = combo_items
        p$bogo_item = bogo_item
        p$pct = pct
        p$pct_applies = pct_applies
        p$fixed_disc = fixed_disc
        p$fixed_items = fixed_items
        p$fixed_min = fixed_min
        p$disc_price = disc_price
        p$start_date = as.character(as.POSIXct(paste(as.character(input$ep_start),"00:00:00")))
        p$end_date   = as.character(as.POSIXct(paste(as.character(input$ep_end),  "23:59:59")))
        p$recurring = input$ep_recurring
        p$visible = isTRUE(input$ep_visible)
      }
      p
    })
    promos(updated)
    session$userData$edit_promo_id = NULL
    removeModal()
    showNotification("Promo updated.", type="message", duration=3)
  })
  
  observeEvent(input$edit_promo_delete, {
    pid = session$userData$edit_promo_id
    p = Filter(function(x) x$id == pid, promos())
    if (length(p) == 0) { showNotification("Promo not found.", type="error"); return() }
    showModal(modalDialog(title="Delete Promo",
                          tags$p(paste0("Delete promo: ", p[[1]]$title, "?")),
                          footer=tagList(modalButton("Cancel"), actionButton("confirm_delete_edit_promo","Yes, Delete Promo",
                                                                              style="background:rgba(235,87,87,0.25);color:#ff8585;border:1px solid rgba(235,87,87,0.5);padding:8px 18px;border-radius:8px;cursor:pointer;")),
                          easyClose=TRUE))
  })
  
  observeEvent(input$confirm_delete_edit_promo, {
    pid = session$userData$edit_promo_id
    if (is.null(pid)) { removeModal(); return() }
    promos(Filter(function(p) p$id != pid, promos()))
    session$userData$edit_promo_id = NULL
    removeModal()
    showNotification("Promo deleted.", type="message", duration=3)
  })
  
  lapply(1:500, function(pid) {
    local({ p_id = pid
    observeEvent(input[[paste0("ptv_",p_id)]], {
      updated = lapply(promos(), function(p) { if (p$id==p_id) p$visible = !isTRUE(p$visible); p })
      promos(updated)
      tp = Filter(function(p) p$id==p_id, updated)
      if (length(tp)>0) showNotification(if(isTRUE(tp[[1]]$visible))"Promo shown" else "Promo hidden",type="message",duration=3)
    }, ignoreInit=TRUE)
    observeEvent(input[[paste0("pedit_",p_id)]], {
      p = Filter(function(x) x$id == p_id, promos())
      if (length(p) == 0) { showNotification("Promo not found.", type="error"); return() }
      p = p[[1]]
      session$userData$edit_promo_id = p_id
      start_val = if (!is.null(p$start_date) && nchar(p$start_date)>0) as.Date(as.POSIXct(p$start_date)) else Sys.Date()
      end_val = if (!is.null(p$end_date) && nchar(p$end_date)>0) as.Date(as.POSIXct(p$end_date)) else Sys.Date()+30
      showModal(modalDialog(title=paste("Edit Promo -", p$title), size="l", easyClose=TRUE,
                            selectInput("ep_type","Promo Type",choices=promo_types,selected=p$type),
                            textInput("ep_title","Promo Title",value=p$title),
                            tags$hr(), uiOutput("ep_fields"), tags$hr(),
                            textInput("ep_terms","Terms (optional)",value=p$terms %||% ""),
                            tags$hr(),
                            fluidRow(
                              column(6, dateInput("ep_start","Start Date",value=start_val)),
                              column(6, dateInput("ep_end",  "End Date",  value=end_val))
                            ),
                            selectInput("ep_recurring","Recurring",choices=recurring_opts,selected=p$recurring %||% "none"),
                            checkboxInput("ep_visible","Visible to customers",value=isTRUE(p$visible)),
                            footer=tagList(
                              tags$button(type="button", class="edit-danger-btn", onclick="Shiny.setInputValue('edit_promo_delete',Math.random())", "Delete Promo"),
                              modalButton("Cancel"),
                              actionButton("save_edit_promo","Save Changes")
                            )))
    }, ignoreInit=TRUE)
    })
  })
}

shinyApp(ui, server)






