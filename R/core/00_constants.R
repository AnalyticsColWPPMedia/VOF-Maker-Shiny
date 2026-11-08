# R/core/00_constants.R
# Global constants shared by every layer of the app. Business rules that are
# "configuration" (keywords, schemas, tags) live here so they can be changed in
# a single place.

# ------------------------------------------------------------------------------
# MFF dimensions
# ------------------------------------------------------------------------------
# Candidate cross-sectional dimensions of the MFF, in the order AWB uses to build
# Analytical column names: VariableName_Geography_Product_Campaign_Outlet_Creative
# (the cross-sectional dimensions are dropped from the name).
MFF_DIMS <- c("Geography", "Product", "Campaign", "Outlet", "Creative")

# Time column of the Analytical.
PERIOD_COL <- "Period"

# Separator used by AWB between VariableName and the longitudinal dimensions.
MFF_NAME_SEP <- "_"

# Tags used in media VOF names for each cross-sectional filter.
CS_NAME_TAGS <- c(Geography = "--g", Product = "--prod", Campaign = "--camp",
                  Outlet = "--out", Creative = "--creat")

# Maximum number of "CS Children" splits handled per dimension.
MAX_CS_CHILDREN <- 10

# ------------------------------------------------------------------------------
# Media VOFs
# ------------------------------------------------------------------------------
EFFECT_CHOICES <- c("", "Direct", "Halo")

# Every media VOF name ends with " ---<metric suffix>" (e.g. " ---Imps", " ---Spend").
# The space keeps the metric apart from the tags ("Channel --p 20250101- ---Imps").
# Older names without the space ("X---Imps") are still read (METRIC_SEP_REGEX).
METRIC_SEP <- " ---"
METRIC_SEP_REGEX <- "\\s*---"

# Metric dictionary. `Keyword` is how the metric appears (whole word, any case)
# in the VariableName part of MFF columns; `Aliases` are other spellings found
# in MFFs. Suffix rule: words of 7+ letters are abbreviated to 4 letters,
# shorter words are kept. Every spend synonym maps to "Spend". The order of
# the spend rows is the search order when looking for a spend twin.
METRICS <- tibble::tribble(
  ~Metric,          ~Type,      ~Suffix,       ~Aliases,
  "Impressions",    "Activity", "Imps",        "Impression|Imps",
  "Clicks",         "Activity", "Clicks",      "Click",
  "Views",          "Activity", "Views",       "View",
  "Completed Views","Activity", "Cmpl Views",  "Complete Views|Video Completions",
  "GRPs",           "Activity", "GRPs",        "GRP",
  "TRPs",           "Activity", "TRPs",        "TRP",
  "Reach",          "Activity", "Reach",       "",
  "Sessions",       "Activity", "Sess",        "Session",
  "Visits",         "Activity", "Visits",      "Visit",
  "Actions",        "Activity", "Acts",        "Action",
  "Circulation",    "Activity", "Circ",        "",
  "Engagements",    "Activity", "Engm",        "Engagement",
  "Conversions",    "Activity", "Conv",        "Conversion",
  "Downloads",      "Activity", "Dnld",        "Download",
  "Installs",       "Activity", "Inst",        "Install",
  "Deliveries",     "Activity", "Dlvr",        "Delivery|Delivered",
  "Sendouts",       "Activity", "Send",        "Sendout|Sends",
  "Opens",          "Activity", "Opens",       "Open",
  "Followers",      "Activity", "Flwr",        "Follower",
  "Interactions",   "Activity", "Intr",        "Interaction",
  "Listens",        "Activity", "Lstn",        "Listen",
  "Streams",        "Activity", "Strm",        "Stream",
  "Spots",          "Activity", "Spots",       "Spot",
  "Insertions",     "Activity", "Insr",        "Insertion",
  "Placements",     "Activity", "Plcm",        "Placement",
  "Subscribers",    "Activity", "Subs",        "Subscriber",
  "Registrations",  "Activity", "Regs",        "Registration",
  "Pageviews",      "Activity", "Pgvw",        "Page Views|Pageview",
  "Mentions",       "Activity", "Mntn",        "Mention",
  "Comments",       "Activity", "Cmnt",        "Comment",
  "Shares",         "Activity", "Shares",      "Share",
  "Likes",          "Activity", "Likes",       "Like",
  "Leads",          "Activity", "Leads",       "Lead",
  "Units",          "Activity", "Units",       "Unit",
  "Attendees",      "Activity", "Attd",        "Attendee",
  "Spend",          "Spend",    "Spend",       "",
  "Cost",           "Spend",    "Spend",       "Costs",
  "Net Cost",       "Spend",    "Spend",       "",
  "Gross Cost",     "Spend",    "Spend",       "",
  "Media Cost",     "Spend",    "Spend",       "",
  "Investment",     "Spend",    "Spend",       "Investments"
)

# Preferred activity metric of a channel when a spend variable has several
# activity twins (pre-selection in the twin dialog). Matched in order against
# the VariableName (media channel) typed by the user.
CHANNEL_METRIC_RULES <- tibble::tribble(
  ~Pattern,                                            ~Metric,
  "Search|SEM|PPC",                                    "Clicks",
  "OLV|Online Video|Video|YouTube|CTV|Connected TV|VOD", "Views",
  "Social|Display|Programmatic|Native|Banner|Meta|Facebook|Instagram|TikTok", "Impressions",
  "OOH|Out of Home|Billboard|DOOH",                    "Impressions",
  "TV|Television|Radio|Audio",                         "GRPs",
  "Print|Magazine|Newspaper|Press",                    "Circulation",
  "CRM|Email|E-mail",                                  "Deliveries",
  "Podcast|Streaming",                                 "Listens"
)

# ------------------------------------------------------------------------------
# Pre-filled VariableName (root of the selected variable)
# ------------------------------------------------------------------------------
# Channel names written as their usual abbreviation. Phrase: alternatives
# separated by "|", matched as whole words, any case, any special characters
# between words ("Out-of-Home" = "Out of Home", "P-MAX" = "P MAX"). Longer
# phrases are tried first ("Online Video CTV" -> CTV before "Online Video" ->
# OLV). Applied before special characters are removed. Add rows here.
CHANNEL_ABBREVIATIONS <- tibble::tribble(
  ~Phrase,                                                        ~Abbreviation,
  "Online Video CTV|OLV CTV",                                     "CTV",
  "Online Video",                                                 "OLV",
  "Connected TV|Connected Television|Connected Video",            "CTV",
  "Advanced TV|Advanced Television",                              "ATV",
  "Over The Top",                                                 "OTT",
  "Digital Out of Home|Digital OOH",                              "DOOH",
  "Out of Home",                                                  "OOH",
  "Broadcast Video on Demand",                                    "BVOD",
  "Advertising Video on Demand",                                  "AVOD",
  "Subscription Video on Demand",                                 "SVOD",
  "Video on Demand",                                              "VOD",
  "Television",                                                   "TV",
  "Search Engine Marketing",                                      "SEM",
  "Search Engine Optimization|Search Engine Optimisation",       "SEO",
  "Pay Per Click",                                                "PPC",
  "Customer Relationship Management",                             "CRM",
  "Public Relations",                                             "PR",
  "Point of Sale",                                                "POS",
  "Direct to Consumer",                                           "DTC",
  "P MAX|Performance Max",                                        "Pmax",
  "E mail",                                                       "Email",
  "E commerce",                                                   "eCommerce"
)

# Words always written in capitals (acronyms), whatever case they come in.
# Any other word is written with the first letter in capitals and the rest in
# lower case ("DIRECT" -> "Direct", "PMAX" -> "Pmax").
NAME_ACRONYMS <- c(
  "TV", "CTV", "ATV", "OTT", "OLV", "VOD", "BVOD", "AVOD", "SVOD", "FAST", "IPTV",
  "OOH", "DOOH", "SEM", "SEO", "PPC", "DSA", "CRM", "SMS", "PR", "POS",
  "DTC", "D2C", "B2B", "B2C", "CPG", "GRP", "TRP", "KPI", "ROI", "US", "UK", "EU",
  "NFL", "NBA", "MLB", "NHL", "AI", "AR", "VR", "SP", "NASDAQ"
)
# Brand names with their own casing (lower-case key -> written form).
NAME_SPECIAL_CASING <- c(
  youtube = "YouTube", tiktok = "TikTok", linkedin = "LinkedIn", whatsapp = "WhatsApp",
  iheartradio = "iHeartRadio", iheart = "iHeart", ecommerce = "eCommerce"
)

# Label of a backcasted VOF (name prefix).
BACKCASTED_LABEL <- "Backcasted"

# Words written in lower case inside Title Case names (team naming convention).
TITLE_CASE_SMALL_WORDS <- c("the", "to", "a", "an", "and", "of", "in", "on")

# Separator between VariableNames (and between Complements) in VOF names.
NAME_AND <- " and "
# Separator between the VariableName part and the Complement part.
NAME_COMPLEMENT_SEP <- " - "

# ------------------------------------------------------------------------------
# Cross-sectional filters
# ------------------------------------------------------------------------------
CS_FILTER_MODES   <- c(Include = "include", Exclude = "exclude")
CS_FILTER_MATCHES <- c("Exact", "StartsWith", "Contains", "EndsWith")
# Name flag: show the entity list only up to this many values ("Atlanta etc"
# otherwise); "topN" when the set is the N largest entities of the roots.
CS_FLAG_GROUP_PREFIX <- "group"
CS_FLAG_TOP_PREFIX   <- "top"

# ------------------------------------------------------------------------------
# KPI
# ------------------------------------------------------------------------------
WEIGHT_VAR_NAME    <- "Weight Variable MMM"
# Column created in Data with the business cycle of each period. Kept as
# "FiscalYear" for compatibility with existing AWB projects.
BUSINESS_CYCLE_VAR_NAME <- "FiscalYear"
OUT_OF_SCOPE_LABEL <- "OUT OF MODEL SCOPE"
WEEKS_PER_CYCLE    <- 52
CYCLE_METHODS <- c("Trailing 52-week cycles (from the last date)" = "trailing",
                   "Calendar years" = "calendar",
                   "Custom table (manual / upload)" = "custom")

# ------------------------------------------------------------------------------
# VOF Metadata schema
# ------------------------------------------------------------------------------
# Final column order is: static columns, active CS dimensions, end columns.
# All columns are CamelCase. Each active CS dimension adds three columns:
#   <Dim>       values (exact entities) or quoted positional patterns
#   <Dim>Filter "Include|Exact", "Exclude|StartsWith;EndsWith", ...
#   <Dim>Group  label written after the dimension tag in the VOF name
#               ("top5", "group1", "uis", "Atlanta etc"); "" when the rows of
#               the VOF use different filters (the name only shows the tag).
# BackcastingStart/End: calibration window of a backcasted VOF ("" otherwise).
# Metric: Impressions, Clicks, ..., Spend (KPI, Weight, External for others).
METADATA_STATIC_COLS <- c("Type", "MainModelVariableName", "AnalyticalVariableName",
                          "VofVariableName", "VofComplement", "Effect",
                          "MinPeriod", "MaxPeriod",
                          "BackcastingStart", "BackcastingEnd")
METADATA_END_COLS  <- c("Metric")
METADATA_DIM_SUFFIXES <- c("", "Filter", "Group")

# Internal column (kept in the app and the browser copy, never exported): code
# of non-media VOFs (KPI, weight, external), which cannot be rebuilt from the
# metadata columns. Media code is always rebuilt from the metadata.
METADATA_CODE_COL <- ".Code"

# Column renames applied when importing older metadata files.
METADATA_LEGACY_COLS <- c(MediaChannel = "VofVariableName", SubChannel = "VofComplement",
                          Backcasting_Start = "BackcastingStart", Backcasting_End = "BackcastingEnd")
METADATA_LEGACY_DIM_SUFFIXES <- c("_Filter" = "Filter", "_Group" = "Group", "Flag" = "Group")
METADATA_DATE_COLS <- c("MinPeriod", "MaxPeriod", "BackcastingStart", "BackcastingEnd")

# Legacy metadata files used plural dimension names.
METADATA_LEGACY_DIM_COLS <- c(Geographies = "Geography", Products = "Product",
                              Campaigns = "Campaign", Outlets = "Outlet",
                              Creatives = "Creative")

METADATA_TYPES <- c(media = "Media", kpi_log = "KPI_Log", weight = "Weight_Var",
                    external = "External", dummy = "Dummy")

# ------------------------------------------------------------------------------
# Purifier
# ------------------------------------------------------------------------------
# Model Details variables matching this pattern are not expected to come from
# the VOF Maker media builds, so they never trigger the "missing VOF" warning.
PURIFY_NON_MEDIA_PATTERN <- "(?i)^(Business|Macro|Seasonal|Season|Holiday|Dummy)\\b"

# Used only to sort the "missing VOF" warning list (media first).
MEDIA_KEYWORDS_PATTERN <- paste0(
  "(?i)Display|Banner|Native|OLV|Online Video|Connected TV|CTV|Magazine|",
  "Newspaper|Event|Social|Search|OOH|TV|Radio|Cinema|Digital|Audio|Podcast|",
  "Influencer|Affiliate|Programmatic|VOD|Youtube|Meta|Facebook|Instagram|",
  "TikTok|Google|Bing|Pinterest|Snapchat"
)

