"""Builds data/holiday_calendar.csv for the VOF Maker Dummies tab.

Public holidays come from the `holidays` package (pip install holidays);
commercial dates (Valentines, Mothers/Fathers Day, Black Friday, ...) follow
the rules below. Names are English, ASCII, without apostrophes or parentheses
(team naming convention). Run from the app root:

    python tools/build_holiday_calendar.py

To add a market, add its ISO code to MARKETS (and its commercial rules if they
differ from the defaults), then run the script again.
"""
import csv, re, unicodedata
from datetime import date, timedelta
from dateutil.easter import easter
import holidays

YEARS = range(2015, 2036)
MARKETS = {
    "US": "United States", "CA": "Canada", "GB": "United Kingdom", "DE": "Germany", "FR": "France",
    "ES": "Spain", "IT": "Italy", "NL": "Netherlands", "BE": "Belgium", "PT": "Portugal", "IE": "Ireland",
    "AT": "Austria", "CH": "Switzerland", "SE": "Sweden", "PL": "Poland",
    "BR": "Brazil", "CO": "Colombia", "CL": "Chile", "MX": "Mexico", "AR": "Argentina", "PE": "Peru", "EC": "Ecuador",
    "AU": "Australia", "JP": "Japan", "CN": "China", "KR": "South Korea", "IN": "India",
}

def nth_weekday(year, month, weekday, n):
    """n-th weekday (Mon=0) of a month; n=-1 for the last one."""
    if n > 0:
        d = date(year, month, 1)
        d += timedelta(days=(weekday - d.weekday()) % 7)
        return d + timedelta(weeks=n - 1)
    d = date(year, month + 1, 1) - timedelta(days=1) if month < 12 else date(year, 12, 31)
    return d - timedelta(days=(d.weekday() - weekday) % 7)

SUN, THU, SAT = 6, 3, 5

def mothers_day(m, y):
    if m == "MX": return date(y, 5, 10)
    if m in ("GB", "IE"): return easter(y) - timedelta(days=21)          # Mothering Sunday
    if m == "FR":
        d = nth_weekday(y, 5, SUN, -1)
        return d + timedelta(weeks=1) if d == easter(y) + timedelta(days=49) else d  # moved if Pentecost
    if m in ("ES", "PT"): return nth_weekday(y, 5, SUN, 1)
    if m == "SE": return nth_weekday(y, 5, SUN, -1)
    if m == "PL": return date(y, 5, 26)
    if m == "AR": return nth_weekday(y, 10, SUN, 3)
    if m == "KR": return date(y, 5, 8)                                      # Parents Day
    return nth_weekday(y, 5, SUN, 2)

def fathers_day(m, y):
    if m == "DE": return easter(y) + timedelta(days=39)                    # Ascension Day
    if m in ("IT", "ES", "PT"): return date(y, 3, 19)
    if m == "AU": return nth_weekday(y, 9, SUN, 1)
    if m == "BR": return nth_weekday(y, 8, SUN, 2)
    if m == "SE": return nth_weekday(y, 11, SUN, 2)
    if m in ("AT", "BE"): return nth_weekday(y, 6, SUN, 2)
    if m == "CH": return nth_weekday(y, 6, SUN, 1)
    if m == "PL": return date(y, 6, 23)
    if m == "KR": return None                                               # same day as Parents Day
    return nth_weekday(y, 6, SUN, 3)

def commercial(m, y):
    thanksgiving_us = nth_weekday(y, 11, THU, 4)
    out = [("Mothers Day", mothers_day(m, y)), ("Fathers Day", fathers_day(m, y)),
           ("Black Friday", thanksgiving_us + timedelta(days=1)), ("Cyber Monday", thanksgiving_us + timedelta(days=4)),
           ("Christmas Eve", date(y, 12, 24)), ("New Years Eve", date(y, 12, 31))]
    if m not in ("CN", "KR", "JP", "IN"): out.append(("Easter Sunday", easter(y)))
    out.append(("Valentines Day", date(y, 6, 12) if m == "BR" else date(y, 2, 14)))
    if m == "CO": out.append(("Love and Friendship Day", nth_weekday(y, 9, SAT, 3)))
    if m in ("US", "CA", "GB", "IE"): out.append(("Halloween", date(y, 10, 31)))
    if m == "CN": out.append(("Singles Day", date(y, 11, 11)))
    if m == "US": out.append(("Super Bowl Sunday", nth_weekday(y, 2, SUN, 2 if y >= 2022 else 1)))
    return [(n, d) for n, d in out if d is not None]

def clean(name):
    name = re.sub(r"\s*\(.*?\)", "", name)                                # "(observed)", "(Spring Festival)"
    name = unicodedata.normalize("NFKD", name).encode("ascii", "ignore").decode()
    name = name.replace("'", "").replace("’", "")
    name = re.sub(r"[^A-Za-z0-9 \-]", " ", name)
    return re.sub(r"\s+", " ", name).strip()

rows = []
for m, market in MARKETS.items():
    for y in YEARS:
        try:
            pub = holidays.country_holidays(m, years=y, language="en_US")
        except Exception:
            pub = holidays.country_holidays(m, years=y)
        seen = set()
        for d, raw in sorted(pub.items()):
            for part in raw.split("; "):
                n = clean(part)
                if n and (n, y) not in seen:                              # keep the first date per holiday/year
                    seen.add((n, y)); rows.append((m, market, d.isoformat(), n, "Public holiday"))
        for n, d in commercial(m, y):
            rows.append((m, market, d.isoformat(), n, "Commercial"))

with open("data/holiday_calendar.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["MarketCode", "Market", "Date", "Name", "Category"])
    w.writerows(sorted(set(rows)))
print(len(rows), "rows")
