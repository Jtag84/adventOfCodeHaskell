#! /bin/zsh

YEAR=$1
DAY=$(printf "%02d" $2)

FILE="./src/Year$YEAR/Day$DAY.hs"

if [ -f "$FILE" ]; then
    echo "$FILE already exists."
    exit 1
fi

MODULE_NAME="Year$YEAR.Day$DAY"

YEAR_DAYS_MAP=`grep year$YEAR ./src/Years.hs`

if [ -z "$YEAR_DAYS_MAP" ]; then
    echo "\nyear$YEAR :: Map DayNumber Day\nyear$YEAR =\n  fromList\n    [\n      ($2, $MODULE_NAME.runDay)\n    ]" >> ./src/Years.hs
    perl -0 -p -e "s/(yearDaysMap =\n  fromList[\n\w\[ \(,.\)]*)/\1, ($YEAR, year$YEAR)/s" -i "./src/Years.hs"
    mkdir ./input/$YEAR
    mkdir ./src/Year$YEAR
else
    perl -0 -p -e "s/(year$YEAR =\n  fromList[\n\w\[ \(,.\)]*)/\1, ($2, $MODULE_NAME.runDay)/s" -i "./src/Years.hs"
fi

cp ./src/Day.template.hs $FILE

gsed -i "s/^module YearXXXX.DayXX (runDay) where$/module $MODULE_NAME (runDay) where/" "$FILE"
gsed -i "1 aimport $MODULE_NAME qualified (runDay)" "./src/Years.hs"

touch "./input/$YEAR/Day$DAY.txt"
touch "./input/$YEAR/Day${DAY}Example.txt"

ormolu -i ./src/Years.hs

curl https://adventofcode.com/$YEAR/day/$2/input --cookie "session=`cat ./cookieSession.txt`" > "./input/$YEAR/Day$DAY.txt"