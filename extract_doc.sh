if [[ $# != 1 ]]; then
  echo "usage $(basename $0) FILE.ml" 2>&1
  exit 1
fi

out="${1%%.ml}.html"
tmp="$(mktemp)"
trap "rm -f \"$tmp\"" EXIT

sed -e 's|(\*begin||;
 s|end\*)||;
 s|(\*\*|-----|;
 s|\*\*)|[source]\n-----|' "$1" > "$tmp"
asciidoc -o "$out" "$tmp"
