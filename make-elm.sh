find . -depth 1 -name \*.elm | while [ 0 ]
do
  read elmFile
  [ $? -eq 0 ] || break
  jsFile=${elmFile/elm/js}
  echo elm make $elmFile --output $jsFile
  elm make $elmFile --output $jsFile
done
