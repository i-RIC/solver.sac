export stdin=$2.stdin
if ! [ -d $2.out ]; then
  rm $2.out
fi
echo $2.dat > $stdin
echo $2.out >> $stdin
echo $3 >> $stdin
$1 < $stdin
echo cmake -E compare_files $2.out.expected $2.out
cmake -E compare_files $2.out.expected $2.out
exit $?
