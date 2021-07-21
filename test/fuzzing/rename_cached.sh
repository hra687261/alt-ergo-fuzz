
i=0
for f in test/fuzzing/crash_output/c_* 
do 
  i=$(($i + 1))
  opf=$(echo "$f" | awk -F'[/._]' '{print "test/fuzzing/crash_output/c_" i "." $8}' i=$i)
  mv $f $opf
done
