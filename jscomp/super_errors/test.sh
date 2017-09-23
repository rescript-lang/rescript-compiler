thisScriptDir=$(dirname "$0")
testPath="$thisScriptDir/tests"
tests="$testPath/source/*.ml"

mkdir -p "$testPath/result"
for sourceFile in $tests
do
  base=$(basename "$sourceFile")
  expectedFile=$testPath/result/$base.expected
  expected=$(cat $expectedFile)
  actualFile=$testPath/result/$base.actual

  ../../bin/bsc.exe -bs-super-errors -color always -w -40+6+7+27+32..39+44+45 -o test -impl ${sourceFile} 2> $actualFile
  # uncomment this next line to override the old tests
  # ../../bin/bsc.exe -bs-super-errors -color always -w -40+6+7+27+32..39+44+45 -o test -impl ${sourceFile} 2> $expectedFile

  actual=$(cat $actualFile)

  if [[ "$expected" = "$actual" ]]; then
    echo "Testing $base: ok"
  else
    echo "Testing $base: wrong"
    # show the error
    diff -u $expectedFile $actualFile
  fi

done

tests="$testPath/source/*.re"

for sourceFile in $tests
do
  base=$(basename "$sourceFile")
  expectedFile=$testPath/result/$base.expected
  expected=$(cat $expectedFile)
  actualFile=$testPath/result/$base.actual

  ../../bin/bsc.exe -bs-super-errors -pp "../../bin/refmt.exe --print binary" -color always -w -40+6+7+27+32..39+44+45 -o test -impl ${sourceFile} 2> $actualFile
  # uncomment this next line to override the old tests
  # ../../bin/bsc.exe -bs-super-errors -pp "../../bin/refmt.exe --print binary" -color always -w -40+6+7+27+32..39+44+45 -o test -impl ${sourceFile} 2> $expectedFile

  actual=$(cat $actualFile)

  if [[ "$expected" = "$actual" ]]; then
    echo "Testing $base: ok"
  else
    echo "Testing $base: wrong"
    # show the error
    diff -u $expectedFile $actualFile
  fi

done

