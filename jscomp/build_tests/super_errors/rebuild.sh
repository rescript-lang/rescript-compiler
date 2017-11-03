DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export PATH=$DIR/../../bin:$PATH
echo `which bsc.exe`
echo `which refmt3.exe`
node $DIR/index.js
