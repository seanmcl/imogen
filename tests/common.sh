set -e -o pipefail

# An interrupt should jump out of loops
function quit {
    echo
    exit 1
}

trap quit INT

timefile=/tmp/imogen-time-tmp
resultfile=/tmp/imogen-result-tmp

function find-imogen-home {
    local oldcwd=$(pwd)
    # Look for the bin/build file
    while [ ! -e bin/build ]; do
        cd ..
        cwd=$(pwd)
        if [ $cwd = "/" ]; then
            echo "Can't find Imogen root dir."
            exit 1
        fi
    done
    export IMOGEN_HOME=$(pwd)
    cd $oldcwd
}

if [[ -z "$IMOGEN_HOME" ]]; then
    find-imogen-home
fi

echo "IMOGEN_HOME: $IMOGEN_HOME"

function ptime {
    TIMEFORMAT=%R
    (trap quit INT; time eval $@) 2> $timefile > $resultfile
    t=$(cat $timefile)
    r=$(cat $resultfile)
    printf "%-12s Time: %s\n" $r $t
}

function emit {
    echo -e -n "\r$1"
}
