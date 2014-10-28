#!/bin/bash

# To use ILTP with Imogen, you need to generate the problem files
# in the Imogen format.  

if [ ! $(which eclipse) ]; then
    echo "Install Eclipse prolog first!"
    exit 1
fi

ILTP_HOME=$(dirname $(readlink -f $0))

# Unpack the archive
cd $ILTP_HOME
tar -xzf ILTP-v1.1.2-firstorder.tar.gz

# Enter the source dir
cd ./ILTP-v1.1.2-firstorder
ILTP_SRC=$(pwd)

# The location for the generated problem files
DEST_DIR=$ILTP_HOME/../problems
mkdir -p $DEST_DIR

# Set up the sources
ln -s $ILTP_HOME/etc/format.imogen TPTP2X

# Patch
patch -p1 < $ILTP_HOME/etc/iltp.patch

# A bit of a kludge.  Answer with the default value for the
# first 4 questions, then answer 'a' to generate all formats
(echo $(pwd); echo -e "\n"; echo -e "\n"; echo -e "\n"; echo "a") | TPTP2X/tptp2X_install 

TPTP2X/tptp2X -t stdfof+add_equality -f imogen

# The above steps will generate the files, but the headers will be
# removed.  We use the headers to decide whether Imogen is working
# correctly.

files=$(find . -name "*.imo")
echo "Removing files in $DEST_DIR"
rm -rf $DEST_DIR/*
for file in $files; do
    echo "Getting header for: $file"
    prob=$(echo $file | sed -e 's/.*\///' -e 's/\+[^0-9].*//')
    class=$(echo $prob | cut -c1-3)
    orig="$ILTP_SRC/Problems/$class/$prob.p"
    header=$(cat $orig | grep % | grep -v %----)
    old=$(readlink -f $file)
    new="$DEST_DIR/$prob.imo"
    # echo "file: $file"
    # echo "orig: $orig"
    # echo "old: $old"
    # echo "new: $new"
    # echo "header: $header"
    rm -f $new
    echo "$header" > $new
    cat $old >> $new
done

echo "Done!"
