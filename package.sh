#!/bin/bash -x

PWD=`pwd`
DIR=`basename ${PWD}`
RELEASE_VERSION=0.1

cd ..
ln -sf ${DIR} ${DIR}_${RELEASE_VERSION}
tar -cvzf ${DIR}_${RELEASE_VERSION}.tar.gz ${DIR}_${RELEASE_VERSION}/*.asd ${DIR}_${RELEASE_VERSION}/*.lisp
gpg -b -a ${DIR}_${RELEASE_VERSION}.tar.gz
scp ${DIR}_${RELEASE_VERSION}.tar.gz* stkni@shuttle:/data/apache/hackinghat.com/releases
rm ${DIR}_${RELEASE_VERSION}.tar.gz*
rm ${DIR}_${RELEASE_VERSION}
cd -
