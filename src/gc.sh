#!/bin/bash

# */10 * * * * $HOME/golbin/utils/gc.sh 2>&1 >> $HOME/golbin/logs/gc.log

cd $HOME/golbin/data/db
git add prod
git commit -am "s db frd @ `date +%F--%T`"
git push
