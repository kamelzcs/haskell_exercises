#! /bin/sh
#
# addGit.sh
# Copyright (C) 2016 zhao <zhao@f45c89c59701.ant.amazon.com>
#
# Distributed under terms of the MIT license.
#


for d in */ ; do
    git add "$d"src/*.hs
done
