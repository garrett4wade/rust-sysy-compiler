#!/bin/bash
docker run -it --rm -v $PWD:/root/compiler maxxing/compiler-dev \
    autotest -koopa -s lv9 /root/compiler