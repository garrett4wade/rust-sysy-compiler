#!/bin/bash
docker run -it --rm -v $PWD:/root/compiler maxxing/compiler-dev \
    autotest -riscv -s lv5 /root/compiler