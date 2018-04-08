#!/bin/sh
ulimit -n 10240
# +A 8 \
exec erl \
  +P 1002400 \
  +K true \
  -name test@127.0.0.1 \
  +zdbbl 8192 \
  -pa $PWD/deps/*/ebin \
  -pa $PWD/ebin \
  -config ./config.config \
  -boot start_sasl -s reloader -s -setcookie XEXIWPUHUJTYKXFMMTXE  
